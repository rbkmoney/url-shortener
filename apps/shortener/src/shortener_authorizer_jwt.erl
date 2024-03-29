% TODO
% Extend interface to support proper keystore manipulation. Refactor it into a more
% general library along with `shortener_acl` and some parts of `shortener_auth`.

-module(shortener_authorizer_jwt).

%%

-export([get_child_spec/1]).
-export([init/1]).

-export([store_key/2]).

-export([issue/2]).
-export([verify/1]).

-export([get_token_id/1]).

-define(CLAIM_TOKEN_ID, <<"jti">>).
-define(CLAIM_SUBJECT_ID, <<"sub">>).
-define(CLAIM_EXPIRES_AT, <<"exp">>).

%%

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jwt.hrl").

-type keyname() :: term().
-type kid() :: binary().
-type key() :: #jose_jwk{}.
-type token() :: binary().
-type claims() :: #{binary() => term()}.
%% Added expiration to subject tuple as part of token service claims
-type subject() :: {subject_id(), shortener_acl:t(), pos_integer() | undefined}.
-type subject_id() :: binary().
-type t() :: {subject(), claims()}.
-type expiration() ::
    {lifetime, Seconds :: pos_integer()}
    | {deadline, UnixTs :: pos_integer()}
    | unlimited.

-export_type([t/0]).
-export_type([subject/0]).
-export_type([claims/0]).
-export_type([token/0]).
-export_type([expiration/0]).

%%

-type options() :: #{
    %% The set of keys used to sign issued tokens and verify signatures on such
    %% tokens.
    keyset => keyset(),
    %% The name of a key used exclusively to sign any issued token.
    %% If not set any token issue is destined to fail.
    signee => keyname()
}.

-type keyset() :: #{
    keyname() => keysource()
}.

-type keysource() ::
    {pem_file, file:filename()}.

-spec get_child_spec(options()) -> supervisor:child_spec() | no_return().
get_child_spec(Options) ->
    #{
        id => ?MODULE,
        start => {supervisor, start_link, [?MODULE, parse_options(Options)]},
        type => supervisor
    }.

parse_options(Options) ->
    Keyset = maps:get(keyset, Options, #{}),
    _ = is_map(Keyset) orelse exit({invalid_option, keyset, Keyset}),
    _ = genlib_map:foreach(
        fun(K, V) ->
            is_keysource(V) orelse exit({invalid_option, K, V})
        end,
        Keyset
    ),
    Signee = maps:find(signee, Options),
    {Keyset, Signee}.

is_keysource({pem_file, Fn}) ->
    is_list(Fn) orelse is_binary(Fn);
is_keysource(_) ->
    false.

%%

-spec init({keyset(), {ok, keyname()} | error}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Keyset, Signee}) ->
    ok = create_table(),
    KeyInfos = maps:map(fun ensure_store_key/2, Keyset),
    ok = select_signee(Signee, KeyInfos),
    {ok, {#{}, []}}.

ensure_store_key(Keyname, Source) ->
    case store_key(Keyname, Source) of
        {ok, KeyInfo} ->
            KeyInfo;
        {error, Reason} ->
            _ = logger:error("Error importing key ~p: ~p", [Keyname, Reason]),
            exit({import_error, Keyname, Source, Reason})
    end.

select_signee({ok, Keyname}, KeyInfos) ->
    case maps:find(Keyname, KeyInfos) of
        {ok, #{sign := true}} ->
            set_signee(Keyname);
        {ok, KeyInfo} ->
            _ = logger:error("Error setting signee: signing with ~p is not allowed", [Keyname]),
            exit({invalid_signee, Keyname, KeyInfo});
        error ->
            _ = logger:error("Error setting signee: no key named ~p", [Keyname]),
            exit({nonexstent_signee, Keyname})
    end;
select_signee(error, _KeyInfos) ->
    ok.

%%

-type keyinfo() :: #{
    kid => kid(),
    sign => boolean(),
    verify => boolean()
}.

-spec store_key(keyname(), {pem_file, file:filename()}) -> {ok, keyinfo()} | {error, file:posix() | {unknown_key, _}}.
store_key(Keyname, {pem_file, Filename}) ->
    store_key(Keyname, {pem_file, Filename}, #{
        kid => fun derive_kid_from_public_key_pem_entry/1
    }).

derive_kid_from_public_key_pem_entry(JWK) ->
    JWKPublic = jose_jwk:to_public(JWK),
    {_Module, PublicKey} = JWKPublic#jose_jwk.kty,
    {_PemEntry, Data, _} = public_key:pem_entry_encode('SubjectPublicKeyInfo', PublicKey),
    jose_base64url:encode(crypto:hash(sha256, Data)).

-type store_opts() :: #{
    kid => fun((key()) -> kid())
}.

-spec store_key(keyname(), {pem_file, file:filename()}, store_opts()) ->
    {ok, keyinfo()} | {error, file:posix() | {unknown_key, _}}.
store_key(Keyname, {pem_file, Filename}, Opts) ->
    case jose_jwk:from_pem_file(Filename) of
        JWK = #jose_jwk{} ->
            Key = construct_key(derive_kid(JWK, Opts), JWK),
            ok = insert_key(Keyname, Key),
            {ok, get_key_info(Key)};
        Error = {error, _} ->
            Error
    end.

get_key_info(#{kid := KID, signer := Signer, verifier := Verifier}) ->
    #{
        kid => KID,
        sign => Signer /= undefined,
        verify => Verifier /= undefined
    }.

derive_kid(JWK, #{kid := DeriveFun}) when is_function(DeriveFun, 1) ->
    DeriveFun(JWK).

construct_key(KID, JWK) ->
    #{
        jwk => JWK,
        kid => KID,
        signer =>
            try
                jose_jwk:signer(JWK)
            catch
                error:_ -> undefined
            end,
        verifier =>
            try
                jose_jwk:verifier(JWK)
            catch
                error:_ -> undefined
            end
    }.

%%

-spec issue(t(), expiration()) ->
    {ok, token()}
    | {error, nonexistent_signee}.
issue(Auth, Expiration) ->
    case get_signee_key() of
        Key = #{} ->
            Claims = construct_final_claims(Auth, Expiration),
            sign(Key, Claims);
        undefined ->
            {error, nonexistent_signee}
    end.

construct_final_claims({{Subject, ACL}, Claims}, Expiration) ->
    maps:merge(
        Claims#{
            ?CLAIM_TOKEN_ID => unique_id(),
            ?CLAIM_SUBJECT_ID => Subject,
            ?CLAIM_EXPIRES_AT => get_expires_at(Expiration)
        },
        encode_roles(shortener_acl:encode(ACL))
    ).

get_expires_at({lifetime, Lt}) ->
    genlib_time:unow() + Lt;
get_expires_at({deadline, Dl}) ->
    Dl;
get_expires_at(unlimited) ->
    0.

unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

sign(#{kid := KID, jwk := JWK, signer := #{} = JWS}, Claims) ->
    JWT = jose_jwt:sign(JWK, JWS#{<<"kid">> => KID}, Claims),
    {_Modules, Token} = jose_jws:compact(JWT),
    {ok, Token}.

%%

-spec verify(token()) ->
    {ok, t()}
    | {error,
        {invalid_token,
            badarg
            | {badarg, term()}
            | {missing, atom()}
            | expired
            | {malformed_acl, term()}}
        | {nonexistent_key, kid()}
        | invalid_operation
        | invalid_signature}.
verify(Token) ->
    try
        {_, ExpandedToken} = jose_jws:expand(Token),
        #{<<"protected">> := ProtectedHeader} = ExpandedToken,
        Header = base64url_to_map(ProtectedHeader),
        Alg = get_alg(Header),
        KID = get_kid(Header),
        verify(KID, Alg, ExpandedToken)
    catch
        %% from get_alg and get_kid
        throw:Reason ->
            {error, Reason};
        %% TODO we're losing error information here, e.g. stacktrace
        error:badarg = Reason ->
            {error, {invalid_token, Reason}};
        error:{badarg, _} = Reason ->
            {error, {invalid_token, Reason}}
    end.

verify(KID, Alg, ExpandedToken) ->
    case get_key_by_kid(KID) of
        #{jwk := JWK, verifier := Algs} ->
            _ = lists:member(Alg, Algs) orelse throw(invalid_operation),
            verify(JWK, ExpandedToken);
        undefined ->
            {error, {nonexistent_key, KID}}
    end.

verify(JWK, ExpandedToken) ->
    case jose_jwt:verify(JWK, ExpandedToken) of
        {true, #jose_jwt{fields = Claims}, _JWS} ->
            {Data = #{subject_id := SubjectID}, Claims1} = validate_claims(Claims),
            ExpiresAt = maps:get(expires_at, Data, undefined),
            get_result({SubjectID, ExpiresAt}, decode_roles(Claims1));
        {false, _JWT, _JWS} ->
            {error, invalid_signature}
    end.

validate_claims(Claims) ->
    validate_claims(Claims, get_validators(), #{}).

validate_claims(Claims, [{Name, Claim, Validator} | Rest], Acc) ->
    V = Validator(Name, maps:get(Claim, Claims, undefined)),
    validate_claims(Claims, Rest, Acc#{Name => V});
validate_claims(Claims, [], Acc) ->
    {Acc, Claims}.

get_result({SubjectID, ExpiresAt}, {Roles, Claims}) ->
    try
        Subject = {SubjectID, shortener_acl:decode(Roles), ExpiresAt},
        {ok, {Subject, Claims}}
    catch
        error:{badarg, _} = Reason ->
            throw({invalid_token, {malformed_acl, Reason}})
    end.

get_kid(#{<<"kid">> := KID}) when is_binary(KID) ->
    KID;
get_kid(#{}) ->
    throw({invalid_token, {missing, kid}}).

get_alg(#{<<"alg">> := Alg}) when is_binary(Alg) ->
    Alg;
get_alg(#{}) ->
    throw({invalid_token, {missing, alg}}).

%%

get_validators() ->
    [
        {token_id, ?CLAIM_TOKEN_ID, fun check_presence/2},
        {subject_id, ?CLAIM_SUBJECT_ID, fun check_presence/2},
        {expires_at, ?CLAIM_EXPIRES_AT, fun check_expiration/2}
    ].

check_presence(_, V) when is_binary(V) ->
    V;
check_presence(C, undefined) ->
    throw({invalid_token, {missing, C}}).

check_expiration(_, Exp = 0) ->
    Exp;
check_expiration(_, Exp) when is_integer(Exp) ->
    case genlib_time:unow() of
        % Now when Exp > Now ->
        %     Exp;
        % Expiration check is disabled.
        % See MSPF-563 for details
        _Now ->
            Exp
        % _ ->
        %     throw({invalid_token, expired})
    end;
check_expiration(C, undefined) ->
    throw({invalid_token, {missing, C}});
check_expiration(C, V) ->
    throw({invalid_token, {badarg, {C, V}}}).

%%

encode_roles(Roles) ->
    #{
        <<"resource_access">> => #{
            <<"url-shortener">> => #{
                <<"roles">> => Roles
            }
        }
    }.

decode_roles(
    Claims = #{
        <<"resource_access">> := #{
            <<"url-shortener">> := #{
                <<"roles">> := Roles
            }
        }
    }
) when is_list(Roles) ->
    {Roles, maps:remove(<<"resource_access">>, Claims)};
decode_roles(_) ->
    throw({invalid_token, {missing, acl}}).

%%

-spec get_token_id(claims()) -> binary().
get_token_id(Claims) ->
    maps:get(?CLAIM_TOKEN_ID, Claims).

%%

insert_key(Keyname, Key = #{kid := KID}) ->
    insert_values(#{
        {keyname, Keyname} => Key,
        {kid, KID} => Key
    }).

get_key_by_name(Keyname) ->
    lookup_value({keyname, Keyname}).

get_key_by_kid(KID) ->
    lookup_value({kid, KID}).

set_signee(Keyname) ->
    insert_values(#{
        signee => {keyname, Keyname}
    }).

get_signee_key() ->
    case lookup_value(signee) of
        {keyname, Keyname} ->
            get_key_by_name(Keyname);
        undefined ->
            undefined
    end.

%%

base64url_to_map(V) when is_binary(V) ->
    {ok, Decoded} = jose_base64url:decode(V),
    jsx:decode(Decoded, [return_maps]).

%%

-define(TABLE, ?MODULE).

create_table() ->
    _ = ets:new(?TABLE, [set, public, named_table, {read_concurrency, true}]),
    ok.

insert_values(Values) ->
    true = ets:insert(?TABLE, maps:to_list(Values)),
    ok.

lookup_value(Key) ->
    case ets:lookup(?TABLE, Key) of
        [{Key, Value}] ->
            Value;
        [] ->
            undefined
    end.
