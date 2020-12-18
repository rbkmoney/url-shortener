-module(shortener_auth_SUITE).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([failed_authorization/1]).
-export([insufficient_permissions/1]).
-export([readonly_permissions/1]).
-export([other_subject_delete/1]).
-export([other_subject_read/1]).

%% tests descriptions

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [test_case_name()].
all() ->
    [
        {group, general}
    ].

-spec groups() -> [{atom(), list(), [test_case_name()]}].
groups() ->
    [
        {general, [], [
            failed_authorization,
            insufficient_permissions,
            readonly_permissions,
            other_subject_delete,
            other_subject_read
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({shortener_swagger_server, '_', '_'}, x),
    Host = "url-shortener",
    Port = 8080,
    Netloc = Host ++ ":" ++ genlib:to_list(Port),
    Apps =
        genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_logger}
        ]) ++
            genlib_app:start_application_with(bouncer_client, shortener_ct_helper:get_bouncer_client_app_config()),
    [
        {suite_apps, Apps},
        {api_endpoint, "http://" ++ Netloc},
        {host, Host},
        {port, Port},
        {netloc, Netloc}
    ] ++ C.

-spec init_per_group(atom(), config()) -> config().
init_per_group(_Group, C) ->
    ShortenerApp =
        genlib_app:start_application_with(
            shortener,
            shortener_ct_helper:get_app_config(
                ?config(port, C),
                ?config(netloc, C),
                get_keysource("keys/local/private.pem", C)
            )
        ),
    [
        {shortener_app, ShortenerApp}
    ] ++ C.

-spec end_per_group(atom(), config()) -> _.
end_per_group(_Group, C) ->
    genlib_app:stop_unload_applications(?config(shortener_app, C)).

get_keysource(Key, C) ->
    filename:join(?config(data_dir, C), Key).

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    shortener_ct_helper:with_test_sup(C).

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, C) ->
    shortener_ct_helper:stop_test_sup(C),
    ok.

%%

-spec failed_authorization(config()) -> _.
-spec insufficient_permissions(config()) -> _.
-spec readonly_permissions(config()) -> _.
-spec other_subject_delete(config()) -> _.
-spec other_subject_read(config()) -> _.

failed_authorization(C) ->
    Params = construct_params(<<"https://oops.io/">>),
    C1 = clean_api_auth_token(C),
    {ok, 401, _, _} = shorten_url(Params, C1),
    {ok, 401, _, _} = delete_shortened_url(<<"42">>, C1),
    {ok, 401, _, _} = get_shortened_url(<<"42">>, C1).

insufficient_permissions(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', _) ->
                {ok, #bdcs_Judgement{
                    resolution = {forbidden, #bdcs_ResolutionForbidden{}},
                    resolution_legacy = forbidden
                }}
            end}
        ],
        C
    ),
    C1 = set_api_auth_token(insufficient_permissions, C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 403, _, _} = shorten_url(Params, C1),
    {ok, 403, _, _} = delete_shortened_url(<<"42">>, C1),
    {ok, 403, _, _} = get_shortened_url(<<"42">>, C1).

readonly_permissions(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', {_RulesetID, Fragments}) ->
                case get_operation_id(Fragments) of
                    <<"ShortenUrl">> ->
                        {ok, #bdcs_Judgement{
                            resolution = {allowed, #bdcs_ResolutionAllowed{}},
                            resolution_legacy = allowed
                        }};
                    <<"GetShortenedUrl">> ->
                        {ok, #bdcs_Judgement{
                            resolution = {allowed, #bdcs_ResolutionAllowed{}},
                            resolution_legacy = allowed
                        }};
                    <<"DeleteShortenedUrl">> ->
                        {ok, #bdcs_Judgement{
                            resolution = {forbidden, #bdcs_ResolutionForbidden{}},
                            resolution_legacy = forbidden
                        }}
                end
            end}
        ],
        C
    ),
    C1 = set_api_auth_token(readonly_permissions, C),
    Params = construct_params(<<"https://oops.io/">>),
    {ok, 201, _, #{<<"id">> := ID}} = shorten_url(Params, C1),
    {ok, 200, _, #{<<"id">> := ID}} = get_shortened_url(ID, C1),
    {ok, 403, _, _} = delete_shortened_url(ID, C1).

other_subject_delete(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', {_RulesetID, Fragments}) ->
                case get_operation_id(Fragments) of
                    <<"ShortenUrl">> ->
                        {ok, #bdcs_Judgement{
                            resolution = {allowed, #bdcs_ResolutionAllowed{}},
                            resolution_legacy = allowed
                        }};
                    <<"GetShortenedUrl">> ->
                        case get_owner_info(Fragments) of
                            {ID, ID} ->
                                {ok, #bdcs_Judgement{
                                    resolution = {allowed, #bdcs_ResolutionAllowed{}},
                                    resolution_legacy = allowed
                                }};
                            _ ->
                                {ok, #bdcs_Judgement{
                                    resolution = {forbidden, #bdcs_ResolutionForbidden{}},
                                    resolution_legacy = forbidden
                                }}
                        end;
                    <<"DeleteShortenedUrl">> ->
                        case get_owner_info(Fragments) of
                            {ID, ID} ->
                                {ok, #bdcs_Judgement{
                                    resolution = {allowed, #bdcs_ResolutionAllowed{}},
                                    resolution_legacy = allowed
                                }};
                            _ ->
                                {ok, #bdcs_Judgement{
                                    resolution = {forbidden, #bdcs_ResolutionForbidden{}},
                                    resolution_legacy = forbidden
                                }}
                        end
                end
            end}
        ],
        C
    ),
    SourceUrl = <<"https://oops.io/">>,
    Params = construct_params(SourceUrl),
    C1 = set_api_auth_token(other_subject_delete_first, C),
    {ok, 201, _, #{<<"id">> := ID, <<"shortenedUrl">> := ShortUrl}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(other_subject_delete_second, C1),
    {ok, 403, _, _} = delete_shortened_url(ID, C2),
    {ok, 301, Headers, _} = hackney:request(get, ShortUrl),
    {<<"location">>, SourceUrl} = lists:keyfind(<<"location">>, 1, Headers).

other_subject_read(C) ->
    shortener_ct_helper:mock_services(
        [
            {bouncer, fun('Judge', {_RulesetID, Fragments}) ->
                case get_operation_id(Fragments) of
                    <<"ShortenUrl">> ->
                        {ok, #bdcs_Judgement{
                            resolution = {allowed, #bdcs_ResolutionAllowed{}},
                            resolution_legacy = allowed
                        }};
                    <<"GetShortenedUrl">> ->
                        case get_owner_info(Fragments) of
                            {ID, ID} ->
                                {ok, #bdcs_Judgement{
                                    resolution = {allowed, #bdcs_ResolutionAllowed{}},
                                    resolution_legacy = allowed
                                }};
                            _ ->
                                {ok, #bdcs_Judgement{
                                    resolution = {forbidden, #bdcs_ResolutionForbidden{}},
                                    resolution_legacy = forbidden
                                }}
                        end;
                    <<"DeleteShortenedUrl">> ->
                        case get_owner_info(Fragments) of
                            {ID, ID} ->
                                {ok, #bdcs_Judgement{
                                    resolution = {allowed, #bdcs_ResolutionAllowed{}},
                                    resolution_legacy = allowed
                                }};
                            _ ->
                                {ok, #bdcs_Judgement{
                                    resolution = {forbidden, #bdcs_ResolutionForbidden{}},
                                    resolution_legacy = forbidden
                                }}
                        end
                end
            end}
        ],
        C
    ),
    Params = construct_params(<<"https://oops.io/">>),
    C1 = set_api_auth_token(other_subject_read_first, C),
    {ok, 201, _, #{<<"id">> := ID}} = shorten_url(Params, C1),
    C2 = set_api_auth_token(other_subject_read_second, C1),
    {ok, 403, _, _} = get_shortened_url(ID, C2).

%%

construct_params(SourceUrl) ->
    construct_params(SourceUrl, 3600).

construct_params(SourceUrl, Lifetime) ->
    #{
        <<"sourceUrl">> => SourceUrl,
        <<"expiresAt">> => format_ts(genlib_time:unow() + Lifetime)
    }.

set_api_auth_token(Name, C) ->
    UserID = genlib:to_binary(Name),
    ACL = construct_shortener_acl([]),
    {ok, T} = shortener_authorizer_jwt:issue({{UserID, shortener_acl:from_list(ACL)}, #{}}, unlimited),
    lists:keystore(api_auth_token, 1, C, {api_auth_token, T}).

clean_api_auth_token(C) ->
    lists:keydelete(api_auth_token, 1, C).

construct_shortener_acl(Permissions) ->
    lists:map(fun(P) -> {['shortened-urls'], P} end, Permissions).

%%

shorten_url(ShortenedUrlParams, C) ->
    swag_client_shortener_api:shorten_url(
        ?config(api_endpoint, C),
        append_common_params(#{body => ShortenedUrlParams}, C)
    ).

delete_shortened_url(ID, C) ->
    swag_client_shortener_api:delete_shortened_url(
        ?config(api_endpoint, C),
        append_common_params(#{binding => #{<<"shortenedUrlID">> => ID}}, C)
    ).

get_shortened_url(ID, C) ->
    swag_client_shortener_api:get_shortened_url(
        ?config(api_endpoint, C),
        append_common_params(#{binding => #{<<"shortenedUrlID">> => ID}}, C)
    ).

append_common_params(Params, C) ->
    append_media_type(
        append_auth(
            append_request_id(
                maps:merge(#{binding => #{}, qs_val => #{}, header => #{}, body => #{}}, Params)
            ),
            C
        )
    ).

append_media_type(Params = #{header := Headers}) ->
    Params#{
        header => Headers#{
            <<"Accept">> => <<"application/json">>,
            <<"Content-Type">> => <<"application/json; charset=utf-8">>
        }
    }.

append_auth(Params = #{header := Headers}, C) ->
    case lists:keyfind(api_auth_token, 1, C) of
        {api_auth_token, AuthToken} ->
            Params#{header => Headers#{<<"Authorization">> => <<"Bearer ", AuthToken/binary>>}};
        _ ->
            Params
    end.

append_request_id(Params = #{header := Headers}) ->
    Params#{header => Headers#{<<"X-Request-ID">> => woody_context:new_req_id()}}.

format_ts(Ts) ->
    genlib_rfc3339:format(Ts, second).

get_operation_id(#bdcs_Context{
    fragments = #{
        <<"shortener">> := #bctx_ContextFragment{
            type = v1_thrift_binary,
            content = Fragment
        }
    }
}) ->
    case decode(Fragment) of
        {error, _} = Error ->
            error(Error);
        #bctx_v1_ContextFragment{
            shortener = #bctx_v1_ContextUrlShortener{op = #bctx_v1_UrlShortenerOperation{id = OperationID}}
        } ->
            OperationID
    end.

get_owner_info(Context) ->
    {get_owner_id(Context), get_user_id(Context)}.

get_owner_id(#bdcs_Context{
    fragments = #{
        <<"shortener">> := #bctx_ContextFragment{
            type = v1_thrift_binary,
            content = Fragment
        }
    }
}) ->
    case decode(Fragment) of
        {error, _} = Error ->
            error(Error);
        #bctx_v1_ContextFragment{
            shortener = #bctx_v1_ContextUrlShortener{op = #bctx_v1_UrlShortenerOperation{shortened_url = Url}}
        } ->
            #bctx_v1_ShortenedUrl{owner = #bctx_v1_Entity{id = OwnerID}} = Url,
            OwnerID
    end.

get_user_id(#bdcs_Context{
    fragments = #{
        <<"user">> := #bctx_ContextFragment{
            type = v1_thrift_binary,
            content = Fragment
        }
    }
}) ->
    case decode(Fragment) of
        {error, _} = Error ->
            error(Error);
        #bctx_v1_ContextFragment{user = #bctx_v1_User{id = UserID}} ->
            UserID
    end.

decode(Content) ->
    Type = {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}},
    Codec = thrift_strict_binary_codec:new(Content),
    case thrift_strict_binary_codec:read(Codec, Type) of
        {ok, CtxThrift, Codec1} ->
            case thrift_strict_binary_codec:close(Codec1) of
                <<>> ->
                    CtxThrift;
                Leftovers ->
                    {error, {excess_binary_data, Leftovers}}
            end;
        Error ->
            Error
    end.
