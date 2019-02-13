-module(shortener_handler).

%% Swagger handler

-behaviour(swag_server_logic_handler).
-export([authorize_api_key/3]).
-export([handle_request/4]).

%% Cowboy http handler

-behaviour(cowboy_handler).
-export([init/2]).
-export([terminate/3]).

%%

%% TODO refactor in case of different classes of users using this API
-define(REALM, <<"external">>).

-type operation_id() :: swag_server:operation_id().
-type request_ctx()  :: swag_server:request_context().
-type request_data() :: #{atom() | binary() => term()}.
-type subject_id()   :: woody_user_identity:id().

-spec authorize_api_key(operation_id(), swag_server:api_key(), swag_server:handler_opts(_)) ->
    Result :: false | {true, shortener_auth:context()}.

authorize_api_key(OperationID, ApiKey, _Opts) ->
    ok = scoper:add_scope('swag.server', #{operation => OperationID}),
    shortener_auth:authorize_api_key(OperationID, ApiKey).

-spec handle_request(operation_id(), request_data(), request_ctx(), any()) ->
    {ok | error, swag_server:response()}.

handle_request(OperationID, Req, Context, _Opts) ->
    try
        AuthContext = get_auth_context(Context),
        WoodyCtx = create_woody_ctx(Req, AuthContext),
        Slug = prefetch_slug(Req, WoodyCtx),
        case shortener_auth:authorize_operation(OperationID, Slug, AuthContext) of
            ok ->
                SubjectID = get_subject_id(AuthContext),
                process_request(OperationID, Req, Slug, SubjectID, WoodyCtx);
            {error, forbidden} ->
                {ok, {403, #{}, undefined}}
        end
    catch
        error:{woody_error, {Source, Class, Details}} ->
            {error, handle_woody_error(Source, Class, Details)}
    after
        ok = scoper:remove_scope()
    end.

-spec prefetch_slug(request_data(), woody_context:ctx()) -> shortener_slug:slug() | no_slug.

prefetch_slug(#{'shortenedUrlID' := ID}, WoodyCtx) ->
    case shortener_slug:get(ID, WoodyCtx) of
        {ok, Slug} ->
            Slug;
        {error, notfound} ->
            no_slug
    end;
prefetch_slug(_Req, _WoodyCtx) ->
    no_slug.

create_woody_ctx(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    WoodyCtx = woody_context:new(RpcID),
    woody_user_identity:put(collect_user_identity(AuthContext), WoodyCtx).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id       => get_subject_id(AuthContext),
        realm    => ?REALM,
        email    => get_claim(<<"email">>, AuthContext, undefined),
        username => get_claim(<<"name">>, AuthContext, undefined)
    }).

get_subject_id({{SubjectID, _ACL}, _}) ->
    SubjectID.

get_claim(ClaimName, {_Subject, Claims}, Default) ->
    maps:get(ClaimName, Claims, Default).

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

handle_woody_error(_Source, result_unexpected, _Details) ->
    {500, #{}, <<>>};
handle_woody_error(_Source, resource_unavailable, _Details) ->
    {503, #{}, <<>>};
handle_woody_error(_Source, result_unknown, _Details) ->
    {504, #{}, <<>>}.

%%

-spec process_request(operation_id(), request_data(), shortener_slug:slug(), subject_id(), woody_context:ctx()) ->
    {ok | error, swag_server:response()}.

process_request(
    'ShortenUrl',
    #{'ShortenedUrlParams' := #{
        <<"sourceUrl">> := SourceUrl,
        <<"expiresAt">> := ExpiresAt
    }},
    no_slug,
    SubjectID,
    WoodyCtx
) ->
    case validate_source_url(SourceUrl) of
        true ->
            Slug = shortener_slug:create(SourceUrl, parse_timestamp(ExpiresAt), SubjectID, WoodyCtx),
            {ok, {201, #{}, construct_shortened_url(Slug)}};
        false ->
            {ok, {400, #{}, #{
                <<"code">> => <<"forbidden_source_url">>,
                <<"message">> => <<"Source URL is forbidden">>
            }}}
    end;

process_request(
    'GetShortenedUrl',
    _Req,
    no_slug,
    _SubjectID,
    _WoodyCtx
) ->
    {error, {404, #{}, undefined}};
process_request(
    'GetShortenedUrl',
    _Req,
    Slug,
    _SubjectID,
    _WoodyCtx
) ->
    {ok, {200, #{}, construct_shortened_url(Slug)}};

process_request(
    'DeleteShortenedUrl',
    #{'shortenedUrlID' := ID},
    _Slug,
    _SubjectID,
    WoodyCtx
) ->
    case shortener_slug:remove(ID, WoodyCtx) of
        ok ->
            {ok, {204, #{}, undefined}};
        {error, notfound} ->
            {error, {404, #{}, undefined}}
    end.

validate_source_url(SourceUrl) ->
    lists:any(
        fun (Pattern) -> is_source_url_valid(SourceUrl, Pattern) end,
        get_source_url_whitelist()
    ).

is_source_url_valid(SourceUrl, Pattern) ->
    genlib_wildcard:match(SourceUrl, genlib:to_binary(Pattern)).

construct_shortened_url(
    #{
        id := ID,
        source := Source,
        expires_at := ExpiresAt
    }
) ->
    #{
        <<"id">> => ID,
        <<"shortenedUrl">> => render_short_url(ID, get_short_url_template()),
        <<"sourceUrl">> => Source,
        <<"expiresAt">> => ExpiresAt
    }.

render_short_url(ID, Template) ->
    iolist_to_binary([
        genlib:to_binary(maps:get(scheme, Template)),
        <<"://">>,
        genlib:to_binary(maps:get(netloc, Template)),
        genlib:to_binary(maps:get(path, Template)),
        ID
    ]).

parse_timestamp(Timestamp) ->
    {ok, {Date, Time, Usec, TZOffset}} = rfc3339:parse(Timestamp),
    Seconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
    {DateUTC, TimeUTC} = calendar:gregorian_seconds_to_datetime(
        case TZOffset of
            _ when is_integer(TZOffset) ->
                Seconds - (60 * TZOffset);
            _ ->
                Seconds
        end
    ),
    {ok, TimestampUTC} = rfc3339:format({DateUTC, TimeUTC, Usec, 0}),
    TimestampUTC.

get_short_url_template() ->
    % TODO
    % Teach the swagger-codegen bastard to behave and accept handler options
    % upon initialization
    maps:get(short_url_template, genlib_app:env(shortener, api)).

get_source_url_whitelist() ->
    % TODO
    % Teach the swagger-codegen bastard to behave and accept handler options
    % upon initialization
    maps:get(source_url_whitelist, genlib_app:env(shortener, api), []).

%%

-type state()            :: undefined.
-type request()          :: cowboy_req:req().
-type terminate_reason() :: {normal, shutdown} | {error, atom()}.

-spec init(request(), _) ->
    {ok, request(), state()}.

init(Req, Opts) ->
    ID = cowboy_req:binding('shortenedUrlID', Req),
    Req = case shortener_slug:get(ID, woody_context:new()) of
        {ok, #{source := Source, expires_at := ExpiresAt}} ->
            {ok, {Date, Time, _, _UndefinedButDialyzerDisagrees}} = rfc3339:parse(ExpiresAt),
            Headers = #{
                <<"location">>      => Source,
                <<"expires">>       => cowboy_clock:rfc1123({Date, Time}),
                <<"cache-control">> => <<"must-revalidate">>
            },
            cowboy_req:reply(301, Headers, Req);
        {error, notfound} ->
            cowboy_req:reply(404, Req)
    end,
    {ok, Req, Opts}.

-spec terminate(terminate_reason(), request(), state()) ->
    ok.

terminate(_Reason, _Req, _St) ->
    ok.
