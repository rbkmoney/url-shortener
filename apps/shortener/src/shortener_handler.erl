-module(shortener_handler).

%% Swagger handler

-behaviour(swag_server_logic_handler).
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% Cowboy http handler

-behaviour(cowboy_http_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%

%% TODO refactor in case of different classes of users using this API
-define(REALM, <<"external">>).

-type operation_id() :: swag_server:operation_id().
-type request_ctx()  :: swag_server:request_context().
-type request_data() :: #{atom() | binary() => term()}.

-spec authorize_api_key(operation_id(), swag_server:api_key()) ->
    Result :: false | {true, shortener_auth:context()}.

authorize_api_key(OperationID, ApiKey) ->
    ok = scoper:add_scope('swag.server', #{operation => OperationID}),
    shortener_auth:authorize_api_key(OperationID, ApiKey).

-spec handle_request(operation_id(), request_data(), request_ctx()) ->
    {ok | error, swag_server_logic_handler:response()}.

handle_request(OperationID, Req, Context) ->
    try
        WoodyCtx = create_context(Req, get_auth_context(Context)),
        process_request(OperationID, Req, WoodyCtx)
    after
        ok = scoper:remove_scope()
    end.

create_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
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

%%

-spec process_request(operation_id(), request_data(), woody_context:ctx()) ->
    {ok | error, swag_server_logic_handler:response()}.

process_request(
    'ShortenUrl',
    #{'ShortenedUrlParams' := #{
        <<"sourceUrl">> := SourceUrl,
        <<"expiresAt">> := ExpiresAt
    }},
    WoodyCtx
) ->
    case validate_source_url(SourceUrl) of
        true ->
            Slug = shortener_slug:create(SourceUrl, parse_timestamp(ExpiresAt), WoodyCtx),
            {ok, {201, [], construct_shortened_url(Slug)}};
        false ->
            {error, {400, [], #{
                <<"code">> => <<"invalid_source_url">>,
                <<"message">> => <<"Source URL is forbidden">>
            }}}
    end;

process_request(
    'GetShortenedUrl',
    #{'shortenedUrlID' := ID},
    WoodyCtx
) ->
    case shortener_slug:get(ID, WoodyCtx) of
        {ok, Slug} ->
            {ok, {200, [], construct_shortened_url(Slug)}};
        {error, notfound} ->
            {error, {404, [], #{<<"message">> => <<"Not found">>}}}
    end;

process_request(
    'DeleteShortenedUrl',
    #{'shortenedUrlID' := ID},
    WoodyCtx
) ->
    case shortener_slug:remove(ID, WoodyCtx) of
        ok ->
            {ok, {204, [], undefined}};
        {error, notfound} ->
            {error, {404, [], #{<<"message">> => <<"Not found">>}}}
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
    maps:get(short_url_template, genlib_app:env(shortener, api)).

get_source_url_whitelist() ->
    % TODO
    maps:get(source_url_whitelist, genlib_app:env(shortener, api), []).

%%

-type state()            :: undefined.
-type request()          :: cowboy_req:req().
-type terminate_reason() :: {normal, shutdown} | {error, atom()}.

-spec init({atom(), http}, request(), _) ->
    {ok, request(), state()}.

init({_, http}, Req, _Opts) ->
    {ok, Req, undefined}.

-spec handle(request(), state()) ->
    {ok, request(), state()}.

handle(Req1, St) ->
    {ID, Req2} = cowboy_req:binding('shortenedUrlID', Req1),
    {ok, Req3} = case shortener_slug:get(ID, woody_context:new()) of
        {ok, #{source := Source, expires_at := ExpiresAt}} ->
            {ok, {Date, Time, _, _UndefinedButDialyzerDisagrees}} = rfc3339:parse(ExpiresAt),
            Headers = [
                {<<"location">>      , Source},
                {<<"expires">>       , cowboy_clock:rfc1123({Date, Time})},
                {<<"cache-control">> , <<"must-revalidate">>}
            ],
            cowboy_req:reply(301, Headers, Req2);
        {error, notfound} ->
            cowboy_req:reply(404, Req2)
    end,
    {ok, Req3, St}.

-spec terminate(terminate_reason(), request(), state()) ->
    ok.

terminate(_Reason, _Req, _St) ->
    ok.
