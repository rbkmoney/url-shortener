-module(shortener_handler).

-behaviour(swag_logic_handler).

-define(NS, <<"shortener">>).
-define(DOMAIN, "rbk.mn/").

-define(shortened_url(ID, ShortenedUrl, SourceUrl, ExpiresAt),
    {ID, ShortenedUrl, SourceUrl, ExpiresAt}
).

%% API callbacks
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key()) ->
    Result :: false | {true, capi_auth:context()}.

authorize_api_key(OperationID, ApiKey) ->
    _ = capi_utils:logtag_process(operation_id, OperationID),
    capi_auth:authorize_api_key(OperationID, ApiKey).

-type request_data() :: #{atom() | binary() => term()}.

-spec handle_request(
    OperationID :: swag_server:operation_id(),
    Req :: request_data(),
    Context :: swag_server:request_context()
) ->
    {ok | error, swag_server_logic_handler:response()}.

handle_request(OperationID, Req, Context) ->
    _ = lager:info("Processing request ~p", [OperationID]),
    % Auth should be here
    ReqContext = create_context(Req, get_auth_context(Context)),
    process_request(OperationID, Req, Context, ReqContext).

%%

-spec process_request(
    OperationID :: swag:operation_id(),
    Req :: request_data(),
    Context :: swag:request_context(),
    ReqCtx :: woody_context:ctx()
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request('ShortenUrl', Req, Context, ReqCtx) ->
    Result = shorten_url(maps:get('ShortenedUrlParams', Req), Context, ReqCtx),
    case Result of
        {ok, ShortenedUrl} ->
            {ok, {201, [], ShortenedUrl}};
        {exception, Exception} ->
            {error, {400, [], Exception}}
    end;
process_request('GetShortenedUrl', Req, Context, ReqCtx) ->
    Result = get_shortened_url(maps:get(shortenedUrlID, Req), Context, ReqCtx),
    case Result of
        {ok, ShortenedUrl} ->
            {ok, {200, [], ShortenedUrl}};
        {exception, Exception} ->
            {error, {400, [], Exception}}
    end;
process_request('DeleteShortenedUrl', Req, Context, ReqCtx) ->
    Result = delete_shortened_url(maps:get(shortenedUrlID, Req), Context, ReqCtx),
    case Result of
        ok ->
            {ok, {204, [], undefined}};
        {exception, Exception} ->
            {error, {400, [], Exception}}
    end.

%%

shorten_url(Params, _Context, ReqCtx) ->
    ShortenedUrl = create_shortened_url(Params),
    {ok, _Result} = shortener_automaton_client:call(?NS, ShortenedUrl, marshal(ShortenedUrl), ReqCtx),
    {ok, ShortenedUrl}.

get_shortened_url(ID, _Context, ReqCtx) ->
    {ok, History} = shortener_automaton_client:get_history(?NS, ID, ReqCtx),
    ShortenedUrl = get_url(History),
    {ok, ShortenedUrl}.

delete_shortened_url(ID, _Context, ReqCtx) ->
    shortener_automaton_client:remove(?NS, ID, ReqCtx).

%%

create_shortened_url(#{'SourceUrl' := SourceUrl, 'ExpiresAt' := ExpiresAt}) ->
    ShortenedUrl = hash_url(SourceUrl),
    ?shortened_url(ShortenedUrl, make_full_shortened_url(ShortenedUrl), SourceUrl, ExpiresAt).

hash_url(Url) ->
    ShortenedUrl = crypto:hash(sha224, Url),
    case get_shortened_url(ShortenedUrl) of
        {error, #'MachineNotFound'{}} ->
            ShortenedUrl;
        {ok, _} ->
            hash_url(ShortenedUrl)
    end.

make_full_shortened_url(Url) ->
    ?DOMAIN ++ Url.

%%

get_auth_context(#{auth_context := AuthContext}) ->
    AuthContext.

create_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    _ = lager:debug("Created TraceID:~p for RequestID:~p", [TraceID , RequestID]),
    WoodyContext = woody_context:new(RpcID),
    woody_user_identity:put(collect_user_identity(AuthContext), WoodyContext).

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

%%

marshal(?shortened_url(ID, ShortenedUrl, SourceUrl, ExpiresAt)) ->
    #{
        <<"id">> => marshal(ID),
        <<"shortened_url">> => marshal(ShortenedUrl),
        <<"source_url">> => marshal(SourceUrl),
        <<"expires_at">> => marshal(ExpiresAt)
    };
marshal(Value) ->
    shortener_msgpack_marshalling:marshal(Value).

unmarshal(#{
    <<"id">> := ID,
    <<"shortened_url">> := ShortenedUrl,
    <<"source_url">> := SourceUrl,
    <<"expires_at">> := ExpiresAt
}) ->
    ?shortened_url(
        unmarshal(ID),
        unmarshal(ShortenedUrl),
        unmarshal(SourceUrl),
        unmarshal(ExpiresAt)
    );
unmarshal(Value) ->
    shortener_msgpack_marshalling:unmarshal(Value).
