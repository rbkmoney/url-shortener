-module(shortener_swagger_server).

-export([child_spec/1]).
-export([request_hook/1]).
-export([response_hook/4]).

-define(APP, shortener).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).

-define(START_TIME_TAG, processing_start_time).

-spec child_spec(module()) -> supervisor:child_spec().

child_spec(LogicHandler) ->
    {Transport, TransportOpts} = get_socket_transport(),
    CowboyOpts = get_cowboy_config(LogicHandler),
    AcceptorsPool = genlib_app:env(?APP, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    ranch:child_spec(?MODULE, AcceptorsPool,
                     Transport, TransportOpts, cowboy_protocol, CowboyOpts).

get_socket_transport() ->
    {ok, IP} = inet:parse_address(genlib_app:env(?APP, ip, ?DEFAULT_IP_ADDR)),
    Port     = genlib_app:env(?APP, port, ?DEFAULT_PORT),
    {ranch_tcp, [{ip, IP}, {port, Port}]}.

get_cowboy_config(LogicHandler) ->
    Dispatch = cowboy_router:compile(swag_router:get_paths(LogicHandler)),
    [
        {env, [
            {dispatch, Dispatch},
            {cors_policy, shortener_cors_policy}
        ]},
        {middlewares, [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
        ]},
        {onrequest, fun ?MODULE:request_hook/1},
        {onresponse, fun ?MODULE:response_hook/4}
    ].

-spec request_hook(cowboy_req:req()) ->
    cowboy_req:req().

request_hook(Req) ->
    cowboy_req:set_meta(?START_TIME_TAG, genlib_time:ticks(), Req).

-spec response_hook(cowboy:http_status(), cowboy:http_headers(), iodata(), cowboy_req:req()) ->
    cowboy_req:req().

response_hook(Code, Headers, _, Req) ->
    try
        {Code1, Headers1, Req1} = handle_response(Code, Headers, Req),
        _ = log_access(Code1, Headers1, Req1),
        Req1
    catch
        Class:Reason ->
            Stack = genlib_format:format_stacktrace(erlang:get_stacktrace(), [newlines]),
            _ = lager:error(
                "Response hook failed for: [~p, ~p, ~p]~nwith: ~p:~p~nstacktrace: ~ts",
                [Code, Headers, Req, Class, Reason, Stack]
            ),
            Req
    end.

handle_response(Code, Headers, Req) ->
    {Code, Headers, Req}.

log_access(Code, Headers, Req) ->
    {Method, _} = cowboy_req:method(Req),
    {Path,   _} = cowboy_req:path(Req),
    {ReqLen, _} = cowboy_req:body_length(Req),
    {ReqId,  _} = cowboy_req:header(<<"x-request-id">>, Req, undefined),
    RemAddr  = get_remote_addr(Req),
    RespLen  = get_response_len(Headers),
    Duration = get_request_duration(Req),
    ReqMeta = [
        {remote_addr, RemAddr},
        {request_method, Method},
        {request_path, Path},
        {request_length, ReqLen},
        {response_length, RespLen},
        {request_time, Duration},
        {'http_x-request-id', ReqId},
        {status, Code}
    ],
    Meta = orddict:merge(fun(_Key, New, _Old) -> New end, ReqMeta, lager:md()),
    %% Call lager:log/5 here directly in order to pass request metadata (fused into
    %% lager metadata) without storing it in a process dict via lager:md/1.
    lager:log(shortener_access_lager_event, info, Meta, "", []).

get_remote_addr(Req) ->
    case swag_handler_api:determine_peer(Req) of
        {{ok, #{ip_address := IP}}, _} ->
            genlib:to_binary(inet:ntoa(IP));
        {_, _} ->
            undefined
    end.
get_request_duration(Req) ->
    case cowboy_req:meta(?START_TIME_TAG, Req) of
        {undefined, _} ->
            undefined;
        {StartTime, _} ->
            (genlib_time:ticks() - StartTime) / 1000000
    end.

get_response_len(Headers) ->
    case lists:keyfind(<<"content-length">>, 1, Headers) of
        {_, Len} ->
            genlib:to_int(Len);
        false ->
            undefined
    end.
