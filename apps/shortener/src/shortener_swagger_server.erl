-module(shortener_swagger_server).

-export([child_spec/2]).

-define(APP, shortener).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).

-spec child_spec(module(), map()) -> supervisor:child_spec().

child_spec(LogicHandler, Opts) ->
    {Transport, TransportOpts} = get_socket_transport(Opts),
    CowboyOpts = get_cowboy_config(LogicHandler, Opts),
    Acceptors = maps:get(acceptors, Opts, ?DEFAULT_ACCEPTORS_POOLSIZE),
    ranch:child_spec(
        ?MODULE,
        Acceptors,
        Transport,
        TransportOpts,
        cowboy_protocol,
        CowboyOpts
    ).

get_socket_transport(Opts) ->
    {ok, IP} = inet:parse_address(maps:get(ip, Opts, ?DEFAULT_IP_ADDR)),
    Port     = maps:get(port, Opts, ?DEFAULT_PORT),
    {ranch_tcp, [{ip, IP}, {port, Port}]}.

get_cowboy_config(LogicHandler, Opts) ->
    ShortUrlTemplate = maps:get(short_url_template, Opts),
    ShortUrlPath = maps:get(path, ShortUrlTemplate),
    Routes =
        swag_router:get_paths(LogicHandler) ++
        [{'_', {ShortUrlPath ++ ":shortenedUrlID", shortener_handler, #{}}}]
    ,
    [
        {env, [
            {dispatch, cowboy_router:compile(Routes)},
            {cors_policy, shortener_cors_policy}
        ]},
        {middlewares, [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
        ]},
        {onrequest, cowboy_access_log:get_request_hook()},
        {onresponse, cowboy_access_log:get_response_hook(shortener_access_lager_event)}
    ].
