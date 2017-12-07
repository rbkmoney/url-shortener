-module(shortener_swagger_server).

-export([child_spec/1]).

-define(APP, shortener).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).

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
    [{'_', PathsList0}] = swag_router:get_paths(LogicHandler),
    PathsList = PathsList0 ++ [{"/v1/shortened-urls/resolve-shortened-url", 'shortener_handler', #{}}],
    Dispatch = cowboy_router:compile([{'_', PathsList}]),
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
        {onrequest, cowboy_access_log:get_request_hook()},
        {onresponse, cowboy_access_log:get_response_hook()}
    ].
