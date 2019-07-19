-module(shortener_swagger_server).

-export([child_spec/3]).

-define(APP, shortener).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 10).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).

-spec child_spec(module(), map(), cowboy_router:routes()) -> supervisor:child_spec().

child_spec(LogicHandler, Opts, AdditionalRoutes) ->
    {Transport, TransportOpts} = get_socket_transport(Opts),
    CowboyOpts = get_cowboy_config(LogicHandler, AdditionalRoutes, Opts),
    ranch:child_spec(
        ?MODULE,
        Transport,
        TransportOpts,
        cowboy_clear,
        CowboyOpts
    ).

get_socket_transport(Opts) ->
    {ok, IP} = inet:parse_address(maps:get(ip, Opts, ?DEFAULT_IP_ADDR)),
    Port     = maps:get(port, Opts, ?DEFAULT_PORT),
    Acceptors = maps:get(acceptors, Opts, ?DEFAULT_ACCEPTORS_POOLSIZE),
    {ranch_tcp, #{socket_opts => [{ip, IP}, {port, Port}],  num_acceptors => Acceptors}}.

get_cowboy_config(LogicHandler, AdditionalRoutes, Opts) ->
    ShortUrlTemplate = maps:get(short_url_template, Opts),
    ShortUrlPath = maps:get(path, ShortUrlTemplate),
    Routes = squash_routes(
        AdditionalRoutes ++
        swag_server_router:get_paths(LogicHandler) ++
        [{'_', [{genlib:to_list(ShortUrlPath) ++ ":shortenedUrlID", shortener_handler, #{}}]}]
    ),
    CowboyOps = #{
        env => #{
            dispatch => cowboy_router:compile(Routes),
            cors_policy => shortener_cors_policy
        },
        middlewares => [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
        ],
        stream_handlers => [cowboy_access_log_h, cowboy_stream_h]
    },
    cowboy_access_log_h:set_extra_info_fun(
        mk_operation_id_getter(CowboyOps),
        CowboyOps
    ).

squash_routes(Routes) ->
    orddict:to_list(lists:foldl(
        fun ({K, V}, D) -> orddict:update(K, fun (V0) -> V0 ++ V end, V, D) end,
        orddict:new(),
        Routes
    )).

mk_operation_id_getter(#{env := Env}) ->
    fun (Req) ->
        case cowboy_router:execute(Req, Env) of
            {ok, _, #{handler_opts := {_Operations, _LogicHandler, _SwaggerHandlerOpts} = HandlerOpts}} ->
                case swag_server_utils:get_operation_id(Req, HandlerOpts) of
                    undefined ->
                        #{};
                    OperationID ->
                        #{operation_id => OperationID}
                end;
            _ ->
                #{}
        end
    end.