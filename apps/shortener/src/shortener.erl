-module(shortener).

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%

-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    ?MODULE:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% API

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    HealthRoutes = get_health_routes(genlib_app:env(?MODULE, health_check, #{})),
    PrometeusRout = get_prometheus_route(),
    AdditionalRoutes = [PrometeusRout | HealthRoutes],
    {ok, {
        {one_for_all, 0, 1},
        % TODO
        get_processor_childspecs(genlib_app:env(?MODULE, processor), AdditionalRoutes) ++
            get_api_childspecs(genlib_app:env(?MODULE, api), HealthRoutes)
    }}.

get_health_routes(Check) ->
    [erl_health_handle:get_route(enable_health_logging(Check))].

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.

enable_health_logging(Check = #{}) ->
    maps:map(
        fun(_, V = {_, _, _}) ->
            #{runner => V, event_handler => {erl_health_event_handler, []}}
        end,
        Check
    ).

get_processor_childspecs(Opts, AdditionalRoutes) ->
    {ok, IP} = inet:parse_address(maps:get(ip, Opts, "::")),
    [
        woody_server:child_spec(
            ?MODULE,
            #{
                ip => IP,
                port => maps:get(port, Opts, 8022),
                protocol_opts => maps:get(protocol_opts, Opts, #{}),
                transport_opts => maps:get(transport_opts, Opts, #{}),
                event_handler => scoper_woody_event_handler,
                handlers => [
                    {"/v1/stateproc", {
                        {mg_proto_state_processing_thrift, 'Processor'},
                        shortener_slug
                    }}
                ],
                additional_routes => AdditionalRoutes
            }
        )
    ].

get_api_childspecs(Opts, HealthRoutes) ->
    AuthorizerSpec = shortener_authorizer_jwt:get_child_spec(maps:get(authorizer, Opts)),
    HealthRouter = [{'_', HealthRoutes}],
    SwaggerServerSpec = shortener_swagger_server:child_spec(shortener_handler, Opts, HealthRouter),
    [AuthorizerSpec, SwaggerServerSpec].
