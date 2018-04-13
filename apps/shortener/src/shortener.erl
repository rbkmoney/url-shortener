-module(shortener).

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop /1]).

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
    HealthCheckers = genlib_app:env(?MODULE, health_checkers, []),
    {ok, {
       {one_for_all, 0, 1},
       % TODO
       get_processor_childspecs(genlib_app:env(?MODULE, processor), HealthCheckers) ++
       get_api_childspecs(genlib_app:env(?MODULE, api), HealthCheckers)
    }}.

get_processor_childspecs(Opts, HealthCheckers) ->
    {ok, IP} = inet:parse_address(maps:get(ip, Opts, "::")),
    [woody_server:child_spec(
        ?MODULE,
        #{
            ip            => IP,
            port          => maps:get(port, Opts, 8022),
            net_opts      => maps:get(net_opts, Opts, []),
            event_handler => scoper_woody_event_handler,
            handlers      => [
                {"/v1/stateproc", {
                    {mg_proto_state_processing_thrift, 'Processor'},
                    shortener_slug
                }}
            ],
            additional_routes => [erl_health_handle:get_route(HealthCheckers)]
        }
    )].

get_api_childspecs(Opts, HealthCheckers) ->
    AuthorizerSpec = shortener_authorizer_jwt:get_child_spec(maps:get(authorizer, Opts)),
    HealthRoutes = [{'_', [erl_health_handle:get_route(HealthCheckers)]}],
    SwaggerServerSpec = shortener_swagger_server:child_spec(shortener_handler, Opts, HealthRoutes),
    [AuthorizerSpec, SwaggerServerSpec].
