-module(shortener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor callbacks

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    AuthorizerSpecs = get_authorizer_child_specs(),
    {LogicHandler, LogicHandlerSpecs} = get_logic_handler_info(),
    SwaggerSpec = shortener_swagger_server:child_spec(LogicHandler),
    {ok, {
       {one_for_all, 0, 1},
       LogicHandlerSpecs ++ [SwaggerSpec] ++ AuthorizerSpecs
    }}.

%%

-spec get_authorizer_child_specs() -> [supervisor:child_spec()].

get_authorizer_child_specs() ->
    Authorizers = genlib_app:env(shortener, authorizers, #{}),
    [get_authorizer_child_spec(jwt, maps:get(jwt, Authorizers))].

-spec get_authorizer_child_spec(Name :: atom(), Options :: #{}) -> supervisor:child_spec().

get_authorizer_child_spec(jwt, Options) ->
    shortener_authorizer_jwt:get_child_spec(Options).

-spec get_logic_handler_info() -> {Handler :: atom(), [Spec :: supervisor:child_spec()] | []} .

get_logic_handler_info() ->
    {shortener_handler, []}.
