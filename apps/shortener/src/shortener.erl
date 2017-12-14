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
    Authorizers = genlib_app:env(shortener, authorizers, #{}),
    AuthorizerSpec = shortener_authorizer_jwt:get_child_spec(maps:get(jwt, Authorizers)),
    SwaggerServerSpec = shortener_swagger_server:child_spec(shortener_handler),
    {ok, {
       {one_for_all, 0, 1},
       [AuthorizerSpec, SwaggerServerSpec]
    }}.
