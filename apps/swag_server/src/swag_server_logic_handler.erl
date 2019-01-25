-module(swag_server_logic_handler).

-export([handle_request/4]).

-type operation_id()    :: swag_server:operation_id().
-type api_key()         :: swag_server:api_key().
-type auth_context()    :: swag_server:auth_context().
-type object()          :: swag_server:object().
-type request_context() :: swag_server:request_context().
-type handler_opts(T)   :: swag_server:handler_opts(T).
-type logic_handler(T)  :: swag_server:logic_handler(T).
-type response()        :: swag_server:response().

%% Behaviour definition

-export([authorize_api_key/3]).

-callback authorize_api_key(operation_id(), api_key(), handler_opts(_)) ->
    boolean() | {boolean(), auth_context()}.

-callback handle_request(operation_id(), object(), request_context(), handler_opts(_)) ->
    {ok | error, response()}.

%% API

-spec handle_request(logic_handler(_), operation_id(), object(), request_context()) ->
    {ok | error, response()}.

handle_request(Handler, OperationID, Request, Context) ->
    {Module, _Opts} = get_mod_opts(Handler),
    Module:handle_request(OperationID, Request, Context).

-spec authorize_api_key(logic_handler(_), operation_id(), api_key()) ->
    false | {true, auth_context()}.
authorize_api_key(Handler, OperationID, ApiKey) ->
    {Module, _Opts} = get_mod_opts(Handler),
    Module:authorize_api_key(OperationID, ApiKey).

%% Internal functions

get_mod_opts(ModOpts= {_, _}) ->
    ModOpts;
get_mod_opts(Module) ->
    {Module, undefined}.
