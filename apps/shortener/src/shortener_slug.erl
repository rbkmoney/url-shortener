-module(shortener_slug).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

%% API

-export([create/4]).
-export([get/2]).
-export([remove/2]).

%% Processor

-behaviour(woody_server_thrift_handler).

-define(NS, <<"url-shortener">>).
-define(DEFAULT_DEADLINE, 5000).

-export([handle_function/4]).

%%

% RFC 3339
-type timestamp() :: binary().

-type id() :: binary().
-type source() :: binary().
-type owner() :: binary().
-type expiration() :: timestamp().

-type slug() :: #{
    id => id(),
    source => source(),
    owner => owner() | undefined,
    expires_at => expiration()
}.

-export_type([slug/0]).
-export_type([id/0]).
-export_type([owner/0]).

-type ctx() :: woody_context:ctx().

-spec create(source(), expiration(), owner(), ctx()) -> slug().
create(Source, ExpiresAt, Owner, Ctx) ->
    create(Source, ExpiresAt, Owner, 0, Ctx).

create(Source, ExpiresAt, Owner, Attempt, Ctx) ->
    ID = construct_id(Source, ExpiresAt, Attempt),
    Slug = #{source => Source, expires_at => ExpiresAt, owner => Owner},
    case start_machine(ID, Slug, Ctx) of
        {ok, _} ->
            Slug#{id => ID};
        {error, #mg_stateproc_MachineAlreadyExists{}} ->
            create(Source, ExpiresAt, Owner, Attempt + 1, Ctx)
    end.

-spec get(id(), ctx()) -> {ok, slug()} | {error, notfound}.
get(ID, Ctx) ->
    case get_machine_history(ID, Ctx) of
        {ok, History} ->
            Slug = collapse_history(History),
            {ok, Slug#{id => ID}};
        {error, #mg_stateproc_MachineNotFound{}} ->
            {error, notfound}
    end.

-spec remove(id(), ctx()) -> ok | {error, notfound}.
remove(ID, Ctx) ->
    case remove_machine(ID, Ctx) of
        {ok, _} ->
            ok;
        {error, #mg_stateproc_MachineNotFound{}} ->
            {error, notfound}
    end.

%%

construct_id(Source, ExpiresAt, Attempt) ->
    SpaceSize = get_space_size(),
    Message = <<Source/binary, ExpiresAt/binary, Attempt:SpaceSize/integer-unit:8>>,
    <<Hash:SpaceSize/integer-unit:8, _/binary>> = crypto:hash(get_hash_algorithm(), Message),
    format_id(Hash).

format_id(ID) ->
    genlib_format:format_int_base(ID, 62).

get_hash_algorithm() ->
    {ok, V} = application:get_env(shortener, hash_algorithm),
    V.

get_space_size() ->
    {ok, V} = application:get_env(shortener, space_size),
    V.

%%

start_machine(ID, Args, Ctx) ->
    issue_call('Start', {?NS, ID, marshal(term, Args)}, Ctx).

get_machine_history(ID, Ctx) ->
    Result = issue_call('GetMachine', {construct_descriptor(?NS, ID)}, Ctx),
    case Result of
        {ok, #mg_stateproc_Machine{history = History}} ->
            {ok, unmarshal_history(History)};
        Error ->
            Error
    end.

remove_machine(ID, Ctx) ->
    issue_call('Remove', {?NS, ID}, Ctx).

construct_descriptor(NS, ID) ->
    construct_descriptor(NS, ID, #mg_stateproc_HistoryRange{}).

construct_descriptor(NS, ID, HistoryRange) ->
    #mg_stateproc_MachineDescriptor{
        ns = NS,
        ref = {id, ID},
        range = HistoryRange
    }.

issue_call(Method, Args, Context) ->
    ClientOpts0 = get_service_client_config(automaton),
    ClientOpts1 = ClientOpts0#{event_handler => scoper_woody_event_handler},
    case call_service(automaton, Method, Args, ClientOpts1, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, #mg_stateproc_NamespaceNotFound{}} ->
            error(namespace_not_found);
        {exception, #mg_stateproc_MachineFailed{}} ->
            error(machine_failed);
        {exception, Exception} ->
            {error, Exception}
    end.

call_service(Service, Method, Args, ClientOpts, Context) ->
    Deadline = get_service_deadline(Service),
    DeadlineContext = set_deadline(Deadline, Context),
    Retry = get_service_retry(Service, Method),
    call_service(Service, Method, Args, ClientOpts, DeadlineContext, Retry).

call_service(Service, Method, Args, ClientOpts, Context, Retry) ->
    Request = {get_service_modname(Service), Method, Args},
    try
        woody_client:call(Request, ClientOpts, Context)
    catch
        error:{woody_error, {_Source, resource_unavailable, _Details}} = Error ->
            NextRetry = apply_retry_strategy(Retry, Error, Context),
            call_service(Service, Method, Args, ClientOpts, Context, NextRetry)
    end.

get_service_deadline(ServiceName) ->
    ServiceClient = get_service_client_config(ServiceName),
    Timeout = maps:get(deadline, ServiceClient, ?DEFAULT_DEADLINE),
    woody_deadline:from_timeout(Timeout).

set_deadline(Deadline, Context) ->
    case woody_context:get_deadline(Context) of
        undefined ->
            woody_context:set_deadline(Deadline, Context);
        _AlreadySet ->
            Context
    end.

get_service_retry(ServiceName, Method) ->
    ServiceClient = get_service_client_config(ServiceName),
    MethodReties = maps:get(retries, ServiceClient, #{}),
    DefaultRetry = maps:get('_', MethodReties, finish),
    maps:get(Method, MethodReties, DefaultRetry).

apply_retry_strategy(Retry, Error, Context) ->
    apply_retry_step(genlib_retry:next_step(Retry), woody_context:get_deadline(Context), Error).

apply_retry_step(finish, _, Error) ->
    erlang:error(Error);
apply_retry_step({wait, Timeout, Retry}, Deadline0, Error) ->
    Deadline1 = woody_deadline:from_unixtime_ms(
        woody_deadline:to_unixtime_ms(Deadline0) - Timeout
    ),
    case woody_deadline:is_reached(Deadline1) of
        true ->
            % no more time for retries
            erlang:error(Error);
        false ->
            ok = timer:sleep(Timeout),
            Retry
    end.

get_service_client_config(ServiceName) ->
    ServiceClients = genlib_app:env(shortener, service_clients, #{}),
    maps:get(ServiceName, ServiceClients, #{}).

%%

get_service_modname(automaton) ->
    {mg_proto_state_processing_thrift, 'Automaton'}.

%%

-type signal() :: mg_proto_state_processing_thrift:'SignalArgs'().
-type signal_result() :: mg_proto_state_processing_thrift:'SignalResult'().

-spec handle_function('ProcessSignal', {signal()}, ctx(), woody:options()) -> {ok, signal_result()} | no_return().
handle_function(Func, Args, Ctx, _Opts) ->
    scoper:scope(
        machine,
        fun() -> handle_function(Func, Args, Ctx) end
    ).

handle_function(
    'ProcessSignal',
    {
        #mg_stateproc_SignalArgs{
            signal = {Type, Signal},
            machine = #mg_stateproc_Machine{id = ID, history = History0} = Machine
        }
    },
    Ctx
) ->
    ok = scoper:add_meta(#{id => ID, signal => Type}),
    History = unmarshal_history(History0),
    Result =
        case Signal of
            #mg_stateproc_InitSignal{arg = Args} ->
                handle_init(unmarshal(term, Args), Ctx);
            #mg_stateproc_TimeoutSignal{} ->
                handle_timeout(collapse_history(History), Ctx);
            #mg_stateproc_RepairSignal{arg = Args} ->
                handle_repair(unmarshal(term, Args), collapse_history(History), Ctx)
        end,
    {ok, handle_signal_result(Result, Machine)}.

handle_signal_result(Result, Machine) ->
    #mg_stateproc_SignalResult{
        change = construct_machine_change(maps:get(events, Result, []), Machine),
        action = construct_complex_action(maps:get(actions, Result, []))
    }.

construct_machine_change(Events, #mg_stateproc_Machine{aux_state = AuxState}) ->
    #mg_stateproc_MachineStateChange{
        events = [construct_content(marshal(event, E)) || E <- Events],
        aux_state = construct_aux_state(AuxState)
    }.

construct_aux_state(undefined) ->
    construct_content({nl, #mg_msgpack_Nil{}});
construct_aux_state(AuxState) ->
    AuxState.

construct_content(Data) ->
    #mg_stateproc_Content{data = Data}.

construct_complex_action(Actions) ->
    lists:foldl(fun apply_action/2, #mg_stateproc_ComplexAction{}, Actions).

apply_action({set_timer, Timer}, CA) ->
    CA#mg_stateproc_ComplexAction{timer = {set_timer, #mg_stateproc_SetTimerAction{timer = Timer}}};
apply_action(remove, CA) ->
    CA#mg_stateproc_ComplexAction{remove = #mg_stateproc_RemoveAction{}}.

unmarshal_history(H) ->
    [unmarshal(event, E) || #mg_stateproc_Event{data = E} <- H].

%%

handle_init(#{source := Source, expires_at := ExpiresAt, owner := Owner}, _Ctx) ->
    #{
        events => [{created, #{source => Source, expires_at => ExpiresAt, owner => Owner}}],
        actions => [{set_timer, {deadline, ExpiresAt}}]
    }.

handle_timeout(_State, _Ctx) ->
    #{
        actions => [remove]
    }.

handle_repair(_Args, _State, _Ctx) ->
    #{}.

%%

collapse_history(History) ->
    lists:foldl(fun apply_event/2, undefined, History).

apply_event({created, Slug}, undefined) ->
    Slug.

%%

marshal(event, {created, #{source := Source, expires_at := ExpiresAt, owner := Owner}}) ->
    {arr, [{i, 2}, marshal(string, Source), marshal(timestamp, ExpiresAt), marshal(string, Owner)]};
marshal(timestamp, V) ->
    marshal(string, V);
marshal(string, V) ->
    {str, V};
marshal(term, V) ->
    {bin, term_to_binary(V)}.

unmarshal(event, {arr, [{i, 2}, Source, ExpiresAt, Owner]}) ->
    {created, #{
        source => unmarshal(string, Source),
        expires_at => unmarshal(timestamp, ExpiresAt),
        owner => unmarshal(string, Owner)
    }};
unmarshal(event, {arr, [{i, 1}, Source, ExpiresAt]}) ->
    {created, #{
        source => unmarshal(string, Source),
        expires_at => unmarshal(timestamp, ExpiresAt),
        owner => undefined
    }};
unmarshal(timestamp, V) ->
    unmarshal(string, V);
unmarshal(string, {str, V}) ->
    V;
unmarshal(term, {bin, V}) ->
    binary_to_term(V).
