-module(shortener_slug).
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

%% API

-export([create/3]).
-export([get/2]).
-export([remove/2]).

%% Processor

-behaviour(woody_server_thrift_handler).

-define(NS, <<"shortener">>).

-export([handle_function/4]).

%%

-type timestamp()  :: binary(). % RFC 3339

-type id()         :: binary().
-type source()     :: binary().
-type expiration() :: timestamp().

-type slug()       :: #{
    id             => id(),
    source         => source(),
    expires_at     => expiration()
}.

-type ctx()        :: woody_context:ctx().

-spec create(source(), expiration(), ctx()) ->
    slug().

create(Source, ExpiresAt, Ctx) ->
    create(Source, ExpiresAt, 0, Ctx).

create(Source, ExpiresAt, Attempt, Ctx) ->
    ID = construct_id(Source, ExpiresAt, Attempt),
    Slug = #{source => Source, expires_at => ExpiresAt},
    case start_machine(ID, Slug, Ctx) of
        {ok, _} ->
            Slug#{id => ID};
        {error, #mg_stateproc_MachineAlreadyExists{}} ->
            create(Source, ExpiresAt, Attempt + 1, Ctx)
    end.

-spec get(id(), ctx()) ->
    {ok, slug()} | {error, notfound}.

get(ID, Ctx) ->
    case get_machine_history(ID, Ctx) of
        {ok, History} ->
            {ok, collapse_history(History)};
        {error, #mg_stateproc_MachineNotFound{}} ->
            {error, notfound}
    end.

-spec remove(id(), ctx()) ->
    ok | {error, notfound}.

remove(ID, Ctx) ->
    case remove_machine(ID, Ctx) of
        {ok, _} ->
            ok;
        {error, #mg_stateproc_MachineNotFound{}} ->
            {error, notfound}
    end.

%%

construct_id(Source, ExpiresAt, _Attempt) ->
    SpaceSize = get_space_size(),
    RandBytes = crypto:strong_rand_bytes(SpaceSize),
    Message = <<Source/binary, ExpiresAt/binary, RandBytes/binary>>,
    HashAlgo = get_hash_algorithm(),
    <<Hash:SpaceSize/integer-unit:8, _/binary>> = crypto:hash(HashAlgo, Message),
    genlib_format:format_int_base(Hash, 62).

get_hash_algorithm() ->
    {ok, V} = application:get_env(shortener, hash_algorithm), V.

get_space_size() ->
    {ok, V} = application:get_env(shortener, space_size), V.

%%

start_machine(ID, Args, Ctx) ->
    issue_call('Start', [?NS, ID, marshal(term, Args)], Ctx).

get_machine_history(ID, Ctx) ->
    Result = issue_call('GetMachine', [construct_descriptor(?NS, ID)], Ctx),
    case Result of
        {ok, #mg_stateproc_Machine{history = History}} ->
            {ok, unmarshal_history(History)};
        Error ->
            Error
    end.

remove_machine(ID, Ctx) ->
    issue_call('Remove', [?NS, ID], Ctx).

construct_descriptor(NS, ID) ->
    construct_descriptor(NS, ID, #mg_stateproc_HistoryRange{}).

construct_descriptor(NS, ID, HistoryRange) ->
    #'mg_stateproc_MachineDescriptor'{
        ns = NS,
        ref = {id, ID},
        range = HistoryRange
    }.

issue_call(Method, Args, Context) ->
    Request = {{mg_proto_state_processing_thrift, 'Automaton'}, Method, Args},
    ClientOpts0 = maps:get(automaton, genlib_app:env(shortener, service_clients)),
    ClientOpts1 = ClientOpts0#{event_handler => scoper_woody_event_handler},
    case woody_client:call(Request, ClientOpts1, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, #mg_stateproc_NamespaceNotFound{}} ->
            error(namespace_not_found);
        {exception, #mg_stateproc_MachineFailed{}} ->
            error(machine_failed);
        {exception, Exception} ->
            {error, Exception}
    end.

%%

-type signal()        :: mg_proto_state_processing_thrift:'SignalArgs'().
-type signal_result() :: mg_proto_state_processing_thrift:'SignalResult'().

-spec handle_function
    ('ProcessSignal', [signal()], ctx(), woody:options()) ->
        {ok, signal_result()} | no_return().

handle_function(Func, Args, Ctx, _Opts) ->
    scoper:scope(machine,
        fun() -> handle_function(Func, Args, Ctx) end
    ).

handle_function('ProcessSignal', [
    #mg_stateproc_SignalArgs{
        signal = {Type, Signal},
        machine = #mg_stateproc_Machine{id = ID, history = History0} = Machine
    }
], Ctx) ->
    ok = scoper:add_meta(#{id => ID, signal => Type}),
    History = unmarshal_history(History0),
    Result = case Signal of
        #mg_stateproc_InitSignal{arg = Args} ->
            handle_init(unmarshal(term, Args), Ctx);
        #mg_stateproc_TimeoutSignal{} ->
            handle_timeout(collapse_history(History), Ctx);
        #mg_stateproc_RepairSignal{arg = Args} ->
            handle_repair(unmarshal(term, Args), collapse_history(History), Ctx)
    end,
    handle_signal_result(Result, Machine).

handle_signal_result(Result, Machine) ->
    #mg_stateproc_SignalResult{
        change = construct_machine_change(maps:get(changes, Result, []), Machine),
        action = construct_complex_action(maps:get(actions, Result, []))
    }.

construct_machine_change(Changes, #mg_stateproc_Machine{aux_state = AuxState}) ->
    #mg_stateproc_MachineStateChange{
        events    = marshal_history(Changes),
        aux_state = construct_aux_state(AuxState)
    }.

construct_aux_state(undefined) ->
    {nl, #mg_msgpack_Nil{}};
construct_aux_state(AuxState) ->
    AuxState.

construct_complex_action(Actions) ->
    lists:foldl(fun apply_action/2, #mg_stateproc_ComplexAction{}, Actions).

apply_action({set_timer, Timer}, CA) ->
    CA#mg_stateproc_ComplexAction{timer = {set_timer, #mg_stateproc_SetTimerAction{timer = Timer}}};
apply_action(remove, CA) ->
    CA#mg_stateproc_ComplexAction{remove = #mg_stateproc_RemoveAction{}}.

%%

marshal_history(V) ->
    marshal({list, event}, V).

unmarshal_history(V) ->
    unmarshal({list, event}, V).

%%

handle_init(#{source := Source, expires_at := ExpiresAt}, _Ctx) ->
    #{
        changes => [{created, #{source => Source, expires_at => ExpiresAt}}],
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
    lists:foldl(fun apply_change/2, undefined, lists:flatten(History)).

apply_change({created, Slug}, undefined) ->
    Slug.

%%

marshal(event, {created, #{source := Source, expires_at := ExpiresAt}}) ->
    [1, marshal(string, Source), marshal(timestamp, ExpiresAt)];

marshal(timestamp, V) ->
    marshal(string, V);
marshal(string, V) ->
    {str, V};
marshal(term, V) ->
    {bin, term_to_binary(V)}.

unmarshal(event, [1, Source, ExpiresAt]) ->
    {created, #{source => unmarshal(string, Source), expires_at => marshal(timestamp, ExpiresAt)}};

unmarshal(timestamp, V) ->
    unmarshal(string, V);
unmarshal(string, {str, V}) ->
    V;
unmarshal(term, {bin, V}) ->
    binary_to_term(V).
