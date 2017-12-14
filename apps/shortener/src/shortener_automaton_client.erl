-module(shortener_automaton_client).
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([call/4]).
-export([get_history/3]).
-export([get_history/4]).
-export([start/3]).
-export([remove/3]).

%%

-type ns()            :: mg_proto_base_thrift:'Namespace'().
-type id()            :: mg_proto_base_thrift:'ID'().
-type args()          :: mg_proto_state_processing_thrift:'Args'().
-type response()      :: mg_proto_state_processing_thrift:'CallResponse'().
-type descriptor()    :: mg_proto_state_processing_thrift:'MachineDescriptor'().
-type history_range() :: mg_proto_state_processing_thrift:'HistoryRange'().
-type history()       :: mg_proto_state_processing_thrift:'History'().
-type context()       :: woody_context:ctx().

%%

-spec call(ns(), id(), args(), context()) ->
    {ok, response()} | {error, notfound} | no_return().
call(NS, ID, Args, Context) ->
    call(NS, ID, #'mg_stateproc_HistoryRange'{}, Args, Context).

call(NS, ID, HistoryRange, Args, Context) ->
    Descriptor = construct_descriptor(NS, ID, HistoryRange),
    case issue_rpc('Call', [Descriptor, Args], Context) of
        {ok, Result} ->
            {ok, Result};
        {error, #'mg_stateproc_MachineNotFound'{}} ->
            {error, notfound}
    end.

-spec get_history(ns(), id(), context()) ->
    {ok, history()} | {error, notfound} | no_return().
get_history(NS, ID, Context) ->
    get_history(NS, ID, #'mg_stateproc_HistoryRange'{}, Context).

get_history(NS, ID, HistoryRange, Context) ->
    Descriptor = construct_descriptor(NS, ID, HistoryRange),
    case issue_rpc('GetMachine', [Descriptor], Context) of
        {ok, #'mg_stateproc_Machine'{history = History}} ->
            {ok, History};
        {error, #mg_stateproc_MachineNotFound{}} ->
            {error, notfound}
    end.

-spec start(ns(), id(), context()) ->
    ok | {error, exists} | no_return().
start(NS, ID, Context) ->
    case issue_rpc('Start', [NS, ID, {nl, #mg_msgpack_Nil{}}], Context) of
        {ok, _} ->
            ok;
        {error, #'mg_stateproc_MachineAlreadyExists'{}} ->
            {error, exists}
    end.

-spec remove(ns(), id(), context()) ->
    ok | {error, notfound} | no_return().
remove(NS, ID, Context) ->
    case issue_rpc('Remove', [NS, ID], Context) of
        {ok, _} ->
            ok;
        {error, #mg_stateproc_MachineNotFound{}} ->
            {error, notfound}
    end.

-spec construct_descriptor(ns(), id(), history_range()) ->
    descriptor().
construct_descriptor(NS, ID, HistoryRange) ->
    #'mg_stateproc_MachineDescriptor'{
        ns = NS,
        ref = {id, ID},
        range = HistoryRange
    }.

-spec issue_rpc(atom(), list(term()), context()) ->
    term() | no_return().
issue_rpc(Method, Args, Context) ->
    Request = {{mg_proto_state_processing_thrift, 'Automaton'}, Method, Args},
    ClientOpts0 = maps:get(automaton, genlib_app:env(shortener, service_clients)),
    ClientOpts1 = ClientOpts0#{event_handler => scoper_woody_event_handler},
    case woody_client:call(Request, ClientOpts1, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, #'mg_stateproc_NamespaceNotFound'{}} ->
            error(namespace_not_found);
        {exception, #'mg_stateproc_MachineFailed'{}} ->
            error(machine_failed);
        {exception, Exception} ->
            {error, Exception}
    end.
