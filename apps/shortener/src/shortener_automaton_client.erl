-module(shortener_automaton_client).
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-export([call/4]).
-export([call/5]).
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
    response() |
    no_return().
call(NS, ID, Args, Context) ->
    call(NS, ID, #'mg_stateproc_HistoryRange'{}, Args, Context).

-spec call(ns(), id(), history_range(), args(), context()) ->
    response() |
    no_return().
call(NS, ID, HistoryRange, Args, Context) ->
    Descriptor = construct_descriptor(NS, ID, HistoryRange),
    case issue_rpc('Call', [Descriptor, Args], Context) of
        {ok, Result} ->
            Result;
        {error, #'mg_stateproc_MachineNotFound'{}} ->
            ok = start(NS, ID, Context),
            call(NS, ID, Args, Context)
    end.

-spec get_history(ns(), id(), context()) ->
    {ok, history()} |
    {error,
        mg_proto_state_processing_thrift:'EventNotFound'() |
        mg_proto_state_processing_thrift:'MachineNotFound'()
    } |
    no_return().
get_history(NS, ID, Context) ->
    get_history(NS, ID, #'mg_stateproc_HistoryRange'{}, Context).

-spec get_history(ns(), id(), history_range(), context()) ->
    {ok, history()} |
    {error,
        mg_proto_state_processing_thrift:'EventNotFound'() |
        mg_proto_state_processing_thrift:'MachineNotFound'()
    } |
    no_return().
get_history(NS, ID, HistoryRange, Context) ->
    Descriptor = construct_descriptor(NS, ID, HistoryRange),
    case issue_rpc('GetMachine', [Descriptor], Context) of
        {ok, #'mg_stateproc_Machine'{history = History}} ->
            {ok, History};
        {error, _} = Error ->
            Error
    end.

-spec start(ns(), id(), context()) ->
    ok | no_return().
start(NS, ID, Context) ->
    case issue_rpc('Start', [NS, ID, {nl, #mg_msgpack_Nil{}}], Context) of
        {ok, _} ->
            ok;
        {error, #'mg_stateproc_MachineAlreadyExists'{}} ->
            ok
    end.

-spec remove(ns(), id(), context()) ->
    ok | no_return().
remove(NS, ID, Context) ->
    case issue_rpc('Remove', [NS, ID], Context) of
        {ok, _} ->
            ok;
        {error, #'mg_stateproc_MachineAlreadyExists'{}} ->
            ok
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
    {ok, URL} = application:get_env(shortener, automaton_service_url),
    Opts = #{
        url => URL,
        transport_opts => [
            {recv_timeout, 60000}
        ]
    },
    case woody_client:call(Request, Opts, Context) of
        {ok, _} = Ok ->
            Ok;
        {exception, #'mg_stateproc_NamespaceNotFound'{}} ->
            error(namespace_not_found);
        {exception, #'mg_stateproc_MachineFailed'{}} ->
            error(machine_failed);
        {exception, Exception} ->
            {error, Exception}
    end.
