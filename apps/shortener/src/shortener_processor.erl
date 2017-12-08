-module(shortener_processor).

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-type id() :: mg_proto_base_thrift:'ID'().
-type tag() :: {tag, mg_proto_base_thrift:'Tag'()}.
-type ref() :: id() | tag().
-type ns() :: mg_proto_base_thrift:'Namespace'().
-type args() :: _.

-type context() :: woody_context:ctx().

-type handler_opts() :: #{
    handler => module(),
    user_identity => undefined | woody_user_identity:user_identity()
}.

-type client_opts() :: #{
    url            := woody:url(),
    transport_opts => [{_, _}]
}.

%%

-spec start(ns(), id(), term()) ->
    {ok, term()} | {error, exists | term()} | no_return().

start(Ns, ID, Args) ->
    call_automaton('Start', [Ns, ID, wrap_args(Args)]).

-spec call(ns(), ref(), term()) ->
    {ok, term()} | {error, notfound | failed} | no_return().

call(Ns, Ref, Args) ->
    Descriptor = prepare_descriptor(Ns, Ref, #'HistoryRange'{}),
    case call_automaton('Call', [Descriptor, wrap_args(Args)]) of
        {ok, Response} ->
            % should be specific to a processing interface already
            {ok, unmarshal_term(Response)};
        {error, _} = Error ->
            Error
    end.

-spec get_history(ns(), ref()) ->
    {ok, history()} | {error, notfound} | no_return().

get_history(Ns, Ref) ->
    get_history(Ns, Ref, #'HistoryRange'{}).

-spec get_history(ns(), ref(), undefined | event_id(), undefined | non_neg_integer()) ->
    {ok, history()} | {error, notfound} | no_return().

get_history(Ns, Ref, AfterID, Limit) ->
    get_history(Ns, Ref, #'HistoryRange'{'after' = AfterID, limit = Limit}).

get_history(Ns, Ref, Range) ->
    Descriptor = prepare_descriptor(Ns, Ref, Range),
    case call_automaton('GetMachine', [Descriptor]) of
        {ok, #'Machine'{history = History}} when is_list(History) ->
            {ok, unmarshal_events(History)};
        Error ->
            Error
    end.

%%

-type func() :: 'ProcessSignal' | 'ProcessCall'.

-spec handle_function(func(), woody:args(), handler_opts()) ->
    term() | no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(machine,
        fun() -> handle_function_(Func, Args, Opts) end
).

-spec handle_function_(func(), woody:args(), #{ns := ns()}) -> term() | no_return().

handle_function_('ProcessSignal', [Args], #{ns := Ns} = _Opts) ->
    #'SignalArgs'{signal = {_Type, Signal}, machine = Machine} = Args,
    % dispatch_signal(Ns, Signal, Machine);

handle_function_('ProcessCall', [Args], #{ns := Ns} = _Opts) ->
    #'CallArgs'{arg = Payload, machine = Machine} = Args,
    % dispatch_call(Ns, Payload, Machine).

