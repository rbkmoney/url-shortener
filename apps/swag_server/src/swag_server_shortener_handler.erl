%% -*- mode: erlang -*-

%% basic handler
-module(swag_server_shortener_handler).

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/3]).
-export([rest_init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request_json/2]).

-record(state, {
    operation_id  :: swag_server:operation_id(),
    logic_handler :: module(),
    context       :: swag_server:request_context()
}).

-type state()              :: state().
-type content_type()       :: {binary(), binary(), '*' | [{binary(), binary()}]}.
-type processed_response() :: {halt, cowboy_req:req(), state()}.

%% Cowboy REST callbacks

-spec init(TransportName :: atom(), Req :: cowboy_req:req(), Opts :: swag_server_router:init_opts()) ->
    {upgrade, protocol, cowboy_rest, Req :: cowboy_req:req(), Opts :: swag_server_router:init_opts()}.

init(_Transport, Req, Opts) ->
    {upgrade, protocol, cowboy_rest, Req, Opts}.

-spec rest_init(Req :: cowboy_req:req(), Opts :: swag_server_router:init_opts()) ->
    {ok, Req :: cowboy_req:req(), State :: state()}.

rest_init(Req0, {Operations, LogicHandler}) ->
    {Method, Req1} = cowboy_req:method(Req0),
    OperationID    = maps:get(Method, Operations, undefined),

    error_logger:info_msg("Attempt to process operation: ~p", [OperationID]),

    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        context       = #{}
    },

    {ok, Req1, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: cowboy_req:req(), State :: state()}.


allowed_methods(
    Req,
    State = #state{
        operation_id = 'DeleteShortenedUrl'
    }
) ->
    {[<<"DELETE">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'GetShortenedUrl'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'ShortenUrl'
    }
) ->
    {[<<"POST">>], Req, State};

allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req   :: cowboy_req:req(),
        State :: state()
    }.

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'DeleteShortenedUrl' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'GetShortenedUrl' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'ShortenUrl' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(Req, State) ->
    {{false, <<"">>}, Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), AcceptResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, handle_request_json}
    ], Req, State}.

-spec valid_content_headers(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'DeleteShortenedUrl'
    }
) ->
    Headers = ["X-Request-ID"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'GetShortenedUrl'
    }
) ->
    Headers = ["X-Request-ID"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'ShortenUrl'
    }
) ->
    Headers = ["X-Request-ID"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), ProvideResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_request_json}
    ], Req, State}.

-spec charsets_provided(Req :: cowboy_req:req(), State :: state()) ->
    {Charsets :: [binary()], Req :: cowboy_req:req(), State :: state()}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

-spec malformed_request(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

malformed_request(Req, State = #state{context = Context}) ->
    {PeerResult, Req1} = swag_server_handler_api:determine_peer(Req),
    case PeerResult of
        {ok, Peer} ->
            Context1 = Context#{peer => Peer},
            State1   = State#state{context = Context1},
            {false, Req1, State1};
        {error, Reason} ->
            error_logger:error_msg("Unable to determine client peer: ~p", [Reason]),
            {true, Req1, State}
    end.

-spec allow_missing_post(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

delete_resource(Req, State) ->
    handle_request_json(Req, State).

-spec known_content_type(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

known_content_type(Req, State) ->
    {true, Req, State}.

-spec valid_entity_length(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.


%% Handlers

-spec handle_request_json(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

handle_request_json(
    Req0,
    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    case populate_request(OperationID, Req0) of
        {ok, Populated, Req1} ->
            {Status, Resp} = handle_request(LogicHandler, OperationID, Populated, Context),
            ok             = validate_response(Status, Resp, OperationID),
            process_response(ok, encode_response(Resp), Req1, State);
        {error, Reason, Req1} ->
            process_response(error, Reason, Req1, State)
    end.


%% Internal

populate_request(OperationID, Req) ->
    swag_server_handler_api:populate_request(get_request_spec(OperationID), Req).

handle_request(LogicHandler, OperationID, Populated, Context) ->
    swag_server_logic_handler:handle_request(LogicHandler, OperationID, Populated, Context).

validate_response(error, _, _) ->
    ok;
validate_response(ok, {Code, _Headers, Body}, OperationID) ->
    Spec = get_response_spec(OperationID, Code),
    swag_server_handler_api:validate_response(Spec, Body).

encode_response(Resp) ->
    swag_server_handler_api:encode_response(Resp).

process_response(Status, Result, Req0, State = #state{operation_id = OperationID}) ->
    Req = swag_server_handler_api:process_response(Status, Result, Req0, OperationID),
    {halt, Req, State}.

validate_headers(_, Req) ->
    {true, Req}.

-spec get_request_spec(OperationID :: swag_server:operation_id()) ->
    Spec :: swag_server_handler_api:request_spec() | no_return().


get_request_spec('DeleteShortenedUrl') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'shortenedUrlID', #{
            source => binding,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }}
    ];

get_request_spec('GetShortenedUrl') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'shortenedUrlID', #{
            source => binding,
            rules  => [{type, 'binary'}, true
, {required, true}]
        }}
    ];

get_request_spec('ShortenUrl') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'ShortenedUrlParams', #{
            source => body,
            rules  => [schema, {required, true}]
        }}
    ];

get_request_spec(OperationID) ->
    error({invalid_operation_id, OperationID}).

-spec get_response_spec(OperationID :: swag_server:operation_id(), Code :: cowboy:http_status()) ->
    Spec :: swag_server_handler_api:response_spec() | no_return().


get_response_spec('DeleteShortenedUrl', 204) ->
    undefined;

get_response_spec('DeleteShortenedUrl', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('DeleteShortenedUrl', 401) ->
    undefined;

get_response_spec('DeleteShortenedUrl', 403) ->
    undefined;

get_response_spec('DeleteShortenedUrl', 404) ->
    undefined;

get_response_spec('GetShortenedUrl', 200) ->
    {'ShortenedUrl', 'ShortenedUrl'};

get_response_spec('GetShortenedUrl', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('GetShortenedUrl', 401) ->
    undefined;

get_response_spec('GetShortenedUrl', 403) ->
    undefined;

get_response_spec('GetShortenedUrl', 404) ->
    undefined;

get_response_spec('ShortenUrl', 201) ->
    {'ShortenedUrl', 'ShortenedUrl'};

get_response_spec('ShortenUrl', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('ShortenUrl', 401) ->
    undefined;

get_response_spec('ShortenUrl', 403) ->
    undefined;

get_response_spec(OperationID, Code) ->
    error({invalid_response_code, OperationID, Code}).
