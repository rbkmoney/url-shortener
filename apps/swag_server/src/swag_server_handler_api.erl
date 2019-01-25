%% -*- mode: erlang -*-
-module(swag_server_handler_api).

-export([authorize_api_key/5]).
-export([determine_peer/1]).
-export([populate_request/2]).
-export([validate_response/2]).
-export([encode_response/1]).
-export([process_response/4]).

%%
-type param_source() ::
    qs_val  |
    binding |
    header  |
    body.

-type request_spec() :: [{
    swag_server:param_name(),
    #{source := param_source(), rules := [swag_server_validation:rule()]}
}].

-type response_spec() :: swag_server_validation:response_spec().

-export_type([param_source/0]).
-export_type([request_spec/0]).
-export_type([response_spec/0]).

-type response() :: {cowboy:http_status(), cowboy:http_headers(), binary() | undefined}.

%% API

-spec authorize_api_key(
    LogicHandler :: module(),
    OperationID  :: swag_server:operation_id(),
    From         :: header | qs_val,
    KeyParam     :: iodata() | atom(),
    Req          :: cowboy_req:req()
)->
    {true,  Context    :: swag_server:auth_context(), Req ::cowboy_req:req()} |
    {false, AuthHeader :: binary(),                       Req ::cowboy_req:req()}.

authorize_api_key(LogicHandler, OperationID, From, KeyParam, Req0) ->
    {ok, ApiKey, Req} = get_value(From, KeyParam, Req0),
    case ApiKey of
        undefined ->
            AuthHeader = <<"">>,
            {false, AuthHeader, Req};
        _ ->
            Result = swag_server_logic_handler:authorize_api_key(
                LogicHandler,
                OperationID,
                ApiKey
            ),
            case Result of
                {true, Context}  ->
                    {true, Context, Req};
                false ->
                    AuthHeader = <<"">>,
                    {false, AuthHeader, Req}
            end
    end.

-spec determine_peer(Req :: cowboy_req:req()) ->
    {
        {ok, Peer :: swag_server:client_peer()} | {error, Reason :: malformed | einval},
        Req :: cowboy_req:req()
    }.

determine_peer(Req) ->
    {Peer, Req1}  = cowboy_req:peer(Req),
    {Value, Req2} = cowboy_req:header(<<"x-forwarded-for">>, Req1),
    {determine_peer_from_header(Value, Peer), Req2}.

-spec populate_request(Spec :: request_spec(), Req :: cowboy_req:req()) ->
    {ok,    Populated :: swag_server:object(),       Req :: cowboy_req:req()} |
    {error, Message   :: swag_server:error_reason(), Req :: cowboy_req:req()}.

populate_request(Spec, Req) ->
    populate_request(Spec, Req, #{}).

-spec validate_response(
    Spec     :: response_spec(),
    RespBody :: swag_server:object() | [swag_server:object()] | undefined
) ->
    ok | no_return().

validate_response(Spec, RespBody) ->
    case swag_server_validation:validate_response(Spec, RespBody) of
        ok ->
            ok;
        {error, Error} ->
            erlang:error({response_validation_failed, Error, RespBody})
    end.

-spec encode_response(Resp :: swag_server_logic_handler:response()) ->
    Encoded :: response().

encode_response(Resp = {_, _, undefined}) ->
    Resp;
encode_response({Code, Headers, Body}) ->
    {Code, Headers, jsx:encode(Body)}.

-spec process_response(
    Status      :: ok | error,
    Result      :: response() | swag_server:error_reason(),
    Req         :: cowboy_req:req(),
    OperationID :: swag_server:operation_id()
) ->
    Req :: cowboy_req:req().

process_response(ok, {Code, Headers, undefined}, Req0, _) ->
    {ok, Req} = cowboy_req:reply(Code, Headers, Req0),
    Req;
process_response(ok, {Code, Headers, Body}, Req0, _) ->
    {ok, Req} = cowboy_req:reply(Code, Headers, Body, Req0),
    Req;
process_response(error, Message, Req0, OperationID) ->
    error_logger:info_msg(
        "Unable to process request for ~p: ~ts",
        [OperationID, Message]
    ),
    {ok, Req} = cowboy_req:reply(400, [], Message, Req0),
    Req.


%% Internal

populate_request([], Req, Populated) ->
    {ok, Populated, Req};
populate_request([ParamSpec | T], Req0, Populated) ->
    case populate_request_param(ParamSpec, Req0) of
        {ok, K, V, Req} ->
            populate_request(T, Req, maps:put(K, V, Populated));
        Error ->
            Error
    end.

populate_request_param({Name, #{rules := Rules, source := Source}}, Req0) ->
    case get_value(Source, Name, Req0) of
        {ok, Value, Req} ->
            case swag_server_validation:prepare_request_param(Rules, Name, Value) of
                {ok, Result} ->
                    {ok, Name, Result, Req};
                {error, Error}  ->
                    {error, error_message(Name, Error), Req}
            end;
        {error, Message, Req} ->
            {error, error_message(Name, wrong_body, Message), Req}
    end.

-spec error_message(Name :: swag_server:param_name(), Error :: swag_server_validation:error()) ->
    Message :: swag_server:error_reason().
error_message(Name, Error = #{type := Type}) ->
    error_message(Name, Type, maps:get(description, Error, <<>>)).

error_message(Name, ErrorType, Description) ->
    jsx:encode(#{
        <<"name">>        => swag_server_utils:to_binary(Name),
        <<"errorType">>   => swag_server_utils:to_binary(ErrorType),
        <<"description">> => swag_server_utils:to_binary(Description)
    }).

determine_peer_from_header(undefined, {IP, Port}) ->
    % undefined, assuming no proxies were involved
    {ok, #{ip_address => IP, port_number => Port}};
determine_peer_from_header(Value, _Peer) when is_binary(Value) ->
    ClientPeer = string:strip(binary_to_list(Value)),
    case string:tokens(ClientPeer, ", ") of
        [ClientIP | _Proxies] ->
            case inet:parse_strict_address(ClientIP) of
                {ok, IP} ->
                    % ok
                    {ok, #{ip_address => IP}};
                Error ->
                    % unparseable ip address
                    Error
            end;
        _ ->
            % empty or malformed value
            {error, malformed}
    end.

-spec get_value(
    Source :: param_source(),
    Name   :: swag_server:param_name(),
    Req    :: cowboy_req:req()
) ->
    {ok,    Value   :: swag_server:value(),        Req :: cowboy_req:req()} |
    {error, Message :: swag_server:error_reason(), Req :: cowboy_req:req()}.

get_value(body, _Name, Req0) ->
    {ok, Body, Req} = cowboy_req:body(Req0),
    case decode_body(Body) of
        {ok, Value} ->
            {ok, Value, Req};
        {error, Message} ->
            {error, Message, Req}
    end;
get_value(qs_val, Name, Req0) ->
    try cowboy_req:qs_vals(Req0) of
        {QS, Req} ->
            Value = swag_server_utils:get_opt(swag_server_utils:to_qs(Name), QS),
            {ok, Value, Req}
    catch
        error:_ ->
            {error, <<"Invalid query">>}
    end;
get_value(header, Name, Req0) ->
    {Headers, Req} = cowboy_req:headers(Req0),
    Value          = swag_server_utils:get_opt(swag_server_utils:to_header(Name), Headers),
    {ok, Value, Req};
get_value(binding, Name, Req0) ->
    {Bindings, Req} = cowboy_req:bindings(Req0),
    Value           = swag_server_utils:get_opt(swag_server_utils:to_binding(Name), Bindings),
    {ok, Value, Req}.

-spec decode_body(Body :: binary()) ->
    {ok,    Decoded :: swag_server:object() | undefined} |
    {error, Message :: swag_server:error_reason()}.
decode_body(<<>>) ->
    {ok, undefined};
decode_body(Body) ->
    try
        {ok, jsx:decode(Body, [return_maps])}
    catch
        _:_ ->
            {error, <<"Invalid json">>}
    end.
