-module(shortener_auth).

-export([authorize_api_key/2]).
-export([authorize_operation/2]).

-type context() :: shortener_authorizer_jwt:t().
-type claims()  :: shortener_authorizer_jwt:claims().

-export_type([context/0]).
-export_type([claims/0]).

-spec authorize_api_key(swag_server:operation_id(), swag_server:api_key()) ->
    {true, Context :: context()} | false.

authorize_api_key(OperationID, ApiKey) ->
    case parse_api_key(ApiKey) of
        {ok, {Type, Credentials}} ->
            case authorize_api_key(OperationID, Type, Credentials) of
                {ok, Context} ->
                    {true, Context};
                {error, Error} ->
                    _ = log_auth_error(OperationID, Error),
                    false
            end;
        {error, Error} ->
            _ = log_auth_error(OperationID, Error),
            false
    end.

log_auth_error(OperationID, Error) ->
    lager:info("API Key authorization failed for ~p due to ~p", [OperationID, Error]).

-spec parse_api_key(swag_server:api_key()) ->
    {ok, {bearer, Credentials :: binary()}} | {error, Reason :: atom()}.

parse_api_key(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, {bearer, Credentials}};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec authorize_api_key(swag_server:operation_id(), Type :: atom(), Credentials :: binary()) ->
    {ok, context()} | {error, Reason :: atom()}.

authorize_api_key(_OperationID, bearer, Token) ->
    shortener_authorizer_jwt:verify(Token).

-spec authorize_operation(swag_server:operation_id(), context()) ->
    ok | {error, forbidden}.

authorize_operation(OperationID, {{_SubjectID, ACL}, _Claims}) ->
    Permissions = shortener_acl:match(['shortened-urls'], ACL),
    case is_operation_permitted(OperationID, Permissions) of
        true ->
            ok;
        false ->
            {error, forbidden}
    end.

is_operation_permitted('ShortenUrl', Ps) ->
    lists:member(write, Ps);
is_operation_permitted('DeleteShortenedUrl', Ps) ->
    lists:member(write, Ps);
is_operation_permitted('GetShortenedUrl', Ps) ->
    lists:member(read, Ps).
