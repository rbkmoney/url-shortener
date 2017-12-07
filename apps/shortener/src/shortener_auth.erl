-module(shortener_auth).

-export([authorize_api_key/2]).
-export([authorize_operation/1]).

-type context() :: capi_authorizer_jwt:t().
-type claims()  :: capi_authorizer_jwt:claims().

-export_type([context/0]).
-export_type([claims/0]).

-spec authorize_api_key(
    OperationID :: swag_server:operation_id(),
    ApiKey      :: swag_server:api_key()
) -> {true, Context :: context()} | false.

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

-spec parse_api_key(ApiKey :: swag_server:api_key()) ->
    {ok, {bearer, Credentials :: binary()}} | {error, Reason :: atom()}.

parse_api_key(ApiKey) ->
    case ApiKey of
        <<"Bearer ", Credentials/binary>> ->
            {ok, {bearer, Credentials}};
        _ ->
            {error, unsupported_auth_scheme}
    end.

-spec authorize_api_key(
    OperationID :: swag_server:operation_id(),
    Type :: atom(),
    Credentials :: binary()
) ->
    {ok, Context :: context()} | {error, Reason :: atom()}.

authorize_api_key(_OperationID, bearer, Token) ->
    % NOTE
    % We are knowingly delegating actual request authorization to the logic handler
    % so we could gather more data to perform fine-grained access control.
    capi_authorizer_jwt:verify(Token).

%%

-spec authorize_operation(Auth :: capi_authorizer_jwt:t()) ->
    ok | {error, unauthorized}.

authorize_operation({{_SubjectID, ACL}, _}) ->
    {Scope, Permission} = {shortened_urls, write},
    case lists:member(Permission, capi_acl:match(Scope, ACL)) of
        true ->
            ok;
        false ->
            {error, unauthorized}
    end.

%%
