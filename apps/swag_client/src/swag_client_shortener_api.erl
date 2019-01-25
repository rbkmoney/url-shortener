%% -*- mode: erlang -*-
-module(swag_client_shortener_api).

%% generated methods

-export([delete_shortened_url/2]).
-export([delete_shortened_url/3]).

-export([get_shortened_url/2]).
-export([get_shortened_url/3]).

-export([shorten_url/2]).
-export([shorten_url/3]).


-spec delete_shortened_url(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
delete_shortened_url(Endpoint, Params) ->
    delete_shortened_url(Endpoint, Params, []).

-spec delete_shortened_url(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
delete_shortened_url(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        delete,
        swag_client_utils:get_url(Endpoint, "/v1/shortened-urls/:shortenedUrlID"),
        Params,
        get_request_spec(delete_shortened_url),
        Opts
    ), delete_shortened_url).

-spec get_shortened_url(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shortened_url(Endpoint, Params) ->
    get_shortened_url(Endpoint, Params, []).

-spec get_shortened_url(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_shortened_url(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/v1/shortened-urls/:shortenedUrlID"),
        Params,
        get_request_spec(get_shortened_url),
        Opts
    ), get_shortened_url).

-spec shorten_url(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
shorten_url(Endpoint, Params) ->
    shorten_url(Endpoint, Params, []).

-spec shorten_url(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
shorten_url(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/v1/shortened-urls"),
        Params,
        get_request_spec(shorten_url),
        Opts
    ), shorten_url).

process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try swag_client_procession:process_response(
        get_response_spec(OperationID, Code),
        RespBody
    ) of
        {ok, Resp} ->
            {ok, Code, Headers, Resp};
        Error ->
            Error
    catch
        error:invalid_response_code ->
            {error, {invalid_response_code, Code}}
    end;
process_response(Error, _) ->
    Error.


-spec get_request_spec(OperationID :: swag_client:operation_id()) ->
    Spec :: swag_client_procession:request_spec().

get_request_spec('delete_shortened_url') ->
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
get_request_spec('get_shortened_url') ->
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
get_request_spec('shorten_url') ->
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
    ].

-spec get_response_spec(OperationID :: swag_client:operation_id(), Code :: swag_client_procession:code()) ->
    Spec :: swag_client_procession:response_spec() | no_return().


get_response_spec('delete_shortened_url', 204) ->
    undefined;

get_response_spec('delete_shortened_url', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('delete_shortened_url', 401) ->
    undefined;

get_response_spec('delete_shortened_url', 403) ->
    undefined;

get_response_spec('delete_shortened_url', 404) ->
    undefined;

get_response_spec('get_shortened_url', 200) ->
    {'ShortenedUrl', 'ShortenedUrl'};

get_response_spec('get_shortened_url', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('get_shortened_url', 401) ->
    undefined;

get_response_spec('get_shortened_url', 403) ->
    undefined;

get_response_spec('get_shortened_url', 404) ->
    undefined;

get_response_spec('shorten_url', 201) ->
    {'ShortenedUrl', 'ShortenedUrl'};

get_response_spec('shorten_url', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('shorten_url', 401) ->
    undefined;

get_response_spec('shorten_url', 403) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).
