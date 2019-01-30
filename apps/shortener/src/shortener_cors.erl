-module(shortener_cors).

-behaviour(cowboy_middleware).

-export([execute/2]).

-spec execute(cowboy_req:req(), Env) -> {ok, cowboy_req:req(), Env} | {stop, cowboy_req:req()} when Env :: any().
execute(Req, Env) ->
    ReqWithCorsHeaders = set_cors_headers(Req),
    Method = cowboy_req:method(ReqWithCorsHeaders),

    case Method of
	<<"OPTIONS">> ->
	    ReqFinal = cowboy_req:reply(200, ReqWithCorsHeaders),
	    {stop, ReqFinal};
	_ ->
	    %% continue as normal
	    {ok, ReqWithCorsHeaders, Env}
    end.

%% ===================================================================
%% Helpers
%% ===================================================================    

set_cors_headers(Req) ->
    Headers = #{
        <<"access-control-allow-headers">> => <<"Origin, X-Requested-With, Content-Type, Accept">>,
        <<"access-control-allow-methods">> => <<"POST, GET, OPTIONS">>,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-max-age">> => <<"1000">>
    },
    cowboy_req:set_resp_headers(Headers, Req).
