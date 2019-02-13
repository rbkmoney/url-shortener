-module(shortener_cors_policy).
-behaviour(cowboy_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2]).
-export([allowed_headers/2]).
-export([allowed_methods/2]).

-spec policy_init(cowboy_req:req()) -> {ok, cowboy_req:req(), any()}.

policy_init(Req) ->
    {ok, Req, undefined}.

-spec allowed_origins(cowboy_req:req(), any()) -> {'*', cowboy_req:req(), any()}.

allowed_origins(Req, State) ->
    {'*', Req, State}.

-spec allowed_headers(cowboy_req:req(), any()) -> {[binary()], cowboy_req:req(), any()}.

allowed_headers(Req, State) ->
    {[
        <<"accept">>,
        <<"access-control-allow-headers">>,
        <<"authorization">>,
        <<"content-type">>,
        <<"origin">>,
        <<"x-request-id">>,
        <<"x-requested-with">>
    ], Req, State}.

-spec allowed_methods(cowboy_req:req(), any()) -> {[binary()], cowboy_req:req(), any()}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.
