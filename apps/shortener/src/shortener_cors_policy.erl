-module(shortener_cors_policy).

-behaviour(cowboy_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2]).
-export([allowed_headers/2]).
-export([allowed_methods/2]).

-spec policy_init(cowboy_req:req()) -> {ok, cowboy_req:req(), any()}.
policy_init(Req) ->
    {ok, Req, undefined}.

-spec allowed_origins(cowboy_req:req(), any()) -> {'*', any()}.
allowed_origins(_, State) ->
    {'*', State}.

-spec allowed_headers(cowboy_req:req(), any()) -> {[binary()], any()}.
allowed_headers(_, State) ->
    {
        [
            <<"accept">>,
            <<"access-control-allow-headers">>,
            <<"authorization">>,
            <<"content-type">>,
            <<"x-request-id">>,
            <<"x-requested-with">>
        ],
        State
    }.

-spec allowed_methods(cowboy_req:req(), any()) -> {[binary()], any()}.
allowed_methods(_, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], State}.
