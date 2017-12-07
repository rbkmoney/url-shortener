-module(shortener_msgpack_marshalling).
-include_lib("dmsl/include/dmsl_msgpack_thrift.hrl").

%% API
-export([marshal/1]).
-export([unmarshal/1]).

-export_type([value/0]).

-type value() :: term().

%%

-spec marshal(value()) ->
    dmsl_msgpack_thrift:'Value'().
marshal(undefined) ->
    {nl, #msgpack_Nil{}};
marshal(Integer) when is_integer(Integer) ->
    {i, Integer};
marshal(String) when is_binary(String) ->
    {str, String}.

-spec unmarshal(dmsl_msgpack_thrift:'Value'()) ->
    value().
unmarshal({nl, #msgpack_Nil{}}) ->
    undefined;
unmarshal({i, Integer}) ->
    Integer;
unmarshal({str, String}) ->
    String.
