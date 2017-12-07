-module(shortener_utils).

-export([unique_id/0]).

-spec unique_id() -> dmsl_base_thrift:'ID'().

unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).
