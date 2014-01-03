-module(parameter_handler).

-export([to_string/1]).

%%==============================================================================
%% API
%%==============================================================================
to_string(string)      -> "string";
to_string(string_list) -> "string list";
to_string(int)         -> "integer";
to_string(uint)        -> "unsigned integer";
to_string(int_list)    -> "integer list";
to_string(uint_list)   -> "unsigned integer list";
to_string(float)       -> "float";
to_string(float_list)  -> "float list";
to_string(range)       -> "integer range";
to_string(boolean)     -> "boolean";
to_string(_Other)      -> "unspecified".