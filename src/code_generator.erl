-module(code_generator).

-export([generate_code/1]).

%%==============================================================================
%% API
%%==============================================================================
generate_code(Params) ->
  case lists:keyfind(generate, 1, Params) of
    false  -> ok;
    {_, L} -> Gens = [string:to_lower(S) || S <- L],
              lists:foreach(fun(X) -> build_code_for(X, Params) end, Gens),
              ok
  end. 

%%==============================================================================
%% Utils
%%==============================================================================
build_code_for("erl", Params) ->
  erl_generator:generate_code(Params);
build_code_for("erl-suite", _Params) ->
  io:format("unimplemented builder erl-suite~n"),
  ok;
build_code_for(Other, _Params) ->
  io:format("undefined builder ~p~n", [Other]),
  ok.