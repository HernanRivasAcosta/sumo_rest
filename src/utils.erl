-module(utils).

-export([verbose/1, skip_examples/1, get_output_dir/1]).

%%==============================================================================
%% API
%%==============================================================================
verbose(Params) ->
  lists:keyfind(v, 1, Params) =/= false andalso
  lists:keyfind(verbose, 1, Params) =/= false.

skip_examples(Params) ->
  lists:keyfind(skip_examples, 1, Params) =/= false.

get_output_dir(Params) ->
  S = case lists:keyfind(output, 1, Params) of
        false     -> "output"; % Default
        {_, Path} -> Path
      end,
  case lists:last(S) of
    $/ -> S;
    _  -> S ++ "/"
  end.