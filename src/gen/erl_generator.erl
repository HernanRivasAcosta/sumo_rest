-module(erl_generator).

-export([generate_code/1]).

%%==============================================================================
%% API
%%==============================================================================
generate_code(Params) ->
  Path = utils:get_output_dir(Params) ++ "generated/erl/",
  filelib:ensure_dir(Path),

  % Move the included file
  file:copy("src/gen/include/param_validations.source",
            Path ++ "param_validations.erl"),

  ok.