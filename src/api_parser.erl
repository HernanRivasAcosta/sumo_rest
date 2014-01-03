-module(api_parser).

-export([init_with/1]).

%%==============================================================================
%% API
%%==============================================================================
init_with(Params) ->
  Sources = case lists:keysearch(source, 1, Params) of
              {value, {source, SourcesValue}} -> SourcesValue;
              _                               -> ["data/model"] % default
            end,

  F = fun(S) ->
        case file:open(S, [read, read_ahead]) of
          {ok, SourceFileValue} -> SourceFileValue;
          _Any                  -> void
        end
      end,

  SourceFiles = [F(S) || S <- Sources],

  lists:foreach(fun(X) -> build(Params, X) end, SourceFiles),

  io:format("done~n").

%%==============================================================================
%% Utils
%%==============================================================================
build(_, void) ->
  ok;
build(Params, SourceFile) ->
  Lines = filter_comments(get_lines(SourceFile)),
  file:close(SourceFile), 
  TokenizedLines = [tokenizer:parse_line(Line) || Line <- Lines, Line =/= []],

  Data = Params ++ build_endpoints([L || L <- TokenizedLines, L =/= void]),
  %io:format("Data = ~p~n", [Data]),

  ok = md_writer:build_md(Data, utils:get_output_dir(Params) ++ "docs/"),
  ok = code_generator:generate_code(Data).

get_lines(File) ->
  get_lines(File, []).

get_lines(File, Lines) ->
  case file:read_line(File) of
    {ok, Data} -> get_lines(File, [Data | Lines]);
    _Any       -> lists:reverse(Lines)
  end.

filter_comments(Lines) ->
  F = fun(Line) ->
        case lists:takewhile(fun(C) -> C =/= $# end, Line) of
          []    -> false;
          Value -> {true, [C || C <- Value, C =/= 10]}
        end
      end,
  lists:filtermap(F, Lines).

build_endpoints(Lines) ->
  F = fun(Line, Acc) ->
        case element(1, Line) of
          overview   -> [Line | Acc];
          auth       -> AuthElement = {element(2, Line), element(3, Line)},
                        case Acc of
                          [{auth, L} | T] ->
                            [{auth, [AuthElement | L]} | T];
                          Other ->
                            [{auth, [AuthElement]} | Other]
                        end;
          definition -> [{endpoint, [Line]} | Acc];
          _Any       -> [{endpoint, Data} | T] = Acc,
                        [{endpoint, [Line | Data]} | T]
        end
      end,
  lists:reverse(lists:foldl(F, [], Lines)).
