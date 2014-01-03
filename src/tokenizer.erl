-module(tokenizer).

-export([parse_line/1]).

%%==============================================================================
%% API
%%==============================================================================
parse_line(Line) ->
  handle_tokens(get_tokens(Line)).

%%==============================================================================
%% Utils
%%==============================================================================
get_tokens(String) ->
  get_tokens(String, [], []).

get_tokens([] = _String, Tokens, _State) ->
  lists:reverse([lists:reverse(L) || L <- Tokens]);
get_tokens([H | T] = _String, Tokens, [] = _State) -> 
  case H of
    % Space
    32   -> get_tokens(T, Tokens, []);
    % Quote
    34   -> get_tokens(T, [[] | Tokens], [quote]);
    % Any other character begins a new token
    Char -> get_tokens(T, [[Char] | Tokens], [token])
  end;
get_tokens([H | T] = _String, Tokens, [quote | States] = State) ->
  case H of
    % Quote
    34   -> get_tokens(T, Tokens, States);
    % Scape character
    92   -> get_tokens(T, Tokens, [scaping | State]);
    Char -> get_tokens(T, add_to_current_token(Char, Tokens), State)
  end;
get_tokens([H | T] = _String, Tokens, [scaping | States] = _State) ->
  get_tokens(T, add_to_current_token(H, Tokens), States);
get_tokens([H | T] = _String, Tokens, [token | States] = State) ->
  case H of
    % Space
    32   -> get_tokens(T, Tokens, States);
    % Scape character
    92   -> get_tokens(T, Tokens, [scaping | State]);
    Char -> get_tokens(T, add_to_current_token(Char, Tokens), State)
  end.

add_to_current_token(Char, [H | T] = _Tokens) ->
  [[Char | H] | T];
add_to_current_token(Char, [] = _Tokens) ->
  [[Char]].

handle_tokens(Line) ->
  case Line of
    % Overview
    ["overview", Name, Description] ->
      {overview, Name, Description};
    ["auth", Name, Key, Secret] ->
      {auth, Name, {Key, Secret}};
    % Endpoint definition
    ["GET", Url, Description] ->
      {definition, get, Url, Description};
    ["PUT", Url, Description] ->
      {definition, put, Url, Description};
    ["POST", Url, Description] ->
      {definition, post, Url, Description};
    ["DELETE", Url, Description] ->
      {definition, delete, Url, Description};
    % Parameters
    ["required", Type, Name, Description] ->
      {parameter, required, parse_parameter_type(Type), Name, Description};
    ["optional", Type, Name, Description] ->
      {parameter, optional, parse_parameter_type(Type), Name, Description};
    % Examples
    ["example" | T] ->
      handle_example(T);
    [StatusCode, Description] ->
      case valid_status_code(StatusCode) of
        false -> io:format("invalid status code ~p~n", [StatusCode]),
                 void;
        Int   -> {response, Int, Description}
      end;
    Other ->
      io:format("unhandled command ~p~n", [string:join(Other, " ")]), 
      void
  end.

handle_example([Description, "curl", Url]) ->
  {example, curl, Description, [{url, Url}]};
handle_example([Description, "curl", Url, Auth]) ->
  {example, curl, Description, [{url, Url}, {auth, Auth}]};
handle_example([Description, "curl", Url, Auth, Method, Type, Body]) ->
  {example, curl, Description, [{url, Url}, {auth, Auth}, {method, Method},
                                {type, Type}, {body, Body}]};
handle_example(Other) ->
  io:format("invalid example ~p~n", [string:join(Other, " ")]),
  void.

parse_parameter_type("string")      -> string;
parse_parameter_type("boolean")     -> boolean;
parse_parameter_type("string_list") -> string_list;
parse_parameter_type("range")       -> range;
parse_parameter_type(Any) ->
  io:format("unexpected parameter type ~p~n", [Any]), 
  void.

valid_status_code(StatusCode) ->
  case string:to_integer(StatusCode) of
    {error, _} -> false;
    {Int, _}   -> case Int >= 100 andalso Int =< 599 of
                    true  -> Int;
                    false -> false
                  end
  end.