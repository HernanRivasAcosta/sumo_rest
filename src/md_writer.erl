-module(md_writer).

-export([build_md/2]).

%%==============================================================================
%% API
%%==============================================================================
build_md(Data, Path) ->
  {value, {_, Name, Text}} = lists:keysearch(overview, 1, Data),

  filelib:ensure_dir(Path),
  {ok, Output} = file:open(Path ++ "/" ++ Name ++ ".md", [write]),

  io:format(Output, "##Overview~n~s~n~n", [format_sentence(Text)]),

  EndpointTable = endpoints_to_table(Name, get_endpoint_list(Data)),
  io:format(Output, "##Endpoints~n~n~s~n~n", [EndpointTable]),

  Endpoints = [endpoint_to_string(X, Data) || {endpoint, X} <- Data],

  io:format(Output, "~s", [string:join(Endpoints, "\n\n\n")]),

  ok.

%%==============================================================================
%% Utils
%%==============================================================================
get_endpoint_list(Data) ->
  Endpoints = [Description || {endpoint, Description} <- Data],
  F = fun(EndpointData) ->
        {value, Res} = lists:keysearch(definition, 1, EndpointData),
        {definition, Type, Name, _} = Res,
        {Type, Name}
      end,
  [F(E) || E <- Endpoints].

endpoints_to_table(Resource, Endpoints) ->
  MethodList = [get, put, post, delete],
  GetMethod = fun(MethodAtom) ->
                case lists:keysearch(MethodAtom, 1, Endpoints) of
                  {value, {_, _}} -> {true, method_to_string(MethodAtom)};
                  _               -> false
                end
              end,
  Methods = lists:filtermap(GetMethod, MethodList),
  GetName = fun(MethodAtom) ->
                case lists:keysearch(MethodAtom, 1, Endpoints) of
                  {value, {_, Name}} -> {true, Name};
                  _                  -> false
                end
              end,
  Names = lists:filtermap(GetName, MethodList),
  Titles = ["RESOURCE" | Methods], 
  Values = [Resource   | Names],
  format_table(Titles, Values).

method_to_string(MethodAtom) -> string:to_upper(atom_to_list(MethodAtom)).

format_table(Row1, Row2) ->
  GetMaxLen = fun(A, B) -> max(length(A), length(B)) end,
  Lengths = lists:zipwith(GetMaxLen, Row1, Row2),

  AddTrailingSpaces = fun(Str, DesiredLen) ->
                        string:left(Str, DesiredLen, 32) 
                      end,
  Line1 = lists:zipwith(AddTrailingSpaces, Row1, Lengths),
  Line2 = [string:copies("-", L) || L <- Lengths],
  Line3 = lists:zipwith(AddTrailingSpaces, Row2, Lengths),
  Lines = [Line1, Line2, Line3],

  io_lib:format("~s~n~s~n~s", [string:join(Line, "|") || Line <- Lines]).


endpoint_to_string(EndpointDefinition, Data) ->
  {value, {_, Method, Name, Text}} = lists:keysearch(definition, 1,
                                                     EndpointDefinition),

  Overview = io_lib:format("#### ~s ~s~n~n##### Overview~n~s",
  %Overview = io_lib:format("#### ``~s ~s``~n~n##### Overview~n~s",
                           [method_to_string(Method),
                            Name,
                            format_sentence(Text)]),
  Parameters = case parameters_to_string(EndpointDefinition) of
                 void  -> "";
                 PValue -> "\n\n" ++ PValue
               end,
  Responses = "\n\n" ++ responses_to_string(EndpointDefinition),
  
  Examples = case utils:skip_examples(Data) of
               true ->
                 "";
               false ->
                 case examples_to_string(EndpointDefinition, Data) of
                   void  -> "";
                   EValue -> "\n\n" ++ EValue
                 end
             end,

  Overview ++ Parameters ++ Responses ++ Examples.

parameters_to_string(Data) ->
  ReqParams = [{type_to_string((Type)), Name, format_sentence(Text)} ||
               {parameter, required, Type, Name, Text} <- Data],
  OpParams = [{type_to_string(Type), Name, format_sentence(Text)} ||
              {parameter, optional, Type, Name, Text} <- Data],

  ReqParamsStr = ["* **" ++ Name ++ "** (" ++ Type ++ "): " ++ Text ||
                {Type, Name, Text} <- lists:reverse(ReqParams)],
  OpParamsStr = ["* **" ++ Name ++ "** (" ++ Type ++ ", optional): " ++ Text ||
               {Type, Name, Text} <- lists:reverse(OpParams)],

  case {ReqParamsStr, OpParamsStr} of
    {[], []} -> void;
    _        -> ParamText = string:join(ReqParamsStr ++ OpParamsStr, "\n"),
                "##### Parameters" ++ "\n" ++ ParamText
  end.

responses_to_string(Data) ->
  Sort = fun({A, _}, {B, _}) -> A =< B end,
  Statuses = [{integer_to_list(Status), format_sentence(Text)} ||
              {response, Status, Text} <- Data],
 
  Url = "http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html",
  {ok, {_, _, HTML}} = httpc:request(Url),

  StatusToString = fun(Status) ->
                     {match, Rest} = re:run(HTML, "h3.*(" ++ Status ++ ".*)h3"),
                     Results = [lists:sublist(HTML, A + 5, B - 6) ||
                                {A, B} <- Rest],
                     lists:nth(2, Results)
                   end,

  ResponseToString = fun({Status, Text}) ->
                       "* **" ++ Status ++ "** (" ++ StatusToString(Status) ++
                       "): " ++ Text
                     end,
  StatusesStr = [ResponseToString(S) || S <- lists:sort(Sort, Statuses)],

  "##### Responses:\nThe server may return:\n\n" ++
  string:join(StatusesStr, "\n").

examples_to_string(EndpointDefinition, Data) ->
  Examples = [{Type, Description, Params} ||
              {example, Type, Description, Params} <- EndpointDefinition],
  F = fun({Type, Description, Params}) ->
        handle_example(Type, Description, Params, Data)
      end,
  ExamplesStr = lists:filtermap(F, Examples), 

  case ExamplesStr of
    [] -> void;
    _L -> "##### Samples:\n\n" ++ string:join(ExamplesStr, "\n\n")
  end.

handle_example(curl, Description, Params, Data) ->
  {ok, Cmd} = build_curl_req(Params, Data, false),
  Response = os:cmd(Cmd),

  % Separate the response in lines
  Lines = [binary_to_list(Bin) || Bin <- re:split(Response, "[\r\n]+")],
  F = fun(Line, {Out, In} = Acc) ->
        case Line of
          [$* | _] -> Acc; % Ignore this
          [${ | _] -> Acc; % Ignore this
          [$> | _] -> {[Line | Out], In};
          [_ | _]  -> {Out, [Line | In]};
          []       -> Acc
        end
      end,
  {Out, In} = lists:foldl(F, {[], []}, lists:reverse(Lines)),

  {ok, ObscuredCmd} = build_curl_req(Params, Data, true),
  Str = "* " ++ format_sentence(Description) ++ "\n```bash\n$ " ++ ObscuredCmd
        ++ "\n" ++ string:join(Out ++ In, "\n") ++ "\n```",

  {true, Str};
handle_example(Type, _Description, _Params, _Data) ->
  io:format("unhandled example type ~p~n", [Type]),
  false.

%% String formatting utils
capitalize([H | T] = _Str) when H >= 97 andalso H =< 122 -> [H - 32 | T];
capitalize(Str) -> Str.
add_period(Str) -> case lists:last(Str) =:= 46 of
                     true -> Str;
                     _ -> Str ++ "."
                   end.
format_sentence(Str) -> add_period(capitalize(Str)).
type_to_string(Type) -> capitalize(parameter_handler:to_string(Type)).

build_curl_req(Params, Data, ObscureAuth) ->
  case lists:keyfind(url, 1, Params) of
    false    -> {error, no_url};
    {_, Url} -> R1 = "curl -sSv ",
                R2 = case lists:keyfind(auth, 1, Params) of
                       false     -> R1;
                       {_, Auth} -> add_auth(R1, Auth, Data, ObscureAuth)
                     end,
                R3 = add_method(R2, Params),
                R4 = add_type(R3, Params),
                R5 = add_body(R4, Params),
                {ok, R5 ++ " " ++ Url}
  end.

%% Curl request
add_auth(Req, AuthName, Data, ObscureAuth) ->
  case lists:keyfind(auth, 1, Data) of
    false  -> io:format("undefined auth ~p~n", [AuthName]),
              io:format("on1 ~p~n", [Data]),
              Req;
    {_, L} -> case lists:keyfind(AuthName, 1, L) of
                false       -> io:format("undefined auth ~p~n", [AuthName]),
                               io:format("on2 ~p~n", [L]),
                               Req;
                {_, {K, S}} -> case ObscureAuth of
                                 false -> Req ++ " -u" ++ K ++ ":" ++ S;
                                 true  -> Req ++ " -uKEY:SECRET"
                               end
              end
  end.

add_method(Req, Params) ->
  case lists:keyfind(method, 1, Params) of
    false       -> Req;
    {_, Method} -> Req ++ " -X " ++ Method
  end.

add_type(Req, Params) ->
  case lists:keyfind(type, 1, Params) of
    false       -> Req;
    {_, "json"} -> Req ++ " -H\"Content-Type:application/json\"";
    Other       -> io:format("unsupported content type ~p~n", [Other]),
                   Req
  end.

add_body(Req, Params) ->
  case lists:keyfind(body, 1, Params) of
    false     -> Req;
    {_, Body} -> Req ++ " -d'" ++ Body ++ "'"
  end.