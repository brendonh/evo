-module(evo).

-export([run/2]).

-export(['#xml-inheritance#'/0,
         '#root#'/4,
         '#element#'/5,
         '#text#'/1]).

-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_lib, [start_tag/2, end_tag/1, empty_tag/2,
                    is_empty_data/1,
                    find_attribute/2, 
                    export_text/1]).

run(Filename, Module) ->
    Self = self(),
    spawn(fun() -> really_run(Filename, Module, Self) end),
    receive
        {result, R} ->
            R;
        Other ->
            io:format("WTF? ~p~n", [Other]),
            none
    after 5000 ->
            io:format("Too slow!~n"),
            none
    end.

really_run(Filename, Module, From) ->
    put(callback_module, Module),
    {Stuff, _Misc}=xmerl_scan:file(Filename),
    io:format("Stuff: ~p~n", [Stuff]),
    try
        MoreStuff = xmerl:export([Stuff], ?MODULE),
        From ! {result, lists:flatten(MoreStuff)}
    catch 
        error:Problem ->
            io:format("Crap: ~p~n", [Problem]),
            From ! {error, something};
        exit:Problem ->
            io:format("CRAP: ~p~n", [Problem]),
            From ! {error, something}
    end.

%% ----------------------------------------------------------- %%
%% Crazy xmerl API

'#xml-inheritance#'() -> [].

'#text#'(Text) ->
    export_text(Text).

'#root#'(Data, Attrs, [], _E) ->
    Ver = case find_attribute(version, Attrs) of
	      {value, V} ->
		  V;
	      false ->
 		  "-//W3C//DTD HTML 4.01//EN"		     % strict
	  end,
    URI = case find_attribute(uri, Attrs) of
	      {value, U} ->
		  [" \"", U, "\""];
	      false ->
		  " \"http://www.w3.org/TR/html4/strict.dtd\""
	  end,
    ["<!DOCTYPE HTML PUBLIC \"", Ver, "\"", URI, ">\n", Data].

'#element#'(Tag, Data, Attrs, _Parents, _E) ->
    {NewData, OtherAttrs} = apply_evos(Attrs, Data),
    do_tag(NewData, Tag, OtherAttrs).

do_tag(Data, Tag, Attrs) ->
    case is_empty_data(Data) of
        true -> better_empty_tag(Tag, Attrs);
        false -> [start_tag(Tag, Attrs), Data, end_tag(Tag)]
    end.

better_empty_tag(Tag, Attrs) ->
    ["/>"|Stupid] = lists:reverse(empty_tag(Tag, Attrs)),
    lists:reverse([" />"|Stupid]).

%% ----------------------------------------------------------- %%
%% Evo stuff

get_evo(A=#xmlAttribute{namespace={NS,Attr}, value=Value}, {Evos, Others}) ->
    case NS of
        "e" -> {[{Attr,Value}|Evos], Others};
        _ -> {Evos, [A|Others]}
    end;
get_evo(A, {Evos, Others}) ->
    {Evos, [A|Others]}.

find_evos(Attrs) ->
    lists:foldl(
      fun get_evo/2,
      {[], []},
      Attrs).

get_func(FuncName, [{FuncName,Value}|Handlers], none) ->
    get_func(FuncName, Handlers, Value);
get_func(FuncName, [{FuncName,_}|Handlers], Already) ->
    io:format("Extra ~p!~n", [FuncName]),
    get_func(FuncName, Handlers, Already);
get_func(FuncName, [_|Handlers], Whatever) ->
    get_func(FuncName, Handlers, Whatever);
get_func(_, [], Whatever) ->
    Whatever. 

get_data(Handlers) ->
    case get_func("data", Handlers, none) of
        none -> none;
        DataName ->
            case get({data, DataName}) of
                undefined ->
                    DataFuncName = list_to_atom("evo_data_" ++ DataName),
                    CallbackModule = get(callback_module),
                    Value = CallbackModule:DataFuncName(),
                    put({data, DataName}, Value),
                    Value;
                Value -> 
                    Value
            end
    end.


apply_evos(Attrs, OldData) ->
    {Handlers, Others} = find_evos(Attrs),
    NewData = apply_data_func(Handlers, OldData),
    {apply_render(Handlers, NewData), Others}.


apply_data_func(Handlers, OldData) ->
    case get_func("dataFunc", Handlers, none) of
        none ->
            OldData;
        DataFunc ->
            DataFuncName = list_to_atom("dataFunc_" ++ DataFunc),
            Data = get_data(Handlers),
            CallbackModule = get(callback_module),
            try
                io:format("Old data: ~p~n", [OldData]),
                [OldData, CallbackModule:DataFuncName(Data)]
            catch
                exit:Reason ->
                    [OldData, io_lib:format("[[Exit: ~p]]", [Reason])];
                error:Reason ->
                    [OldData, io_lib:format("[[Error: ~p]]", [Reason])]
            end
    end.


apply_render(Handlers, OldData) ->
    case get_func("render", Handlers, none) of
        none -> 
            OldData;
        RenderFunc ->
            RenderFuncName = list_to_atom("evo_render_" ++ RenderFunc),
            Data = get_data(Handlers),
            CallbackModule = get(callback_module),
            try
                [OldData, CallbackModule:RenderFuncName(Data)]
            catch
                exit:Reason ->
                    [OldData, io_lib:format("[[Exit: ~p]]", [Reason])];
                error:Reason ->
                    [OldData, io_lib:format("[[Error: ~p]]", [Reason])]
            end
    end.
