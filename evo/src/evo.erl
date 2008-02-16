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
    put(callbackModule, Module),
    {Stuff, _Misc}=xmerl_scan:file(Filename),
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

%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    export_text(Text).


%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

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
    case is_empty_data(NewData) of
        true -> better_empty_tag(Tag, OtherAttrs);
        false -> [start_tag(Tag, OtherAttrs), NewData, end_tag(Tag)]
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
    io:format("Found ~p: ~p~n", [FuncName, Value]),
    get_func(FuncName, Handlers, Value);
get_func(FuncName, [{FuncName,_}|Handlers], Already) ->
    io:format("Extra ~p!~n", [FuncName]),
    get_func(FuncName, Handlers, Already);
get_func(FuncName, [_|Handlers], Whatever) ->
    get_func(FuncName, Handlers, Whatever);
get_func(_, [], Whatever) ->
    Whatever.

apply_evos(Attrs, Data) ->
    {Handlers, Others} = find_evos(Attrs),
    io:format("~p / ~p~n", [Handlers, Others]),

    DataFunc = get_func("data", Handlers, none),
    io:format("Data: ~p~n", [DataFunc]),

    {Data, Others}.

    %%lists:map(fun(H) -> apply_evo(H, Data) end, Handlers).

%% ----------------------------------------------------------- %%
%% Evo API

evo_render(FuncName) ->
    io:format("Render: ~p~n", [FuncName]),
    M = get(callbackModule),
    M:FuncName().
