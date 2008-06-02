-module(evo).

-import (utf8, [from_binary/1, to_binary/1]).
-export([run/2]).

-record(state,{
  tag=none,
  render=none,
  data=none,
  level=0,
  attrs=[],
  buffer=[],
  stack=[]
}).

run(Filename, Module) ->
    Self = self(),
    spawn_link(fun() -> really_run(Filename, Module, Self) end),
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

    {ok, UTF8} = file:read_file(Filename),
    {ok, UTF32} = from_binary(UTF8),
    Self = self(),
    spawn(fun() -> evoxml:parse(UTF32, Self) end),
    FinalState = watch_parsing(#state{}),
    TopTags = FinalState#state.buffer,
    Pretty = lists:flatten(lists:foldl(
               fun(Elem, InAcc) -> [indenticate(Elem), $\n, InAcc] end,
               [], TopTags)),
    {ok, Output} = to_binary(Pretty),
    From ! {result, Output}.

watch_parsing(State) ->
    receive

        {tag_start, Name} ->
            %io:format("~s[~s]~n", [debug_spaces(State), lists:flatten(Name)]),
            NewState = #state{tag=Name,
                              data=State#state.data, 
                              level=State#state.level+1, 
                              stack=[State|State#state.stack]},
            watch_parsing(NewState);

        {attr, {"e:data", Value}} ->
            watch_parsing(State#state{data=string:tokens(Value, " ")});

        {attr, {"e:render", Value}} ->
            watch_parsing(State#state{render=Value});

        {attr, {Name, Value}} ->
            %{ok, BinVal} = to_binary(Value),
            %io:format("~s|~s => ~s~n", [debug_spaces(State), Name, BinVal]),
            watch_parsing(State#state{attrs=[{Name, Value}|State#state.attrs]});

        {text, Text} ->
            %{ok, BinVal} = to_binary(Text),
            %io:format("~s\"~s\"~n", [debug_spaces(State), BinVal]),
            watch_parsing(State#state{buffer=[Text|State#state.buffer]});

        {unhandled_tag, {Tag}} ->
            watch_parsing(State#state{buffer=[Tag|State#state.buffer]});

        {tag_end, _} ->
            Result = emitTag(State),
            %io:format("Result: ~p~n", [Result]),
            [NextState|_Rest] = State#state.stack,
            watch_parsing(NextState#state{buffer=[Result|NextState#state.buffer]});

        done ->
            io:format("Done.~n"),
            State

    end.


emitTag(State) ->
    case State#state.buffer of
        [] -> emitEmptyTag(State);
        _ -> emitFullTag(State)
    end.


emitFullTag(State) ->
    {openTag(State),
     lists:reverse(State#state.buffer),
     closeTag(State)}.

emitEmptyTag(State) ->
    [lists:flatten([$<, State#state.tag, 
                    flatten_attrs(State#state.attrs),
                    " />"])].

openTag(State) ->
    [$<, State#state.tag, flatten_attrs(State#state.attrs), $>].

closeTag(State) ->
    [$<, $/, State#state.tag, $>].

flatten_attrs([]) -> "";
flatten_attrs(Attrs) ->
    " " ++ string:join(
      lists:map(fun ({Name, Value}) -> lists:flatten([Name, $=, $", Value, $"]) end,
                Attrs),
      " ").


indenticate(TagSoup) ->
    lists:flatten(indenticate(TagSoup, 0)).

indenticate({Tag, Content, End}, Indent) ->
    [[spaces(Indent), Tag, $\n],
     [lists:map(fun(L) -> [indenticate(L, Indent+1), $\n] end, Content)],
     [spaces(Indent), End]];
indenticate(Other, Indent) -> [spaces(Indent), newlines_to_spaces(Other)].

newlines_to_spaces(Bytes) -> lists:reverse(newlines_to_spaces(Bytes, [])).

newlines_to_spaces([$\n|Bytes], Buffer) ->
    newlines_to_spaces(Bytes, [$\s|Buffer]);
newlines_to_spaces([C|Bytes], Buffer) ->
    newlines_to_spaces(Bytes, [C|Buffer]);
newlines_to_spaces([], Buffer) ->
    Buffer.

spaces(Indent) -> string:chars(32, Indent*2).

%debug_spaces(State) -> spaces(State#state.level).
