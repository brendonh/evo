-module(evo).

-import (utf8, [from_binary/1, to_binary/1]).
-export([run/2]).

-include("evo.hrl").

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
    spawn_link(fun() -> evoxml:parse(UTF32, Self) end),
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
            NewState = #state{tag=Name,
                              data=State#state.data, 
                              level=State#state.level+1, 
                              stack=[State|State#state.stack]},
            watch_parsing(NewState);

        {attr, {{e, data}, Value}} ->
            watch_parsing(State#state{data=string:tokens(Value, " ")});

        {attr, {{e, render}, Value}} ->
            watch_parsing(State#state{render=Value});

        {attr, {{e, Attr}, Value}} ->
            erlang:error({"Unknown evo attribute", Attr});

        {attr, {NSAttr, Value}} ->
            watch_parsing(State#state{attrs=[{NSAttr, Value}|State#state.attrs]});

        {text, Text} ->
            watch_parsing(State#state{buffer=[Text|State#state.buffer]});

        {unhandled_tag, {Tag}} ->
            watch_parsing(State#state{buffer=[Tag|State#state.buffer]});

        {tag_end, _} ->
            State2 = applyRender(State),
            {Result, NewState} = emitTag(State2),
            [NextState|_Rest] = NewState#state.stack,
            watch_parsing(NextState#state{buffer=[Result|NextState#state.buffer]});

        done ->
            io:format("Done.~n"),
            State

    end.

emitTag(State) when State#state.tag =:= "e:attr" ->
    Name = list_to_atom(proplists:get_value({none, name}, State#state.attrs)),
    Value = State#state.buffer,
    [Parent|Rest] = State#state.stack,
    ParentAttrs2 = proplists:delete({none, Name}, Parent#state.attrs),
    NewParent = Parent#state{attrs=[{{none, Name}, Value}|ParentAttrs2]},
    {"", State#state{stack=[NewParent|Rest]}};

emitTag(State) ->
    case lists:flatten(State#state.buffer) of
        [] -> emitEmptyTag(State);
        _ -> emitFullTag(State)
    end.


applyRender(State) when State#state.render =:= none ->
    State;
applyRender(State) ->
    Render = list_to_atom(State#state.render),
    evorender:Render(State).


emitFullTag(State) ->
    {{openTag(State),
      lists:reverse(State#state.buffer),
      closeTag(State)},
     State}.

emitEmptyTag(State) ->
    {[lists:flatten([$<, State#state.tag, 
                     flatten_attrs(State#state.attrs),
                     " />"])],
     State}.

openTag(State) ->
    [$<, State#state.tag, flatten_attrs(State#state.attrs), $>].

closeTag(State) ->
    [$<, $/, State#state.tag, $>].

flatten_attrs([]) -> "";
flatten_attrs(Attrs) ->
    " " ++ string:join(
      lists:map(fun ({NSAttr, Value}) -> lists:flatten([flatten_attrName(NSAttr), $=, $", Value, $"]) end,
                Attrs),
      " ").

flatten_attrName({none, Attr}) -> atom_to_list(Attr);
flatten_attrName({NS, Attr}) -> lists:flatten([atom_to_list(NS), ":", atom_to_list(Attr)]).
    

indenticate(TagSoup) ->
    lists:flatten(indenticate(TagSoup, 0)).

indenticate({Tag, Content, End}, Indent) ->
    [spaces(Indent), Tag, $\n, 
     lists:map(fun(L) -> [indenticate(L, Indent+1), $\n] end, Content),
     spaces(Indent), End];
indenticate(Other, Indent) -> [spaces(Indent), newlines_to_spaces(Other)].


newlines_to_spaces(Bytes) -> lists:reverse(newlines_to_spaces(Bytes, [])).

newlines_to_spaces([$\n|Bytes], Buffer) ->
    newlines_to_spaces(Bytes, [$\s|Buffer]);
newlines_to_spaces([C|Bytes], Buffer) ->
    newlines_to_spaces(Bytes, [C|Buffer]);
newlines_to_spaces([], Buffer) ->
    Buffer.

spaces(Indent) -> string:chars(32, Indent*2).
