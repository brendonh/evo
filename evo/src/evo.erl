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
               fun(Elem, InAcc) -> [indenticate(Elem)|InAcc] end,
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

        {attr, {{e, Attr}, _}} ->
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
    PF = partial_flatten(TagSoup),
    Indented = indent(PF, 0),
    Flattened = lists:flatten(Indented),
    LineList = lists:map(fun erlang:tuple_to_list/1, Flattened),
    string:join(lists:map(fun lists:flatten/1, LineList), "\n").

partial_flatten({Tag, Content, End}) ->
    FlatTag = lists:flatten(Tag),
    FlatEnd = lists:flatten(End),
    Inside = partial_flatten(Content),
    case Inside of
        [C1|_] when is_integer(C1) and C1 =:= $< -> 
            lists:concat([FlatTag, Inside, FlatEnd]);
        _ ->
            {FlatTag, Inside, FlatEnd}
    end;

partial_flatten([C|_]=Text) when is_integer(C) ->
    newlines_to_spaces(Text);

partial_flatten(Stuff) when is_list(Stuff) ->
    Before = lists:map(fun partial_flatten/1, Stuff),
    {After, AllText} = join_text(Before),
    Flat = lists:flatten(After),
    case AllText andalso length(Flat) < 80 of
        true -> Flat;
        false -> After
    end.

indent({Tag, [C1|_]=Content, End}, Indent) when is_integer(C1) ->
    Stripped = string:strip(Content),
    TotalLen = length(Tag) + length(Stripped) + length(End),
    case TotalLen < 40 of
        true ->
            [{spaces(Indent), Tag, Stripped, End}];
        false ->
            [{spaces(Indent), Tag},
             {spaces(Indent+1), Stripped},
             {spaces(Indent), End}]
    end;
indent({Tag, Content, End}, Indent) ->
    [{spaces(Indent), Tag},
     lists:map(fun(L) -> indent(L, Indent+1) end, Content),
     {spaces(Indent), End}];
indent([C1|_]=Line, Indent) when is_integer(C1) ->
    Stripped = string:strip(Line),
    case Stripped of
        [] -> [];
        _ -> [{spaces(Indent), Stripped}]
    end;
indent(Lines, Indent) ->
    lists:map(fun(L) -> indent(L, Indent+1) end, Lines).

join_text(Bits) ->
    {Text, AllText} = lists:foldl(fun maybe_join/2, {[], true}, Bits),
    {lists:reverse(Text), AllText}.
      
maybe_join([C1|_]=Text, {[[C2|_]=Last|Rest], All}) when is_integer(C1), is_integer(C2) ->
    {[lists:concat([Last, Text])|Rest], All};
maybe_join([C1|_]=Text, {[], All}) when is_integer(C1) ->
    {[Text], All};
maybe_join(Something, {Acc, _}) ->
    {[Something|Acc], false}.


newlines_to_spaces(Bytes) -> lists:reverse(newlines_to_spaces(Bytes, [])).

newlines_to_spaces([$\n|Bytes], Buffer) ->
    newlines_to_spaces(Bytes, [$\s|Buffer]);
newlines_to_spaces([C|Bytes], Buffer) ->
    newlines_to_spaces(Bytes, [C|Buffer]);
newlines_to_spaces([], Buffer) ->
    Buffer.

spaces(Indent) -> string:chars(32, Indent*2).     
