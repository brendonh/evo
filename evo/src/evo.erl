-module(evo).

-import (utf8, [from_binary/1, to_binary/1]).
-export([run/3, new_id/0, get_cache/1, put_cache/2]).

-include("evo.hrl").

run(Filename, InitialData, Module) ->
    Self = self(),
    spawn_link(fun() -> really_run(Filename, InitialData, Module, Self) end),
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

really_run(Filename, InitialData, Module, From) ->
    put(callback_module, Module),
    put(newID, 0),
    ets:new(dataCache, [private, named_table, set]),
    {ok, UTF8} = file:read_file(Filename),
    {ok, UTF32} = from_binary(UTF8),
    Self = self(),
    spawn_link(fun() -> evoxml:parse(UTF32, Self) end),
    ID=new_id(),
    put_cache(ID, InitialData),
    FinalState = watch_parsing(#state{id=ID}),
    TopTags = emitChildren(FinalState),
    Pretty = lists:flatten(lists:foldl(
               fun(Elem, InAcc) -> [indenticate(Elem)|InAcc] end,
               [], TopTags)),
    {ok, Output} = to_binary(Pretty),
    From ! {result, Output}. 

watch_parsing(State) ->
    receive

        {tag_start, Name} ->
            NewState = #state{id=new_id(),
                              tag=Name,
                              level=State#state.level+1, 
                              parent=State},
            watch_parsing(NewState);

        {attr, {{e, dataExp}, Value}} ->
            watch_parsing(State#state{dataExpression=Value});

        {attr, {{e, data}, Value}} ->
            put_cache(State#state.id, Value),
            watch_parsing(State);

        {attr, {{e, render}, Value}} ->
            watch_parsing(State#state{render=Value});

        {attr, {{e, Attr}, _}} ->
            erlang:error({"Unknown evo attribute", Attr});

        {attr, {NSAttr, Value}} ->
            watch_parsing(State#state{attrs=[{NSAttr, Value}|State#state.attrs]});

        {text, Text} ->
            watch_parsing(State#state{children=[Text|State#state.children]});

        {unhandled_tag, {Tag}} ->
            watch_parsing(State#state{children=[Tag|State#state.children]});

        {tag_end, _} ->
            Parent = State#state.parent,
            watch_parsing(Parent#state{children=[State|Parent#state.children]});

        done ->
            State

    end.

emitTag(Text) when is_list(Text) ->
    {Text, none};

emitTag(Int) when is_integer(Int) ->
    {integer_to_list(Int), none};

emitTag(Atom) when is_atom(Atom) ->
    {atom_to_list(Atom), none};

emitTag(#state{tag={e,attr}}=State) ->
    NewState = applyRender(State),
    Name = list_to_atom(proplists:get_value({none, name}, NewState#state.attrs)),
    Value = emitChildren(NewState),
    Parent = NewState#state.parent,
    ParentAttrs2 = proplists:delete({none, Name}, Parent#state.attrs),
    NewParent = Parent#state{attrs=[{{none, Name}, Value}|ParentAttrs2]},
    {"", NewState#state{parent=NewParent}};

emitTag(#state{tag={e,inv}}=State) ->
    NewState = applyRender(State),
    {lists:reverse(emitChildren(NewState)), none};

emitTag(#state{tag={e,Tag}}) ->
    erlang:error({"Unknown evo tag", Tag});

emitTag(#state{render=none}=State) ->
    {renderTag(State), State};

emitTag(#state{}=State) ->
    NewState = applyRender(State),
    emitTag(NewState).


emitChildren(State) ->
    {_, Children} = emitChildren(State, true),
    Children.

emitChildren(State, true) ->
    lists:foldl(
      fun(S, {Parent, Children}) ->
              case S of
                  #state{} ->
                      NS = S#state{parent=Parent},
                      {Child, NS2} = emitTag(NS),
                      case NS2 of
                          none ->
                              {Parent, [Child|Children]};
                          _ ->
                              {NS2#state.parent, [Child|Children]}
                      end;
                  _ ->
                      {Child, _} = emitTag(S),
                      {Parent, [Child|Children]}
              end
      end,
      {State, []},
      lists:reverse(State#state.children)).


renderTag(State) ->
    case lists:flatten(State#state.children) of
        [] -> emitEmptyTag(State);
        _ -> emitFullTag(State)
    end.

applyRender(#state{render=none}=State) ->
    State;
applyRender(State) ->
    Render = list_to_atom(State#state.render),
    NewState = evorender:Render(State),
    NewState.

emitFullTag(State) ->
    {NewState, Children} = emitChildren(State, true),
    {openTag(NewState),
     lists:reverse(Children),
     closeTag(NewState)}.

emitEmptyTag(State) ->
    [lists:flatten([$<, flatten_name(State#state.tag), 
                    flatten_attrs(State#state.attrs),
                    " />"])].

openTag(State) ->
    [$<, flatten_name(State#state.tag), flatten_attrs(State#state.attrs), $>].

closeTag(State) ->
    [$<, $/, flatten_name(State#state.tag), $>].

flatten_attrs([]) -> "";
flatten_attrs(Attrs) ->
    " " ++ string:join(
      lists:map(fun ({NSAttr, Value}) -> lists:flatten([flatten_name(NSAttr), $=, $", Value, $"]) end,
                Attrs),
      " ").

flatten_name({none, Attr}) -> atom_to_list(Attr);
flatten_name({NS, Attr}) -> lists:flatten([atom_to_list(NS), ":", atom_to_list(Attr)]).


indenticate(TagSoup) ->
    PF = partial_flatten(TagSoup),
    Indented = indent(PF, 0),
    Flattened = lists:flatten(Indented),
    LineList = lists:map(fun erlang:tuple_to_list/1, Flattened),
    string:join(lists:map(fun lists:flatten/1, LineList), "\n") ++ [$\n].

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

new_id() ->
    NewID = get(newID),
    put(newID, NewID+1),
    put_cache(NewID, undefined),
    NewID.

get_cache(ID) ->
    ets:lookup_element(dataCache, ID, 2).

put_cache(ID, Data) ->
    ets:insert(dataCache, {ID, Data}).
                       
