-module(evo).

-import (utf8, [from_binary/1, to_binary/1]).
-export([prepare/1, run/2, run/3, run_file/2, run_file/3, new_id/0, get_cache/1, put_cache/2]).

-include("evo.hrl").

-define(MAX_LINE, 80).

run_file(Filename, InitialData) ->
    run_file(Filename, InitialData, true).

run_file(Filename, InitialData, Pretty) ->
    {ok, UTF8} = file:read_file(Filename),
    {ok, UTF32} = from_binary(UTF8),
    case run(UTF32, InitialData, Pretty) of
        {'EXIT', _, Error} -> {error, Error};
        Result -> to_binary(Result)
    end.

run(Content, InitialData) ->   
    run(Content, InitialData, true).

run(Content, InitialData, Pretty) ->   
    Self = self(),
    Template = spawn_link(fun() -> prepare(Content) end),
    Template ! {run, InitialData, Pretty, Self},
    get_result(Template).


get_result(Template) ->
    receive
        {Template, result, R} ->
            Template ! finished,
            after_death(R);
        {Template, 'EXIT', _, _}=Exit ->
            Exit;
        Other ->
            io:format("WTF? ~p~n", [Other]),
            after_death(none)
    after 5000 ->
            io:format("Too slow!~n"),
            after_death(none)
    end.
    
after_death(Result) ->
    receive
        {'EXIT', _, normal} -> Result;
        Other -> Other
    after 5000 ->
            io:format("Death too slow~n"),
            none
    end.


prepare(Content) ->
    put(dataCache, ets:new(dataCache, [private, set])),
    put(finalCache, ets:new(finalCache, [private, set])),
    put(newID, 1),
    Self = self(),
    spawn_link(fun() -> evoxml:parse(Content, Self) end),
    FinalState = watch_parsing(#state{id=0}),
    ets:insert(get(finalCache), ets:tab2list(get(dataCache))),
    accept_runs(FinalState).


accept_runs(State) ->
    receive
        
        {run_raw, InitialData, From} ->
            From ! {self(), result, {tags, get_tags(State, InitialData)}},
            accept_runs(State);

        {run, InitialData, Pretty, From} ->
            TopTags = get_tags(State, InitialData),
            I = case Pretty of
                    true -> fun indenticate/1;
                    false -> fun noindenticate/1
                end,
            Output = lists:flatten(lists:foldl(
                                     fun(Elem, InAcc) -> [I(Elem)|InAcc] end,
                                     [], TopTags)),
            From ! {self(), result, string:strip(Output, right, $\n)},
            accept_runs(State);

        finished -> ok;

        Other ->
            cr:dbg({unknown_command, Other}),
            accept_runs(State)

    end.


get_tags(State, InitialData) ->
    ets:delete_all_objects(get(dataCache)),
    ets:insert(get(dataCache), ets:tab2list(get(finalCache))),
    put_cache(0, InitialData),
    emitChildren(State).


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

        {attr, {{e, format}, Key}} ->
            [Func|Args] = string:tokens(Key, " "),
            watch_parsing(State#state{formatFunc={list_to_atom(Func), Args}});

        {attr, {{e, key}, CompoundKey}} ->
            Exp = lists:flatten(
                    lists:foldl(
                      fun(E, A) -> 
                              io_lib:format("proplists:get_value(~s, ~s)", [E, A]) 
                      end, ["D"], 
                      string:tokens(CompoundKey, "."))),
            watch_parsing(State#state{dataExpression=Exp});

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

emitTag("") ->
    {"", none};

emitTag([C1|_]=Text) when is_integer(C1) ->
    {Text, none};

emitTag(Int) when is_integer(Int) ->
    {integer_to_list(Int), none};

emitTag(Atom) when is_atom(Atom) ->
    {atom_to_list(Atom), none};

emitTag(#state{tag={e,attr}}=State) ->
    NewState = applyRender(State),
    Name = list_to_atom(proplists:get_value({none, name}, NewState#state.attrs)),
    Value = lists:reverse(emitChildren(NewState)),
    Parent = NewState#state.parent,
    ParentAttrs2 = proplists:delete({none, Name}, Parent#state.attrs),
    NewParent = Parent#state{attrs=[{{none, Name}, Value}|ParentAttrs2]},
    {"", NewState#state{parent=NewParent}};

emitTag(#state{tag={e,inv}}=State) ->
    NewState = applyRender(State),
    {lists:reverse(emitChildren(NewState)), none};

emitTag(#state{tag={e,slot}}=State) ->
    Data = evorender:get_data(State),
    CompoundKey = proplists:get_value({none, key}, State#state.attrs),
    Final = case CompoundKey of
        undefined ->
            Data;
        Keys ->
            lists:foldl(
              fun(E, A) -> proplists:get_value(list_to_atom(E), A) end,
              Data, string:tokens(Keys, "."))
    end,
    emitTag(evorender:format(State, Final));

emitTag(#state{tag={e,key}}=State) ->
    emitTag(proplists:get_value(key, evorender:format(State, evorender:get_data(State))));

emitTag(#state{tag={e,value}}=State) ->
    emitTag(proplists:get_value(value, evorender:format(State, evorender:get_data(State))));

emitTag(#state{tag={e,Tag}}) ->
    erlang:error({"Unknown evo tag", Tag});

emitTag(#state{render=none}=State) ->
    {renderTag(State), State};

emitTag(#state{}=State) ->
    NewState = applyRender(State),
    emitTag(NewState);

emitTag({tags, Tags}) ->
    {Tags, none};

emitTag(Other) ->
    {io_lib:format("~p", [Other]), none}.


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
                              {Parent, maybe_append(Child, Children)};
                          _ ->
                              {NS2#state.parent, maybe_append(Child, Children)}
                      end;
                  _ ->
                      {Child, _} = emitTag(S),
                      {Parent, [Child|Children]}
              end
      end,
      {State, []},
      lists:reverse(State#state.children)).


maybe_append("", Children) -> Children;
maybe_append(" ", Children) -> Children;
maybe_append(Other, Children) -> [Other|Children].


renderTag(State) ->
    {NewState, Children} = emitChildren(State, true),
    case lists:flatten(Children) of
        [] -> emitEmptyTag(NewState);
        _ -> emitFullTag(NewState, Children)
    end.

applyRender(#state{render=none}=State) ->
    State;
applyRender(State) ->
    Render = list_to_atom(State#state.render),
    NewState = evorender:Render(State),
    NewState.

emitFullTag(State, Children) ->
    {openTag(State),
     lists:reverse(Children),
     closeTag(State)}.

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

noindenticate(TagSoup) ->
    PF = partial_flatten(TagSoup),
    Joined = noindent(PF),
    lists:flatten(Joined).

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

partial_flatten({tags, Tags}) ->
    partial_flatten(Tags);

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
    TotalLen = length(Tag) + length(Content) + length(End),
    case TotalLen < ?MAX_LINE of
        true ->
            [{spaces(Indent), Tag, Content, End}];
        false ->
            [{spaces(Indent), Tag},
             {spaces(Indent+1), string:strip(Content)},
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
    lists:map(fun(L) -> indent(L, Indent) end, Lines).
  

noindent({Tag, [C1|_]=Content, End}) when is_integer(C1) ->
    [Tag, Content, End];
noindent({Tag, Content, End}) ->
    [Tag, lists:map(fun(L) -> noindent(L) end, Content), End];
noindent([C1|_]=Line) when is_integer(C1) ->
    [Line];
noindent(Lines) ->
    lists:map(fun(L) -> noindent(L) end, Lines).


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
    ets:lookup_element(get(dataCache), ID, 2).

put_cache(ID, Data) ->
    ets:insert(get(dataCache), {ID, Data}).
                       
