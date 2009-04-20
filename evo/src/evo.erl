-module(evo).

-import (utf8, [from_binary/1, to_binary/1]).
-export([prepare/1, run/2, run/3, run_file/2, run_file/3, new_id/0, 
         get_cache/1, put_cache/2,
         var/1, put_var/2, put_var_cache/3, conf/1, 
         tag/3, escape_entities/1]).

-include("evo.hrl").
 
-define(MAX_LINE, 80).

run_file(Filename, InitialData) ->
    run_file(Filename, InitialData, true).

run_file(Filename, InitialData, Conf) ->
    {ok, UTF8} = file:read_file(Filename),
    {ok, UTF32} = from_binary(UTF8),
    case run(UTF32, InitialData, Conf) of
        {'EXIT', _, Error} -> {error, Error};
        Result -> to_binary(Result)
    end.

run(Content, InitialData) ->   
    run(Content, InitialData, []).

run(Content, InitialData, Conf) ->   
    process_flag(trap_exit, true),
    Self = self(),
    Template = spawn_link(fun() -> prepare(Content) end),
    Template ! {run, InitialData, Conf, Self},
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
    put(varCache, ets:new(varCache, [private, set])),
    put(newID, 1),
    Self = self(),
    spawn_link(fun() -> evoxml:parse(Content, Self) end),
    FinalState = watch_parsing(#templateState{id=0}),
    ets:insert(get(finalCache), ets:tab2list(get(dataCache))),
    accept_runs(FinalState).


accept_runs(State) ->
    receive
        
        {run_raw, InitialData, Conf, From} ->
            From ! {self(), result, {tags, get_tags(State, InitialData, Conf)}},
            accept_runs(State);

        {run, InitialData, Conf, From} ->
            TopTags = get_tags(State, InitialData, Conf),
            Pretty = proplists:get_value(pretty, Conf, true),
            I = case Pretty of
                    true -> fun indenticate/1;
                    false -> fun noindenticate/1;
                    raw -> fun rawindenticate/1
                end,
            Output = lists:flatten(lists:foldl(
                                     fun(Elem, InAcc) -> [I(Elem)|InAcc] end,
                                     [], TopTags)),

            Replace = proplists:get_value(replace_entities, Conf, false),
            Final = case Replace of
                        true -> replace_entities(Output);
                        false -> Output
                    end,

            From ! {self(), result, string:strip(Final, right, $\n)},
            accept_runs(State);

        finished -> ok;

        Other ->
            ?DBG({unknown_command, Other}),
            accept_runs(State)

    end.


replace_entities(String) ->
    replace_entities(String, [], []).

replace_entities([$&|Rest], Out, []) ->
    replace_entities(Rest, Out, [$&]);
replace_entities([C|Rest], Out, []) ->
    replace_entities(Rest, [C|Out], []);
replace_entities([$;|Rest], Out, Buf) ->
    Char = translate_entity(lists:reverse([$;|Buf])),
    replace_entities(Rest, [Char|Out], []);
replace_entities([C|Rest], Out, Buf) ->
    replace_entities(Rest, Out, [C|Buf]);
replace_entities([], Out, []) ->
    lists:reverse(Out);
replace_entities([], Out, Buf) ->
    ?DBG({entity_buffer_remaining, Buf}),
    lists:reverse(Out).


translate_entity("&lt;") -> $<;
translate_entity("&gt;") -> $>;
translate_entity("&amp;") -> $&;
translate_entity("&quot;") -> $";
translate_entity("&apos;") -> $';
translate_entity(Other) ->
    ?DBG({unknown_entity, Other}),
    $?.
     

escape_entities(String) ->
    lists:flatten([escape_entity(C) || C <- String]).

escape_entity($<) -> "&lt;";
escape_entity($>) -> "&gt;";
escape_entity($&) -> "&amp;";
escape_entity($") -> "&quot;";
escape_entity($') -> "&apos;";
escape_entity(C) -> C.


get_tags(State, InitialData, Conf) ->
    ets:delete_all_objects(get(dataCache)),
    ets:delete_all_objects(get(varCache)),
    ets:insert(get(dataCache), ets:tab2list(get(finalCache))),
    put_cache(0, InitialData),
    put(evoconf, Conf),
    emitChildren(State).


watch_parsing(State) ->
    receive

        {tag_start, Name} ->
            NewState = #templateState{id=new_id(),
                              tag=Name,
                              level=State#templateState.level+1, 
                              parent=State},
            watch_parsing(NewState);

        {attr, {{e, dataExp}, Value}} ->
            watch_parsing(State#templateState{dataExpression=Value});

        {attr, {{e, data}, Value}} ->
            put_cache(State#templateState.id, Value),
            watch_parsing(State);

        {attr, {{e, format}, Key}} ->
            [Func|Args] = string:tokens(Key, " "),
            watch_parsing(State#templateState{formatFunc={list_to_atom(Func), Args}});

        {attr, {{e, key}, CompoundKey}} ->
            Exp = lists:flatten(
                    lists:foldl(
                      fun(E, A) -> 
                              io_lib:format("proplists:get_value(~s, ~s)", [E, A]) 
                      end, ["D"], 
                      string:tokens(CompoundKey, "."))),
            watch_parsing(State#templateState{dataExpression=Exp});

        {attr, {{e, render}, Value}} ->
            watch_parsing(State#templateState{render=Value});

        {attr, {{e, Attr}, _}} ->
            erlang:error({"Unknown evo attribute", Attr});

        {attr, {NSAttr, Value}} ->
            watch_parsing(State#templateState{
                            attrs=[{NSAttr, Value}|State#templateState.attrs]});

        {text, Text} ->
            watch_parsing(State#templateState{
                            children=[Text|State#templateState.children]});

        {unhandled_tag, {Tag}} ->
            watch_parsing(State#templateState{
                            children=[Tag|State#templateState.children]});

        {tag_end, _} ->
            Parent = State#templateState.parent,
            watch_parsing(Parent#templateState{
                            children=[State|Parent#templateState.children]});

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

emitTag(#templateState{tag={e,attr}}=State) ->
    NewState = applyRender(State),
    Name = list_to_atom(proplists:get_value({none, name}, 
                                            NewState#templateState.attrs)),
    Value = lists:reverse(emitChildren(NewState)),
    Parent = NewState#templateState.parent,
    ParentAttrs2 = proplists:delete({none, Name}, 
                                    Parent#templateState.attrs),
    NewParent = Parent#templateState{attrs=[{{none, Name}, Value}|ParentAttrs2]},
    {"", NewState#templateState{parent=NewParent}};

emitTag(#templateState{tag={e,inv}}=State) ->
    NewState = applyRender(State),
    {lists:reverse(emitChildren(NewState)), none};

emitTag(#templateState{tag={e,slot}}=State) ->
    Data = evorender:get_data(State),
    CompoundKey = proplists:get_value({none, key}, State#templateState.attrs),
    Final = case CompoundKey of
        undefined ->
            Data;
        Keys ->
            lists:foldl(
              fun(E, A) -> proplists:get_value(list_to_atom(E), A) end,
              Data, string:tokens(Keys, "."))
    end,
    emitTag(evorender:format(State, Final));

emitTag(#templateState{tag={e,key}}=State) ->
    emitTag(evorender:format(State, proplists:get_value(key, evorender:get_data(State))));

emitTag(#templateState{tag={e,value}}=State) ->
    emitTag(evorender:format(State, proplists:get_value(value, evorender:get_data(State))));

emitTag(#templateState{tag={e,Tag}}) ->
    erlang:error({"Unknown evo tag", Tag});

emitTag(#templateState{render=none}=State) ->
    {renderTag(State), State};

emitTag(#templateState{}=State) ->
    NewState = applyRender(State),
    emitTag(NewState);

emitTag({tags, Tags}) ->
    {Tags, none};

emitTag(Other) when is_binary(Other) ->
    {binary_to_list(Other), none};

emitTag(Other) ->
    {io_lib:format("~p", [Other]), none}.


emitChildren(State) ->
    {_, Children} = emitChildren(State, true),
    Children.

emitChildren(State, true) ->
    lists:foldl(
      fun(S, {Parent, Children}) ->
              case S of
                  #templateState{} ->
                      NS = S#templateState{parent=Parent},
                      {Child, NS2} = emitTag(NS),
                      case NS2 of
                          none ->
                              {Parent, maybe_append(Child, Children)};
                          _ ->
                              {NS2#templateState.parent, 
                               maybe_append(Child, Children)}
                      end;
                  _ ->
                      {Child, _} = emitTag(S),
                      {Parent, [Child|Children]}
              end
      end,
      {State, []},
      lists:reverse(State#templateState.children)).


maybe_append("", Children) -> Children;
maybe_append(" ", Children) -> Children;
maybe_append(Other, Children) -> [Other|Children].


renderTag(State) ->
    {NewState, Children} = emitChildren(State, true),
    case lists:flatten(Children) of
        [] -> emitEmptyTag(NewState);
        _ -> emitFullTag(NewState, Children)
    end.

applyRender(#templateState{render=none}=State) ->
    State;
applyRender(State) ->
    [FuncName|Args] = string:tokens(State#templateState.render, " "),
    Render = list_to_atom(FuncName),
    apply(evorender, Render, [State|Args]).

emitFullTag(State, Children) ->
    {openTag(State),
     lists:reverse(Children),
     closeTag(State)}.


emitEmptyTag(#templateState{tag={none,script}}=State) ->
    emitFullTag(State, []); %% Screw you, HTML
emitEmptyTag(#templateState{tag={none,'div'}}=State) ->
    emitFullTag(State, []); %% Seriously HTML, I will hit you with a bat
emitEmptyTag(#templateState{tag={none,'textarea'}}=State) ->
    emitFullTag(State, []); %% Until you fall over and cry
emitEmptyTag(State) ->
    [lists:flatten([$<, flatten_name(State#templateState.tag), 
                    flatten_attrs(State#templateState.attrs),
                    " />"])].

openTag(State) ->
    [$<, flatten_name(State#templateState.tag), 
     flatten_attrs(State#templateState.attrs), $>].

closeTag(State) ->
    [$<, $/, flatten_name(State#templateState.tag), $>].

flatten_attrs([]) -> "";
flatten_attrs(Attrs) ->
    " " ++ string:join(
      lists:map(
        fun ({NSAttr, Value}) -> 
                lists:flatten([flatten_name(NSAttr), $=, $", Value, $"])
        end,
        Attrs),
      " ").

flatten_name({none, Attr}) -> atom_to_list(Attr);
flatten_name({NS, Attr}) -> lists:flatten([atom_to_list(NS), ":", atom_to_list(Attr)]);
flatten_name(Attr) -> flatten_name({none, Attr}).


indenticate(TagSoup) ->
    PF = partial_flatten(TagSoup),
    Indented = indent(PF, 0),
    Flattened = lists:flatten(Indented),
    LineList = lists:map(fun erlang:tuple_to_list/1, Flattened),
    string:join(lists:map(fun lists:flatten/1, LineList), "\n") ++ [$\n].

noindenticate(TagSoup) ->
    noindenticate(TagSoup, false).

rawindenticate(TagSoup) ->
    noindenticate(TagSoup, true).


noindenticate(TagSoup, PreserveWhitespace) ->
    PF = partial_flatten(TagSoup, PreserveWhitespace),
    Joined = noindent(PF),
    lists:flatten(Joined).


partial_flatten(TagSoup) ->
    partial_flatten(TagSoup, false).

partial_flatten({Tag, Content, End}, PreserveWhitespace) ->
    FlatTag = lists:flatten(Tag),
    FlatEnd = lists:flatten(End),
    Inside = partial_flatten(Content, PreserveWhitespace),
    case Inside of
        [C1|_] when is_integer(C1) and C1 =:= $< -> 
            lists:concat([FlatTag, Inside, FlatEnd]);
        _ ->
            {FlatTag, Inside, FlatEnd}
    end;

partial_flatten({tags, Tags}, PreserveWhitespace) ->
    partial_flatten(Tags, PreserveWhitespace);

partial_flatten([C|_]=Text, false) when is_integer(C) ->
    newlines_to_spaces(Text);

partial_flatten([C|_]=Text, true) when is_integer(C) ->
    Text;

partial_flatten(Stuff, PreserveWhitespace) when is_list(Stuff) ->
    Before = lists:map(fun(E) -> partial_flatten(E, PreserveWhitespace) end, Stuff),
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

maybe_join([C1|_]=Text, {[[C2|_]=Last|Rest], All}) 
  when is_integer(C1), is_integer(C2) ->
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
                       

var(Name) ->
    State = get(formatState),
    get_state_var(State, Name).

put_var(Name, Value) ->
    State = get(formatState),
    put_var_cache(State#templateState.id, Name, Value).

get_state_var(none, _Name) ->
    undefined;
get_state_var(State, Name) ->
    ID = State#templateState.id,
    case get_var_cache(ID, Name) of
        undefined -> get_state_var(State#templateState.parent, Name);
        Value -> Value
    end.

get_var_cache(ID, Name) ->
    case ets:lookup(get(varCache), ID) of
        [{ID, Vars}] ->
            proplists:get_value(Name, Vars);
        [] -> undefined
    end.

put_var_cache(ID, Name, Value) ->
    OldVars = case ets:lookup(get(varCache), ID) of
        [{ID, Vars}] -> proplists:delete(Name, Vars);
        [] -> []
    end,
    ets:insert(get(varCache), {ID, [{Name, Value}|OldVars]}).

conf(Name) ->
    proplists:get_value(Name, get(evoconf)).


tag({NS, Name}, Attrs, Content) ->
    {tags, [{lists:flatten([$<, flatten_name({NS, Name}), flatten_attrs(Attrs), $>]),
             Content,
             lists:flatten([$<, $/, flatten_name({NS, Name}), $>])}]};

tag(Name, Attrs, "") -> empty_tag({none, Name}, Attrs);
tag(Name, Attrs, Content) -> tag({none, Name}, Attrs, Content).

empty_tag({NS, Name}, Attrs) ->
    {tags, [lists:flatten([$<, flatten_name({NS, Name}), flatten_attrs(Attrs), " />"])]}.
