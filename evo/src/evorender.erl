-module(evorender).

-include("evo.hrl").

-export([data/1, foreach/1, foreach_zip/1, items/1, get_data/1, format/2]).

data(State) ->
    Data = get_data(State),
    State#state{children=[format(State, Data)|State#state.children],
                render=none}.

foreach(State) ->
    Data = get_data(State),
    foreach(State, Data).

foreach_zip(State) ->
    {Data,Add} = get_data(State),
    Zipped = [{Datum, Add} || Datum <- Data],
    foreach(State, Zipped).

foreach(State, Data) ->
    {_, NewChildren} = lists:foldl(
                         fun(Data, {Row, Acc}) ->
                                 ID = evo:new_id(),
                                 evo:put_cache(ID, Data),
                                 NewState = #state{id=ID,
                                                   tag={e,inv},
                                                   row=Row,
                                                   level=State#state.level+1,
                                                   children=State#state.children},
                                 Final = set_parent(NewState, State),
                                 {Row+1, [Final|Acc]}
                         end,
                         {0, []}, Data),
    State#state{render=none, children=NewChildren}.

items(State) ->
    Data = get_data(State),
    NewData = lists:map(fun({K,V}) -> [{key, K}, {value, V}] end, Data),
    evo:put_cache(State#state.id, NewData),
    foreach(State).

get_data(#state{id=ID, dataExpression=none, parent=none}) ->
    evo:get_cache(ID);
get_data(#state{id=ID, dataExpression=none, parent=Parent}) ->
    case evo:get_cache(ID) of
        undefined -> 
            Data = get_data(Parent),
            evo:put_cache(ID, Data),
            Data;
        Data -> 
            Data
    end;
get_data(#state{id=ID, dataExpression=DataExp, parent=Parent}=State) ->
    case evo:get_cache(ID) of
        undefined ->
            ParentData = get_data(Parent),
            Data = eval(DataExp, ParentData, get_row(State)),
            evo:put_cache(ID, Data),
            Data;
        Data ->
            Data
    end.

format(#state{formatFunc=none}, Data) -> Data;
format(#state{formatFunc={Key, Args}}, Data) ->
    Func = proplists:get_value(Key, evo:get_cache(0)),
    case Func of
        undefined -> "Missing format function: " ++ Key;
        _ ->
            TopData = evo:get_cache(0),
            apply(Func, [Data,TopData|Args])
    end.          

get_row(#state{row=none, parent=none}) -> none;
get_row(#state{row=none, parent=Parent}) -> get_row(Parent);
get_row(#state{row=Row}) -> Row.


eval(String, OldData, Row) ->
    {ok,Scanned,_} = erl_scan:string(String ++ "."),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    B = erl_eval:add_binding('S', fun atom_to_list/1, erl_eval:new_bindings()), 
    B2 = erl_eval:add_binding('D', OldData, B),
    B3 = erl_eval:add_binding('R', Row, B2),
    B4 = case Row of
             none -> erl_eval:add_binding('OddEven', none, B3);
             _ -> erl_eval:add_binding('OddEven', lists:nth((Row rem 2) + 1, ['odd', 'even']), B3)
         end,
    {value, Result, _Env2} = erl_eval:exprs(Parsed,B4),
    Result.

set_parent(Text, _) when is_list(Text) -> Text;
set_parent(State, Parent) ->
    NewChildren = lists:map(fun(C) -> set_parent(C, State) end,
                            State#state.children),       
    ID = evo:new_id(),
    evo:put_cache(ID, evo:get_cache(State#state.id)),
    State#state{id=ID, parent=Parent, children=NewChildren}.

