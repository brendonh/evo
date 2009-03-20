-module(evorender).

-include("evo.hrl").

-export([data/1, foreach/1, foreach/2, items/1, ifdata/1, get_data/1, format/2]).

data(State) ->
    Data = get_data(State),
    State#templateState{children=[format(State, Data)|State#templateState.children],
                render=none}.

foreach(State) ->
    foreach(State, "none").

foreach(State, NameStr) ->
    Name = list_to_atom(NameStr),
    {_, NewChildren} = lists:foldl(
                         fun(Data, {Row, Acc}) ->
                                 ID = evo:new_id(),
                                 evo:put_cache(ID, Data),
                                 NewState = #templateState{id=ID,
                                                   tag={e,inv},
                                                   row=Row,
                                                   level=State#templateState.level+1,
                                                   children=State#templateState.children},
                                 Final = set_parent(NewState, State),

                                 case Name of
                                     none -> ok;
                                     Name -> evo:put_var_cache(Final#templateState.id, Name, Data)
                                 end,

                                 {Row+1, [Final|Acc]}
                         end,
                         {0, []}, get_data(State)),
    State#templateState{render=none, children=NewChildren}.

items(State) ->
    Data = get_data(State),
    NewData = lists:map(fun({K,V}) -> [{key, K}, {value, V}] end, Data),
    evo:put_cache(State#templateState.id, NewData),
    foreach(State).


ifdata(State) ->
    case get_data(State) of
        true -> 
            evo:put_cache(State#templateState.id, undefined),
            State#templateState{dataExpression=none};
        false -> 
            State#templateState{children=[]}
    end.

get_data(#templateState{id=ID, dataExpression=none, parent=none}) ->
    evo:get_cache(ID);
get_data(#templateState{id=ID, dataExpression=none, parent=Parent}) ->
    case evo:get_cache(ID) of
        undefined -> 
            Data = get_data(Parent),
            evo:put_cache(ID, Data),
            Data;
        Data -> 
            Data
    end;
get_data(#templateState{id=ID, dataExpression=DataExp, parent=Parent}=State) ->
    case evo:get_cache(ID) of
        undefined ->
            ParentData = get_data(Parent),
            Data = eval(DataExp, ParentData, get_row(State)),
            evo:put_cache(ID, Data),
            Data;
        Data ->
            Data
    end.

format(#templateState{formatFunc=none}, Data) -> Data;
format(#templateState{formatFunc={Key, Args}}=State, Data) ->
    Func = proplists:get_value(Key, get(evoconf)),
    case Func of
        undefined -> "Missing format function: " ++ Key;
        _ ->
            put(formatState, State),
            Result = apply(Func, [Data|Args]),
            put(formatState, undefined),
            Result
    end.

get_row(#templateState{row=none, parent=none}) -> none;
get_row(#templateState{row=none, parent=Parent}) -> get_row(Parent);
get_row(#templateState{row=Row}) -> Row.


eval(String, OldData, Row) ->
    {ok,Scanned,_} = erl_scan:string(String ++ "."),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    B = erl_eval:add_binding('S', fun atom_to_list/1, erl_eval:new_bindings()), 
    B2 = erl_eval:add_binding('D', OldData, B),
    B3 = erl_eval:add_binding('Row', Row, B2),
    B4 = erl_eval:add_binding('Key', fun(K) -> proplists:get_value(K, OldData) end, B3),
    B5 = case Row of
             none -> erl_eval:add_binding('OddEven', none, B4);
             _ -> erl_eval:add_binding('OddEven', lists:nth((Row rem 2) + 1, ['odd', 'even']), B4)
         end,

    case (catch erl_eval:exprs(Parsed,B5)) of
        {value, Result, _Env2} -> Result;
        Other -> throw({eval_error, String, Other})
    end.
%    {value, Result, _Env2} = erl_eval:exprs(Parsed,B5),
%    Result.

set_parent(Text, _) when is_list(Text) -> Text;
set_parent(State, Parent) ->
    ID = evo:new_id(),
    NewChildren = lists:map(fun(C) -> set_parent(C, State) end,
                            State#templateState.children),       
    evo:put_cache(ID, evo:get_cache(State#templateState.id)),
    State#templateState{id=ID, parent=Parent, children=NewChildren}.
