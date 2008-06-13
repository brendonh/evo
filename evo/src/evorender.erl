-module(evorender).

-include("evo.hrl").

-export([data/1, foreach/1, get_data/1]).

data(State) ->
    Data = get_data(State),
    State#state{children=[Data|State#state.children],
                render=none}.

foreach(State) ->
    NewChildren = lists:map(
                    fun(Data) ->
                            ID = evo:new_id(),
                            evo:put_cache(ID, Data),
                            NewState = #state{id=ID,
                                              tag={e,inv},
                                              level=State#state.level+1,
                                              children=State#state.children},
                            set_parent(NewState, State)
                    end,
                    lists:reverse(get_data(State))),
    State#state{render=none, children=NewChildren}.


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
get_data(#state{id=ID, dataExpression=DataExp, parent=Parent}) ->
    case evo:get_cache(ID) of
        undefined ->
            ParentData = get_data(Parent),
            Data = eval(DataExp, ParentData),
            evo:put_cache(ID, Data),
            Data;
        Data ->
            Data
    end.

eval(String, OldData) ->
    {ok,Scanned,_} = erl_scan:string(String ++ "."),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    B = erl_eval:add_binding('S', fun atom_to_list/1, erl_eval:new_bindings()), 
    B2 = erl_eval:add_binding('D', OldData, B),
    {value, Result, _Env2} = erl_eval:exprs(Parsed,B2),
    Result.

set_parent(Text, _) when is_list(Text) -> Text;
set_parent(State, Parent) ->
    NewChildren = lists:map(fun(C) -> set_parent(C, State) end,
                            State#state.children),
    State#state{id=evo:new_id(), parent=Parent, children=NewChildren}.

