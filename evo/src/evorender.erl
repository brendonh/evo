-module(evorender).

-include("evo.hrl").

-export([data/1, foreach/1]).

data(State) ->
    Data = get_data(State),
    State#state{children=[Data|State#state.children],
                render=none}.

foreach(State) ->
    NewChildren = lists:map(
                    fun(Data) ->
                            NewState = #state{tag={e,inv},
                                              data=Data,
                                              level=State#state.level+1,
                                              children=State#state.children},
                            set_parent(NewState, State)
                    end,
                    lists:reverse(get_data(State))),
    State#state{render=none, children=NewChildren}.


get_data(#state{data=none,parent=none}) ->
    none;
get_data(#state{data=none,parent=Parent}) ->
    get_data(Parent);
get_data(#state{data=Data}) ->
    Data.
    

set_parent(Text, _) when is_list(Text) -> Text;
set_parent(State, Parent) ->
    NewChildren = lists:map(fun(C) -> set_parent(C, State) end,
                            State#state.children),
    State#state{parent=Parent, children=NewChildren}.

