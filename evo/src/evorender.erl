-module(evorender).

-include("evo.hrl").

-export([data/1]).

data(State) ->
    State#state{buffer=lists:append(State#state.data, State#state.buffer)}.
                                   
