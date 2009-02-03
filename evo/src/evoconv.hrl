%%%-------------------------------------------------------------------
%%% File    : evoconv.hrl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Frequently used bits and bobs
%%%
%%% Created : 29 Dec 2008 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------

-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).

-define(DBG(Term), io:format("~p: ~p~n", [self(), Term])).
