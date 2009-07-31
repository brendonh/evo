%%%-------------------------------------------------------------------
%%% File    : evo_entry.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evo_app).

-behaviour(application).

-include("evo.hrl").

%% API
-export([launch/0]).

%% Application callbacks
-export([start/2, stop/1]).


%%====================================================================
%% API
%%====================================================================

launch() ->
    application:start(evo).


%%====================================================================
%% Application callbacks
%%====================================================================
start(_Type, _StartArgs) ->
    ?DBG(evo_starting),

    mnesia:start(),
    ?DBG(waiting_for_mnesia_tables),
    case mnesia:wait_for_tables([session, comet_proc], 5000) of
        ok -> 
            ?DBG(got_tables),
            evo_sup:start_link();
        Other ->
            ?DBG({oh_noes, Other}),
            none
    end.
            

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
