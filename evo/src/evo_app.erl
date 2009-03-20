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
    ?DBG("Evo starting up"),
    evo_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
