%%%-------------------------------------------------------------------
%%% File    : evo_sup.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evo_sup).

-include("evo.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0, start_site/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->

    Specs = case application:get_env(evo, sites) of
                undefined -> [];
                {ok, Sites} ->
                    lists:map(fun spec_from_site/1, Sites)
            end,

    {ok,{{one_for_one,0,10}, Specs}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_site(Conf) ->
    supervisor:start_child(?MODULE, spec_from_site(Conf)).

spec_from_site(Conf) ->
    {?CONFNAME(Conf, "sup"),
     {evosite_sup, start_link, [Conf]},
     permanent,2000,supervisor,[evosite_sup]}.
