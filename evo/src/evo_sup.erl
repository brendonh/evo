%%%-------------------------------------------------------------------
%%% File    : evo_sup.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evo_sup).

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
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->

    Specs = case application:get_env(sites) of
                undefined -> [];
                {ok, Sites} ->
                    lists:map(fun spec_from_site/1, Sites)
            end,

    {ok,{{one_for_one,1,10}, Specs}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_site(Site) ->
    supervisor:start_child(?MODULE, spec_from_site(Site)).


spec_from_site({SiteName, _Conf}=Site) ->
    {evoutil:concat_atoms([SiteName, "_sup"]),
     {evosite_sup, start_link, [Site]},
     permanent,2000,worker,[evosite_sup]}.
