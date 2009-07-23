%%%-------------------------------------------------------------------
%%% File    : evoepgsql.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Serialize epgsql calls
%%%
%%% Created : 23 Jul 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(evoepgsql).

-behaviour(gen_server).

-include("evo.hrl").

%% API
-export([start_link/5, equery/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  conn
}).



%%====================================================================
%% API
%%====================================================================
start_link(Name, Host, User, Pass, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Host, User, Pass, Opts], []).

equery(Self, SQL, Args) ->
    gen_server:call(Self, {equery, SQL, Args}).



%%====================================================================
%% gen_server callbacks
%%====================================================================


init([Host, User, Pass, Opts]) ->
    {ok, C} = pgsql:connect(Host, [User], [Pass], Opts),
    {ok, #state{conn=C}}.


handle_call({equery, SQL, Args}, _From, State) ->
    Reply = pgsql:equery(State#state.conn, SQL, Args),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    pgsql:close(State#state.conn).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
