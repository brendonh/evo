%%%-------------------------------------------------------------------
%%% File    : evosite.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosite).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  siteName,
  staticRoot = "static",
  contentType = "text/html; charset=utf-8"
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(SiteName, StaticRoot) ->
    gen_server:start_link({local, SiteName}, ?MODULE, [SiteName, StaticRoot], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([SiteName, StaticRoot]) ->
    cr:dbg({evosite_running, SiteName}),
    {ok, #state{siteName=SiteName,
                staticRoot=StaticRoot}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({respond, Req}, _From, State) ->
    cr:dbg({call, State#state.siteName, Req:get(path)}),
    Req:cleanup(),
    {reply, Req:ok({State#state.contentType, <<"Hi.">>}), State};

handle_call(Request, _From, State) ->
    cr:dbg({unknown_call, Request}),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



%-define(DSN, "DSN=routecomplete_dev;UID=routecomplete;PWD=secret").
%-define(DATABASE, "routecomplete").


%start([Port, DB, User, Pass]) ->
%    start(atom_to_list(Port), 
%          lists:flatten(io_lib:format("DSN=~s;UID=~s;PWD=~s", [DB, User, Pass]))).

%start(Port, DSN) ->
%    cr:start_link(),
%    cr:dbg({evosite, running}),
%    receive
%        finish -> ok
%    end.

%request(Req) ->
%    Bits = string:tokens(Req:get(path), "/"),
%    Real = case lists:last(Req:get(path)) == $/ of
%               true -> Bits ++ [""];
%               false -> Bits
%           end,
%    
%    case dispatch(Req, Req:get(method), Real) of
%        {ok, Type, Title, Result} ->
%            SiteData = [{title, Title}, {content, Result}],
%            case gen_server:call(evotemplate, {run, site, SiteData}) of
%                {ok, Final} -> Req:ok({Type, Final});
%                {error, Error} -> Req:ok({?CONTENT_TYPE, Error})
%            end;
%        {other, Result} ->
%            Result;
%        _ ->
%            Req:respond({500, [], <<"Internal error">>})
%    end.
