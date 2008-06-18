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
  components,
  templates,
  contentType = "text/html; charset=utf-8"
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(SiteName, Components) ->
    gen_server:start_link({local, SiteName}, ?MODULE, [SiteName, Components], []).

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
init([EvoName, SiteConf]) ->
    ComponentTable = ets:new(list_to_atom(atom_to_list(EvoName) ++ "_components"), [private, set]),
    lists:map(
      fun({Path, {Mod, Args}}) -> ets:insert(ComponentTable, {Path, Mod:new(Args)}) end,
      proplists:get_value(components, SiteConf, [])),
    cr:dbg({evosite_running, EvoName}),
    {ok, #state{siteName=EvoName,
                components=ComponentTable,
                templates=proplists:get_value(templates, SiteConf, [])}}.

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
    SplitPath = string:tokens(Req:get(path), "/"),
    Path = case lists:last(Req:get(path)) == $/ of
               true -> SplitPath ++ [""];
               false -> SplitPath
           end,
    Response = get_response(State, Req, Path),
    Req:cleanup(),
    {reply, Response, State};

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

get_response(State, Req, [Top|Rest]=Path) ->
    cr:dbg({responding_to, Top, Path}),
    case ets:lookup(State#state.components, Top) of
        [] -> Req:not_found();
        [{Top, Component}] -> run_responders(Req, Component, Rest)
    end.
                
run_responders(Req, Component, Args) ->
    case catch Component:respond(Req, [Args]) of
        {response, Response} -> 
            Response;
        {child, NewComponent, NewArgs} -> 
            run_responders(Req, NewComponent, NewArgs);
        {'EXIT', Reason} ->
            Req:ok({"text/plain", lists:flatten(io_lib:format("Error: ~p~n", [Reason]))})
    end.
