%%%-------------------------------------------------------------------
%%% File    : evotemplate.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 15 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evotemplate).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  templateCallback
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link({_, _}=TemplateCallback) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [TemplateCallback], []).

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
init([TemplateCallback]) ->
    ets:new(templateCache, [private, set, named_table]),
    cr:dbg({evotemplate, running}),
    {ok, #state{templateCallback=TemplateCallback}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({run, TemplateName, Data}, _From, State) ->
    handle_call({run, TemplateName, Data, run}, _From, State);

handle_call({run, TemplateName, Data, Command}, _From, State) ->
    case ets:lookup(templateCache, TemplateName) of
        [] ->
            cr:dbg({generating, TemplateName}),
            {Mod, Func} = State#state.templateCallback,
            Content = apply(Mod, Func, [TemplateName]),
            Template = spawn_link(fun() -> evo:prepare(Content) end),
            ets:insert(templateCache, {TemplateName, Template});
        [{TemplateName, Template}] -> ok
    end,
    case Command of
        run ->
            Template ! {run, Data, true, self()};
        run_raw ->
            Template ! {run_raw, Data, self()}
    end,
    receive
        {Template, result, R} ->
            Result = {ok, R};
        {'Template, EXIT', _, Error} ->
            Result = {error, Error}
    after 1000 ->
            % Restart template?
            Result = {error, "Template dead :-("}
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
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
