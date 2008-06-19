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
-export([start_link/1, start_link/2]).

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

start_link(Name, TemplateCallback) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, TemplateCallback], []).

start_link(TemplateCallback) ->
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

init([EvoName, TemplateCallback]) ->
    Name = list_to_atom(atom_to_list(EvoName) ++ "_templateCache"),
    ets:new(Name, [private, set, named_table]),
    cr:dbg({evotemplate, EvoName, running}),
    {ok, #state{templateCallback=TemplateCallback}};

init([TemplateCallback]) ->
    ets:new(templateCache, [private, set, named_table]),
    cr:dbg({evotemplate, singleton, running}),
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

handle_call({Command, {reload, TemplateName}, Data}, _From, State) ->
    ets:delete(templateCache, TemplateName),
    handle_call({Command, TemplateName, Data}, _From, State);

handle_call({Command, TemplateName, Data}, _From, State) ->
    case ets:lookup(templateCache, TemplateName) of
        [] -> Template = generate_template(State, TemplateName);
        [{TemplateName, Template}] -> ok
    end,
    case Template of
        not_found -> Result = {error, {TemplateName, not_found}};
        _ ->  Result = run_template(Command, TemplateName, Template, Data)
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

generate_template(State, TemplateName) ->
    cr:dbg({generating, TemplateName}),
    Callback = State#state.templateCallback,
    case Callback(TemplateName) of
        not_found -> not_found;
        Content ->
            Template = spawn(fun() -> evo:prepare(Content) end),
            ets:insert(templateCache, {TemplateName, Template}),
            Template
    end.


run_template(run, Name, Template, Data) ->
    Template ! {run, Data, true, self()},
    get_result(Name, Template);
run_template(run_raw, Name, Template, Data) ->
    Template ! {run_raw, Data, self()},
    get_result(Name, Template). 


get_result(TemplateName, Template) ->
    receive
        {Template, result, R} ->
            {ok, R};
        {'EXIT', _, Error} ->
            {error, Error}
    after 3000 ->
            exit(Template, too_slow),
            cr:dbg({killing, TemplateName}),
            ets:delete(templateCache, TemplateName),
            {error, "Template too slow"}
    end.
