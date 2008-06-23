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
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  cacheName
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

init([EvoName]) ->
    process_flag(trap_exit, true),
    Name = evoutil:concat_atoms([EvoName, "_templateCache"]),
    ets:new(Name, [private, set, named_table]),
    cr:dbg({evotemplate, EvoName, running}),
    {ok, #state{cacheName=Name}};

init([]) ->
    process_flag(trap_exit, true),
    ets:new(templateCache, [private, set, named_table]),
    cr:dbg({evotemplate, singleton, running}),
    {ok, #state{cacheName=templateCache}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({Command, {reload, TemplateName}, Data, Callback}, _From, State) ->
    ets:delete(State#state.cacheName, TemplateName),
    handle_call({Command, TemplateName, Data, Callback}, _From, State);

handle_call({Command, TemplateName, Data, Callback}, _From, State) ->
    Cache = State#state.cacheName,
    case ets:lookup(Cache, TemplateName) of
        [] -> Template = generate_template(Cache, TemplateName, Callback);
        [{TemplateName, Template}] -> ok
    end,
    Result = case Template of
                 not_found -> 
                     {error, {TemplateName, not_found}};
                 {file_error, Filename, Error} -> 
                     {error, {TemplateName, file_error, Filename, Error}};
                 _ ->  
                     run_template(Command, TemplateName, Template, Data, Cache)
    end,
    {reply, Result, State};

handle_call(Request, _From, State) ->
    cr:dbg({other_request, Request}),
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

generate_template(Cache, TemplateName, Callback) ->
    case Callback(TemplateName) of
        not_found -> not_found;
        {file, Filename} ->
            cr:dbg({reading, Filename}),
            case file:read_file(Filename) of
                {ok, Content} ->
                    init_template(Cache, TemplateName, binary_to_list(Content));
                {error, Error} ->
                    {file_error, Filename, Error}
            end;
        Content -> 
            init_template(Cache, TemplateName, Content)
    end.


init_template(Cache, Name, Content) ->
    Template = spawn_link(fun() -> evo:prepare(Content) end),
    ets:insert(Cache, {Name, Template}),
    Template.

run_template(run, Name, Template, Data, Cache) ->
    Template ! {run, Data, true, self()},
    get_result(Name, Template, Cache);
run_template(run_raw, Name, Template, Data, Cache) ->
    Template ! {run_raw, Data, self()},
    get_result(Name, Template, Cache). 


get_result(TemplateName, Template, Cache) ->
    receive
        {Template, result, R} ->
            {ok, R};
        {'EXIT', _, Error} ->
            ets:delete(Cache, TemplateName),            
            {error, Error}
    after 3000 ->
            exit(Template, too_slow),
            ets:delete(Cache, TemplateName),
            {error, "Template too slow"}
    end.
