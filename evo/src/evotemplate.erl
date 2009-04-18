%%%-------------------------------------------------------------------
%%% File    : evotemplate.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 15 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evotemplate).

-behaviour(gen_server).

-include("evo.hrl").

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

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%%====================================================================
%% gen_server callbacks
%%====================================================================

init([EvoName]) ->
    process_flag(trap_exit, true),
    Name = evoutil:concat_atoms([EvoName, "_templateCache"]),
    ets:new(Name, [private, set, named_table]),
    ?DBG({evotemplate, EvoName, running}),
    {ok, #state{cacheName=Name}};

init([]) ->
    process_flag(trap_exit, true),
    ets:new(templateCache, [private, set, named_table]),
    ?DBG({evotemplate, singleton, running}),
    {ok, #state{cacheName=templateCache}}.


handle_call({Command, {reload, TemplateName}, Data, Conf, Callback}, _From, State) ->
    ets:delete(State#state.cacheName, TemplateName),
    handle_call({Command, TemplateName, Data, Conf, Callback}, _From, State);

handle_call({Command, TemplateName, Data, Conf, Callback}, _From, State) ->
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
                     run_template(Command, TemplateName, Template, Data, Conf, Cache)
    end,
    {reply, Result, State};

handle_call(Request, _From, State) ->
    ?DBG({other_request, Request}),
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

generate_template(Cache, TemplateName, Callback) ->
    case Callback(TemplateName) of
        not_found -> not_found;
        {file, Filename} ->
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


run_template(Command, TemplateName, Template, Data, Conf, Cache) ->
    Template ! {Command, Data, Conf, self()},
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
