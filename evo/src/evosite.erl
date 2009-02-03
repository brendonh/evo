%%%-------------------------------------------------------------------
%%% File    : evosite.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosite).

-behaviour(gen_server).

-include("evoconv.hrl").

%% API
-export([start_link/2, db/2, template/2, link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  evoname,
  components,
  templates,
  templateCallback,
  contentType = "text/html; charset=utf-8"
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(SiteName, Conf) ->
    gen_server:start_link({local, SiteName}, ?MODULE, [SiteName, Conf], []).

db(EvoName, Args) ->
    MagicName = evoutil:concat_atoms([EvoName, "_magicdb"]),
    gen_server:call(MagicName, Args).

template(EvoName, Args) ->
    TemplateServerName = evoutil:concat_atoms([EvoName, "_evotemplate"]),
    gen_server:call(TemplateServerName, Args).

link(EvoName, ComponentName, SubBits) ->
    ComponentPathTable = evoutil:concat_atoms([EvoName, "_componentPaths"]),
    FullComponentName = evoutil:concat_atoms([EvoName, "_component_", ComponentName]),
    case ets:lookup(ComponentPathTable, FullComponentName) of
        [] -> not_found;
        [{FullComponentName, Path}|_] -> 
            [$/|string:join([Path|SubBits], "/")]
    end.
            

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

    ComponentTable = ets:new(evoutil:concat_atoms([EvoName, "_components"]), [private, set]),

    lists:map(fun({Path, Type, Args}) ->
                      ets:insert(ComponentTable, {Path, get_callback(EvoName, Type, Args)})
              end,
              proplists:get_value(components, SiteConf, [])),

    Templates = proplists:get_value(templates, SiteConf, []),
    Callback = fun(Name) -> 
                       case proplists:get_value(Name, Templates, not_found) of
                           not_found -> not_found;
                           {_Type, _Reload, Filename} -> {file, Filename}
                       end
               end,

    ?DBG({evosite_running, EvoName}),
    {ok, #state{evoname=EvoName,
                components=ComponentTable,
                templates=Templates,
                templateCallback=Callback}}.

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
    Path = string:tokens(Req:get(path), "/"),
    Response = get_response(State, Req, Path),
    Req:cleanup(),
    {reply, Response, State};

handle_call(Request, _From, State) ->
    ?DBG({unknown_call, Request}),
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

get_callback(EvoName, module, {Mod, InitArgs}) ->
    Instance = apply(Mod, new, [EvoName|InitArgs]),
    fun(Req, Method, Args) -> Instance:respond(Req, Method, Args) end;
get_callback(EvoName, gen_server, {_Mod, Name, _InitArgs}) ->
    get_callback(EvoName, gen_server, Name);
get_callback(EvoName, gen_server, Name) ->
    CompName = evoutil:concat_atoms([EvoName, "_component_", Name]),
    fun(Req, Method, Args) -> gen_server:call(CompName, {respond, Req, Method, Args}) end.

get_response(State, Req, []) ->
    get_response(State, Req, [""]);
get_response(State, Req, [Top|Rest]) ->
    case ets:lookup(State#state.components, Top) of
        [] -> Req:not_found();
        [{Top, Callback}] -> run_responders(State, Req, Callback, Rest)
    end.

run_responders(State, Req, Callback, Args) ->
    case catch Callback(Req, Req:get(method), Args) of
        {response, Response} -> 
            Response;
        {wrap, TemplateName, Data} ->
            wrap_template(State, Req, TemplateName, Data);
        {child, NewComponent, NewArgs} -> 
            run_responders(State, Req, NewComponent, NewArgs);
        {error, Error} ->
            display_error(Req, "Error: ~p~n", [Error]);
        {'EXIT', Error} ->
            display_error(Req, "Error: ~p~n", [Error]);
        not_found ->
            Req:not_found();
        Other ->
            display_error(Req, "Unknown component response: ~p~n", [Other])
    end.

wrap_template(State, Req, TemplateName, Data) ->
    case proplists:get_value(TemplateName, State#state.templates) of
        undefined ->
            display_error(Req, "Site template not found: ~p~n", [TemplateName]);
        {Type, reload, _Filename} ->
            run_wrap_template(State, Req, Type, {reload, TemplateName}, Data);
        {Type, cache, _Filename} ->
            run_wrap_template(State, Req, Type, TemplateName, Data);
        Other ->
            display_error(Req, "Unknown template config: ~p~n", [Other])
    end.


run_wrap_template(State, Req, Type, TemplateName, Data) ->
    case template(State#state.evoname, 
                  {run, TemplateName, Data, [],
                   State#state.templateCallback}) of
        {ok, Final} -> Req:ok({Type, Final});
        {error, Error} -> display_error(Req, "Template error: ~p~n", [Error]);
        Other -> display_error(Req, "Unknown template response: ~p~n", [Other])
    end.


display_error(Req, Template, Content) ->
    Req:ok({"text/plain", lists:flatten(io_lib:format(Template, Content))}).
