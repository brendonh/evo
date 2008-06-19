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
  templateServer,
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
    process_flag(trap_exit, true),
    ComponentTable = ets:new(list_to_atom(atom_to_list(EvoName) ++ "_components"), [private, set]),

    lists:map(fun({Path, Type, Args}) ->
                      ets:insert(ComponentTable, {Path, get_callback(EvoName, Type, Args)})
              end,
              proplists:get_value(components, SiteConf, [])),

    TemplateServer = list_to_atom(atom_to_list(EvoName) ++ "_evotemplate"),

    cr:dbg({evosite_running, EvoName}),
    {ok, #state{siteName=EvoName,
                components=ComponentTable,
                templates=proplists:get_value(templates, SiteConf, []),
                templateServer=TemplateServer}}.

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

get_callback(_EvoName, module, {Mod, InitArgs}) ->
    Instance = Mod:new(InitArgs),
    fun(Req, Method, Args) -> Instance:respond(Req, Method, Args) end;
get_callback(EvoName, gen_server, {_Mod, Name, _InitArgs}) ->
    get_callback(EvoName, gen_server, Name);
get_callback(EvoName, gen_server, Name) ->
    CompName = concat_atoms([EvoName, "_component_", Name]),
    fun(Req, Method, Args) -> gen_server:call(CompName, {respond, Req, Method, Args}) end.

get_response(State, Req, [Top|Rest]=Path) ->
    cr:dbg({responding_to, Top, Path}),
    get_response(State, Req, Top, Rest).

get_response(State, Req, Name, Args) ->
    case ets:lookup(State#state.components, Name) of
        [] -> Req:not_found();
        [{Name, Callback}] -> run_responders(State, Req, Callback, Args)
    end.

run_responders(State, Req, Callback, Args) ->
    case catch Callback(Req, Req:get(method), Args) of
        {response, Response} -> 
            Response;
        {wrap, TemplateName, Data} ->
            wrap_template(State, Req, TemplateName, Data);
        {child, NewComponent, NewArgs} -> 
            run_responders(State, Req, NewComponent, NewArgs);
        {'EXIT', Error} ->
            Req:ok({"text/plain", lists:flatten(io_lib:format("Error: ~p~n", [Error]))})
    end.

wrap_template(State, Req, TemplateName, Data) ->
    case proplists:get_value(TemplateName, State#state.templates) of
        undefined ->
            Req:ok("text/plain", "Site template not found: " ++ atom_to_list(TemplateName));
        {Type, TemplateConf} ->
            case gen_server:call(State#state.templateServer, 
                                 {run, TemplateConf, Data}) of
                {ok, Final} -> Req:ok({Type, Final});
                {error, Error} -> Req:ok({"text/plain", Error})
            end
    end.


concat_atoms(Bits) ->
    concat_atoms(Bits, []).

concat_atoms([], Acc) -> 
    list_to_atom(lists:flatten(lists:reverse(Acc)));
concat_atoms([Atom|Rest], Acc) when is_atom(Atom) ->
    concat_atoms(Rest, [atom_to_list(Atom)|Acc]);
concat_atoms([String|Rest], Acc) ->
    concat_atoms(Rest, [String|Acc]).
