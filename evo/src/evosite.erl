%%%-------------------------------------------------------------------
%%% File    : evosite.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosite).

%-behaviour(gen_server).

-include("evo.hrl").

%% API
-export([respond/2]).

%% API
%-export([start_link/2, db/2, template/2, link/3]).

%% gen_server callbacks
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%         terminate/2, code_change/3]).




%-record(state, {
%  evoname,
%  components,
%  templates,
%  templateCallback,
%  contentType = "text/html; charset=utf-8"
%}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%start_link(SiteName, Conf) ->
%    gen_server:start_link({local, SiteName}, ?MODULE, [SiteName, Conf], []).

%db(EvoName, Args) ->
%    MagicName = evoutil:concat_atoms([EvoName, "_magicdb"]),
%    gen_server:call(MagicName, Args).

%template(EvoName, Args) ->
%    TemplateServerName = evoutil:concat_atoms([EvoName, "_evotemplate"]),
%    gen_server:call(TemplateServerName, Args).

%link(EvoName, ComponentName, SubBits) ->
%    ComponentPathTable = evoutil:concat_atoms([EvoName, "_componentPaths"]),
%    FullComponentName = evoutil:concat_atoms([EvoName, "_component_", ComponentName]),
%    case ets:lookup(ComponentPathTable, FullComponentName) of
%        [] -> not_found;
%        [{FullComponentName, Path}|_] -> 
%            [$/|string:join([Path|SubBits], "/")]
%    end.
            

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
%% init([EvoName, SiteConf]) ->

%%     ComponentTable = ets:new(evoutil:concat_atoms([EvoName, "_components"]), [private, set]),

%%     lists:map(fun({Path, Type, Args}) ->
%%                       ets:insert(ComponentTable, {Path, get_callback(EvoName, Type, Args)})
%%               end,
%%               proplists:get_value(components, SiteConf, [])),

%%     Templates = proplists:get_value(templates, SiteConf, []),
%%     Callback = fun(Name) -> 
%%                        case proplists:get_value(Name, Templates, not_found) of
%%                            not_found -> not_found;
%%                            {_Type, _Reload, Filename} -> {file, Filename}
%%                        end
%%                end,

%%     ?DBG({evosite_running, EvoName}),
%%     {ok, #state{evoname=EvoName,
%%                 components=ComponentTable,
%%                 templates=Templates,
%%                 templateCallback=Callback}}.


respond(Req, Conf) ->
    Path = string:tokens(Req:get(path), "/"),
    get_response(Path, Req, Conf).
    %Req:cleanup(),
    %{reply, Response, State};


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_callback(Conf, {module, {Mod, InitArgs}}) ->
    Instance = apply(Mod, new, [?SITENAME(Conf)|InitArgs]),
    fun(Req, Method, Args) -> Instance:respond(Req, Method, Args) end;
get_callback(Conf, {gen_server, {_Mod, Name, _InitArgs}}) ->
    get_callback(Conf, {gen_server, Name});
get_callback(Conf, {gen_server, Name}) ->
    fun(Req, Method, Args) -> 
            gen_server:call(?COMPONENT(Conf, Name), 
                            {respond, Req, Method, Args}) 
    end.

get_response([], Req, Conf) ->
    get_response([""], Req, Conf);
get_response([Top|Rest], Req, Conf) ->
    %case ets:lookup(State#state.components, Top) of
    case ?GVD(Top, ?GVD(components, Conf, []), none) of
        none -> Req:not_found();
        CallbackConf -> 
            Callback = get_callback(Conf, CallbackConf),
            run_responders(Callback, Rest, Req, Conf)
    end.

run_responders(Callback, Args, Req, Conf) ->
    case catch Callback(Req, Req:get(method), Args) of
        {response, Response} -> 
            Response;
        {wrap, TemplateName, Data} ->
            wrap_template(TemplateName, Data, Req, Conf);
                %State, Req, TemplateName, Data);
        {child, NewCallback, NewArgs} -> 
            run_responders(NewCallback, NewArgs, Req, Conf);
        {error, Error} ->
            display_error(Req, "Error: ~p~n", [Error]);
        {'EXIT', Error} ->
            display_error(Req, "Error: ~p~n", [Error]);
        not_found ->
            Req:not_found();
        Other ->
            display_error(Req, "Unknown component response: ~p~n", [Other])
    end.

wrap_template(TemplateName, Data, Req, Conf) ->
    %case proplists:get_value(TemplateName, State#state.templates) of
    case ?GVD(TemplateName, ?GVD(templates, Conf, []), undefined) of
        undefined ->
            display_error(Req, "Site template not found: ~p~n", [TemplateName]);
        {Type, reload, Filename} ->
            run_wrap_template(Type, {reload, TemplateName}, Filename, Data, Req, Conf);
        {Type, cache, Filename} ->
            run_wrap_template(Type, TemplateName, Filename, Data, Req, Conf);
        Other ->
            display_error(Req, "Unknown template config: ~p~n", [Other])
    end.


run_wrap_template(Type, Template, Filename, Data, Req, Conf) ->

    TemplateName = case Template of
                       {reload, Name} -> Name;
                       JustName -> JustName
                   end,

    Callback = fun(Name) when Name == TemplateName -> 
                       {file, Filename};
                  (Other) -> 
                       ?DBG({wrong_template, Other, isnt, TemplateName}),
                       not_found
               end,

    case gen_server:call(?CONFNAME(Conf, "evotemplate"),
                         {run, TemplateName, 
                          Data, [], Callback}) of
        {ok, Final} -> Req:ok({Type, Final});
        {error, Error} -> display_error(Req, "Template error: ~p~n", [Error]);
        Other -> display_error(Req, "Unknown template response: ~p~n", [Other])
    end.


display_error(Req, Template, Content) ->
    Req:ok({"text/plain", lists:flatten(io_lib:format(Template, Content))}).
