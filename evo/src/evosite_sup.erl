%%%-------------------------------------------------------------------
%%% File    : evosite_sup.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosite_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_PORT, 80).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link({SiteName, _Conf}=SiteSpec) ->
    SupName = evoutil:concat_atoms(["evosite_", SiteName, "_sup"]),
    supervisor:start_link({local, SupName}, ?MODULE, [SiteSpec]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([{SiteName, SiteConf}]) ->

    EvoName = evoutil:concat_atoms(["evosite_", SiteName]),

    ets:new(evoutil:concat_atoms([EvoName, "_componentPaths"]), [public, bag, named_table]),

    Bits = lists:concat(
             lists:map(
               fun (B) -> start(B, EvoName, SiteConf) end,
               [mochiweb, magicdb, evosite, evotemplate, components])),

    {ok,{{one_for_one,10,60}, Bits}}.

%%====================================================================
%% Internal functions
%%====================================================================

start(mochiweb, EvoName, SiteConf) ->
    MochiName = evoutil:concat_atoms([EvoName, "_mochiweb"]),
    [{MochiName, {mochiweb_http, start,
                  [[{name, MochiName},
                    {port, proplists:get_value(port, SiteConf, ?DEFAULT_PORT)}, 
                    {loop, make_loop(EvoName)}]]},
      permanent,2000,worker,[mochiweb_socket_server]}];

start(magicdb, EvoName, SiteConf) ->
    case proplists:get_value(dbinfo, SiteConf) of
        {DB, User, Pass} ->
            DSN = lists:flatten(io_lib:format("DSN=~s;UID=~s;PWD=~s", [DB, User, Pass])),
            MagicName = evoutil:concat_atoms([EvoName, "_magicdb"]),
            [{MagicName, {magicdb, start_link, [{dsn, DSN}, {local, MagicName}]},
              permanent,2000,worker,[magicdb]}];
        undefined ->
            [];
        Other ->
            cr:dbg({invalid_dbinfo, Other}),
            []              
    end;

start(evosite, EvoName, SiteConf) ->
    [{EvoName, {evosite, start_link, [EvoName, SiteConf]},
      permanent,2000,worker,[evosite]}];

start(evotemplate, EvoName, _SiteConf) ->
    TemplateServerName = evoutil:concat_atoms([EvoName, "_evotemplate"]),
    [{TemplateServerName, 
      {evotemplate, start_link, [TemplateServerName]},
      permanent,5000,worker,[evotemplate]}];

start(components, EvoName, SiteConf) ->
    lists:concat(
      lists:map(
        fun({Path, Type, Args}) -> init_component(Path, EvoName, Type, Args) end,
        proplists:get_value(components, SiteConf, []))).

init_component(Path, EvoName, gen_server, {Mod, Name, InitArgs}) ->
    CompName = evoutil:concat_atoms([EvoName, "_component_", Name]),
    CompPathTable = evoutil:concat_atoms([EvoName, "_componentPaths"]),
    ets:insert(CompPathTable, {CompName, Path}),
    [{CompName, {Mod, start_link, [EvoName, Name, InitArgs]},
      permanent,2000,worker,[Mod]}];
init_component(Path, EvoName, gen_server, Name) -> 
    CompPathTable = evoutil:concat_atoms([EvoName, "_componentPaths"]),
    ets:insert(CompPathTable, {Name, Path}),
    [];
init_component(_Path, _EvoName, module, {_Mod, _InitArgs}) ->  [].

make_loop(EvoName) ->
    fun(Req) -> gen_server:call(EvoName, {respond, Req}) end.
