%%%-------------------------------------------------------------------
%%% File    : evosite_sup.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosite_sup).

-behaviour(supervisor).

-include("evo.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_PORT, 80).

%%====================================================================
%% API functions
%%====================================================================
start_link(SiteSpec) ->
    SupName = ?CONFNAME(SiteSpec, "sup"),
    supervisor:start_link({local, SupName}, ?MODULE, [SiteSpec]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([Conf]) ->

    ets:new(?CONFNAME(Conf, "componentPaths"), [public, bag, named_table]),

    Bits = lists:concat(
             lists:map(
               fun (B) -> start(B, Conf) end,
               [mochiweb, cometd, magicdb, evotemplate, components])),

    {ok,{{one_for_one,0,60}, Bits}}.


%%====================================================================
%% Internal functions
%%====================================================================

start(mochiweb, Conf) ->
    MochiName = ?CONFNAME(Conf, "mochiweb"),
    Port = ?GVD(port, Conf, ?DEFAULT_PORT),
    [{MochiName, {mochiweb_http, start,
                  [[{name, MochiName},
                    {port, Port}, 
                    {loop, make_loop(Conf)}]]},
      permanent,2000,worker,[mochiweb_socket_server]}];

start(cometd, Conf) ->
    application:load(cometd),
    CometdName = ?CONFNAME(Conf, "cometd"),
    [{CometdName, {cometd_sup, start_link, []},
      permanent, 2000, supervisor, [cometd_sup]}];

start(magicdb, Conf) ->
    case proplists:get_value(dbinfo, Conf) of
        {DB, User, Pass} ->
            DSN = lists:flatten(io_lib:format("DSN=~s;UID=~s;PWD=~s", [DB, User, Pass])),
            MagicName = ?CONFNAME(Conf, "magicdb"),
            [{MagicName, {magicdb, start_link, [{dsn, DSN}, {local, MagicName}]},
              permanent,2000,worker,[magicdb]}];
        undefined ->
            [];
        Other ->
            ?DBG({invalid_dbinfo, Other}),
            []              
    end;

start(evotemplate, Conf) ->
    TemplateServerName = ?CONFNAME(Conf, "evotemplate"),
    [{TemplateServerName, 
      {evotemplate, start_link, [TemplateServerName]},
      permanent,5000,worker,[evotemplate]}];

start(components, Conf) ->
    lists:concat(
      lists:map(
        fun({Path, {Type, Args}}) -> init_component(Path, Type, Args, Conf) end,
        proplists:get_value(components, Conf, []))).

init_component(Path, gen_server, {Mod, Name, InitArgs}, Conf) ->
    CompName = ?COMPONENT(Conf, Name),
    CompPathTable = ?CONFNAME(Conf, "componentPaths"),
    ets:insert(CompPathTable, {CompName, Path}),
    [{CompName, {Mod, start_link, [Conf, Name, InitArgs]},
      permanent,2000,worker,[Mod]}];
init_component(Path, gen_server, Name, Conf) -> 
    CompPathTable = ?CONFNAME(Conf, "componentPaths"),
    ets:insert(CompPathTable, {Name, Path}),
    [];
init_component(_Path, module, {_Mod, _InitArgs}, _Conf) ->  [].

make_loop(Conf) ->
    fun(Req) -> evosite:respond(Req, Conf) end.
