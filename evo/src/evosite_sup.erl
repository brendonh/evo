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
-export([start_link/1, start_epgsql/5]).

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

    Bits = lists:concat(
             lists:map(
               fun (B) -> start(B, Conf) end,
               [mochiweb, pgsql, evotemplate])),

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


start(pgsql, Conf) ->
    case ?GV(pgsql, Conf) of
        undefined -> [];
        PgStartup ->
            PgName = ?CONFNAME(Conf, "pgsql"),
            [{PgName, {?MODULE, start_epgsql, [PgName|PgStartup]},
              permanent,5000,worker,[pgsql_connection]}]
    end;


start(evotemplate, Conf) ->
    TemplateServerName = ?CONFNAME(Conf, "evotemplate"),
    [{TemplateServerName, 
      {evotemplate, start_link, [TemplateServerName]},
      permanent,5000,worker,[evotemplate]}].


make_loop(Conf) ->
    fun(Req) -> evosite:respond(Req, Conf) end.



start_epgsql(Name, Host, User, Pass, Opts) ->
    {ok, C} = gen_fsm:start_link({local, Name}, pgsql_connection, [], []),
    pgsql_connection:connect(C, Host, User, Pass, Opts),
    {ok, C}.
