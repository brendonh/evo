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

    Bits = lists:concat(
             lists:map(
               fun (B) -> start(B, Conf) end,
               [mochiweb, pgsql, amqp, evotemplate])),

    {ok,{{one_for_one,2,60}, Bits}}.


%%====================================================================
%% Internal functions
%%====================================================================

start(mochiweb, Conf) ->
    MochiName = ?CONFNAME(Conf, "mochiweb"),
    Port = ?GVD(port, Conf, ?DEFAULT_PORT),
    ?DBG({evosite, ?SITENAME(Conf), Port}),
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



start(pgsql, Conf) ->
    case ?GV(pgsql, Conf) of
        undefined -> [];
        PgStartup ->
            PgName = ?CONFNAME(Conf, "pgsql"),
            [{PgName, {evoepgsql, start_link, [PgName|PgStartup]},
             permanent,5000,worker,[evoepgsql]}]
    end;

start(amqp, Conf) ->
    case ?GV(amqp, Conf) of
        undefined -> [];
        AMQPStartup ->
            AMQPName = ?CONFNAME(Conf, "amqp"),
            [{AMQPName, {evoamqp, start_link, [AMQPName|AMQPStartup]},
              permanent,2000,worker,[evoamqp]}]
    end;


start(evotemplate, Conf) ->
    TemplateServerName = ?CONFNAME(Conf, "evotemplate"),
    [{TemplateServerName, 
      {evotemplate, start_link, [TemplateServerName]},
      permanent,5000,worker,[evotemplate]}].


make_loop(Conf) ->
    fun(Req) -> evosite:respond(Req, Conf) end.


