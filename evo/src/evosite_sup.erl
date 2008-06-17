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

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link({SiteName, _Port, _StaticRoot, _DBInfo}=SiteSpec) ->
    SupName = concat_atoms(["evosite_", SiteName, "_sup"]),
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
init([{SiteName, Port, {DB, User, Pass}, Components}]) ->

    DSN = lists:flatten(io_lib:format("DSN=~s;UID=~s;PWD=~s", [DB, User, Pass])),

    EvoName = concat_atoms(["evosite_", SiteName]),
    MochiName = concat_atoms([EvoName, "_mochiweb"]),
    MagicName = concat_atoms([EvoName, "_magicdb"]),

    Mochiweb = {MochiName, {mochiweb_http, start,
                            [[{name, MochiName},
                              {port, Port}, 
                              {loop, make_loop(EvoName)}]]},
                permanent,2000,worker,[mochiweb_socket_server]},

    MagicDB = {MagicName, {magicdb, start_link, [{dsn, DSN}, {local, MagicName}]},
               permanent,2000,worker,[magicdb]},

    EvoSite = {EvoName, {evosite, start_link, [EvoName, Components]},
               permanent,2000,worker,[evosite]},

    % Allow ten crashes per minute
    {ok,{{one_for_one,10,60}, [Mochiweb, MagicDB, EvoSite]}}.

%%====================================================================
%% Internal functions
%%====================================================================

make_loop(EvoName) ->
    fun(Req) -> gen_server:call(EvoName, {respond, Req}) end.
             
concat_atoms(Bits) ->
    concat_atoms(Bits, []).

concat_atoms([], Acc) -> 
    list_to_atom(lists:flatten(lists:reverse(Acc)));
concat_atoms([Atom|Rest], Acc) when is_atom(Atom) ->
    concat_atoms(Rest, [atom_to_list(Atom)|Acc]);
concat_atoms([String|Rest], Acc) ->
    concat_atoms(Rest, [String|Acc]).
