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
start_link({SiteName, SiteConf}=SiteSpec) ->
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
init([{SiteName, SiteConf}]) ->

    EvoName = concat_atoms(["evosite_", SiteName]),

    MochiName = concat_atoms([EvoName, "_mochiweb"]),

    Mochiweb = {MochiName, {mochiweb_http, start,
                            [[{name, MochiName},
                              {port, proplists:get_value(port, SiteConf, ?DEFAULT_PORT)}, 
                              {loop, make_loop(EvoName)}]]},
                permanent,2000,worker,[mochiweb_socket_server]},

    Bits = [Mochiweb],

    case proplists:get_value(dbinfo, SiteConf) of
        {DB, User, Pass} ->
            DSN = lists:flatten(io_lib:format("DSN=~s;UID=~s;PWD=~s", [DB, User, Pass])),
            MagicName = concat_atoms([EvoName, "_magicdb"]),
            MagicDB = {MagicName, {magicdb, start_link, [{dsn, DSN}, {local, MagicName}]},
                       permanent,2000,worker,[magicdb]},
            Bits2 = [MagicDB|Bits];
        undefined ->
            Bits2 = Bits;
        Other ->
            cr:dbg({invalid_dbinfo, Other}),
            Bits2 = Bits
    end,

    EvoSite = {EvoName, {evosite, start_link, [EvoName, SiteConf]},
               permanent,2000,worker,[evosite]},

    Bits3 = [EvoSite|Bits2],

    % Allow ten crashes per minute
    {ok,{{one_for_one,10,60}, Bits3}}.

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
