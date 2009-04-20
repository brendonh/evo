%%%-------------------------------------------------------------------
%%% File    : evodefault.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Default landing page
%%%
%%% Created : 20 Apr 2009 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evodefault).

-include("evo.hrl").

-export([nav/2, respond/5]).


respond(Req, 'GET', _Path, _Conf, [File, Title]) ->

    {ok, Content} = file:read_file(File),

    {wrap, site, [{content, Content}, {title, Title}]};

respond(Req, _, _, _, _) ->
    Req:not_found().


nav(_Conf, _Args) -> 
    {"Home", [{"Home", "/"}]}.
