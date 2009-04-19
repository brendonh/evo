%%%-------------------------------------------------------------------
%%% File    : evostatic.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Serve static files
%%%
%%% Created : 19 Apr 2009 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evostatic).

-include("evo.hrl").

-export([respond/5, nav/2]).

nav(_Conf, _Args) -> [].
    

respond(Req, 'GET', [], _Conf, _Args) ->
    {response, Req:not_found()};
respond(Req, 'GET', PathBits, _Conf, [DocRoot]) ->
    {response, Req:serve_file(string:join(PathBits, "/"), DocRoot)}.
