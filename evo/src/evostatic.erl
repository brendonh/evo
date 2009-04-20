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

    % We should be using serve_file here, but it seems to take a while
    % to notice changes -- perhaps a clock issue? Anyway, turn it on later.
    %{response, Req:serve_file(string:join(PathBits, "/"), DocRoot)}.

    File = filename:join([DocRoot|PathBits]),
    ContentType = mochiweb_util:guess_mime(File),
    {ok, Content} = file:read_file(File),
    {response, Req:ok({ContentType, Content})}.

