-module(evostatic, [EvoName, DocRoot]).

-export([respond/3]).

respond(Req, 'GET', []) ->
    {response, Req:not_found()};
respond(Req, 'GET', PathBits) ->
    {response, Req:serve_file(string:join(PathBits, "/"), DocRoot)}.
