-module(evostatic, [DocRoot]).

-export([respond/3]).

respond(Req, 'GET', PathBits) ->
    {response, Req:serve_file(string:join(PathBits, "/"), DocRoot)}.
