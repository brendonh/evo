-module(evostatic, [DocRoot]).

-export([respond/2]).

respond(Req, [PathBits]) ->
    {response, Req:serve_file(string:join(PathBits, "/"), DocRoot)}.
