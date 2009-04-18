-module(evostatic, [DocRoot]).

-export([respond/4]).

respond(Req, 'GET', [], _Conf) ->
    {response, Req:not_found()};
respond(Req, 'GET', PathBits, _Conf) ->
    {response, Req:serve_file(string:join(PathBits, "/"), DocRoot)}.
