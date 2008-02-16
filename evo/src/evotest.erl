-module(evotest).

-export([test/0]).

test() ->
    io:format("Got: ~s~n", [evo:run("test.xml", ?MODULE)]).
