-module(evo_entry).

-export([start/0]).

start() ->
    application:start(evo).
