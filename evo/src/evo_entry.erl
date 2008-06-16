-module(evo_entry).

-export([start/0]).

start() ->

    case error_logger:add_report_handler(cr) of
        ok -> ok;
        Error -> io:format("OH NO: ~p~n", [Error])
    end,

    application:start(evo).
