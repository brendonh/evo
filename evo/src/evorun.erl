-module(evorun).

-export([run/0]).

-record(testRec, {
  one,
  two,
  three
}).

testRec_to_proplist(TR) ->
    Bits = tuple_to_list(TR),
    [testRec|Fields] = Bits,
    lists:zip(record_info(fields, testRec), Fields).

run() ->
    Data = [{title, "Welcome to Nai's Page"},
            {items, ["Apple", "Orange", "Pear"]}],
    case evo:run_file("test.xml", Data) of
        {error, Error} ->
            io:format("Broke: ~p~n", [Error]);
        {ok, Result} ->
            io:format("~s~n", [Result])
    end.
