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
    R = #testRec{one=1, two="hello", three=apple},
    case evo:run_file("test.xml", testRec_to_proplist(R)) of
        {error, Error} ->
            io:format("Broke: ~p~n", [Error]);
        {ok, Result} ->
            io:format("~s~n", [Result])
    end.
