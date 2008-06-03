-module(evotest).

-export([test/0, evo_data_woo/0, evo_render_foo/1]).

test() ->
    io:format("~s~n", [evo:run("test.xml", ?MODULE)]).


evo_data_woo() ->
    "Hi there".

evo_render_foo(Data) ->
    [Data, " ", Data].


%evo_dataFunc_foreach(Data) ->
    
