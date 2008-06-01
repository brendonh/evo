-module(evo).

-import (utf8, [from_binary/1, to_binary/1]).
-export([run/2]).

run(Filename, Module) ->
    Self = self(),
    spawn_link(fun() -> really_run(Filename, Module, Self) end),
    receive
        {result, R} ->
            R;
        Other ->
            io:format("WTF? ~p~n", [Other]),
            none
    after 5000 ->
            io:format("Too slow!~n"),
            none
    end.

really_run(Filename, Module, From) ->
    put(callback_module, Module),

    {ok, UTF8} = file:read_file(Filename),
    {ok, UTF32} = from_binary(UTF8),
    Self = self(),
    spawn(fun() -> evoxml:parse(UTF32, Self) end),
    watch_parsing(0),
    From ! {result, ""}.

watch_parsing(Indent) ->
    receive
        {tag_start, Name} ->
            io:format("~s[~s]~n", [string:chars(32, Indent), lists:flatten(Name)]),
            watch_parsing(Indent+2);
        {attr, {Name, Value}} ->
            {ok, BinVal} = to_binary(Value),
            io:format("~s|~s => ~s~n", [string:chars(32, Indent), Name, BinVal]),
            watch_parsing(Indent);
        {text, Text} ->
            {ok, BinVal} = to_binary(Text),
            io:format("~s\"~s\"~n", [string:chars(32, Indent), BinVal]),
            watch_parsing(Indent);
        {tag_end, _} ->
            watch_parsing(Indent-2);
        done ->
            io:format("Done.~n"),
            done
    end.
