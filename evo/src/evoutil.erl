-module(evoutil).

-export([concat_atoms/1, md5_hex/1]).

concat_atoms(Bits) ->
    list_to_atom(lists:concat(Bits)).

md5_hex(S) -> lists:flatten([io_lib:format("~.16b", [N]) 
                             || <<N:4>> <= erlang:md5(S)]).
