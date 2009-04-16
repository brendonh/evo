-module(evoutil).

-export([concat_atoms/1]).

concat_atoms(Bits) ->
    list_to_atom(lists:concat(Bits)).
