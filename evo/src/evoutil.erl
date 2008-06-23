-module(evoutil).

-export([concat_atoms/1]).

concat_atoms(Bits) ->
    concat_atoms(Bits, []).

concat_atoms([], Acc) -> 
    list_to_atom(lists:flatten(lists:reverse(Acc)));
concat_atoms([Atom|Rest], Acc) when is_atom(Atom) ->
    concat_atoms(Rest, [atom_to_list(Atom)|Acc]);
concat_atoms([String|Rest], Acc) ->
    concat_atoms(Rest, [String|Acc]).
