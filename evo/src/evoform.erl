-module(evoform).

-include("evo.hrl").

-export([form_from_colspec/1, render_field/3]).



form_from_colspec(ColSpec) ->
    Fields = [field_from_column(Col) || Col <- ColSpec],
    %cr:dbg({fields, Fields}),
    #evoform{fields=Fields}.

field_from_column({NameStr, Type}) ->
    {Out, In} = filters_for_column(Type),
    Render = render_for_column(Type),
    Name = list_to_atom(NameStr),
    {Name, #evofield{localName=Name,
                     in_to_out=Out,
                     out_to_in=In,
                     render=Render}}.


filters_for_column(sql_integer) ->
    {fun integer_to_list/1, fun list_to_integer/1};
filters_for_column({sql_float, _}) ->
    {fun(F) -> lists:flatten(io_lib:format("~.4f", [F])) end,
     fun(S) -> case lists:member($., S) of
                   true -> list_to_float(S);
                   false -> float(list_to_integer(S))
               end end};
filters_for_column(Other) ->
    %cr:dbg({other, Other}),
    {undefined, undefined}.


render_for_column(_) ->
    fun(Field, Values) ->
            Name = Field#evofield.localName,
            Value = proplists:get_value(Name, Values, ""),
            OutValue = get_out_value(Value, Field#evofield.in_to_out),
            evo:tag(input, 
                    [{type, "text"},
                     {name, atom_to_list(Name)},
                     {value, OutValue}], "")
    end.


get_out_value(null, _) -> "";
get_out_value(Val, undefined) -> Val;
get_out_value(Val, Func) -> Func(Val).


render_field(Form, Name, Values) ->
    Field = proplists:get_value(Name, Form#evoform.fields),
    Render = Field#evofield.render,
    Render(Field, Values).
