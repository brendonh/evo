-module(evoform).

-include("evo.hrl").

-export([form_from_colspec/1, render_field/3, parse_form/2]).



form_from_colspec(ColSpec) ->
    Fields = [field_from_column(Col) || Col <- ColSpec],
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
filters_for_column(sql_real) -> filters_for_column({sql_float, 4});
filters_for_column(Other) ->
    cr:dbg({other, Other}),
    {undefined, undefined}.


render_for_column(_) ->
    fun(Field, Values) ->
            Name = Field#evofield.localName,
            Value = proplists:get_value(Name, Values, ""),
            OutValue = get_out_value(Value, Field#evofield.in_to_out),
            evo:tag(input, 
                    [{type, "text"},
                     {name, atom_to_list(Name)},
                     {value, OutValue}], [])
    end.


get_out_value(null, _) -> "";
get_out_value(Val, undefined) -> Val;
get_out_value(Val, Func) -> Func(Val).


render_field(Form, Name, Values) ->
    Field = proplists:get_value(Name, Form#evoform.fields),
    (Field#evofield.render)(Field, Values).


parse_form(Form, OutValues) ->
    InValues = [extract_value(Form, Field, OutValues) || {_Name, Field} <- Form#evoform.fields],
    ok.

extract_value(Form, Field, OutValues) ->
    Func = Field#evofield.out_to_in,
    Name = atom_to_list(Field#evofield.localName),
    OutValue = proplists:get_value(Name, OutValues),

    InValue = get_in_value(OutValue, Func, Field#evofield.null_if_empty),

    cr:dbg({Field#evofield.localName, InValue}),
    OutValue.


get_in_value("", Func, true) -> null;
get_in_value(Val, undefined, _) -> Val;
get_in_value(Val, Func, _) -> Func(Val).
                          
    
