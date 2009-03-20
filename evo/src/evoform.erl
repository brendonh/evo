-module(evoform).

-include("evo.hrl").

-export([form_from_colspec/1, render_field/3, parse_form/2]).



form_from_colspec(ColSpec) ->
    Fields = [field_from_column(Col) || Col <- ColSpec],
    #form{fields=Fields}.

field_from_column({NameStr, Type}) ->
    {Out, In} = filters_for_column_type(Type),
    Render = render_for_column_type(Type),
    Name = list_to_atom(NameStr),
    {Name, #field{localName=Name,
                     in_to_out=Out,
                     out_to_in=In,
                     render=Render}}.


filters_for_column_type(sql_integer) ->
    {fun integer_to_list/1, fun list_to_integer/1};

filters_for_column_type({sql_float, _}) ->
    {fun(F) -> lists:flatten(io_lib:format("~.4f", [F])) end,
     fun(S) -> case lists:member($., S) of
                   true -> list_to_float(S);
                   false -> float(list_to_integer(S))
               end end};

filters_for_column_type(sql_real) -> 
    filters_for_column_type({sql_float, 4});

filters_for_column_type(Other) ->
    %?DBG({other, Other}),
    {undefined, undefined}.


render_for_column_type(_) ->
    fun(Field, Values) ->
            Name = Field#field.localName,
            Value = proplists:get_value(Name, Values, ""),
            OutValue = get_out_value(Value, Field#field.in_to_out),
            evo:tag(input, 
                    [{type, "text"},
                     {name, atom_to_list(Name)},
                     {value, evo:escape_entities(OutValue)}], [])
    end.


get_out_value(null, _) -> "";
get_out_value(Val, undefined) -> Val;
get_out_value(Val, Func) -> Func(Val).


render_field(Form, Name, Values) ->
    Field = proplists:get_value(Name, Form#form.fields),
    (Field#field.render)(Field, Values).


parse_form(Form, OutValues) ->
    AllInValues = [extract_value(Form, Field, OutValues) 
                   || {_Name, Field} <- Form#form.fields],
    [X || X <- AllInValues, X /= none].


extract_value(Form, Field, OutValues) ->
    Func = Field#field.out_to_in,
    Name = atom_to_list(Field#field.localName),
    OutValue = proplists:get_value(Name, OutValues, none),

    case OutValue of
        none -> 
            %?DBG({Field#field.localName, not_given}),
            none;
        _ ->
            InValue = get_in_value(OutValue, Func, Field#field.null_if_empty),
            %?DBG({Field#field.localName, InValue}),
            {Field#field.localName, InValue}
    end.


get_in_value("", Func, true) -> null;
get_in_value(Val, undefined, _) -> Val;
get_in_value(Val, Func, _) -> Func(Val).
                          
    
