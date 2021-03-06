-module(evoxml).

-export([parse/2]).

parse(XML, From) ->
    put(callback_module, From),
    XML2 = skip_whitespace(XML),
    XML3 = absorb_typeDecl(XML2),
    XML4 = absorb_docType(XML3),
    XML5 = parse_tag(XML4),
    case skip_whitespace(XML5) of
        [] -> From ! done;
        false -> erlang:error("No top-level tag found");
        Stuff -> erlang:error({"Junk after document end", Stuff})
    end.

absorb_typeDecl(XML) ->
    absorb_unhandledTag("<?xml", "?>", XML).

absorb_docType(XML) ->
    absorb_unhandledTag("<!", ">", XML).

absorb_unhandledTag(Start, End, XML) ->
    case expect(Start, XML) of
        {true, Rest} ->
            {Content, Etc} = absorb_until(End, Rest),
            get(callback_module) ! {unhandled_tag, {Start ++ Content}},
            Etc;
        false ->
            XML
    end.

parse_tag(XML) ->
    XML2 = skip_whitespace(XML),
    case XML2 of
        [$<|Rest] ->
            {Name, More} = absorb_name(Rest),
            get(callback_module) ! {tag_start, split_name(Name)},
            EvenMore = absorb_attrs(More),
            case skip_whitespace(EvenMore) of
                [$/,$>|Etc] ->
                    get(callback_module) ! {tag_end, Name},
                    Etc;
                [$>|Etc] -> 
                    tag_body(Etc, Name);
                Erk -> erlang:error({"Missing closing bracket", Erk})
            end;
        _ -> false
    end.

end_tag(XML, TagName) ->
    case expect("</", skip_whitespace(XML)) of
        false -> false;
        {true, XML2} ->
            case absorb_name(XML2) of
                false -> erlang:error("Broken closing tag");
                {TagName, [$>|XML3]} -> XML3;
                {Name, _} -> 
                    Msg = io_lib:format("Wrong closing tag, got '~s' instead of '~s'", [Name, TagName]),
                    erlang:error(lists:flatten(Msg))
            end
    end.
                     
tag_body(XML, TagName) ->
    XML2 = absorb_text(XML),
    case end_tag(XML2, TagName) of
        false -> 
            XML3 = parse_tag(XML2),
            tag_body(XML3, TagName);
        XML3 ->
            get(callback_module) ! {tag_end, TagName},
            XML3
    end.
 
absorb_text(XML) -> 
    {AfterSkip, PrefixWS} = skip_whitespace2(XML),
    absorb_text(AfterSkip, [], PrefixWS).

absorb_text([$<|_Rest]=XML, [], false) -> 
    XML;
absorb_text([$<|_Rest]=XML, Buffer, PWS) -> 
    {Buffer2, SWS} = skip_whitespace2(Buffer),
    get(callback_module) ! {text, lists:concat([PWS,
                                                lists:reverse(Buffer2), 
                                                SWS])},
    XML;
absorb_text([First|Rest], Buffer, PWS) -> 
    absorb_text(Rest, [First|Buffer], PWS);
absorb_text([], _Buffer, _) ->
    erlang:error("Hit EOF while reading text").

absorb_attrs(XML) ->
    XML2 = skip_whitespace(XML),
    case absorb_attr(XML2) of
        false -> XML2;
        XML3 -> absorb_attrs(XML3)
    end.
            

absorb_attr(XML) ->
    XML2 = skip_whitespace(XML),
    case absorb_name(XML2) of
        false -> false;
        {Name, XML3} -> 
            case XML3 of
                [$=|XML4] ->
                    {Value, XML5} = absorb_attrVal(XML4),
                    get(callback_module) ! {attr, {split_name(Name), Value}},
                    XML5;
                _ -> erlang:error({"Missing = after name", Name, XML3})
            end
    end.


split_name(Name) ->
    case string:tokens(Name, ":") of
        [Attr] -> {none, list_to_atom(Attr)};
        [NS, Attr] -> {list_to_atom(NS), list_to_atom(Attr)};
        _ -> erlang:error({"Can't parse attribute namespace", Name})
    end.
            

absorb_name([First|Rest]) ->
    case is_name_start(First) of
        true -> absorb_name(Rest, [First]);
        false -> false
    end;
absorb_name([]) ->
    false.

absorb_name([First|Rest]=XML, Buffer) ->
    case is_name_char(First) of
        true -> absorb_name(Rest, [First|Buffer]);
        false -> 
            case Buffer of
                [] -> false;
                _ -> {lists:reverse(Buffer), XML}
            end
    end;
absorb_name([], Buffer) ->
    {lists:reverse(Buffer), []}.


absorb_attrVal([$"|Rest]) -> absorb_val(Rest, []);
absorb_attrVal(_) -> erlang:error("Missing starting quotation mark").

absorb_val([$"|Rest], Buffer) -> {lists:reverse(Buffer), Rest};
absorb_val([First|Rest], Buffer) -> absorb_val(Rest, [First|Buffer]);
absorb_val([], _) -> erlang:error("Missing ending quotation mark").

absorb_until(End, XML) ->
    absorb_until(End, XML, []).

absorb_until([First|_]=End, [First|Rest]=XML, Buffer) ->
    case expect(End, XML) of
        {true, Etc} ->
            {lists:reverse(Buffer) ++ End, Etc};
        false ->
            absorb_until(End, Rest, [First|Buffer])
    end;
absorb_until(End, [First|XML], Buffer) ->
    absorb_until(End, XML, [First|Buffer]);
absorb_until(End, [], _Buffer) ->
    erlang:error({"Missing end tag", End}).


expect([First|Rest], [First|MoreXML]) ->
    expect(Rest, MoreXML);
expect([], RestOfXML) ->
    {true, RestOfXML};
expect(_, _RestOfXML) ->
    false.
    
is_name_start($:) -> true;
is_name_start($_) -> true;
is_name_start(Char) when Char >= $a, Char =< $z -> true;
is_name_start(Char) when Char >= $A, Char =< $Z -> true;
is_name_start(Char) when Char >= 16#C0, Char =< 16#D6 -> true;
is_name_start(Char) when Char >= 16#D8, Char =< 16#F6 -> true;
is_name_start(Char) when Char >= 16#F8, Char =< 16#2FF -> true;
is_name_start(Char) when Char >= 16#370, Char =< 16#37D -> true;
is_name_start(Char) when Char >= 16#37F, Char =< 16#1FFF -> true;
is_name_start(Char) when Char >= 16#200C, Char =< 16#200D -> true;
is_name_start(Char) when Char >= 16#2070, Char =< 16#218F -> true;
is_name_start(Char) when Char >= 16#2C00, Char =< 16#2FEF -> true;
is_name_start(Char) when Char >= 16#3001, Char =< 16#D7FF -> true;
is_name_start(Char) when Char >= 16#F900, Char =< 16#FDCF -> true;
is_name_start(Char) when Char >= 16#FDF0, Char =< 16#FFFD -> true;
is_name_start(Char) when Char >= 16#10000, Char =< 16#EFFFF -> true;
is_name_start(_) -> false.

is_name_char($-) -> true;
is_name_char($.) -> true;
is_name_char(Char) when Char >= $0, Char =< $9 -> true;
is_name_char(16#B7) -> true;
is_name_char(Char) when Char >= 16#0300, Char =< 16#036F -> true;
is_name_char(Char) when Char >= 16#203F, Char =< 16#2040 -> true;
is_name_char(Char) ->
    is_name_start(Char).

skip_whitespace([32|Rest]) -> skip_whitespace(Rest);
skip_whitespace([$\r|Rest]) -> skip_whitespace(Rest);
skip_whitespace([$\n|Rest]) -> skip_whitespace(Rest);
skip_whitespace([$\t|Rest]) -> skip_whitespace(Rest);
skip_whitespace(Rest) -> Rest.


skip_whitespace2(Text) -> skip_whitespace2(Text, []).

skip_whitespace2([32|Rest], WS) -> skip_whitespace2(Rest, [32|WS]);
skip_whitespace2([$\r|Rest], WS) -> skip_whitespace2(Rest, [$\r|WS]);
skip_whitespace2([$\n|Rest], WS) -> skip_whitespace2(Rest, [$\n|WS]);
skip_whitespace2([$\t|Rest], WS) -> skip_whitespace2(Rest, [$\t|WS]);
skip_whitespace2(Rest, WS) -> {Rest, lists:reverse(WS)}.
