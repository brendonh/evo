-module(evofilter).

-export([test/0, safe_match/2]).

-define(DBG(T), io:format("~p: ~p~n", [self(), T])).

op(X) when is_binary(X) -> op(binary_to_list(X));
op(">") -> fun erlang:'>'/2;
op("<") -> fun erlang:'<'/2;
op(">=") -> fun erlang:'>='/2;
op("<=") -> fun erlang:'=<'/2;
op("==") -> fun erlang:'=='/2;
op("in") -> fun lists:member/2;

op("contains") ->
    fun(X,Y) -> 
            string:str(binary_to_list(X), binary_to_list(Y)) =/= 0 end.

apply_op(Op, Args) ->
    try apply(op(Op), Args) of
        X -> {ok, X}
    catch
        error:{badarity, _} -> {error, op_badarity, Op, Args};
        error:function_clause -> {error, op_not_found, Op}
    end.

map_tuples(Obj, Fun, Tuples) ->
    try Fun(
      fun(Tuple) ->
              % Don't smother errors. Fail on the first one.
              {ok, Val} = evaluate_tuple(Obj, Tuple),
              Val
      end,
      Tuples) of
        Result -> {ok, Result}
    catch
        % Badmatch from the fun above (hopefully!)
        error:{badmatch, RealError} -> {error, RealError}
    end.   

evaluate_tuple(Obj, {Anything, Tuple}) when is_binary(Anything) ->
    evaluate_tuple(Obj, {binary_to_list(Anything), Tuple});

evaluate_tuple(Obj, {"not", Tuple}) ->
    case evaluate_tuple(Obj, Tuple) of
        {ok, Val} -> {ok, not(Val)};
        Error -> Error
    end;

evaluate_tuple(Obj, {"and", Tuples}) ->
    map_tuples(Obj, fun lists:all/2, Tuples);

evaluate_tuple(Obj, {"or", Tuples}) ->
    map_tuples(Obj, fun lists:any/2, Tuples);

evaluate_tuple(Obj, {RawFieldName, Op, Args}) when is_list(Args) ->
    FieldName = case is_binary(RawFieldName) of
                    true -> RawFieldName;
                    false -> list_to_binary(RawFieldName)
                end,
    case lists:keysearch(FieldName, 1, Obj) of
        {value, {_, Value}} ->
            apply_op(Op, [Value|Args]);
        false ->
            {error, field_not_found}
    end;

evaluate_tuple(Obj, {FieldName, Op, Args}) ->
    evaluate_tuple(Obj, {FieldName, Op, [Args]});

evaluate_tuple(_Obj, true) -> {ok, true};
evaluate_tuple(_Obj, false) -> {ok, false}.


safe_match({struct, Obj}, Filter) ->
    try evaluate_tuple(Obj, Filter) of
        {ok, Result} -> 
            Result;
        {error, Error} ->
            ?DBG({filter_fail, Obj, Filter, Error}),
            error
    catch
        Type:Error -> 
            ?DBG({filter_crash, Obj, Filter, Type, Error}),
            error
    end.

o(Name, Expected, Expected) ->
    io:format("[OK]   ~p: ~p~n", [Name, Expected]);
o(Name, Result, Expected) ->
    io:format("[FAIL] ~p: ~p (expected ~p)~n", [Name, Result, Expected]).

test() ->
    Obj = {struct, [{<<"foo">>, 5},{<<"bar">>, <<"Hello">>}]},
    One = {"foo", ">", [6]},
    Two = {"bar", "==", [<<"Hello">>]},
    o(crash1, safe_match(Obj, ["garbage"]), error),
    o(one, safe_match(Obj, One), false),
    o(two, safe_match(Obj, Two), true),
    o('and', safe_match(
               Obj, {"and", [One, Two]}), 
      false),
    o('or', safe_match(
               Obj, {"or", [One, Two]}), 
      true),
    o('nor', safe_match(
               Obj, {"not", {"or", [One, Two]}}),
      false),
    o('in', safe_match(
               Obj, {"foo", "in", [[1,2,3,5]]}),
      true),
    o('nin', safe_match(
               Obj, {"not", {"foo", "in", [[1,2,3,5]]}}),
      false),
    o('contains', safe_match(
                     Obj, {"bar", "contains", [<<"ell">>]}),
      true),
    o(true, safe_match(Obj, true),
      true),
    o(false, safe_match(Obj, false),
      false).
