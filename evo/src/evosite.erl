%%%-------------------------------------------------------------------
%%% File    : evosite.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evosite).

-include("evo.hrl").

%% API
-export([respond/2, get_response/3]).

respond(Req, Conf) ->
    case run_always(?GVD(always, Conf, []), Req, Conf) of
        {continue, Update} -> 
            Args = string:tokens(Req:get(path), "/"),
            Conf2 = Update ++ Conf,
            get_response(Args, Req, Conf2);
        Other -> Other
    end.


get_response([], Req, Conf) ->
    get_response([""], Req, Conf);
get_response([Top|Rest], Req, Conf) ->
    case ?GVD(Top, ?GVD(components, Conf, []), none) of
        none -> Req:not_found();
        CallbackConf -> 
            Callback = get_callback(Conf, CallbackConf),
            run_last_responder(Callback, Rest, Req, Conf)
    end.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

run_always([], _Req, Conf) ->
    {continue, Conf};
run_always([Always|Rest], Req, Conf) ->
    Callback = get_callback(Conf, Always),
    Result = run_responder(Callback, [], Req, Conf),
    case Result of
        {update, NewConf} -> run_always(Rest, Req, NewConf);
        Other -> Other
    end.


get_callback(Conf, {module, {Mod, InitArgs}}) ->
    Instance = apply(Mod, new, InitArgs),
    fun(Req, Method, Args) -> Instance:respond(Req, Method, Args, Conf) end;
get_callback(Conf, {gen_server, {_Mod, Name, _InitArgs}}) ->
    get_callback(Conf, {gen_server, Name});
get_callback(Conf, {gen_server, Name}) ->
    fun(Req, Method, Args) -> 
            gen_server:call(?COMPONENT(Conf, Name), 
                            {respond, Req, Method, Args}) 
    end.


run_last_responder(Callback, Args, Req, Conf) ->
    case run_responder(Callback, Args, Req, Conf) of
        {update, _} ->
            display_error(Req, "Responder didn't return a body.", []);
        Other -> Other
    end.    


run_responder(Callback, Args, Req, Conf) ->
    case catch Callback(Req, Req:get(method), Args) of
        {response, Response} -> 
            Response;
        {wrap, TemplateName, Data} ->
            wrap_template(TemplateName, Data, Req, Conf);
        {child, NewCallback, NewArgs, NewConf} -> 
            run_responder(NewCallback, NewArgs, Req, NewConf);
        {error, Error} ->
            display_error(Req, "Error: ~p~n", [Error]);
        {'EXIT', Error} ->
            display_error(Req, "Error: ~p~n", [Error]);
        not_found ->
            Req:not_found();
        {update, NewConf} ->
            {update, NewConf};
        Other ->
            display_error(Req, "Unknown component response: ~p~n", [Other])
    end.

wrap_template(TemplateName, Data, Req, Conf) ->
    case ?GVD(TemplateName, ?GVD(templates, Conf, []), undefined) of
        undefined ->
            display_error(Req, "Site template not found: ~p~n", [TemplateName]);
        {Type, reload, Filename} ->
            run_wrap_template(Type, {reload, TemplateName}, Filename, Data, Req, Conf);
        {Type, cache, Filename} ->
            run_wrap_template(Type, TemplateName, Filename, Data, Req, Conf);
        Other ->
            display_error(Req, "Unknown template config: ~p~n", [Other])
    end.


run_wrap_template(Type, Template, Filename, Data, Req, Conf) ->

    TemplateName = case Template of
                       {reload, Name} -> Name;
                       JustName -> JustName
                   end,

    Callback = fun(Name) when Name == TemplateName -> 
                       {file, Filename};
                  (Other) -> 
                       ?DBG({wrong_template, Other, isnt, TemplateName}),
                       not_found
               end,

    case gen_server:call(?CONFNAME(Conf, "evotemplate"),
                         {run, TemplateName, Data, [], Callback}) of
        {ok, Final} -> Req:ok({Type, ?GV(headers, Conf), Final});
        {error, Error} -> display_error(Req, "Template error: ~p~n", [Error]);
        Other -> display_error(Req, "Unknown template response: ~p~n", [Other])
    end.


display_error(Req, Template, Content) ->
    Req:ok({"text/plain", lists:flatten(io_lib:format(Template, Content))}).
