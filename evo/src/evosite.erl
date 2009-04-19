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
        {Module, Args} -> 
            ?DBG({module, Module, args, Args}),
            run_last_responder(Module, Rest, Req, Conf, Args)
    end.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

run_always([], _Req, Conf) ->
    {continue, Conf};
run_always([{Module, Args}|Rest], Req, Conf) ->
    Result = run_responder(Module, always, Req, Conf, Args),
    case Result of
        {update, NewConf} -> run_always(Rest, Req, NewConf);
        Other -> Other
    end.

run_last_responder(Module, PathBits, Req, Conf, Args) ->
    case run_responder(Module, PathBits, Req, Conf, Args) of
        {update, _} ->
            display_error(Req, "Responder didn't return a body.", []);
        Other -> Other
    end.    


run_responder(Module, PathBits, Req, Conf, Args) ->
    case catch Module:respond(Req, Req:get(method), PathBits, Conf, Args) of
        {response, Response} -> 
            Response;
        {wrap, TemplateName, Data} ->
            wrap_template(TemplateName, Data, Req, Conf);
        {child, {NewModule, NewArgs}, NewPathBits, NewConf} -> 
            run_responder(NewModule, NewPathBits, Req, NewConf, NewArgs);
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
