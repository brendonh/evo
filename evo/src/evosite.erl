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
-export([respond/2]).

respond(Req, Conf) ->
    Path = string:tokens(Req:get(path), "/"),
    get_response(Path, Req, Conf).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_callback(Conf, {module, {Mod, InitArgs}}) ->
    Instance = apply(Mod, new, [?SITENAME(Conf)|InitArgs]),
    fun(Req, Method, Args) -> Instance:respond(Req, Method, Args) end;
get_callback(Conf, {gen_server, {_Mod, Name, _InitArgs}}) ->
    get_callback(Conf, {gen_server, Name});
get_callback(Conf, {gen_server, Name}) ->
    fun(Req, Method, Args) -> 
            gen_server:call(?COMPONENT(Conf, Name), 
                            {respond, Req, Method, Args}) 
    end.

get_response([], Req, Conf) ->
    get_response([""], Req, Conf);
get_response([Top|Rest], Req, Conf) ->
    case ?GVD(Top, ?GVD(components, Conf, []), none) of
        none -> Req:not_found();
        CallbackConf -> 
            Callback = get_callback(Conf, CallbackConf),
            run_responders(Callback, Rest, Req, Conf)
    end.

run_responders(Callback, Args, Req, Conf) ->
    case catch Callback(Req, Req:get(method), Args) of
        {response, Response} -> 
            Response;
        {wrap, TemplateName, Data} ->
            wrap_template(TemplateName, Data, Req, Conf);
                %State, Req, TemplateName, Data);
        {child, NewCallback, NewArgs} -> 
            run_responders(NewCallback, NewArgs, Req, Conf);
        {error, Error} ->
            display_error(Req, "Error: ~p~n", [Error]);
        {'EXIT', Error} ->
            display_error(Req, "Error: ~p~n", [Error]);
        not_found ->
            Req:not_found();
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
                         {run, TemplateName, 
                          Data, [], Callback}) of
        {ok, Final} -> Req:ok({Type, Final});
        {error, Error} -> display_error(Req, "Template error: ~p~n", [Error]);
        Other -> display_error(Req, "Unknown template response: ~p~n", [Other])
    end.


display_error(Req, Template, Content) ->
    Req:ok({"text/plain", lists:flatten(io_lib:format(Template, Content))}).
