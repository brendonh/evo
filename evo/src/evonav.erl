%%%-------------------------------------------------------------------
%%% File    : evonav.erl
%%% Author  : Brendon Hogger <brendonh@lightblue>
%%% Description : Navigation bar
%%%
%%% Created : 19 Apr 2009 by Brendon Hogger <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evonav).

-include("evo.hrl").

-export([respond/5, nav/2]).

template(nav) -> {file, "templates/auto/nav.html"}.

respond(Req, 'GET', [], Conf, Args) ->
    Components = ?GV(components, Conf),

    StaticNav = ?GVD(static, Args, []),

    UserNav = case evosession:user_info(Conf) of
                   [] -> [];
                   _ -> ?GVD(user, Args, [])
               end,

    Nav = [{T, [X || X <- N, X /= undefined]} 
           || {_C, {M,A}} <- Components,
              {T, N} <- [M:nav(Conf,A)],
              N /= []] ++ StaticNav ++ UserNav,

    {ok, Content} = gen_server:call(?CONFNAME(Conf, "evotemplate"),
                                    {run, {reload, nav}, 
                                     [{nav, Nav}], [{pretty, raw}], 
                                     fun template/1}),

    {response, Req:ok({"text/html", Content})}.


nav(_Conf, _Args) -> none.
