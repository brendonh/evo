%%%-------------------------------------------------------------------
%%% File    : evosetup.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : One-time setup for e.g. mnesia
%%%
%%% Created : 31 Jul 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(evosetup).

-include("evo.hrl").

-export([setup/0]).


setup() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    ?DBG(mnesia:create_table(session, [{attributes, record_info(fields, session)},
                                       {disc_copies, [node()]}])),
    ?DBG(mnesia:create_table(comet_proc, [{attributes, record_info(fields, comet_proc)}])),
    mnesia:info().
