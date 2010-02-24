%%%-------------------------------------------------------------------
%%% File    : evoamqp.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : AMQP subscription wrangler
%%%
%%% Created : 30 Jul 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(evoamqp).

-behaviour(gen_server).

-include("evo.hrl").
%-include("rabbit.hrl").
%-include("rabbit_framing.hrl").
-include("amqp_client.hrl").

%% API
-export([start_link/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  connection,
  channel,
  ticket,
  exchange
}).


-define(BASIC_PROPERTIES, 
        #'P_basic'{content_type = <<"application/octet-stream">>,
                   delivery_mode = 1,
                   priority = 0}).

%%====================================================================
%% API
%%====================================================================

start_link(Name, Host, Port, User, Password, Realm, Exchange) ->
    gen_server:start_link({local, Name}, ?MODULE, 
                          [Host, Port, User, Password, Realm, Exchange], 
                          []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Port, User, Password, Realm, Exchange]) ->

    Params = #amqp_params{username=User, password=Password,
                          host=Host, port=Port},

    Connection = amqp_connection:start_network(Params),
    Channel = amqp_connection:open_channel(Connection),

    Access = #'access.request'{realm = Realm,
                               exclusive = false,
                               passive = true,
                               active = true,
                               write = true,
                               read = true},
    #'access.request_ok'{ticket = Ticket} =  
        amqp_channel:call(Channel, Access),


    ExchangeDeclare = #'exchange.declare'{ticket = Ticket,
                                          exchange = Exchange,
                                          type= <<"topic">>,
                                          passive = false,
                                          durable = false,
                                          auto_delete=false,
                                          internal = false,
                                          nowait = false,
                                          arguments = []},
    #'exchange.declare_ok'{} 
        = amqp_channel:call(Channel, ExchangeDeclare),    

    ?DBG({amqp_connected, Realm, Exchange}),

    {ok, #state{
       connection=Connection,
       channel=Channel,
       ticket=Ticket, 
       exchange=Exchange}}.


handle_call({listen, Queue}, {Pid, _}=From, State) ->
    handle_call({listen, Queue, Pid}, From, State);

handle_call({listen, Queue, RoutingKey, Pid}, _From, State) ->

    QueueDeclare = #'queue.declare'{ticket = State#state.ticket, 
                                    queue = Queue,
                                    passive = false, 
                                    durable = false,
                                    exclusive = false,
                                    auto_delete = true,
                                    nowait = false,
                                    arguments = []},
    #'queue.declare_ok'{queue = Queue,
                        message_count = _MessageCount,
                        consumer_count = _ConsumerCount}
        = amqp_channel:call(State#state.channel, QueueDeclare),

    QueueBind = #'queue.bind'{ticket = State#state.ticket,
                              queue = Queue,
                              exchange = State#state.exchange,
                              routing_key = RoutingKey,
                              nowait = false, 
                              arguments = []},
    #'queue.bind_ok'{} 
        = amqp_channel:call(State#state.channel, QueueBind),

    BasicConsume = #'basic.consume'{ticket = State#state.ticket,
                                    queue = Queue,
                                    no_local = false,
                                    no_ack = true,
                                    exclusive = false,
                                    nowait = false},
    #'basic.consume_ok'{consumer_tag = _ConsumerTag}
        = amqp_channel:subscribe(State#state.channel, BasicConsume, Pid),

    {reply, ok, State};

handle_call({unlisten, Queue}, _From, State) ->

    QueueDelete = #'queue.delete'{ticket = State#state.ticket,
                                  queue = Queue},
    #'queue.delete_ok'{}
        = amqp_channel:call(State#state.channel, QueueDelete),
    
    {reply, ok, State};

handle_call(Message, _From, State) ->
    ?DBG({unexpected_call, Message}),
    {reply, ok, State}.



handle_cast({send, Queue, Payload}, State) ->
    BasicPublish = #'basic.publish'{ticket = State#state.ticket,
                                    exchange = State#state.exchange,
                                    routing_key = Queue,
                                    mandatory = false,
                                    immediate = false},
    Content = #amqp_msg{payload=Payload},
    amqp_channel:cast(State#state.channel, BasicPublish, Content),
    {noreply, State};

handle_cast(Msg, State) ->
    ?DBG({unexpected_cast, Msg}),
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.



terminate(Reason, State) ->

    ?DBG({wtf, Reason}),

    amqp_channel:close(State#state.channel),

    amqp_connection:close(State#state.connection),

    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
