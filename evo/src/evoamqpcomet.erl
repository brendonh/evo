%%%-------------------------------------------------------------------
%%% File    : evoamqpcomet.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : AMQP->Comet bridge
%%%
%%% Created : 31 Jul 2009 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(evoamqpcomet).

-include("evo.hrl").
-include("rabbit.hrl").
-include("rabbit_framing.hrl").


%% Evo component callbacks
-export([respond/5, nav/2, update_message_filter/3]).

-define(LONGPOLL_TIMEOUT, 10000).
-define(DEFUNCT_TIMEOUT, 11000).
-define(SESSION_TIMEOUT, 31000).

nav(_Conf, _Args) -> [].


-record(state, {
  listener=none,
  buffer=[],
  timer=none,
  listen_timer=none,
  queues=[],
  amqp,
  filter
}).



respond(Req, 'GET', ["ping"], Conf, _Args) ->
    spawn(fun() ->
                  AMQP = ?CONFNAME(Conf, "amqp"),
                  ping(AMQP, 0)
          end),
    {response, Req:ok({"text/plain", "Ping started"})};


respond(Req, 'GET', ["id"], _Conf, _Args) ->
    ID = uuid:to_string(uuid:srandom()),
    {response, Req:ok({"text/plain", [$", ID, $"]})};


respond(Req, 'GET', [], Conf, Args) ->

    QS = mochiweb_request:parse_qs(Req),
    AMQP = ?CONFNAME(Conf, "amqp"),

    ID = ?GV("id", QS),

    {atomic, Proc} = mnesia:transaction(fun() -> get_proc(ID, AMQP, Args) end),

    case ?GV("key", QS) of
        undefined -> ok;
        StrKey ->
            StrQueueName = ?GV("queue", QS),            
            Filter = get_filter(queue, Args),
            Allowed = case Filter of
                          none -> true;
                          {M,F} -> apply(M, F, [StrQueueName, StrKey])
                      end,
            
            case Allowed of        
                true ->
                    QueueName = list_to_binary(StrQueueName),
                    Key = list_to_binary(StrKey),
                    ok = gen_server:call(AMQP, {listen, QueueName, Key, Proc});
                _ ->
                    ?DBG({queue_denied, StrKey, StrQueueName, Filter})
            end
    end,

    Proc ! {listener, self()},
    Messages = receive
        {Proc, Ms} -> Ms
    after ?DEFUNCT_TIMEOUT -> []
    end,

    JSON = mochijson2:encode(Messages),
    {response, Req:ok({"text/plain", JSON})}.


update_message_filter(ID, AMQP, NewFilter) ->
    {atomic, Proc} = mnesia:transaction(fun() -> get_proc(ID, AMQP, []) end),
    Proc ! {newfilter, NewFilter}.


% Internal
% ----------------------------------------------


get_proc(ID, AMQP, Args) ->
    Procs = mnesia:read(comet_proc, ID),
    case Procs of
        [Proc] -> 
            Pid = Proc#comet_proc.proc,
            case is_process_alive(Pid) of
                true -> Pid;
                false -> create_comet_process(ID, AMQP, Args)
            end;
        [] -> 
            create_comet_process(ID, AMQP, Args);
        Other ->
            throw({'EXIT', multple_comet_procs, Other})
    end.


create_comet_process(ID, AMQP, Args) ->
    Filter = get_filter(message, Args),
    Pid = spawn(fun() -> comet_loop(#state{amqp=AMQP, filter=Filter}) end),
    ok = mnesia:write(#comet_proc{id=ID, proc=Pid}),
    Pid.


get_filter(FilterName, Args) ->
    case ?GV(filters, Args) of
        undefined -> none;
        Filters ->
            case ?GV(FilterName, Filters) of 
                undefined -> none;
                MF -> MF
            end
    end.

ping(AMQP, X) ->
    receive
    after 2000 ->
            Message = list_to_binary("PING " ++ integer_to_list(X)),
            ?DBG({ping, Message}),
            gen_server:cast(AMQP, {send, <<"ping">>, Message})
    end,
    ping(AMQP, X+1).


% ----------------------------------------------
% Per-session comet process

comet_loop(State) ->
    receive

        {listener, MochiProc} ->
            timer:cancel(State#state.timer),
            timer:cancel(State#state.listen_timer),
            {ok, SessionTRef} = timer:send_after(?SESSION_TIMEOUT, session_timeout),
            {ok, ListenTRef} = timer:send_after(?LONGPOLL_TIMEOUT, {listen_timeout, MochiProc}),
            replace_comet_listener(State#state{timer=SessionTRef, listen_timer=ListenTRef}, 
                                   MochiProc);

        {newfilter, NewFilter} ->
            comet_loop(State#state{filter=NewFilter});

        #'basic.consume_ok'{consumer_tag=ConsumerTag} ->
            comet_loop(State#state{queues=[ConsumerTag|State#state.queues]});

        {#'basic.deliver'{consumer_tag=Tag, routing_key=Key}, 
         #content{payload_fragments_rev = [Payload]}} ->
            handle_amqp_message(State, {Tag, Key, Payload});

        {listen_timeout, Pid} ->
            case State#state.listener of
                Pid ->
                    State#state.listener ! {self(), []},
                    comet_loop(State#state{listener=none});
                _ ->
                    comet_loop(State)
            end;

        session_timeout ->
            AMQP = State#state.amqp,
            [gen_server:call(AMQP, {unlisten, Q}) || Q <- State#state.queues],
            ok;

        Other ->
            ?DBG({unexpected_message, Other}),
            comet_loop(State)

    end.



replace_comet_listener(#state{listener=none, buffer=[]}=State, NewMochiProc) ->
    comet_loop(State#state{listener=NewMochiProc});

replace_comet_listener(#state{listener=SameProc, buffer=[]}=State, SameProc) ->
    comet_loop(State);

replace_comet_listener(#state{listener=none, buffer=Buffer}=State, NewMochiProc) ->
    NewMochiProc ! {self(), lists:reverse(Buffer)},
    comet_loop(State#state{buffer=[]});

replace_comet_listener(#state{listener=OldMochiProc, buffer=Buffer}=State, NewMochiProc) ->
    OldMochiProc ! {self(), lists:reverse(Buffer)},
    comet_loop(State#state{listener=NewMochiProc, buffer=[]}).



handle_amqp_message(#state{filter=none}=State, Message) ->
    send_amqp_message(State, Message);
handle_amqp_message(#state{filter=Filter}=State, {Tag, Key, Payload}=Message) ->
    case Filter(Key, Payload) of
        true -> send_amqp_message(State, Message);
        _ -> comet_loop(State)
    end.
             


send_amqp_message(State, {Tag, Key, Payload}) ->
    ?DBG({amqp, Tag, Key, Payload}),
    Message = {struct, [{tag, Tag}, {key, Key}, {payload, Payload}]},
    NewBuffer = [Message|State#state.buffer],
    case State#state.listener of
        none ->
            comet_loop(State#state{buffer=NewBuffer});
        MochiProc ->
            MochiProc ! {self(), lists:reverse(NewBuffer)},
            comet_loop(State#state{listener=none, buffer=[]})
    end.
