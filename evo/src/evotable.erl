%%%-------------------------------------------------------------------
%%% File    : evotable.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evotable).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {

 table,
 listCols,
 viewTemplate,
 editTemplate,

 columns,
 templates,

 pageSize = 20

}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Table) ->
    start_link(Table, all).

start_link(Table, ListCols) ->
    start_link(Table, ListCols, {auto, auto}).

start_link(Table, ListCols, {View, Edit}) ->
    gen_server:start_link({local, Table}, ?MODULE, [Table, ListCols, {View, Edit}], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Table, ListCols, {View, Edit}]) ->
    gen_server:cast(self(), regenerate),
    {ok, #state{table=Table,
                listCols=ListCols,
                viewTemplate=View,
                editTemplate=Edit}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({'GET', []}, _From, State) ->
    Table = State#state.table,
    Columns = State#state.columns,
    ColNames = lists:map(fun({K,_V}) -> K end, Columns),

    PageSize = State#state.pageSize,

    Rows = gen_server:call(magicdb, {getRows, Table, [], {PageSize, 0}}),

    Data = [{columns, ColNames}, {rows, Rows}, 
            {table, Table},
            {db_value, fun format_db_value/1}],

    Template = get_template(State, list),

    Template ! {run_raw, Data, self()},

    receive
        {Template, result, Result} -> ok
    after 3000 ->
            cr:dbg({died_waiting_for_template, list}),
            exit(Template, too_slow),
            Result = none
    end,

    {reply, {ok, Result}, State};
   

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(reload_columns, State) ->
    Table = State#state.table,
    cr:dbg({reloading_columns, Table}),
    Columns = gen_server:call(magicdb, {getColumns, Table}),
    {noreply, State#state{columns=Columns}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_template(State, Name) ->
    case proplists:get_value(Name, State#state.templates) of
        undefined ->
            cr:dbg({generating, Name}),
            Content = template(State, Name),
            Template = spawn_link(fun() -> evo:prepare(Content) end),
            cr:dbg({template, Template}),
            NewTemplates = [{Name,Template}|State#state.templates],
            {Template, State#state{templates=NewTemplates}};
        Template -> 
            {Template, State}
    end.

% - Render list (GET)
%   - AJAX per-column search
% - Render details (GET)
% - Render edit screen (GET)
% - Save (POST)

format_db_value({_Col, null}) ->
    {tags, [{"<div class=\"null\">", "null", "</div>"}]};
format_db_value({_Col, Val}) when is_integer(Val) ->
    {tags, [{"<div class=\"number\">", integer_to_list(Val), "</div>"}]};
format_db_value({_Col, Val}) when is_float(Val) ->
    {tags, [{lists:flatten(io_lib:format("<div class=\"number\" title=\"~p\">", [Val])), 
             lists:flatten(io_lib:format("~.2f", [Val])), 
             "</div>"}]};
format_db_value({_Col, [C|_]=Val}) when is_integer(C) ->
    Val;
format_db_value({_Col, []}) ->
    "";
format_db_value({_Col, Val}) ->
    lists:flatten(io_lib:format("~p", [Val])).


template(_State, list) ->
    auto_template(tableList);

template(State, detailView) ->
    case State#state.viewTemplate of
        auto -> auto_template(tableContent);
        _ -> State#state.viewTemplate
    end;

template(State, detailEdit) ->
    case State#state.editTemplate of
        auto -> auto_template(tableContent);
        _ -> State#state.editTemplate
    end.
           

auto_template(tableList) -> atom_to_list('
<ul class="tableList" e:render="foreach" e:key="tables">
  <li>
    <e:attr name="class" e:render="data" e:dataExp="OddEven" />
    <a><e:attr name="href" e:render="data">/tables/</e:attr><e:slot e:format="pretty" /></a>
  </li>
</ul>
');

auto_template(tableContent) -> "
<div>
<div class=\"pageLinks\">
  <e:slot e:format=\"pageLink -1 Prev\" />
  <e:slot e:format=\"pageLink +1 Next\" />
</div>
<div class=\"tableScroll\">
<table class=\"dbTable\">
  <tr e:key=\"columns\">
    <th class=\"first\" e:render=\"data\" e:dataExp=\"lists:nth(1,D)\" />
    <e:inv e:render=\"foreach\" e:dataExp=\"lists:nthtail(1,D)\"><th e:render=\"data\" /></e:inv>
  </tr>
  <tbody e:render=\"foreach\" e:key=\"rows\">
    <tr>
      <e:attr name=\"class\" e:render=\"data\" e:dataExp=\"OddEven\" />
      <td class=\"first\" e:render=\"data\" e:format=\"db_value\" e:dataExp=\"lists:nth(1,D)\" />
      <e:inv e:render=\"foreach\" e:dataExp=\"lists:nthtail(1,D)\"><td e:render=\"data\" e:format=\"db_value\" /></e:inv>
    </tr>
  </tbody>
</table>
</div>
</div>
".
