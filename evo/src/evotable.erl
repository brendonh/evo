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
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DB(Args), evosite:db(State#state.evoname, Args)).
-define(Template(C, T, D), evosite:template(State#state.evoname, {C,T,D, State#state.templateCallback})).

-record(state, {

 evoname,
 name,

 table,
 listCols,

 columns = [],
 templateCallback,

 pageSize = 20

}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(EvoName, Name, Args) ->
    CompName = evoutil:concat_atoms([EvoName, "_component_", Name]),
    gen_server:start_link({local, CompName}, ?MODULE, [EvoName, Name, Args], []).

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
init([EvoName, TableName, [Table, ListCols, {List, View, Edit}]]) ->
    
    Templates = [{list, List}, {view, View}, {edit, Edit}],
    Callback = fun(Name) -> 
                       case proplists:get_value(Name, Templates, not_found) of
                           auto -> auto_template(Name);
                           Other -> Other
                       end
               end,

    gen_server:cast(self(), reload_columns),
    {ok, #state{evoname=EvoName,
                name=TableName,
                table=Table,
                listCols=ListCols,
                templateCallback=Callback}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({respond, Req, 'GET', []}, _From, State) ->
    Response = Req:respond({302, [{<<"Location">>, "/customers/1"}], <<"">>}),
    {reply, {response, Response}, State};

handle_call({respond, Req, 'GET', [PageStr]}, _From, State) ->
    Table = State#state.table,
    Columns = State#state.columns,
    ColNames = lists:map(fun({K,_V}) -> K end, Columns),

    PageSize = State#state.pageSize,

    Page = list_to_integer(PageStr),
    OffsetIndex = Page - 1,

    Rows = ?DB({getRows, Table, [], {PageSize, PageSize*OffsetIndex}}),

    case State#state.listCols of
        all -> 
            ListCols = ColNames,
            ListRows = Rows;        
        List -> 
            ListCols = List,
            ListRows = [get_list_cols(Row, ListCols) || Row <- Rows]
    end,

    Data = [{all_columns, ColNames}, 
            {columns, ListCols},
            {rows, ListRows}, 
            {table, Table}, {page, Page},
            {db_value, fun format_db_value/1},
            {pageLink, fun page_link/3}],

    Reply = case ?Template(run_raw, {reload, list}, Data) of
                {ok, Result} -> {wrap, site, [{content, Result}]};
                {error, Error} -> {response, Req:ok({"text/plain", Error})}
            end,

    {reply, Reply, State};

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
    Columns = ?DB({getColumns, Table}),
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

get_list_cols(Row, Cols) ->
    [{Col, proplists:get_value(Col, Row)} || Col <- Cols].

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


page_link(Data, Offset, Label) ->
    Table = proplists:get_value(table, Data),
    Page = proplists:get_value(page, Data) + list_to_integer(Offset),
    case Page of
        I when I < 1 ->
            Label;
        _ ->
            {tags, [{lists:flatten(io_lib:format("<a href=\"/~s/~B\">", [Table, Page])),
                     Label,
                     "</a>"}]}
    end.


auto_template(list) -> {file, "templates/auto/table_list.html"};
auto_template(view) -> "<div>Not yet</div>";
auto_template(edit) -> auto_template(view).
