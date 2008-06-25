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
-define(Template(C, T, D, Cf), evosite:template(State#state.evoname, {C,T,D,Cf,State#state.templateCallback})).

-record(state, {

 evoname,
 compname,

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
                compname=TableName,
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
    Response = Req:respond({302, [{<<"Location">>, "/customers/list/1"}], <<"">>}),
    {reply, {response, Response}, State};

handle_call({respond, Req, 'GET', ["list"]}, _From, State) ->
    Response = Req:respond({302, [{<<"Location">>, "/customers/list/1"}], <<"">>}),
    {reply, {response, Response}, State};

handle_call({respond, Req, 'GET', ["list", PageStr]}, _From, State) ->
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
            {columns, [get_col_name(Col) || Col <- ListCols]},
            {rows, ListRows}],

    Conf = [{table, Table}, {page, Page},
            {db_value, fun format_value/1},
            {pageLink, fun page_link/3},
            {listCols, column_conf(State#state.listCols)},
            {evoname, State#state.evoname},
            {compname, State#state.compname}],

    Reply = case ?Template(run_raw, {reload, list}, Data, Conf) of
                {ok, Result} -> {wrap, site, [{content, Result}, {title, State#state.table}]};
                {error, Error} -> {response, Req:ok({"text/plain", Error})}
            end,

    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, not_found, State}.

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

column_conf(ListCols) ->
    column_conf(ListCols, []).

column_conf([], Conf) -> 
    Conf;
column_conf([{Name, Func}|Rest], Conf) ->
    column_conf(Rest, [{Name, Func}|Conf]);
column_conf([Name|Rest], Conf) ->
    column_conf(Rest, [{Name, none}|Conf]).

get_list_cols(Row, Cols) ->
    [{get_col_name(Col), proplists:get_value(get_col_name(Col), Row)} || Col <- Cols].

get_col_name({Col, _Func}) -> Col;
get_col_name(Col) -> Col.


format_value({Col, Val}) ->
    Func = proplists:get_value(Col, evo:conf(listCols)),
    case Func of
        none -> format_db_value(Val);
        view -> link_view(Val);
        Other -> format_db_value({unknown_func, Other})
    end.

link_view(Val) ->
    Row = evo:var(row),

    % Spot the bug!
    ID = proplists:get_value(id, Row),

    EvoName = evo:conf(evoname),
    CompName = evo:conf(compname),

    Link = evosite:link(EvoName, CompName, ["view", integer_to_list(ID)]),

    % This is so gonna get frameworkified
    OpenTag = lists:flatten(io_lib:format("<a href=\"~s\">", [Link])),
    {tags, [{OpenTag,
             format_db_value(Val),
             "</a>"}]}.


format_db_value(null) ->
    {tags, [{"<div class=\"null\">", "null", "</div>"}]};

format_db_value(Val) when is_integer(Val) ->
    {tags, [{"<div class=\"number\">", integer_to_list(Val), "</div>"}]};

format_db_value(Val) when is_float(Val) ->
    {tags, [{lists:flatten(io_lib:format("<div class=\"number\" title=\"~p\">", [Val])), 
             lists:flatten(io_lib:format("~.2f", [Val])), 
             "</div>"}]};

format_db_value([C|_]=Val) when is_integer(C) ->
    Val;
format_db_value([]) ->
    "";
format_db_value(Val) ->
    lists:flatten(io_lib:format("~p", [Val])).


page_link(Data, Offset, Label) ->
    EvoName = evo:conf(evoname),
    CompName = evo:conf(compname),
    Page = evo:conf(page) + list_to_integer(Offset),
    Link = evosite:link(EvoName, CompName, ["list", integer_to_list(Page)]),

    case Page of
        I when I < 1 ->
            Label;
        _ ->
            {tags, [{lists:flatten(io_lib:format("<a href=\"~s\">", [Link])),
                     Label,
                     "</a>"}]}
    end.


auto_template(list) -> {file, "templates/auto/table_list.html"};
auto_template(view) -> "<div>Not yet</div>";
auto_template(edit) -> auto_template(view).
