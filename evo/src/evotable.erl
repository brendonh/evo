%%%-------------------------------------------------------------------
%%% File    : evotable.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 17 Jun 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(evotable).

-behaviour(gen_server).

-include("evo.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(DB(Args), gen_server:call(?CONFNAME(State#state.conf, "magicdb"), Args)).

-define(Template(C, T, D, Cf), 
        gen_server:call(?CONFNAME(State#state.conf, "evotemplate"),
                        {C,T,D,Cf,State#state.templateCallback})).

-define(SelfLink(Path), ?LINK(State#state.conf, 
                              atom_to_list(State#state.table), 
                              Path)).

-record(state, {

 evoname,
 compname,
 conf,
 tableConf,

 table,
 listCols,

 columns = [],
 immutable,
 templateCallback,
 editForm,

 pageSize = 20

}).

%%====================================================================
%% API
%%====================================================================
start_link(SiteConf, Name, TableConf) ->
    CompName = ?COMPONENT(SiteConf, Name),
    gen_server:start_link({local, CompName}, ?MODULE, [SiteConf, Name, TableConf], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([SiteConf, Name, TableConf]) ->
    
    ?DBG({?MODULE, ?COMPONENT(SiteConf, Name), starting}),

    Table = ?GV(table, TableConf),

    Templates = case ?GVD(templates, TableConf, none) of
                    none -> [{list, auto}, {view, auto}, {edit, auto}];
                    Given -> [{list, ?GVD(list, Given, auto)},
                              {view, ?GVD(view, Given, auto)},
                              {edit, ?GVD(edit, Given, auto)}]
                end,

    Callback = fun(Name) -> 
                       case proplists:get_value(Name, Templates, not_found) of
                           auto -> auto_template(Name);
                           Other -> Other
                       end
               end,

    Immutable = ?GVD(immutable, TableConf, [id]),

    gen_server:cast(self(), reload_columns),

    {ok, #state{evoname=?COMPONENT(SiteConf, Name),
                compname=Name,
                conf=SiteConf,
                tableConf=TableConf,
                table=Table,
                immutable=Immutable,
                templateCallback=Callback}}.


handle_call({respond, Req, 'GET', []}, _From, State) ->
    Link = ?SelfLink(["list", "1"]),
    Response = Req:respond({302, [{<<"Location">>, Link}], <<"">>}),
    {reply, {response, Response}, State};

handle_call({respond, Req, 'GET', ["list"]}, _From, State) ->
    Link = ?SelfLink(["list", "1"]),
    Response = Req:respond({302, [{<<"Location">>, Link}], <<"">>}),
    {reply, {response, Response}, State};

handle_call({respond, Req, 'GET', ["list", PageStr]}, _From, State) ->
    Table = State#state.table,
    Columns = State#state.columns,
    ColNames = lists:map(fun({K,_V}) -> K end, Columns),

    PageSize = State#state.pageSize,

    Page = list_to_integer(PageStr),
    OffsetIndex = Page - 1,

    Rows = ?DB({getRows, Table, [], {id, PageSize, PageSize*OffsetIndex}}),

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
            {state, State}],

    Reply = case ?Template(run_raw, {reload, list}, Data, Conf) of
                {ok, Result} -> {wrap, site, [{content, Result}, {title, State#state.table}]};
                {error, Error} -> {error, Error}
            end,

    {reply, Reply, State};


handle_call({respond, Req, 'GET', ["view", RowID]}, _From, State) ->
    {reply, row_page(State, Req, RowID, view), State};

handle_call({respond, Req, 'GET', ["edit", RowID]}, _From, State) ->
    {reply, row_page(State, Req, RowID, edit), State};

handle_call({respond, Req, 'POST', ["edit", StrRowID]}, _From, State) ->

    RowID = list_to_integer(StrRowID),
    OutValues = mochiweb_multipart:parse_form(Req),

    InValues = evoform:parse_form(State#state.editForm, OutValues),

    ?DB({update, [{table, State#state.table},
                  {values, InValues},
                  {where, [{id, <<"=">>, RowID}]}]}),

    Link = ?SelfLink(["view", StrRowID]),
    Reply = {response, Req:respond({302, [{<<"Location">>, Link}], <<"">>})},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, not_found, State}.



handle_cast(reload_columns, State) ->
    Table = State#state.table,
    ?DBG({reloading_columns, Table}),
    Columns = ?DB({getColumns, Table}),

    ListCols = case ?GVD(listCols, State#state.tableConf, auto) of
                   auto -> [list_to_atom(N) || {N, T} <- Columns];
                   Given -> Given
               end,

    MutableColumns = [X || {C, T}=X <- Columns,
                           not lists:member(list_to_atom(C), 
                                            State#state.immutable)],

    Form = evoform:form_from_colspec(MutableColumns),

    {noreply, State#state{columns=Columns,
                          editForm=Form#form{domain=Table},
                          listCols=ListCols}};

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

row_page(State, Req, RowID, Action) ->
    Table = State#state.table,
    Columns = State#state.columns,
    Immutable = State#state.immutable,
    ColNames = lists:map(fun({K,_V}) -> K end, Columns),

    ID = list_to_integer(RowID),

    [Row] = ?DB({rowByID, Table, ID}),

    ValueRenderer = fun({Col,_}=X) ->
                            case lists:member(Col, Immutable) of
                                true -> cell_value(view, X);
                                false -> cell_value(Action, X)
                            end
                    end,

    Data = [{columns, ColNames}, 
            {row, Row}, {id, ID},
            {action, Action}],

    Conf = [{table, Table}, {row, Row},
            {db_value, ValueRenderer},
            {state, State}],

    case ?Template(run_raw, {reload, view}, Data, Conf) of
        {ok, Result} -> {wrap, site, [{content, Result}, {title, State#state.table}]};
        {error, Error} -> {error, Error}
    end.


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

    State = evo:conf(state),

    Link = ?SelfLink(["view", integer_to_list(ID)]),
    evo:tag(a, [{href, Link}], format_db_value(Val)).


cell_value(view, {_, Val}) -> format_db_value(Val);

cell_value(edit, {Col, Val}) -> 
    State = evo:conf(state),
    Form = State#state.editForm,
    Row = evo:conf(row),
    evoform:render_field(Form, Col, Row).


format_db_value(null) ->
    evo:tag('div', [{class, "null"}], "null");

format_db_value(Val) when is_integer(Val) ->
    evo:tag('div', [{class, "number"}], integer_to_list(Val));

format_db_value(Val) when is_float(Val) ->
    Long = lists:flatten(io_lib:format("~p", [Val])),
    Short = lists:flatten(io_lib:format("~.2f", [Val])),
    evo:tag('div', [{class, "number"}, {title, Long}], Short);

format_db_value([C|_]=Val) when is_integer(C) ->
    Val;
format_db_value([]) ->
    "";
format_db_value(Val) ->
    lists:flatten(io_lib:format("~p", [Val])).


page_link(_Data, Offset, Label) ->
    State = evo:conf(state),
    Page = evo:conf(page) + list_to_integer(Offset),

    Link = ?SelfLink(["list", integer_to_list(Page)]),

    case Page of
        I when I < 1 ->
            Label;
        _ ->
            evo:tag(a, [{href, Link}], Label)
    end.


auto_template(list) -> {file, "templates/auto/table_list.html"};
auto_template(view) -> {file, "templates/auto/table_view.html"};
auto_template(edit) -> auto_template(view).
