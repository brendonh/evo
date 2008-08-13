%%%-------------------------------------------------------------------
%%% File    : magicdb.erl
%%% Author  : Brendon <brendonh@lightblue>
%%% Description : 
%%%
%%% Created : 21 Mar 2008 by Brendon <brendonh@lightblue>
%%%-------------------------------------------------------------------
-module(magicdb).

-behaviour(gen_server).

%% API
-export([start_link/2, munge_where/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  dbRef,
  schemas
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(DBEnv, Evoname) ->
    gen_server:start_link(Evoname, ?MODULE, [DBEnv, Evoname], []).

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
init([DBEnv, Evoname]) ->
    process_flag(trap_exit, true),

    application:start(odbc),

    {dsn, Dsn} = DBEnv,
    {ok, DBRef} = odbc:connect(Dsn, []),
    cr:dbg({magicdb_connected, Evoname, DBRef}),

    Schemas = ets:new(magicdb_schemas, [set]),

    {ok, #state{
       dbRef=DBRef,
       schemas=Schemas
    }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get, QueryInfo}, _From, State) ->
    Schemas = State#state.schemas,
    DBRef = State#state.dbRef,
    Reply = retrieve_rows(Schemas, DBRef, QueryInfo),
    {reply, Reply, State};

handle_call({getColumns, Table}, _From, State) ->
    Schemas = State#state.schemas,
    DBRef = State#state.dbRef,
    Schema = get_schema(Table, Schemas, DBRef),
    {reply, proplists:get_value(columns, Schema), State};

handle_call({getTables, Owner}, _From, State) ->
    SQL = "SELECT tablename " ++
        "FROM pg_tables " ++
        "WHERE tableowner = ?",
    Args = [{{sql_char, 128}, [Owner]}],
    {selected, _Cols, Names} = odbc:param_query(State#state.dbRef, SQL, Args),
    {reply, lists:map(fun({N}) -> N end, Names), State};

% Convert record to JSON object format
% TODO: this breaks unless all the fields are included in the object :-(
handle_call({tuple_to_json_obj, Table, Record}, _From, State) ->
    Schemas = State#state.schemas,
    DBRef = State#state.dbRef,
    Schema = get_schema(Table, Schemas, DBRef),
    Columns = proplists:get_value(columns, Schema),

    % TODO: probably cache this in the schema as well
    Names = lists:map(fun(T) -> erlang:element(1, T) end, Columns), 
    Types = lists:map(fun(T) -> fix_coltype(erlang:element(2, T)) end, Columns), 

    % cr:dbg({columns, Columns}),
    % cr:dbg({types, Types}),

    Plist = lists:zipwith3(fun(K, T, V) -> {K, convert_to_binary(T, V)} end, 
                           Names, Types, tuple_to_list(Record)),
    {reply, {obj, Plist}, State};


% Backward compatibility
handle_call({rowByID, Table, ID}, From, State) ->
    handle_call({get, 
                 [{table, Table}, {where, [{id, <<"=">>, ID}]}]},
                From, State);

% Backward compatibility
handle_call({getRows, Table, Wheres}, From, State) ->
    handle_call({get, 
                 [{table, Table}, {where, Wheres}]},
                From, State);

% Backward compatibility
handle_call({getRows, Table, Wheres, {Order,Limit,Offset}}, From, State) ->
    handle_call({get, 
                 [{table, Table}, {where, Wheres}, 
                  {order, Order}, {limit, Limit}, {offset, Offset}]},
                From, State);

% Backward compatibility
handle_call({getRows, Table, Wheres, {Order,Limit,Offset}, RefAction}, From, State) ->
    handle_call({get, 
                 [{table, Table}, {where, Wheres},
                  {order, Order}, {limit, Limit}, {offset, Offset},
                  {ref_action, RefAction}]},
                From, State);


handle_call(Request, _From, State) ->
    cr:dbg({unknown_request, Request}),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast(debug, State) ->
    cr:dbg(debug),
    {reply, Reply, State} = handle_call({getRows, routes, 
                                         [{name, <<"=">>, "Red 30"}], none}, self(), State),
    cr:dbg(Reply),
    {noreply, State};

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

%retrieve_rows(Table, Wheres, OLO, Schemas, DBRef, RefAction) ->
retrieve_rows(Schemas, DBRef, QueryInfo) ->
    Table = proplists:get_value(table, QueryInfo),
    RefAction = proplists:get_value(ref_action, QueryInfo, none),
    TableSchemas = get_schemas(Table, Schemas, DBRef),
    Aliases = build_alias_list(Table),
    case get_rows(TableSchemas, Aliases, DBRef, QueryInfo) of
        {ok, RawRows} ->
            lists:map(fun(Row) -> 
                              handle_row(Row, RefAction, TableSchemas, Schemas, Aliases, DBRef) 
                      end,
                      RawRows);
        {error, Error} ->
            {error, Error}
    end.

handle_row(Row, RefAction, TableSchemas, Schemas, Aliases, DBRef) ->
    PropList = build_proplist(Row, TableSchemas, Aliases),
    case RefAction of
        bag ->
            Referees = get_referee_values(PropList, TableSchemas, Schemas, DBRef),
            {PropList, flatten_reftree(Referees)};
        tree ->
            Referees = get_referee_values(PropList, TableSchemas, Schemas, DBRef),
            {PropList, Referees};
        none ->
            PropList
    end.


flatten_reftree(Referees) ->
    flatten_reftree(Referees, []).

flatten_reftree([], Bag) ->
    Bag;
flatten_reftree([{Table, {Props,Children}}|Tail], Bag) ->
    SubRefs = flatten_reftree(Children, []),
    SubBits = lists:keysort(1, SubRefs ++ [{Table, Props}]),
    NewBag = lists:ukeymerge(1, Bag, SubBits),
    flatten_reftree(Tail, NewBag).   


get_schemas(Tables, Schemas, DBRef) ->
    get_schemas(Tables, Schemas, DBRef, []).

get_schemas([], _, _, Acc) ->
    lists:reverse(Acc);
get_schemas([First|Rest], Schemas, DBRef, Acc) ->
    get_schemas(Rest, Schemas, DBRef, 
                [get_schema(First, Schemas, DBRef)|Acc]).


get_schema(TableSpec, Schemas, DBRef) ->

    Table = case TableSpec of
                {Tab, _Alias} -> Tab;
                Tab -> Tab
            end,

    case ets:lookup(Schemas, Table) of
        [{Table, Schema}] ->
            Schema;
        [] ->
            TableName = atom_to_list(Table),
            {ok, Columns} = odbc:describe_table(DBRef, TableName),
            OID = get_table_oid(TableName, DBRef),
            Keys = get_table_foreignkeys(OID, DBRef),
            Schema = [{name, Table}, {columns, Columns}, {oid, OID}, {foreign_keys, Keys}],
            ets:insert(Schemas, {Table, Schema}),
            cr:dbg({schema_okay, Table})
    end,
    Schema.


get_table_oid(TableName, DBRef) ->
    SQL = "select oid from pg_class where pg_class.relname=?",
    Args = [{{sql_varchar, 128}, [TableName]}],
    {selected, _Cols, [{OID}]} = odbc:param_query(DBRef, SQL, Args),
    OID.


get_table_foreignkeys(TableOID, DBRef) ->
    SQL = "SELECT pg_attribute.attname, pg_class.relname " ++
        "FROM pg_attribute, pg_class, " ++
        "(SELECT conrelid, conkey, confrelid " ++
        " FROM pg_constraint " ++
        " WHERE conrelid = ?) " ++
        " AS indexdef " ++
        "WHERE pg_attribute.attrelid = ? " ++
        "  AND ARRAY[pg_attribute.attnum] @> conkey " ++
        "  AND pg_class.oid = indexdef.confrelid",
    Args = [{sql_integer, [TableOID]}, {sql_integer, [TableOID]}],
    {selected, _Cols, Keys} = odbc:param_query(DBRef, SQL, Args),
    Keys.


get_rows(TableSchemas, Aliases, DBRef, QueryInfo) ->

    FromClause = case proplists:get_value(from, QueryInfo, none) of
                     none -> proplists:get_value(table, QueryInfo);
                     Something -> Something
                 end,

    FromList = build_from_list(FromClause, Aliases),
    Wheres = proplists:get_value(where, QueryInfo, []),

    Columns = [{proplists:get_value(name, TableSchema), 
                proplists:get_value(columns, TableSchema)}
               || TableSchema <- TableSchemas],

    {WhereClause, WhereArgs} = build_where_bits(Columns, Aliases, Wheres),
    OLO = build_olo(QueryInfo),

    SQL = lists:flatten(["SELECT * FROM ", FromList, WhereClause, OLO]),

    cr:dbg({sql, SQL}),

    % Believe it or not, param_query breaks if there are no placeholders.
    case WhereArgs of
        [] ->
            Result = odbc:sql_query(DBRef, SQL);
        _Stuff ->
            Result = odbc:param_query(DBRef, SQL, lists:reverse(WhereArgs))
    end,

    case Result of
        {selected, _Cols, Rows} -> 
            {ok, Rows};
        {error, Reason} -> 
            error_logger:error_msg("SQL error ~p~n", [Reason]),
            cr:dbg({error, Reason}),
            {error, sql}
    end.


build_alias_list(TableSpec) ->
    build_alias_list(TableSpec, []).

build_alias_list([], Acc) ->
    lists:reverse(Acc);
build_alias_list([{_T, _A}=TA|Rest], Acc) ->
    build_alias_list(Rest, [TA|Acc]);
build_alias_list([Tab|Rest], Acc) ->
    build_alias_list(Rest, [{Tab, none}|Acc]).


build_from_list({leftjoin, [{LT, _, _, _}|_]=Conds}, Aliases) ->
    lists:foldl(
      fun({LTab, LCol, RTab, RCol}, Acc) ->
              LColN = column_name(LTab, LCol, Aliases),
              RColN = column_name(RTab, RCol, Aliases),
              [Acc, " LEFT JOIN ", table_name(RTab, Aliases), 
               " ON ", LColN, " = ", RColN]
      end,
      [table_name(LT, Aliases)],
      Conds);
build_from_list(Table, Aliases) ->
    table_name(Table, Aliases).
   

table_name(TableRef, Aliases) ->
    case get_table_for_alias(TableRef, Aliases) of
        {alias_for, Table} -> [atom_to_list(Table), " AS ", atom_to_list(TableRef)];
        Table -> atom_to_list(Table)
    end.
            

get_table_for_alias(Table, [{Table, none}|_Rest]) ->
    Table;
get_table_for_alias(Alias, [{Table, Alias}|_Rest]) ->
    {alias_for, Table};
get_table_for_alias(Alias, [_|Rest]) ->
    get_table_for_alias(Alias, Rest).


column_name(Tab, Col, Aliases) ->
    TableName = case proplists:get_value(Tab, Aliases, none) of
                    none -> atom_to_list(Tab);
                    Alias -> atom_to_list(Alias)
                end,
    [TableName, ".", atom_to_list(Col)].


build_where_bits(_Columns, _Aliases, []) ->
    {"", []};
build_where_bits(Columns, Aliases, Wheres) ->
    {WhereBits, WhereArgs} = 
        lists:foldl(
          fun(Where, {Bits, Args}) ->
                  case Where of
                      none -> {Bits, Args};
                      _ ->
                          {WhereBit, ArgBit} = munge_where(Where, Columns, Aliases),
                          {[WhereBit|Bits], lists:append(ArgBit, Args)}
                  end
          end,
          {[], []},
          Wheres),
    WhereClause = lists:concat(
                    lists:reverse(
                      lists:foldl(
                        fun maybe_and/2, [], lists:reverse(WhereBits)))),

    {[" WHERE ", WhereClause], WhereArgs}.


build_olo(QueryInfo) ->

    Order = proplists:get_value(order, QueryInfo, none),

    OrderBy = case Order of
        none -> "";
        {Col, asc} -> [" ORDER BY ", atom_to_list(Col), " "];
        {Col, desc} -> [" ORDER BY ", atom_to_list(Col), " DESC "];
        Col -> [" ORDER BY ", atom_to_list(Col), " "]
    end,

    Limit = proplists:get_value(limit, QueryInfo, none),
    Offset = proplists:get_value(offset, QueryInfo, none),

    [OrderBy, case {Limit, Offset} of
                  {none, none} -> 
                      "";
                  {Limit, none} -> 
                      [" LIMIT ", integer_to_list(Limit)];
                  {Limit, Offset} -> 
                      [" LIMIT ", integer_to_list(Limit),
                       " OFFSET ", integer_to_list(Offset)]
    end].


maybe_and(Bit, []) ->
    [Bit];
maybe_and(Bit, Acc) ->
    [Bit," and "|Acc].

get_referee_values(Row, Schema, Schemas, DBRef) ->
    lists:map(fun({ColumnName, RefTableName}) ->
                      RefID = proplists:get_value(list_to_atom(ColumnName), Row),
                      RefTable = list_to_atom(RefTableName),
                      [RefRow] = retrieve_rows(Schemas, DBRef, 
                                               [{table, RefTable}, 
                                                {where, [{id, <<"=">>, RefID}]},
                                                {ref_action, tree}]),
                      {RefTable, RefRow}
              end,
              proplists:get_value(foreign_keys, Schema)).


build_proplist(RawRow, TableSchemas, Aliases) ->
    Columns = lists:flatten(
                [build_table_proplist(S, Aliases) || S <- TableSchemas]),
    Values = tuple_to_list(RawRow),
    lists:zip(Columns, Values).

build_table_proplist(Schema, Aliases) ->
    Table = proplists:get_value(name, Schema),
    Alias = proplists:get_value(Table, Aliases),
    [table_column_atom(Table, Alias, Col)
     || {Col, _Type} <- proplists:get_value(columns, Schema)].


table_column_atom(_Table, none, Col) ->
    list_to_atom(Col);
table_column_atom(Table, Alias, Col) ->
    list_to_atom(lists:concat([Alias, "_", Col])).

maybe_null(null) -> null;
maybe_null(0) -> 0;
maybe_null(Str) when is_list(Str) -> list_to_binary(Str).

convert_to_binary({sql_varchar, _}, Value) when is_list(Value) ->
    maybe_null(Value);    
convert_to_binary({sql_char, _}, Value) when is_list(Value) ->
    maybe_null(Value);    
convert_to_binary('SQL_LONGVARCHAR', Value) when is_list(Value) ->
    maybe_null(Value);    
convert_to_binary(_, Value) ->
    Value.

fix_coltype('SQL_TYPE_DATE') -> {sql_char, 10};
fix_coltype('SQL_TYPE_TIME') -> {sql_char, 8};
fix_coltype('SQL_TYPE_TIMESTAMP') -> {sql_char, 19};
fix_coltype(Else) -> Else.

munge_where(BitTuple, Columns, Aliases) ->
    BitList = tuple_to_list(BitTuple),
    Column = find_column(BitList),
    ColType = fix_coltype(find_colType(Column, Columns, Aliases)),
    case ColType of
        none ->
            {Where, Args} = {BitList, []};
        ColType ->
            {Where, Args} = munge_wherebits(BitList, ColType, {[], []})
    end,
    WhereString = space_join(Where),
    {WhereString, Args}.

find_column([Col|_Rest]) when is_atom(Col) ->
    Col;
find_column([{Table, Col}|_Rest])
  when is_atom(Table) andalso is_atom(Col) ->
    {Table, Col};
find_column([_Bit|Rest]) ->
    find_column(Rest);
find_column([]) ->
    none.


find_colType({TableRef, Col}, Columns, Aliases) ->
    Table = case get_table_for_alias(TableRef, Aliases) of
                {alias_for, T} -> T;
                T -> T
            end,
    TableColumns = proplists:get_value(Table, Columns),
    proplists:get_value(atom_to_list(Col), TableColumns);
find_colType(Col, Columns, _Aliases) ->
    ColName = atom_to_list(Col),
    case [ColType || {_Table, TableColumns} <- Columns,
                     ColType <- [proplists:get_value(ColName, TableColumns, none)],
                     ColType =/= none] of
        [] -> exit({unknown_column, Col});
        [ColType] -> ColType;
        Several -> exit({ambiguous_column, Col, Several})
    end.


munge_wherebits([Bit|Rest], ColType, {Where, Args}) when is_atom(Bit) ->
    munge_wherebits(Rest, ColType, {[Bit|Where], Args});

munge_wherebits([{Table, Col}|Rest], ColType, {Where, Args})
  when is_atom(Table) andalso is_atom(Col) ->
    munge_wherebits(Rest, ColType, {[lists:concat([Table,".",Col])|Where], Args});

munge_wherebits([{list, Entries}|Rest], ColType, {Where, Args}) ->
    % SQL placeholders don't work as "in ?", only as "in (?, ?, ...)"
    Count = length(Entries),
    Placeholders = comma_join(lists:duplicate(Count, "?")),
    WhereBit = lists:concat(["(", Placeholders, ")"]),
    ArgBits = lists:map(fun(E) -> {ColType, [E]} end, Entries),
    munge_wherebits(Rest, ColType, {[WhereBit|Where], ArgBits ++ Args});

munge_wherebits([Bit|Rest], ColType, {Where, Args}) when is_binary(Bit) ->
    Operator = binary_to_list(Bit),
    munge_wherebits(Rest, ColType, {[Operator|Where], Args});
munge_wherebits([Bit|Rest], ColType, {Where, Args}) ->
    WhereBit = "?",
    ArgBit = {ColType, [Bit]},
    munge_wherebits(Rest, ColType, {[WhereBit|Where], [ArgBit|Args]});
munge_wherebits([], _, {Where, Args}) ->
    {lists:reverse(Where), lists:reverse(Args)}.


comma_join(Stuff) ->
    string_join(lists:reverse(Stuff), ",", []).

space_join(Stuff) ->
    string_join(lists:reverse(Stuff), " ", []).

string_join([], _Sep, Acc) ->
    lists:concat(Acc);
string_join([Last], Sep, Acc) ->
    string_join([], Sep, [Last|Acc]);
string_join([Head|Rest], Sep, Acc) ->
    string_join(Rest, Sep, [Sep,Head|Acc]).
