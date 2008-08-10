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
-export([start_link/2, munge_where/2]).

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
handle_call({rowByID, Table, ID}, From, State) ->
    handle_call({getRows, Table, [{id, <<"=">>, ID}]}, From, State);

handle_call({getRows, Table, Wheres}, From, State) ->
    handle_call({getRows, Table, Wheres, {none, none, none}, none}, From, State);

handle_call({getRows, Table, Wheres, {_,_,_}=OLO}, From, State) ->
    handle_call({getRows, Table, Wheres, OLO, none}, From, State);

handle_call({getRows, Table, Wheres, {_,_,_}=OLO, RefAction}, _From, State) ->
    Schemas = State#state.schemas,
    DBRef = State#state.dbRef,
    Reply = retrieve_rows(Table, Wheres, OLO, Schemas, DBRef, RefAction),
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

retrieve_rows(Table, Wheres, OLO, Schemas, DBRef, RefAction) ->
    Schema = get_schema(Table, Schemas, DBRef),
    case get_rows(Table, Wheres, OLO, Schema, DBRef) of
        {ok, RawRows} ->
            lists:map(fun(Row) -> 
                              handle_row(Row, RefAction, Schema, Schemas, DBRef) 
                      end,
                      RawRows);
        {error, Error} ->
            {error, Error}
    end.

handle_row(Row, RefAction, Schema, Schemas, DBRef) ->
    PropList = build_proplist(Row, Schema),
    case RefAction of
        bag ->
            Referees = get_referee_values(PropList, Schema, Schemas, DBRef),
            {PropList, flatten_reftree(Referees)};
        tree ->
            Referees = get_referee_values(PropList, Schema, Schemas, DBRef),
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


get_schema(Table, Schemas, DBRef) ->
    case ets:lookup(Schemas, Table) of
        [{Table, Schema}] ->
            Schema;
        [] ->
            TableName = atom_to_list(Table),
            {ok, Columns} = odbc:describe_table(DBRef, TableName),
            OID = get_table_oid(TableName, DBRef),
            Keys = get_table_foreignkeys(OID, DBRef),
            Schema = [{columns, Columns}, {oid, OID}, {foreign_keys, Keys}],
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


get_rows(Table, [], {Order, Limit, Offset},_Schema, DBRef) ->
    TableName = atom_to_list(Table),

    case Order of
        none -> OrderBy = "";
        {Col, asc} -> OrderBy = " ORDER BY " ++ atom_to_list(Col) ++ " ";
        {Col, desc} -> OrderBy = " ORDER BY " ++ atom_to_list(Col) ++ " DESC ";
        Col -> OrderBy = " ORDER BY " ++ atom_to_list(Col) ++ " "
    end,

    BaseSQL = "SELECT * FROM " ++ TableName ++ OrderBy,

    case {Limit, Offset} of
        {none, none} -> SQL = BaseSQL;
        {_, none} -> SQL = BaseSQL ++ " LIMIT " ++ integer_to_list(Limit);
        {_, _} -> SQL = BaseSQL ++ " LIMIT " ++ integer_to_list(Limit)
                      ++ " OFFSET " ++ integer_to_list(Offset)
    end,

    Result = odbc:sql_query(DBRef, SQL),

    case Result of
        {selected, _Cols, Rows} -> 
            {ok, Rows};
        {error, Reason} -> 
            error_logger:error_msg("SQL error ~p~n", [Reason]),
            cr:dbg({error, Reason}),
            {error, sql}
    end;

get_rows(Table, Wheres, {Order, Limit, Offset}, Schema, DBRef) ->
    TableName = atom_to_list(Table),
    
    Columns = proplists:get_value(columns, Schema),

    {WhereBits, WhereArgs} = lists:foldl(
             fun(Where, {Bits, Args}) ->
                     case Where of
                         none -> {Bits, Args};
                         _ ->
                             {WhereBit, ArgBit} = munge_where(Where, Columns),
                             {[WhereBit|Bits], lists:append(ArgBit, Args)}
                     end
             end,
             {[], []},
             Wheres),
        
    WhereClause = lists:concat(lists:reverse(lists:foldl(
                    fun maybe_and/2, [], lists:reverse(WhereBits)))),

    BaseSQL = "SELECT * FROM " ++ TableName ++ " WHERE " ++ WhereClause,

    case Order of
        none -> OrderBy = "";
        {Col, asc} -> OrderBy = " ORDER BY " ++ atom_to_list(Col) ++ " ";
        {Col, desc} -> OrderBy = " ORDER BY " ++ atom_to_list(Col) ++ " DESC ";
        Col -> OrderBy = " ORDER BY " ++ atom_to_list(Col) ++ " "
    end,

    BaseSQL2 = BaseSQL ++ OrderBy,

    case {Limit, Offset} of
        {none, none} -> SQL = BaseSQL2;
        {Limit, none} -> SQL = BaseSQL2 ++ " LIMIT " ++ integer_to_list(Limit);
        {Limit, Offset} -> SQL = BaseSQL2 
                               ++ " LIMIT " ++ integer_to_list(Limit)
                               ++ " OFFSET " ++ integer_to_list(Offset)
    end,

    % Believe it or not, param_query breaks if there are no placeholders.
    case WhereArgs of
        [] ->
            Result = odbc:sql_query(DBRef, SQL);
        _Stuff ->
            Result = odbc:param_query(DBRef, SQL, lists:reverse(WhereArgs))
    end,

    case Result of
        {selected, _Cols, Rows} -> 
            %cr:dbg({rows, Rows}),
            {ok, Rows};
        {error, Reason} -> 
            error_logger:error_msg("SQL error ~p~n", [Reason]),
            cr:dbg({error, Reason}),
            {error, sql}
    end.

maybe_and(Bit, []) ->
    [Bit];
maybe_and(Bit, Acc) ->
    [Bit," and "|Acc].

get_referee_values(Row, Schema, Schemas, DBRef) ->
    lists:map(fun({ColumnName, RefTableName}) ->
                      RefID = proplists:get_value(list_to_atom(ColumnName), Row),
                      RefTable = list_to_atom(RefTableName),
                      [RefRow] = retrieve_rows(RefTable, [{id, <<"=">>, RefID}], {none,none}, Schemas, DBRef, tree),
                      {RefTable, RefRow}
              end,
              proplists:get_value(foreign_keys, Schema)).


build_proplist(RawRow, Schema) ->
    ColumnNames = proplists:get_value(columns, Schema),
    Columns = lists:map(
                fun({Name, _Type}) -> list_to_atom(Name) end,
                ColumnNames),
    Values = tuple_to_list(RawRow),
    lists:zip(Columns, Values).


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

munge_where(BitTuple, Columns) ->
    BitList = tuple_to_list(BitTuple),
    Column = find_column(BitList),
    ColType = fix_coltype(proplists:get_value(atom_to_list(Column), Columns)),
    case ColType of
        none ->
            {Where, Args} = {BitList, []};
        ColType ->
            {Where, Args} = munge_wherebits(BitList, ColType, {[], []})
    end,
    WhereString = space_join(Where),
    {WhereString, Args}.

find_column([Bit|_Rest]) when is_atom(Bit) ->
    Bit;
find_column([_Bit|Rest]) ->
    find_column(Rest);
find_column([]) ->
    none.

munge_wherebits([Bit|Rest], ColType, {Where, Args}) when is_atom(Bit) ->
    munge_wherebits(Rest, ColType, {[Bit|Where], Args});

munge_wherebits([Bit|Rest], ColType, {Where, Args}) when is_tuple(Bit) ->
    % SQL placeholders don't work with "in (...)"
    Entries = tuple_to_list(Bit),
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
