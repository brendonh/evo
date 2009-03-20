-module(evopgsql).

-include("evo.hrl").

-export([start/0, start/1, request/1, template/1]).

-define(DOC_ROOT, "static").
-define(CONTENT_TYPE, "text/html; charset=utf-8").
-define(PAGE_SIZE, 10).

-define(DSN, "DSN=routecomplete_dev;UID=routecomplete;PWD=secret").
-define(DATABASE, "routecomplete").

start() ->
    start("1235").

start([Port]) when is_atom(Port) ->
    start(atom_to_list(Port));
start(Port) ->

    cr:start_link(),

    case error_logger:add_report_handler(cr) of
        ok -> ok;
        Error -> io:format("OH NO: ~p~n", [Error])
    end,

    evotemplate:start_link(fun(T) -> template(T) end),
    magicdb:start_link({dsn, ?DSN}),
    mochiweb_http:start([{port, Port}, {loop, {?MODULE, request}}]),

    ?DBG({evopgsql, running}),

    receive
        finish -> ok
    end.


request(Req) ->
    Bits = string:tokens(Req:get(path), "/"),
    Real = case lists:last(Req:get(path)) == $/ of
               true -> Bits ++ [""];
               false -> Bits
           end,
    
    case dispatch(Req, Req:get(method), Real) of
        {ok, Type, Title, Result} ->
            SiteData = [{title, Title}, {content, Result}],
            case gen_server:call(evotemplate, {run, site, SiteData}) of
                {ok, Final} -> Req:ok({Type, Final});
                {error, Error} -> Req:ok({?CONTENT_TYPE, Error})
            end;
        {other, Result} ->
            Result;
        _ ->
            Req:respond({500, [], <<"Internal error">>})
    end.

dispatch(_Req, 'GET', ["tables"]) ->
    Tables = gen_server:call(magicdb, {getTables, ?DATABASE}),
    Data = [{tables, lists:sort(Tables)}, {pretty, fun pretty_name/1}],
    case gen_server:call(evotemplate, {run_raw, tableList, Data}) of
        {ok, Result} -> {ok, ?CONTENT_TYPE, "Tables", Result};
        {error, Error} -> {ok, ?CONTENT_TYPE, "Error", Error}
    end;

dispatch(Req, 'GET', ["tables",Name]) ->
    {other, Req:respond({302, [{<<"Location">>, list_to_binary("/tables/"++Name++"/1")}], <<"">>})};

dispatch(_Req, 'GET', ["tables",Name,Page]) ->
    Table = list_to_atom(Name),
    Columns = gen_server:call(magicdb, {getColumns, Table}),
    ColNames = lists:map(fun({K,_V}) -> K end, Columns),
    OffsetIndex = list_to_integer(Page) - 1,
    Rows = gen_server:call(magicdb, {getRows, Table, [], {?PAGE_SIZE, OffsetIndex*?PAGE_SIZE}}),
    Data = [{columns, ColNames}, {rows, Rows}, 
            {table, Table}, {page, list_to_integer(Page)}, 
            {db_value, fun format_db_value/1}, 
            {pageLink, fun page_link/3}],
    case gen_server:call(evotemplate, {run_raw, tableContent, Data}) of
        {ok, Result} -> {ok, ?CONTENT_TYPE, Name, Result};
        {error, Error} -> {ok, ?CONTENT_TYPE, "Error", Error}
    end;

dispatch(Req, 'GET', [""]) ->
    {other, Req:respond({302, [{<<"Location">>, <<"/tables">>}], <<"">>})};

dispatch(Req, 'GET', ["static"|PathBits]) ->
    {other, Req:serve_file(string:join(PathBits, "/"), ?DOC_ROOT)};

dispatch(Req, _, _) ->
    {other, Req:not_found()}.


pretty_name(Name) ->
    Bits = string:tokens(Name, "_"),
    TitleBits = lists:map(fun([C|R]) -> [string:to_upper(C)|R] end, Bits),
    string:join(TitleBits, " ").


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
            {tags, [{lists:flatten(io_lib:format("<a href=\"/tables/~s/~B\">", [Table, Page])),
                     Label,
                     "</a>"}]}
    end.


template(site) -> "
<!DOCTYPE html
    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
    <title>RouteComplete Browser</title>
    <link rel=\"stylesheet\" href=\"/static/site.css\" type=\"text/css\" />
  </head>
  <body>
    <div id=\"main\">
      <div class=\"mainTitle\">RouteComplete Browser</div>
      <div class=\"subTitle\"><e:slot key=\"title\" /></div>
      <div class=\"content\"><e:slot key=\"content\" /></div>
    </div>
  </body>
</html>
";

template(tableList) -> atom_to_list('
<ul class="tableList" e:render="foreach" e:key="tables">
  <li>
    <e:attr name="class" e:render="data" e:dataExp="OddEven" />
    <a><e:attr name="href" e:render="data">/tables/</e:attr><e:slot e:format="pretty" /></a>
  </li>
</ul>
');

template(tableContent) -> "
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

