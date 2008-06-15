-module(evopgsql).

-export([start/0, start/1, request/1, template/1]).

-define(DOC_ROOT, "static").

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

    evotemplate:start_link({?MODULE, template}),
    magicdb:start_link({dsn, "DSN=routecomplete_dev;UID=routecomplete;PWD=secret"}),
    mochiweb_http:start([{port, Port}, {loop, {?MODULE, request}}]),

    cr:dbg({evopgsql, running}),

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
            {ok, Final} = gen_server:call(evotemplate, {run, site, SiteData}),
            Req:ok({Type, Final});
        {other, Result} ->
            Result;
        _ ->
            Req:respond({500, [], <<"Internal error">>})
    end.

dispatch(Req, 'GET', ["tables"]) ->
    Tables = gen_server:call(magicdb, {getTables, "routecomplete"}),
    Data = [{tables, lists:sort(Tables)}, {pretty, fun pretty_name/1}],
    {ok, Result} = gen_server:call(evotemplate, {run, tableList, Data, run_raw}),
    {ok, "text/html", "Tables", Result};

dispatch(Req, 'GET', ["model",ID]) ->
    {ok, "text/html", "Model", list_to_binary(lists:flatten(io_lib:format("Model ~p", [ID])))};

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


template(site) -> "
<!DOCTYPE html
    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
  <head>
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
    <a>
     <e:attr name="href" e:render="data">/tables/</e:attr>
     <e:slot e:format="pretty" />
    </a>
  </li>
</ul>
').
