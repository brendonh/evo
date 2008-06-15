-module(evopgsql).

-export([start/0, start/1, request/1, template/1]).

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
    {ok, Result} = gen_server:call(evotemplate, {run, tableList, Tables, run_raw}),
    {ok, "text/html", "Tables", Result};

dispatch(Req, 'GET', ["model",ID]) ->
    {ok, "text/html", "Model", list_to_binary(lists:flatten(io_lib:format("Model ~p", [ID])))};

dispatch(Req, 'GET', [""]) ->
    {other, Req:respond({302, [{<<"Location">>, <<"/tables">>}], <<"">>})};

dispatch(Req, _, _) ->
    {other, Req:not_found()}.


template(site) -> "
<!DOCTYPE html
    PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">
  <head>
    <title>RouteComplete Browser</title>
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
<ul e:render="foreach">
  <li><a><e:attr name="href" e:render="data">/tables/</e:attr><e:slot /></a></li>
</ul>
').
