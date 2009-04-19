%% Shorthands

-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).
-define(DBG(Term), io:format("~p: ~p~n", [self(), Term])).

-define(SITENAME(Conf), list_to_atom(lists:concat(["evosite_", ?GV(name, Conf)]))).
-define(CONFNAME(Conf, Suffix), 
        evoutil:concat_atoms([?SITENAME(Conf), '_', Suffix])).
-define(COMPONENT(Conf, Name),
        evoutil:concat_atoms([?SITENAME(Conf), '_component_', Name])).

-define(LINK(Component, Path),
        [$/|string:join([Component|Path],"/")]).


%% Sites

-record(site, {
  evoname,
  components,
  templates,
  templateCallback,
  contentType = "text/html; charset=utf-8"
}).


%% Templates

-record(templateState,{
  id,
  tag=none,
  render=none,
  dataExpression=none,
  formatFunc=none,
  level=0,
  row=none,
  attrs=[],
  parent=none,
  children=[]
}).



%% Forms

-record(field,{
   localName,
   in_to_out,
   out_to_in,
   check_value,
   render,
   null_if_empty = true
}).

-record(form,{
   domain,
   fields
}).

