-define(GV(E, P), proplists:get_value(E, P)).
-define(GVD(E, P, D), proplists:get_value(E, P, D)).

-define(DBG(Term), io:format("~p: ~p~n", [self(), Term])).


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

