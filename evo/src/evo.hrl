-record(state,{
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


-record(evofield,{
   localName,
   in_to_out,
   out_to_in,
   check_value,
   render
}).

-record(evoform,{
   fields
}).
