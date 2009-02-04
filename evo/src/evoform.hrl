-record(evofield,{
   localName,
   in_to_out,
   out_to_in,
   check_value,
   render,
   null_if_empty = true
}).

-record(evoform,{
   domain,
   fields
}).

