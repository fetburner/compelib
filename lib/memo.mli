val memoize : int -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
val memoize_cps :
  int ->
  (('a -> ('b -> 'c) -> 'c) -> 'a -> ('b -> 'c) -> 'c) ->
  'a -> ('b -> 'c) -> 'c
