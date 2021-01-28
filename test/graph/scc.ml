module A = struct
  type t = int array
  type key = int
  type elt = int
  type size = int
  let make = Fun.flip Array.make 0
  let get = Array.get
  let set = Array.set
end

module L = struct
  type t = int list
  type elt = int
  let nil = []
  let cons = List.cons
end

module LL = struct
  type t = int list list
  type elt = int list
  let nil = []
  let cons = List.cons
end

module M = Compelib.Scc.F (A) (L) (LL)

module G = struct
  module Vertex = struct
    type t = int
    type set = int
    let eq = ( = )
    let universe = 7
    let iter_universe f =
      for u = 0 to universe - 1 do
        f u
      done
    let iter_adjacencies u f =
      List.iter f @@
      match u with
      | 0 -> [1]
      | 1 -> [2]
      | 2 -> [0; 3]
      | 3 -> [4]
      | 4 -> [3; 5; 6]
      | 5 -> []
      | 6 -> []
  end
end

let%test _ =
  List.map (List.sort_uniq compare) (M.scc (module G))
  = List.map (List.sort_uniq compare) [[1; 2; 0]; [3; 4]; [6]; [5]]
