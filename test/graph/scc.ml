module A = struct
  type t = bool array
  type key = int
  type elt = bool
  type size = int
  let make n = Array.make (n + 1) false
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

module M = Compelib.Scc.F (A) (L)
module N = Compelib.Scc.G (A) (L) (LL)

module G = struct
  module Vertex = struct
    type t = int
    type set = int
    let universe = 7
    let rec foldn n f acc =
      if n = 0
      then acc
      else foldn (n - 1) f (f n acc)
    let fold_universe f acc = foldn universe f acc
    let fold_adjacencies v f acc =
      List.fold_right f
        begin match v with
        | 1 -> [2]
        | 2 -> [3]
        | 3 -> [1; 4]
        | 4 -> [5]
        | 5 -> [4; 6; 7]
        | 6 -> []
        | 7 -> []
        end acc
  end
end

let%test _ =
  List.map (List.sort_uniq compare) (N.scc (module G))
  = List.map (List.sort_uniq compare) [[2; 3; 1]; [4; 5]; [6]; [7]]

let%test _ = M.sort (module G) = [3; 1; 2; 5; 4; 6; 7]
