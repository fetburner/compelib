(* 重みの総和だけ分かれば良い場合 *)

module Int = struct
  type t = int
  let zero = 0
  let ( + ) = ( + )
  let compare = compare
end

module IntMap = Map.Make (Int)

module G = Compelib.Prim.F
  (struct
    type t = int array
    type key = int
    type elt = int
    type size = int
    let make = Fun.flip Array.make max_int
    let get = Array.get
    let set = Array.set
  end)
  (struct
    type t = (int * int) list IntMap.t ref
    type key = int
    type elt = int * int
    type size = int
    let make _ = ref IntMap.empty
    let take_min_binding q =
      match IntMap.min_binding !q with
      | exception Not_found -> None
      | (w, [v]) -> q := IntMap.remove w !q; Some (w, v)
      | (w, v :: vs) -> q := IntMap.add w vs !q; Some (w, v)
    let add q w v  =
      q := IntMap.update w (fun vs -> Some (v :: Option.value ~default:[] vs)) !q
  end)

let es =
  [|[(1, 7); (3, 5)];
    [(0, 7); (2, 8); (3, 9); (4, 7)];
    [(1, 8); (4, 5)];
    [(0, 5); (1, 9); (4, 15); (5, 6)];
    [(1, 7); (2, 5); (3, 15); (5, 8); (6, 9)];
    [(3, 6); (4, 8); (6, 11)];
    [(4, 9); (5, 11)]|]

let%test _ =
  let module T = (val G.minimum_spanning_tree
    (module struct
      module Weight = Int
      module Vertex = struct
        type t = int
        type set = int
        let universe = 7
        let iter_connected_edges u f = List.iter f es.(u)
      end
      module Edge = struct
        type t = int * int
        let weight = snd
        let endpoint = fst
      end
    end) 0) in
  T.Edge.fold_universe (fun (_, w) -> ( + ) w) 0 = 39

(* 使った辺を列挙したい場合 *)

module G' = Compelib.Prim.F
  (struct
    type t = int array
    type key = int
    type elt = int
    type size = int
    let make = Fun.flip Array.make max_int
    let get = Array.get
    let set = Array.set
  end)
  (struct
    type t = (int * int * int) list IntMap.t ref
    type key = int
    type elt = int * int * int
    type size = int
    let make _ = ref IntMap.empty
    let take_min_binding q =
      match IntMap.min_binding !q with
      | exception Not_found -> None
      | (w, [v]) -> q := IntMap.remove w !q; Some (w, v)
      | (w, v :: vs) -> q := IntMap.add w vs !q; Some (w, v)
    let add q w v  =
      q := IntMap.update w (fun vs -> Some (v :: Option.value ~default:[] vs)) !q
  end)

let%test _ =
  let module T = (val G'.minimum_spanning_tree
    (module struct
      module Weight = Int
      module Vertex = struct
        type t = int
        type set = int
        let universe = 7
        let iter_connected_edges u f = List.iter (fun (v, w) -> f (u, v, w)) es.(u)
      end
      module Edge = struct
        type t = int * int * int
        let weight (_, _, w) = w
        let endpoint (_, v, _) = v
      end
    end) 0) in
  List.sort compare (T.Edge.fold_universe (fun (u, v, w) -> List.cons (min u v, max u v, w)) [])
  = [(0, 1, 7); (0, 3, 5); (1, 4, 7); (2, 4, 5); (3, 5, 6); (4, 6, 9)]
