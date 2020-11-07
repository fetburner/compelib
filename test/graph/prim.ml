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
    type t = int
    type v = int
    type e = int
    let empty = 0
    let add = ( + )
  end)
  (struct
    type t = int
    let zero = 0
    let compare = compare
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
  (struct
    type t = int array
    type key = int
    type elt = int
    type size = int
    let make = Fun.flip Array.make max_int
    let get = Array.get
    let set = Array.set
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
  G.minimum_spanning_tree 7
    (fun u f -> List.iter (fun (v, w) -> f v w w) es.(u)) 0 = 39

(* 使った辺を列挙したい場合 *)

module WeightedRoute = struct
  (* 重みは同じでも違う頂点への辺を同一視されないようにする *)
  type t = int * int * ((int * int * int) list -> (int * int * int) list)
  let zero = (0, min_int, fun xs -> xs)
  let ( + ) (w, _, f) (w', v, g) = (w + w', v, fun xs -> g (f xs))
  let compare (w, u, _) (w', v, _) =
    match compare w w' with
    | 0 -> compare u v
    | x -> x
end

module G' = Compelib.Prim.F
  (struct
    type t = (int * int * int) list
    type v = int
    type e = (int * int * int)
    let empty = []
    let add = List.cons
  end)
  (struct
    type t = int
    let zero = 0
    let compare = compare
  end)
  (struct
    type t = (int * (int * int * int)) list IntMap.t ref
    type key = int
    type elt = int * (int * int * int)
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
  (struct
    type t = int array
    type key = int
    type elt = int
    type size = int
    let make = Fun.flip Array.make max_int
    let get = Array.get
    let set = Array.set
  end)

let t =
  G'.minimum_spanning_tree 7
    (fun u f -> List.iter (fun (v, w) -> f v w (u, v, w)) es.(u)) 0

let%test _ =
  List.sort_uniq compare t =
  List.sort_uniq compare [(4, 6, 9); (4, 2, 5); (1, 4, 7); (0, 1, 7); (3, 5, 6); (0, 3, 5)]
