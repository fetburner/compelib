(* 最短距離だけ求める *)
module Int = struct
  type t = int
  let zero = 0
  let compare = compare
end

module IntMap = Map.Make (Int)

module G = Compelib.Dijkstra.F
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
    type t = int list IntMap.t ref
    type elt = int
    type key = int
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

let e =
  [|[ (1, 7); (2, 9); (5, 14) ];
    [ (0, 7); (2, 10); (3, 15) ];
    [ (0, 9); (1, 10); (3, 11); (5, 2) ];
    [ (1, 15); (2, 11); (4, 6) ];
    [ (3, 6); (5, 9) ];
    [ (0, 14); (2, 2); (4, 9) ]|]

let%test _ =
  List.init 7 (G.shortest_path
    (module struct
      module Distance = Int
      module Vertex = struct
        type t = int
        type set = int
        let universe = 7
        let iter_adjacency u f = Fun.flip List.iter e.(u) @@ fun (v, c) -> f v @@ ( + ) c
      end
    end) 0)
  = [ 0; 7; 9; 20; 20; 11; max_int ]

(* 経路長をMapに保存する *)
module G' = Compelib.Dijkstra.F
  (struct
    type t = int IntMap.t ref
    type key = int
    type elt = int
    type size = unit
    let make () = ref IntMap.empty
    let get m k = try IntMap.find k !m with Not_found -> max_int
    let set m k x = m := IntMap.add k x !m
  end)
  (struct
    type t = int list IntMap.t ref
    type elt = int
    type key = int
    type size = unit
    let make () = ref IntMap.empty
    let take_min_binding q =
      match IntMap.min_binding !q with
      | exception Not_found -> None
      | (w, [v]) -> q := IntMap.remove w !q; Some (w, v)
      | (w, v :: vs) -> q := IntMap.add w vs !q; Some (w, v)
    let add q w v  =
      q := IntMap.update w (fun vs -> Some (v :: Option.value ~default:[] vs)) !q
  end)

let%test _ =
  List.init 10 (G'.shortest_path
    (module struct
      module Distance = Int
      module Vertex = struct
        type t = int
        type set = unit
        let universe = ()
        let iter_adjacency u f = Fun.flip List.iter e.(u) @@ fun (v, c) -> f v @@ ( + ) c
      end
    end) 0)
  = [ 0; 7; 9; 20; 20; 11; max_int; max_int; max_int; max_int ]

(* 無限グラフも可 *)
let%test _ =
  List.init 10 (G'.shortest_path
    (module struct
      module Distance = Int
      module Vertex = struct
        type t = int
        type set = unit
        let universe = ()
        let iter_adjacency u f = f (u + 1) succ
      end
    end) 0)
  = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
