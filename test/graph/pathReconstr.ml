(* まずダイクストラ法で最短経路を求める *)

module Int = struct
  type t = int
  let zero = 0
  let equal = ( = )
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

let d =
  G.shortest_path
    (module struct
      module Weight = Int
      module Vertex = struct
        type t = int
        type set = int
        let universe = 7
        let iter_adjacency u f = Fun.flip List.iter e.(u) @@ fun (v, c) -> f v @@ ( + ) c
      end
    end) 0

(* 経路復元を行う *)

type 'a thunk = Running | Pending | Value of 'a

module M = Compelib.PathReconstr.F (Int)
  (struct
    type t = int * int
    type vertex = int
    type weight = int
    let source = fst
    let add_weight (_, w) = ( + ) w
  end)
  (struct
    type t = int list
    type edge = int * int
    let nil = []
    let empty = []
    let join = Fun.const
    let snoc p (v, _) = v :: p
  end)
  (struct
    type t = int list thunk
    type elt = int list
    let value x = Value x
    let running = Running
    let case t ~value ~pending ~running =
      match t with
      | Value x -> value x
      | Running -> running ()
      | Pending -> pending ()
  end)
  (struct
    type t = int list thunk array
    type size = int
    type key = int
    type elt = int list thunk
    let make = Fun.flip Array.make Pending
    let get = Array.get
    let set = Array.set
  end)

let%test _ =
  let e' = Array.make 6 [] in
  Array.iteri (fun u ->
    List.iter (fun (v, w) ->
      e'.(v) <- (u, w) :: e'.(v))) e;
  ( = ) [[]; [0]; [0]; [2; 0]; [5; 2; 0]; [2; 0]] @@
  List.init 6 @@
  M.path_reconstruction 6 (fun v -> List.to_seq e.(v)) 0 d

