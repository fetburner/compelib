(* 最短距離だけ求める *)
module Int = struct
  type t = int
  let zero = 0
  let compare = compare
end

module IntMap = Map.Make (Int)

module G = Compelib.Dijkstra.F
  (struct
    type t = int list IntMap.t ref
    type elt = int
    type key = int
    let create () = ref IntMap.empty
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
  let d = Array.make 7 max_int in
  List.init 7 (G.shortest_path
    (module struct
      module Distance = Int
      module Vertex = struct
        type t = int
        let get_distance = Array.get d
        let set_distance = Array.set d
        let iter_adjacencies u f = Fun.flip List.iter e.(u) @@ fun (v, c) -> f v @@ d.(u) + c
      end
    end) 0)
  = [ 0; 7; 9; 20; 20; 11; max_int ]
