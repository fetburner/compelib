(* 座標圧縮 *)
module CoordComp
  (* 座標の型 *)
  (Coord : sig
    type t
    val compare : t -> t -> int
  end) :
sig
  val compress :
    (* 座標のリスト *)
    Coord.t list ->
    (* 座標の数，圧縮する関数と解凍する関数を返す *)
    int * (Coord.t -> int) * (int -> Coord.t)
end = struct
  module IntMap = Map.Make (struct
    type t = int
    let compare = compare
  end)
  module CoordMap = Map.Make (Coord)

  let compress cs =
    let (n, comp, decomp) =
      List.fold_left (fun (n, comp, decomp) c ->
        if CoordMap.mem c comp then (n, comp, decomp)
        else (n + 1, CoordMap.add c n comp, IntMap.add n c decomp))
      (0, CoordMap.empty, IntMap.empty) cs in
    (n, (fun c -> CoordMap.find c comp), (fun n -> IntMap.find n decomp))
end

(* sample code *)

module CC = CoordComp (struct
  type t = int
  let compare = compare
end)

let l = [100; 200; 300; 400]
let n, f, g = CC.compress l;;
List.map f l;;
Array.init n g;;

let l = [100; 100; 100]
let n, f, g = CC.compress l;;
List.map f l;;
Array.init n g;;
