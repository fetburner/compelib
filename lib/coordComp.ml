(* 座標圧縮 *)
module F
  (* 座標の型 *)
  (Coord : sig
    type t
    val compare : t -> t -> int
  end)
= struct
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
  let compress cs = compress @@ List.sort Coord.compare cs
end
