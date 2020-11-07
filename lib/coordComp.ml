(* 座標圧縮 *)
module F
  (* 座標の型 *)
  (Coord : sig
    type t
    val compare : t -> t -> int
  end)
= struct
  let compress cs =
    let a =
      Array.of_list @@
      List.sort_uniq Coord.compare cs in
    let n = Array.length a in
    (n, (fun x -> Bsearch.upper_bound 0 n (fun i _ -> 0 <= Coord.compare x a.(i))), Array.get a)
end
