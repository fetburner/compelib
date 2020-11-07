module F
  (Weight : sig
    type t
    val compare : t -> t -> int
  end)
= struct
  module UF = UnionFind.Make (struct
    type t = unit
    let union _ _ = ()
  end)

  let kruskal n es f acc =
    let uf = Array.init n @@ fun _ -> UF.make () in
    List.fold_left (fun acc (u, v, w) ->
      if UF.equal uf.(u) uf.(v)
      then acc
      else (UF.unite uf.(u) uf.(v); f u v w acc)) acc @@
    List.sort (fun (_, _, w) (_, _, w') -> Weight.compare w w') es 
end
