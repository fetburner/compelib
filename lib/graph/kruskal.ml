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

  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  let kruskal n es =
    { fold = fun f init ->
      let uf = Array.init n @@ fun _ -> UF.make () in
      List.fold_left (fun acc (u, v, w) ->
        if UF.equal uf.(u) uf.(v)
        then acc
        else (UF.unite uf.(u) uf.(v); f (u, v, w) acc)) init @@
      List.sort (fun (_, _, w) (_, _, w') -> Weight.compare w w') es }
end
