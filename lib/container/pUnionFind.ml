module F
  (Elt : sig
    type t
    val compare : t -> t -> int
  end)
  (Map : sig
    type key = Elt.t
    type size
    type 'a t
    val make : size -> 'a t
    val find : key -> 'a t -> 'a
    val add : key -> 'a -> 'a t -> 'a t
  end)
= struct
  type node = Leaf of int | Link of Elt.t

  type elt = Elt.t
  type dom = Map.size
  type t = node Map.t ref

  module Class = Elt

  let make n = ref @@ Map.make n

  let cardinal x uf =
    match Map.find x !uf with
    | exception Not_found -> 1
    | Leaf x -> x
    | Link _ -> raise (Invalid_argument "cardinal")

  let rec find x uf =
    match Map.find x !uf with
    | exception Not_found | Leaf _ -> x
    | Link y ->
        let z = find y uf in
        uf := Map.add x (Link z) !uf; z

  let union x y uf =
    if Class.compare x y = 0
    then uf
    else begin
      let x, y = 
        if cardinal x uf <= cardinal y uf then x, y else y, x in
      ref @@
      Map.add y (Link x) @@
      Map.add x (Leaf (cardinal x uf + cardinal y uf)) !uf
    end
end
