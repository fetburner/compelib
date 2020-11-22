module F
  (Array : sig
    type t
    type key
    type size
    type elt = int
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type vertex = Array.key
  type vertices = Array.size

  let lowest_common_ancestor n es s ds =
    let module ST = PSegtree.F (struct
      type t = vertex
      let op u v = if ds u <= ds v then u else v
    end) in
    let euler_tour = ref [] in
    let rec dfs v =
      euler_tour := v :: !euler_tour;
      es v @@ fun u ->
        dfs u;
        euler_tour := v :: !euler_tour in
    dfs s;
    let is = Array.make n in
    List.iteri (fun i v -> Array.set is v i) !euler_tour;
    let st = ST.of_list !euler_tour in
    fun u v ->
      let i = Array.get is u in
      let j = Array.get is v in
      ST.query (min i j) (1 + max i j) st
end
