module F
  (Weight : sig
    type t
    val equal : t -> t -> bool
  end)
  (Edge : sig
    type t
    type vertex
    type weight = Weight.t
    val source : t -> vertex
    val add_weight : t -> weight -> weight
  end)
  (Path : sig
    type t
    type edge = Edge.t
    val nil : t
    val empty : t
    val join : t -> t -> t
    val snoc : t -> edge -> t
  end)
  (Array : sig
    type t
    type key = Edge.vertex
    type elt = Path.t
    type size
    val make : size -> t
    val get : t -> key -> elt option
    val set : t -> key -> elt -> unit
  end)
= struct
  type edge = Edge.t
  type path = Path.t
  type weight = Edge.weight
  type vertex = Edge.vertex
  type vertices = Array.size

  let path_reconstruction n es s d =
    let ps = Array.make n in
    Array.set ps s Path.nil;
    let rec path v =
      match Array.get ps v with
      | Some p -> p
      | None ->
          let p = ref Path.empty in
          es v (fun e ->
            let u = Edge.source e in
            if Weight.equal (d v) (Edge.add_weight e (d u)) then
              p := Path.join !p (Path.snoc (path u) e));
          Array.set ps v !p; !p in
    path
end
