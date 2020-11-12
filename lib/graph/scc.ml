module F
  (Vertices : sig
    type t
    type vertex
    val fold : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  end)
  (Array : sig
    type t
    type elt = bool
    type key = Vertices.vertex
    type size = Vertices.t
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type vertex = Vertices.vertex
  type vertices = Vertices.t

  let rec visit es vs v l =
    if Array.get vs v
    then l
    else (Array.set vs v true; v :: List.fold_right (visit es vs) (es v) l)

  let sort n es =
    let vs = Array.make n in
    Vertices.fold (visit es vs) n []

  let scc n es =
    let vs = Array.make n in
    List.fold_right (fun v l ->
      if Array.get vs v
      then l
      else visit es vs v [] :: l) (sort n es) []
end
