module F
  (Vertex : sig
    type t
    type universe
    val universe_fold : (t -> 'a -> 'a) -> universe -> 'a -> 'a
  end)
  (Array : sig
    type t
    type elt = bool
    type key = Vertex.t
    type size = Vertex.universe
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type vertex = Vertex.t
  type vertices = Vertex.universe

  let rec visit es vs v l =
    if Array.get vs v
    then l
    else (Array.set vs v true; v :: List.fold_right (visit es vs) (es v) l)

  let sort n es =
    let vs = Array.make n in
    Vertex.universe_fold (visit es vs) n []

  let scc n es =
    let vs = Array.make n in
    List.fold_right (fun v l ->
      if Array.get vs v
      then l
      else visit es vs v [] :: l) (sort n es) []
end
