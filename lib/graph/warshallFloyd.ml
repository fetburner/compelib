module F
  (Vertex : sig
    type t
    type universe
    val universe_iter : (t -> unit) -> universe -> unit
  end)
  (Weight : sig
    type t
    val zero : t
    val self : t -> t
    val min : t -> t -> t
    val ( + ) : t -> t -> t
    val is_infinite : t -> bool
  end)
  (SquareMatrix : sig
    type t
    type key = Vertex.t
    type elt = Weight.t
    type size = Vertex.universe
    val make : size -> t
    val get : t -> key -> key -> elt
    val set : t -> key -> key -> elt -> unit
  end)
= struct
  type vertex = Vertex.t
  type weight = Weight.t
  type vertices = Vertex.universe

  let shortest_path n es =
    let d = SquareMatrix.make n in
    Vertex.universe_iter (fun u ->
      SquareMatrix.set d u u Weight.zero) n;
    es (fun u v c -> SquareMatrix.set d u v @@ min c @@ SquareMatrix.get d u v);
    Vertex.universe_iter (fun i ->
      let dii = Weight.self @@ SquareMatrix.get d i i in
      Vertex.universe_iter (fun j ->
        let dji = SquareMatrix.get d j i in
        if not (Weight.is_infinite dji) then
          let open Weight in
          let dji = dji + dii in
          Vertex.universe_iter (fun k ->
            let dik = SquareMatrix.get d i k in
            if not (Weight.is_infinite dik) then
              SquareMatrix.set d j k @@ min (dji + dik) @@ SquareMatrix.get d j k) n) n) n;
    SquareMatrix.get d
end
