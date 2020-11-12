module F
  (Vertices : sig
    type t
    type vertex
    val iter : (vertex -> unit) -> t -> unit
  end)
  (Weight : sig
    type t
    val min : t -> t -> t
    val ( + ) : t -> t -> t
    val is_finite : t -> bool
  end)
  (SquareMatrix : sig
    type t
    type elt = Weight.t
    type key = Vertices.vertex
    val get : t -> key -> key -> elt
    val set : t -> key -> key -> elt -> unit
  end)
= struct
  type vertex = Vertices.vertex
  type weight = Weight.t
  type vertices = Vertices.t
  type adjacency_matrix = SquareMatrix.t

  let shortest_path n d =
    Fun.flip Vertices.iter n @@ fun i ->
      Fun.flip Vertices.iter n @@ fun j ->
        let dji = SquareMatrix.get d j i in
        Fun.flip Vertices.iter n @@ fun k ->
          let dik = SquareMatrix.get d i k in
          if Weight.is_finite dji && Weight.is_finite dik then
            let open Weight in
            SquareMatrix.set d j k @@ min (dji + dik) @@ SquareMatrix.get d j k
end
