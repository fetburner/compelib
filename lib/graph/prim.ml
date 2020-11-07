module F
  (Weight : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end)
  (Heap : sig
    type t
    type elt
    type key = Weight.t
    val take_min_binding : t -> (key * elt) option
    val add : t -> key -> elt -> unit
  end)
  (Array : sig
    type t
    type key = Heap.elt
    type elt = Heap.key
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type weight = Array.elt
  type vertex = Array.key

  let minimum_spanning_tree q d es s =
    let rec prim acc u =
      Array.set d u Weight.zero;
      es u (fun v w ->
        if 0 < Weight.compare (Array.get d v) w then
          (Array.set d v w; Heap.add q w v));
      prim' acc
    and prim' acc =
      match Heap.take_min_binding q with
      | None -> acc
      | Some (w, u) ->
          if 0 < Weight.compare w (Array.get d u)
          then prim' acc
          else prim (Weight.( + ) acc w) u in
    prim Weight.zero s
end
