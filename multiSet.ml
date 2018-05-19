module type S = sig
  type elt
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val count : elt -> t -> int
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val clear : elt -> t -> t
  val cardinal : t -> int
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val max_elt : t -> elt
  val subset : t -> t -> bool
  val inter : t -> t -> t
end

module Make (E : Set.OrderedType) : S with type elt = E.t = struct
  module EltMap = Map.Make (E)
  type elt = EltMap.key
  type t = int EltMap.t

  let empty = EltMap.empty
  let is_empty = EltMap.is_empty
  let mem = EltMap.mem
  let count x m = try EltMap.find x m with Not_found -> 0
  let add x m = EltMap.add x (count x m + 1) m
  let remove x m =
    match EltMap.find x m with
    | 1 -> EltMap.remove x m
    | n -> EltMap.add x (n - 1) m
    | exception Not_found -> m
  let clear = EltMap.remove
  let cardinal m = EltMap.fold (fun _ -> ( + )) m 0
  let fold f = EltMap.fold (fun x n -> Array.fold_right f (Array.make n x))
  let max_elt m = fst (EltMap.max_binding m)
  let min_elt m = fst (EltMap.min_binding m)
  let subset m1 m2 = EltMap.for_all (fun x n1 -> n1 <= count x m2) m1
  let inter m1 m2 = EltMap.merge (fun _ o1 o2 ->
    match o1, o2 with
    | None, _ | _, None -> None
    | Some n, Some m -> Some (min n m)) m1 m2
end
