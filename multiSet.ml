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
end

module Make (EltMap : Map.S) : S with type elt = EltMap.key = struct
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
end
