module F
  (Array : sig
    type t
    type key
    type elt
    type size
    val make : size -> t
    val get : t -> key -> elt option
    val set : t -> key -> elt -> unit
  end)
= struct
  type dom = Array.key
  type cod = Array.elt
  type size = Array.size

  let memoize n f =
    let dp = Array.make n in
    let rec get i =
      match Array.get dp i with
      | Some x -> x
      | None ->
          let x = f get i in
          Array.set dp i x; x in get

  let memoize_cps n f =
    let dp = Array.make n in
    let rec get i k =
      match Array.get dp i with
      | Some x -> k x
      | None ->
          f get i @@ fun x ->
            Array.set dp i x; k x in get
end
