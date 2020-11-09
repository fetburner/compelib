module F
  (Thunk : sig
    type t
    type elt
    val value : elt -> t
    val running : t
    val case : t -> value:(elt -> 'a) -> pending:(unit -> 'a) -> running:(unit -> 'a) -> 'a
  end)
  (Array : sig
    type t
    type key
    type elt = Thunk.t
    type size
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  exception InfiniteLoop

  type dom = Array.key
  type cod = Thunk.elt
  type size = Array.size

  let memoize n f =
    let dp = Array.make n in
    let rec get i =
      Thunk.case (Array.get dp i)
        ~value:Fun.id
        ~running:(fun () -> raise InfiniteLoop)
        ~pending:(fun () ->
          Array.set dp i Thunk.running;
          let x = f get i in
          Array.set dp i (Thunk.value x); x) in get

  let memoize_cps n f =
    let dp = Array.make n in
    let rec get i k =
      Thunk.case (Array.get dp i)
        ~value:k
        ~running:(fun () -> raise InfiniteLoop)
        ~pending:(fun () ->
          Array.set dp i Thunk.running;
          f get i @@ fun x ->
            Array.set dp i (Thunk.value x); k x) in get
end
