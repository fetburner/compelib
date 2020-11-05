module PArray : sig
  (* persistent array *)
  type 'a t

  val init : int -> (int -> 'a) -> 'a t
  val make : int -> 'a -> 'a t
  val of_list : 'a list -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
  (** [set a n x] returns a persistent array replacing element number [n] of [a] with [x].
     [a] is not modified. *)
  val length : 'a t -> int
  val to_list : 'a t -> 'a list
  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('b -> 'a -> 'a) -> 'b t -> 'a -> 'a
end = struct
  type 'a t = 'a data ref
  and 'a data = Arr of 'a array | Diff of int * 'a * 'a t

  let init n f = ref (Arr (Array.init n f))
  let make n v = ref (Arr (Array.make n v))
  let of_list l = ref (Arr (Array.of_list l))

  let rec reroot k = function
    | { contents = Arr a } -> k a
    | { contents = Diff (i, v, t') } as t ->
        reroot (fun a ->
          t' := Diff (i, a.(i), t);
          a.(i) <- v;
          k a) t'
  let reroot k = function
    | { contents = Arr a } -> k a
    | { contents = Diff (_, _, _) } as t ->
        reroot (fun a -> t := Arr a; k a) t

  let get t i = reroot (fun a -> a.(i)) t

  let set t i v =
    reroot (fun a ->
      if v = a.(i) then t
      else begin
        let result = ref (Arr a) in
        t := Diff (i, a.(i), result);
        a.(i) <- v;
        result
      end) t

  let length t = reroot Array.length t
  let to_list t = reroot Array.to_list t
  let iter f = reroot (Array.iter f)
  let iteri f = reroot (Array.iteri f)
  let fold_left f x = reroot (Array.fold_left f x)
  let fold_right f t x = reroot (fun a -> Array.fold_right f a x) t
end
