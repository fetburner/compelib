module F
  (Weight : sig
    type t
    val equal : t -> t -> bool
  end)
  (Edge : sig
    type t
    type vertex
    type weight = Weight.t
    val source : t -> vertex
    val add_weight : t -> weight -> weight
  end)
  (Path : sig
    type t
    type edge = Edge.t
    val nil : t
    val empty : t
    val join : t -> t -> t
    val snoc : t -> edge -> t
  end)
  (Thunk : sig
    type t
    type elt = Path.t
    val value : elt -> t
    val running : t
    val case : t -> value:(elt -> 'a) -> pending:(unit -> 'a) -> running:(unit -> 'a) -> 'a
  end)
  (Array : sig
    type t
    type key = Edge.vertex
    type elt = Thunk.t
    type size

    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
= struct
  type edge = Edge.t
  type path = Path.t
  type weight = Edge.weight
  type vertex = Edge.vertex
  type vertices = Array.size

  exception ZeroCycle

  (* 実装としては完全にメモ化再帰だが，
     訪れた頂点が始点かどうかの判定を書きたくないので独自実装に走る *)
  let path_reconstruction n es s d =
    let ps = Array.make n in
    Array.set ps s (Thunk.value Path.nil);
    let rec path v =
      Thunk.case (Array.get ps v)
        ~value:Fun.id
        ~running:(fun () -> raise ZeroCycle)
        ~pending:(fun () ->
          Array.set ps v Thunk.running;
          let p = ref Path.empty in
          es v (fun e ->
            let u = Edge.source e in
            if Weight.equal (d v) (Edge.add_weight e (d u)) then
              p := Path.join !p (Path.snoc (path u) e));
          Array.set ps v (Thunk.value !p); !p) in
    path
end
