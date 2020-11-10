module F
  (Vertex : sig
    type t
    val compare : t -> t -> int
  end)
  (Weight : sig
    type t
    val inf : t (* オーバーフローの恐れはないので，max_intとか突っ込んでも良い *)
    val zero : t
    val neg_inf : t (* オーバーフローの恐れはないので，min_intとか突っ込んでも良い *)
    val compare : t -> t -> int
  end)
  (Array : sig
    type t
    type key = Vertex.t
    type elt = Weight.t
    type size
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
    val size_to_int : size -> int
  end)
= struct
  type vertex = Vertex.t
  type weight = Weight.t
  type vertices = Array.size

  let bellman_ford n es s =
    let es = List.sort (fun (u, v, _) (u', v', _) ->
      match Vertex.compare u v, Vertex.compare u' v' with
      (* 自己辺同士は始点の小さい順に緩和 *)
      | 0, 0 -> Vertex.compare u u'
      (* 添字の小さい頂点から大きい頂点への辺同士は，始点の小さい順に緩和 *)
      | x, y when x < 0 && y < 0 -> Vertex.compare u u'
      (* 添字の大きい頂点から小さい頂点への辺同士は，始点の大きい順に緩和 *)
      | x, y when x > 0 && y > 0 -> Vertex.compare u u'
      (* 自己辺は添字の小さい頂点から大きい頂点への辺と一緒のタイミングで緩和する *)
      | 0, y when y < 0 ->
          begin match Vertex.compare u u' with
          | 0 -> -1
          | x -> x
          end
      | x, 0 when x < 0 ->
          begin match Vertex.compare u u' with
          | 0 -> 1
          | x -> x
          end
      (* 添字の小さい頂点から大きい頂点への辺の緩和が終わってから，
         添字の大きい頂点から小さい頂点への辺を緩和する *)
      | x, y when x < 0 && y > 0 -> -1
      | x, y when x > 0 && y < 0 -> 1
      | 0, y when y > 0 -> -1
      | x, 0 when x > 0 -> 1
      | _, _ -> failwith "bellman_ford") es in
    (* 距離を覚えるやつ *)
    let d = Array.make n in
    Array.set d s Weight.zero;
    (* 残りの反復回数 *)
    let i = ref (Array.size_to_int n) in
    (* 負閉路とみなす閾値 *)
    let th = succ @@ Array.size_to_int n lsr 1 in
    (* 更新が行われたか *)
    let is_modified = ref true in
    while 0 < !i && !is_modified do
      is_modified := false;
      List.iter (fun (u, v, c) ->
        let open Weight in
        (* 原点から到達できない頂点は更新しない *)
        let du = Array.get d u in
        if 0 < Weight.compare inf du then
          let dv = if 0 <= Weight.compare neg_inf du then neg_inf else c du in
          if 0 < Weight.compare (Array.get d v) dv then
            (is_modified := true; Array.set d v @@ if !i <= th then neg_inf else dv)) es;
        decr i
    done;
    Array.get d
end
