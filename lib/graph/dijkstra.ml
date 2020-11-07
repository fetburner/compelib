module F
  (Weight : sig
    type t
    val zero : t
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

  let shortest_path q d es s =
    (* 始点への経路長を0にする *)
    Array.set d s Weight.zero;
    (* 既に最短距離が確定した辺へのクエリを高速化するため，
       ヒープの最小要素をメモしておく *)
    let min_binding = ref (Some (Weight.zero, s)) in
    let rec dijkstra t =
      match !min_binding with
      (* もう既に全ての頂点までの距離が分かっている *)
      | None -> Array.get d t
      | Some (w, u) ->
          match Array.get d t with
          (* 既に終点までの距離が分かっているので返す *)
          | x when 0 <= Weight.compare w x -> x
          (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
          | _ ->
              (* 未だ頂点uを訪れていない *)
              if 0 <= Weight.compare (Array.get d u) w then
                es u (fun v f ->
                  (* uからvに伸びる辺を通った際の経路長 *)
                  let c = f w in
                  if 0 < Weight.compare (Array.get d v) c then
                    (Heap.add q c v; Array.set d v c));
              min_binding := Heap.take_min_binding q;
              dijkstra t in
    dijkstra
end
