module WeightedDirectedGraph
  (Vertex : sig
    type t
    val compare : t -> t -> int
  end)
  (Weight : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  val dijkstra :
    (* 隣接リスト *)
    (Vertex.t -> (Vertex.t * Weight.t) list) ->
    (* 始点 *)
    Vertex.t ->
    (* 始点から辿り着けなければNoneを返す *)
    (Vertex.t -> Weight.t option)
end =
struct
  module WMap = Map.Make (Weight)
  module VMap = Map.Make (Vertex)

  let dijkstra e s =
    (* 始点sからの距離 *)
    (* d に入っていない頂点への距離は無限大とみなす *)
    let d = ref @@ VMap.singleton s Weight.zero in
    (* 優先度付きキュー *)
    let q = ref @@ WMap.singleton Weight.zero [s] in
    (* ダイクストラ法のメインループ *)
    let rec dijkstra_aux t =
      match WMap.min_binding !q with
      | exception Not_found ->
          (try Some (VMap.find t !d) with Not_found -> None)
      | (w, us) ->
          if
            (* 現時点で終点までの距離が分かっているか *)
            try Weight.compare (VMap.find t !d) w <= 0
            with Not_found -> false
          (* 既に終点までの距離が分かっているので返す *)
          then Some (VMap.find t !d)
          else begin
            (* 終点までの距離が分かっていないので，ダイクストラ法を続行 *)
            q := WMap.remove w !q;
            List.iter (fun u ->
              if 0 <= Weight.compare (VMap.find u !d) w then
                (* 未だ頂点uを訪れていない *)
                List.iter (fun (v, c) ->
                  let open Weight in
                  if
                    try 0 < Weight.compare (VMap.find v !d) (w + c)
                    with Not_found -> true  (* d.(v) は無限大 *)
                  then begin
                    d := VMap.add v (w + c) !d;
                    q := WMap.add (w + c) (v :: try WMap.find (w + c) !q with Not_found -> []) !q
                  end) (e u)) us;
            dijkstra_aux t
          end in dijkstra_aux
end

(* sample code *)

module Int = struct
  type t = int
  let compare = compare
end

module Float = struct
  type t = float
  let zero = 0.
  let ( + ) = ( +. )
  let compare = compare
end

module G = WeightedDirectedGraph (Int) (Float)

let e =
  [|[ (1, 7.); (2, 9.); (5, 14.) ];
    [ (0, 7.); (2, 10.); (3, 15.) ];
    [ (0, 9.); (1, 10.); (3, 11.); (5, 2.) ];
    [ (1, 15.); (2, 11.); (4, 6.) ];
    [ (3, 6.); (5, 9.) ];
    [ (0, 14.); (2, 2.); (4, 9.) ]|];;

Array.init 7 (G.dijkstra (fun u -> e.(u)) 0);;

module G' = WeightedDirectedGraph (Int)
  (struct
    type t = float * (string list -> string list)
    let zero = (0., fun xs -> xs)
    let ( + ) (c, f) (d, g) = (c +. d, fun xs -> f (g xs))
    let compare (c, _) (d, _) = compare c d
  end)

let e' =
  Array.mapi (fun u ->
    List.map (fun (v, c) ->
      let s = Printf.sprintf "%d->%d" u v in
      (v, (c, fun xs -> s :: xs)))) e;;

Array.map (fun (Some (c, f)) -> (c, f [])) @@ Array.init 6 (G'.dijkstra (fun u -> e'.(u)) 0);;

(* 無限グラフ!!! *)
module IntPair = struct
  type t = int * int
  let compare = compare
end

module G = WeightedDirectedGraph (IntPair) (Float)

let d = G.dijkstra
  (fun (x, y) ->
    List.map (fun v -> (v, float_of_int (abs x + abs y)))
    [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]) (0, 0);;
Array.init 10 @@ fun x ->
  Array.init 10 @@ fun y -> d (x, y);;
