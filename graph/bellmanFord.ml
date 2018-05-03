module WeightedDirectedGraph
  (Weight : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  val bellman_ford :
    (* 頂点数 *)
    int ->
    (* 辺のリスト *)
    ('v * 'v * Weight.t) list ->
    (* 始点 *)
    'v ->
    ('v -> [ `Weight of Weight.t | `Inf | `NegInf ])
end =
struct
  let bellman_ford n es s =
    (* 距離を覚えるやつ *)
    (* d に入っていない頂点への距離は無限大とみなす *)
    let d = Hashtbl.create n in
    (* 負閉路に含まれる頂点を覚えるやつ *)
    (* 負閉路に含まれている iff neg に含まれる *)
    let neg = Hashtbl.create n in
    Hashtbl.replace d s Weight.zero;
    for i = 0 to 2 * n - 1 do
      List.iter (fun (u, v, c) ->
        (* c は u から v への辺の重さ *)
        (* d.(v) <- min d.(v) (d.(u) + c) *)
        let open Weight in
        match Hashtbl.find d u with
        | exception Not_found -> () (* d.(u) は無限大 *)
        | du ->
            (* d.(v) > d.(u) + c *)
            if
              try 0 < compare (Hashtbl.find d v) (du + c)
              with Not_found -> true (* d.(v) は無限大 *)
            then begin
              Hashtbl.replace d v (du + c);
              (* n 回目以降に変更が起こった場合，v までの経路に負閉路が含まれている *)
              if n - 1 <= i then
                Hashtbl.replace neg v ()
            end) es
    done;
    fun v ->
      if Hashtbl.mem neg v then `NegInf
      else try `Weight (Hashtbl.find d v) with Not_found -> `Inf
end

(* sample code *)

module G = WeightedDirectedGraph (struct
  type t = float
  let zero = 0.
  let ( + ) = ( +. )
  let compare = compare
end)

let d = G.bellman_ford 7
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.) ] 1;;
Array.init 8 d;;

let d = G.bellman_ford 7
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.); (7, 4, -1.) ] 1;;
Array.init 8 d;;

let d = G.bellman_ford 7
  [ (1, 2, 1.); (2, 3, 1.); (3, 7, 1.); (4, 5, -1.); (5, 6, -1.); (6, 4, -1.); (7, 4, -1.); (4, 7, 1.) ] 1;;
Array.init 8 d;;
