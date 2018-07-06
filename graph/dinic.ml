module FlowNetwork
  (* 流量 *)
  (Flow : sig
    type t
    val inf : t
    val zero : t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  val max_flow :
    (* 頂点の数（Hashtblを用いるので目安程度） *)
    int ->
    (* 各辺の容量 逆辺を含める必要はない *)
    ('v * 'v * Flow.t) list ->
    (* 始点 *)
    'v ->
    (* 終点 *)
    'v ->
    (* 最大フロー *)
    Flow.t
end =
struct
  module G = DirectedGraph (struct
    type t = int
    type edge = unit
    let nil = 0
    let snoc t _ = t + 1
  end)

  (* 増加パスをDFSで探し，流せるだけ流していく *)
  let rec dfs capacity add_edge iter level v t f =
    if v = t then f
    else begin
      let rec loop () =
        match Hashtbl.find iter v with
        | exception Not_found -> Flow.zero
        | (u, i) ->
            Hashtbl.remove iter v;
            (* capacity i <= 0 *)
            if Flow.compare (capacity i) Flow.zero <= 0 || level u <= level v then loop ()
            else begin
              let d = dfs capacity add_edge iter level u t @@
                (* min f (capacity i) *)
                if Flow.compare f (capacity i) <= 0 then f else capacity i in
              (* d <= 0 *)
              if Flow.compare d Flow.zero <= 0 then loop ()
              else (add_edge i d; d)
            end in
      loop ()
    end

  let max_flow n es s t =
    assert (s <> t);
    (* 各辺に流せる流量 *)
    let capacity = Array.make (2 * List.length es) Flow.zero in
    (* 逆辺を張りつつ，隣接リスト形式に変換 *)
    let adj = Hashtbl.create n in
    List.iteri (fun i (u, v, c) ->
      (* 各辺は番号で管理する *)
      Hashtbl.add adj u (v, 2 * i);
      (* 容量はcapacityを見てほしい *)
      capacity.(2 * i) <- c;
      (* 逆辺 *)
      Hashtbl.add adj v (u, 2 * i + 1);
      capacity.(2 * i + 1) <- Flow.zero) es;
    (* 番号iの辺にcだけフローを流す *)
    let add_edge i c =
      let open Flow in
      capacity.(i) <- capacity.(i) - c;
      (* 逆辺 *)
      capacity.(i lxor 1) <- capacity.(i lxor 1) + c in
    let rec loop flow =
      let level = G.bfs n (fun v ->
        List.map (fun (u, _) -> (u, ())) @@
        List.filter (fun (_, i) ->
          (* 0 < capacity.(i) *)
          Flow.compare Flow.zero capacity.(i) < 0) @@
          Hashtbl.find_all adj v) s in
      if level t = None then flow
      else
        let iter = Hashtbl.copy adj in
        let rec inner flow =
          let f = dfs (fun i -> capacity.(i)) add_edge iter level s t Flow.inf in
          if Flow.compare f Flow.zero <= 0 then flow
          else inner (Flow.( + ) flow f) in
        loop @@ inner flow in
    loop Flow.zero
end

(* 蟻本p. 188のグラフで試す *)
module G = FlowNetwork (struct
  type t = int
  let inf = max_int
  let zero = 0
  let ( + ) = ( + )
  let ( - ) = ( - )
  let compare = compare
end)

let 11 = G.max_flow 5
  [ (0, 1, 10); (0, 2, 2); (1, 2, 6); (1, 3, 6);
    (3, 2, 3); (3, 4, 8); (2, 4, 5) ] 0 4
