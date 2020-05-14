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
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  val max_flow :
    (* 頂点の数n *)
    int ->
    (* 各辺と逆辺の容量のリスト
       頂点は0からn-1までの整数でなくてはならない *)
    (int * int * Flow.t * Flow.t) church_list ->
    (* 始点 *)
    int ->
    (* 終点 *)
    int ->
    (* 最大フロー *)
    Flow.t *
    (* 最大フローを流した際の各辺の流量 *)
    (int * int * Flow.t) church_list
end =
struct
  module G = DirectedGraph.ByArray.Make (struct
    type t = int
    type edge = unit
    let nil = 0
    let snoc t _ = t + 1
  end)

  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }
  type edge = { from : int; to_ : int; mutable capacity : Flow.t; original_capacity : Flow.t; rev : edge }

  let max_flow n es s t =
    assert (s <> t);
    (* 逆辺を張りつつ，隣接リスト形式に変換 *)
    let adj = Array.make n [] in
    es.fold (fun (u, v, c, c') () ->
      let rec edge = { from = u; to_ = v; capacity = c; original_capacity = c; rev = rev_edge }
      and rev_edge = { from = v; to_ = u; capacity = c'; original_capacity = c'; rev = edge } in
      adj.(u) <- edge :: adj.(u);
      adj.(v) <- rev_edge :: adj.(v)) ();

    let iter = Array.make n [] in
    (* 増加パスをDFSで探し，流せるだけ流していく *)
    let rec dfs level v f =
      if v = t then f else
      let rec find () =
        match iter.(v) with
        | [] -> Flow.zero
        | e :: rest ->
            iter.(v) <- rest;
            let d =
              if Flow.compare e.capacity Flow.zero <= 0 || level e.to_ <= level v
              then Flow.zero
              else dfs level e.to_ @@
                     if Flow.compare f e.capacity <= 0 then f else e.capacity in
            if Flow.compare d Flow.zero <= 0
            then find ()
            else begin
              let open Flow in
              e.capacity <- e.capacity - d;
              e.rev.capacity <- e.rev.capacity + d; d
            end in
      find () in

    let rec outer flow =
      let level = G.bfs n (fun v ->
        { G.fold = fun f acc ->
          List.fold_left (fun acc e ->
            if Flow.compare e.capacity Flow.zero <= 0
            then acc
            else f (e.to_, ()) acc) acc adj.(v) }) s in
      match level t with
      | None -> flow
      | Some _ ->
          let rec inner flow =
            let f = dfs level s Flow.inf in
            if Flow.compare f Flow.zero <= 0
            then flow
            else let open Flow in inner (flow + f) in
          Array.iteri (Array.set iter) adj;
          outer (inner flow) in

    (outer Flow.zero, { fold = fun f ->
      Array.fold_right
        (Fun.flip (List.fold_left (fun acc e ->
          let open Flow in
          if 0 <= Flow.compare e.capacity e.original_capacity
          then acc
          else f (e.from, e.to_, e.original_capacity - e.capacity) acc))) adj })
end;;

(* 蟻本p. 188のグラフで試す *)
module G = FlowNetwork
(struct
  type t = int
  let inf = max_int
  let zero = 0
  let ( + ) = ( + )
  let ( - ) = ( - )
  let compare = compare
end);;

let (f, e) = G.max_flow 5
  { fold = fun f -> List.fold_right f 
    [ (0, 1, 10, 0); (0, 2, 2, 0);
      (1, 2, 6, 0); (1, 3, 6, 0);
      (2, 4, 5, 0);
      (3, 2, 3, 0); (3, 4, 8, 0) ] } 0 4;;
e.fold List.cons [];;
