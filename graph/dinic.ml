module FlowNetwork
  (* 頂点 *)
  (Vertex : Hashtbl.HashedType)
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
    (* 頂点の数（ハッシュテーブルを用いるので目安程度） *)
    int ->
    (* 各辺とその容量 *)
    (Vertex.t * Vertex.t * Flow.t) church_list ->
    (* 始点 *)
    Vertex.t ->
    (* 終点 *)
    Vertex.t ->
    (* 最大フロー *)
    Flow.t *
    (* 最大フローを流した際の各辺の流量 *)
    (Vertex.t * Vertex.t * Flow.t) church_list
end =
struct
  module VHash = Hashtbl.Make (Vertex)
  module G = DirectedGraph (VHash) (struct
    type t = int
    type edge = unit
    let nil = 0
    let snoc t _ = t + 1
  end)

  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }
  type edge = { to_ : Vertex.t; mutable capacity : Flow.t; is_rev : bool; rev : edge }

  let max_flow n es s t =
    assert (not (Vertex.equal s t));
    (* 逆辺を張りつつ，隣接リスト形式に変換 *)
    let adj = VHash.create n in
    es.fold (fun (u, v, c) () ->
      let rec edge = { to_ = v; capacity = c; is_rev = false; rev = rev_edge }
      and rev_edge = { to_ = u; capacity = Flow.zero; is_rev = true; rev = edge } in
      VHash.add adj u edge;
      VHash.add adj v rev_edge) ();

    (* 増加パスをDFSで探し，流せるだけ流していく *)
    let rec dfs iter level v f =
      if Vertex.equal v t
      then f
      else begin
        let rec find () =
          match VHash.find iter v with
          | exception Not_found -> Flow.zero
          | e ->
              VHash.remove iter v;
              let d =
                if Flow.compare e.capacity Flow.zero <= 0 || level e.to_ <= level v
                then Flow.zero
                else dfs iter level e.to_ @@
                  if Flow.compare f e.capacity <= 0 then f else e.capacity in
              if Flow.compare d Flow.zero <= 0
              then find ()
              else begin
                let open Flow in
                e.capacity <- e.capacity - d;
                e.rev.capacity <- e.rev.capacity + d; d
              end in 
        find ()
      end in

    let rec outer flow =
      let level = G.bfs n (fun v ->
        { G.fold = fun f ->
          List.fold_right (fun e acc ->
            if Flow.compare e.capacity Flow.zero <= 0
            then acc
            else f (e.to_, ()) acc) @@
          VHash.find_all adj v }) s in
      match level t with
      | None -> flow
      | Some _ ->
          let iter = VHash.copy adj in
          let rec inner flow =
            let f = dfs iter level s Flow.inf in
            if Flow.compare f Flow.zero <= 0
            then flow
            else let open Flow in inner (flow + f) in
          outer (inner flow) in

    let f = outer Flow.zero in
    let open Flow in
    (f, { fold = fun f ->
      VHash.fold (fun u e acc ->
        if e.is_rev
        then acc
        else f (u, e.to_,  e.rev.capacity) acc) adj })
end;;

(* 蟻本p. 188のグラフで試す *)
module G = FlowNetwork
(struct
  type t = int
  let equal = ( = )
  let hash = Hashtbl.hash
end)
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
    [ (0, 1, 10); (0, 2, 2);
      (1, 2, 6); (1, 3, 6);
      (2, 4, 5);
      (3, 2, 3); (3, 4, 8) ] } 0 4;;
e.fold List.cons [];;
