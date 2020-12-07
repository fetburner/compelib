module F
  (Vertex : sig
    type t
    val equal : t -> t -> bool
  end)
  (Flow : sig
    type t
    val inf : t
    val zero : t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val compare : t -> t -> int
  end)
  (Array : sig
    type 'a t
    type key = Vertex.t
    type size
    val make : size -> 'a -> 'a t
    val get : 'a t -> key -> 'a
    val set : 'a t -> key -> 'a -> unit
    val blit : 'a t -> 'a t -> unit
  end)
= struct
  type flow = Flow.t
  type vertex = Vertex.t
  type capacity = Flow.t
  type vertices = Array.size

  module G = Bfs.F (struct
    type t = int Array.t
    type key = vertex
    type elt = int
    type size = vertices
    let make = Fun.flip Array.make max_int
    let get = Array.get
    let set = Array.set
  end)

  type edge =
    { rev : edge;
      src : vertex;
      dst : vertex;
      mutable capacity : capacity }

  let max_flow n es ~src:s ~dst:t =
    assert (not (Vertex.equal s t));
    (* 逆辺を張りつつ，隣接リスト形式に変換 *)
    let adj = Array.make n [] in
    es (fun ~src:u ~dst:v c ->
      let rec edge = { src = u; dst = v; capacity = c; rev = rev_edge }
      and rev_edge = { src = v; dst = u; capacity = Flow.zero; rev = edge } in
      Array.set adj u (edge :: Array.get adj u);
      Array.set adj v (rev_edge :: Array.get adj v);
      fun () -> rev_edge.capacity);

    let iter = Array.make n [] in
    (* 増加パスをDFSで探し，流せるだけ流していく *)
    let rec dfs level v f =
      if Vertex.equal v t
      then f
      else
        let rec find () =
          match Array.get iter v with
          | [] -> Flow.zero
          | e :: es ->
              Array.set iter v es;
              if Flow.compare e.capacity Flow.zero <= 0 || level e.dst <= level v
              then find ()
              else
                let d = dfs level e.dst @@ if Flow.compare f e.capacity <= 0 then f else e.capacity in
                if Flow.compare d Flow.zero <= 0
                then find ()
                else (let open Flow in e.capacity <- e.capacity - d; e.rev.capacity <- e.rev.capacity + d; d) in
        find () in

    let rec outer flow =
      let level =
        G.shortest_path
          (module struct
            module Vertex = struct
              type t = vertex
              type set = vertices
              let universe = n
              let iter_adjacency v f =
                Fun.flip List.iter (Array.get adj v) @@ fun e ->
                  if 0 < Flow.compare e.capacity Flow.zero then f e.dst
            end
          end) s in
      if max_int <= level t
      then flow
      else
        let rec inner flow =
          let f = dfs level s Flow.inf in
          if Flow.compare f Flow.zero <= 0
          then flow
          else let open Flow in inner (flow + f) in
        Array.blit adj iter;
        outer (inner flow) in

    outer Flow.zero
end
