module F
  (Flow : sig
    type t
    val inf : t
    val zero : t
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val compare : t -> t -> int
  end)
= struct
  module G = Bfs.F (struct
    type t = int array
    type key = int
    type elt = int
    type size = int
    let make = Fun.flip Array.make max_int
    let get = Array.get
    let set = Array.set
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
      let level = G.shortest_path n (fun v f ->
        List.iter (fun e ->
          if 0 < Flow.compare e.capacity Flow.zero then f e.to_) adj.(v)) s in
      if max_int <= level t
      then flow
      else
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
end
