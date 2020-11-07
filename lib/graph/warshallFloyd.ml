module F
  (Weight : sig
    type t
    val inf : t
    val zero : t
    val neg_inf : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end)
= struct
  type 'a church_list = { fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b }

  let make_adjmatrix n es =
    let d = Array.make_matrix n n Weight.inf in
    for v = 0 to n - 1 do
      d.(v).(v) <- Weight.zero
    done;
    es.fold (fun (u, v, c) () ->
      (* c < d.(u).(v) *)
      if 0 < Weight.compare d.(u).(v) c
      then d.(u).(v) <- c) (); d

  (* ワーシャルフロイド法の処理の本体 *)
  let raw_warshall_floyd n d =
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        (* 経路がない場合は更新しない *)
        if 0 < Weight.compare Weight.inf d.(j).(i) then
          for k = 0 to n - 1 do
            let open Weight in
            if 
              (* 経路がない場合は更新しない *)
              0 < Weight.compare Weight.inf d.(i).(k)
              (* d.(j).(i) + d.(i).(k) < d.(j).(k) *)
              && 0 < Weight.compare d.(j).(k) (d.(j).(i) + d.(i).(k))
            then d.(j).(k) <- d.(j).(i) + d.(i).(k)
          done
      done
    done;
    for i = 0 to n - 1 do
      (* 頂点iを通る負閉路がある *)
      if 0 < Weight.compare Weight.zero d.(i).(i) then
        for j = 0 to n - 1 do
          if 0 < Weight.compare Weight.inf d.(j).(i) then
            for k = 0 to n - 1 do
              (* 負閉路を通る経路があれば，最短距離を負の無限大で更新 *)
              if 0 < Weight.compare Weight.inf d.(i).(k) then
                d.(j).(k) <- Weight.neg_inf
          done
        done
    done

  let warshall_floyd n es =
    let d = make_adjmatrix n es in
    raw_warshall_floyd n d;
    fun u v -> try d.(u).(v) with _ -> Weight.inf
end
