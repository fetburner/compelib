let detect_cycle ( = ) f ?(f2 = fun x -> f (f x)) s =
  (* 一方を等速で，もう一方を倍速で動かしたときに，
     何ステップ後に再び出会うかと出会った頂点の組を返す *)
  let rec tortoise_and_hare acc u v k =
    tortoise_and_hare' (1 + acc) (f u) (f2 v) k
  and tortoise_and_hare' acc u v k =
    if u = v
    then k acc v
    else tortoise_and_hare acc u v k in
  (* 両方等速で動かした時に，何ステップ後に出会うかと
     出会った頂点の組を返す *)
  let rec tortoise_and_tortoise acc u v k =
    if u = v
    then k acc v
    else tortoise_and_tortoise (1 + acc) (f u) (f v) k in
  tortoise_and_hare 0 s s @@ fun _ u ->
    tortoise_and_tortoise 0 s u @@ fun m v ->
      tortoise_and_hare 0 v v @@ fun l _ -> (m, l)
