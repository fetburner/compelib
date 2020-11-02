(* フロイドの循環検出アルゴリズム *)
let detect_cycle :
  (* 等しい頂点か判定をする関数 *)
  ('v -> 'v -> bool) ->
  (* 頂点を受け取って，次の頂点を返す関数 *)
  ('v -> 'v) ->
  (* 頂点を受け取って，2つ先の頂点を返す関数
     高速化したい時だけ特殊化するとよい *)
  ?f2:('v -> 'v) ->
  (* 循環検出を行うスタート地点 *)
  'v ->
  (* スタート地点から循環開始地点までの長さと，
     循環の長さの組 *)
  int * int
= fun ( = ) f ?(f2 = fun x -> f (f x)) s ->
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
      tortoise_and_hare 0 v v @@ fun l _ -> (m, l);;

(* test *)
detect_cycle ( = ) (function
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 6
  | 6 -> 7
  | 7 -> 2
  | x -> x) 0

