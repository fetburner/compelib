(* フロイドの循環検出アルゴリズム *)
val detect_cycle :
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
