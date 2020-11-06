(* ストリームの先頭n要素をマージソートし，ソート前の転倒数を計算する
   ストリームの要素数がnに満たない場合は適当な例外を投げる *)
val sort_count :
  (* 要素の比較関数（compareみたいなの） *)
  ('a -> 'a -> int) ->
  (* 先頭から何個までの要素をソートするか *)
  int ->
  (* ソートする要素の入ったストリーム *)
  'a Seq.t ->
  (* 継続渡しスタイル *)
  ('a Seq.t -> (* 先頭からn要素を取り除いたストリーム *)
   int -> (* 転倒数 *)
   'a list -> (* ソート結果 *)
   'b) -> 'b