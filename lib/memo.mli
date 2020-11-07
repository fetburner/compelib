module F
  (Array : sig
    type t
    type key
    type elt
    type size
    (* None で初期化された配列を作成する *)
    val make : size -> t
    val get : t -> key -> elt option
    (* set a i x : 配列 a の i 番目の要素を Some x で更新する *)
    val set : t -> key -> elt -> unit
  end)
: sig
  type dom = Array.key
  type cod = Array.elt
  type size = Array.size

  (* メモ化付き不動点コンビネータ *)
  val memoize :
    size -> (* 計算結果を覚えておく配列のサイズ *)
    ((dom -> cod) -> dom -> cod) ->
    (dom -> cod)
  (* 継続渡しスタイルのメモ化付き不動点コンビネータ *)
  val memoize_cps :
    size -> (* 計算結果を覚えておく配列のサイズ *)
    ((dom -> (cod -> 'a) -> 'a) -> dom -> (cod -> 'a) -> 'a) ->
    (dom -> (cod -> 'a) -> 'a)
end
