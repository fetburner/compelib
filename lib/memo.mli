module F
  (Thunk : sig
    type t
    type elt
    val value : elt -> t
    val running : t
    val case : t -> value:(elt -> 'a) -> pending:(unit -> 'a) -> running:(unit -> 'a) -> 'a
  end)
  (Array : sig
    type t
    type key
    type elt = Thunk.t
    type size
    (* Thunk.pending で初期化された配列を作成する *)
    val make : size -> t
    val get : t -> key -> elt
    val set : t -> key -> elt -> unit
  end)
: sig
  type dom = Array.key
  type cod = Thunk.elt
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
