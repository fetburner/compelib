module type S = sig
  type t
  type elt
  module Class : sig
    (* 集合の識別子 *)
    type t
    val compare : t -> t -> int
  end
  (* 要素がどの集合に属するか調べる *)
  val find : elt -> t -> Class.t
  (* 与えられた集合同士を合併する *)
  val union : Class.t -> Class.t -> t -> t
  (* 与えられた集合に属する要素の数を求める *)
  val cardinal : Class.t -> t -> int
end

(* 永続ハッシュテーブルを用いた実装 *)
module ByHashtbl : sig
  module Make :
    functor (Elt : Map.OrderedType) ->
    sig
      include S
      (* n要素が異なる集合に属した素集合データ構造を作成 *)
      val make : int -> t
    end with type elt = Elt.t
end

(* Mapを用いた実装 *)
module ByMap : sig
  module Make :
    functor (Elt : Map.OrderedType) ->
    sig
      include S
      (* 全ての要素が異なる集合に属した素集合データ構造を作成 *)
      val make : unit -> t
    end with type elt = Elt.t
end
