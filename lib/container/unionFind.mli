module Make (Set : sig
  type t
  val union : t -> t -> t
end)
: sig
  type t
  (* 与えられた集合から素集合データ構造を作る *)
  val make : Set.t -> t
  (* 与えられた素集合同士が同一かどうか判定する *)
  val equal : t -> t -> bool
  (* 与えられた素集合同士を合併する *)
  val unite : t -> t -> unit
  (* 与えられた素集合に属する要素を列挙する *)
  val elements : t -> Set.t
end
