module type CommutativeMonoid = sig
  type t
  val e : t
  val op : t -> t -> t
end

module Make (CM : CommutativeMonoid) : sig
  type t
  (* n要素の単位元からなるBITを作る *)
  val make : int -> t
  (* 添字iの要素にはf iが入ったn要素のBITを作る *)
  val init : int -> (int -> CM.t) -> t
  (* 与えられたリストの要素が入ったBITを作る *)
  val of_list : CM.t list -> t
  (* i番目の要素にxを加える *)
  val accumulate : t -> int -> CM.t -> unit
  (* 添字が[0, i)の範囲の要素を全て加えた値を求める *)
  val query : t -> int -> CM.t
end
