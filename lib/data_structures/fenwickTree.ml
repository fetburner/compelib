(* 0-indexedなBIT *)
module FenwickTree = struct
  (* 可換モノイド *)
  module type CommutativeMonoid = sig
    type t
    val e : t
    val op : t -> t -> t
  end

  module Make (CM : CommutativeMonoid)
  : sig
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
  = struct
    type t = CM.t array

    let make n = Array.make n CM.e

    let initialize a =
      for i = 0 to Array.length a - 1 do
        if i lor (i + 1) < Array.length a then
          a.(i lor (i + 1)) <- CM.op a.(i) a.(i lor (i + 1))
      done

    let init n f =
      let a = Array.init n f in
      initialize a; a

    let of_list l =
      let a = Array.of_list l in
      initialize a; a

    let rec accumulate a i x =
      if i < Array.length a then begin
        a.(i) <- CM.op x a.(i);
        accumulate a (i lor (i + 1)) x
      end

    let rec query acc a i =
      if i < 0
      then acc
      else query (CM.op acc a.(i)) a ((i land (i + 1)) - 1)
    let query a i = query CM.e a (i - 1)
  end
end


