module F
  (Elt : sig
    type t
    val compare : t -> t -> int
  end)
  (Map : sig
    type key = Elt.t
    type size
    type 'a t
    (* make n : 定義域 n で全要素が未定義の Map を作る *)
    val make : size -> 'a t
    (* 未定義の添字について呼び出した場合 Not_found を投げる *)
    val find : key -> 'a t -> 'a
    val add : key -> 'a -> 'a t -> 'a t
  end)
: sig
  type t
  type elt = Elt.t
  type dom = Map.size
  module Class : sig
    (* 集合の識別子 *)
    type t
    val compare : t -> t -> int
  end
  val make : dom -> t
  (* 要素がどの集合に属するか調べる *)
  val find : elt -> t -> Class.t
  (* 与えられた集合同士を合併する *)
  val union : Class.t -> Class.t -> t -> t
  (* 与えられた集合に属する要素の数を求める *)
  val cardinal : Class.t -> t -> int
end
