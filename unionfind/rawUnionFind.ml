(* 配列版素集合データ構造 *)
module UnionFind : sig
  type t

  module Class : sig
    type t
    val compare : t -> t -> int
  end

  (* n要素の素集合データ構造を作る *)
  val make : int -> t
  (* 要素がどの集合に属するか調べる *)
  (* 要素は0からn-1の整数である必要がある *)
  val find : t -> int -> Class.t
  (* 与えられた集合同士を合併する *)
  (* 破壊的操作であるため注意 *)
  val unite : t -> Class.t -> Class.t -> unit
  (* 与えられた集合に属する要素の数を求める *)
  val cardinal : t -> Class.t -> int
end = struct
  type t = int array

  module Class = struct
    type t = int
    let compare = compare
  end

  let make n = Array.make n (-1)

  let rec find uf x =
    if uf.(x) < 0 then x
    else begin
      let y = find uf uf.(x) in
      uf.(x) <- y; y
    end

  let unite uf x y =
    if x <> y then begin
      let x, y = if uf.(x) <= uf.(y) then x, y else y, x in
      uf.(x) <- uf.(x) + uf.(y);
      uf.(y) <- x
    end

  let cardinal uf x = ~- (uf.(x))
end
