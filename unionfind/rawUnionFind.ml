(* 配列版素集合データ構造 *)
module RawUnionFind : sig
  type t
  type class_

  (* n要素の素集合データ構造を作る *)
  val make : int -> t
  (* 要素がどの集合に属するか調べる *)
  (* 要素は0からn-1の整数である必要がある *)
  val find : t -> int -> class_
  (* 与えられた要素が属する集合同士を合併する *)
  val unite : t -> int -> int -> unit

  val compare_class : class_ -> class_ -> int
end = struct
  type t = { rank : int array; parent : int array }
  type class_ = int

  let make n =
    { rank = Array.make n 0;
      parent = Array.init n (fun c -> c) }

  let rec find uf x =
    if x = uf.parent.(x) then x
    else begin
      let y = find uf uf.parent.(x) in
      uf.parent.(x) <- y; y
    end

  let unite uf i j =
    let x = find uf i in
    let y = find uf j in
    if x <> y then begin
      if uf.rank.(x) < uf.rank.(y) then uf.parent.(x) <- y
      else begin
        uf.parent.(y) <- x;
        if uf.rank.(x) = uf.rank.(y) then
          uf.rank.(x) <- 1 + uf.rank.(x)
      end
    end

  let compare_class = compare
end

