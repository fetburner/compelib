(* 素集合データ構造 *)
module UnionFind : sig
  type t

  (* 1要素だけを含む集合を作る *)
  val make : unit -> t
  (* 与えられた集合同士が同一かどうか判定する *)
  val equal : t -> t -> bool
  (* 与えられた集合同士を合併する *)
  val unite : t -> t -> unit
  (* 与えられた集合に属する要素の数を求める *)
  val cardinal : t -> int
end = struct
  (* type t = node ref
     and node = Root int | Link t
     みたいなのがやりたくて，OCamlの実行時表現はintとそれ以外を区別できるので
     省メモリのためにこうしている *)
  type t = Obj.t ref

  let make () = ref (Obj.repr 1)

  let rec find uf =
    if Obj.is_int !uf
    then uf
    else begin
      let root = find (Obj.magic !uf) in
      uf := Obj.repr root; root
    end

  let equal uf uf' = find uf == find uf'

  let unite uf uf' =
    let root = find uf in
    let root' = find uf' in
    if root != root' then begin
      let card : int = Obj.magic !root in
      let card' : int = Obj.magic !root' in
      let root, root' =
        if card <= card'
        then root, root'
        else root', root in
      root := Obj.repr (card + card');
      root' := Obj.repr root
    end

  let cardinal uf = Obj.magic @@ ( ! ) @@ find uf
end
