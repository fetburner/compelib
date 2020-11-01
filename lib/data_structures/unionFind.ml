(* 素集合データ構造 *)
module UnionFind = struct
  module Make (Set : sig
    type t
    val union : t -> t -> t
  end) : sig
    type t
    (* 与えられた集合から素集合データ構造を作る *)
    val make : Set.t -> t
    (* 与えられた素集合同士が同一かどうか判定する *)
    val equal : t -> t -> bool
    (* 与えられた素集合同士を合併する *)
    val unite : t -> t -> unit
    (* 与えられた素集合に属する要素を列挙する *)
    val elements : t -> Set.t
  end = struct
    type t = node ref
    and node =
      | Root of int * Set.t (* 木の高さと要素の集合の二つ組 *)
      | Link of t

    let make s = ref @@ Root (0, s)

    let rec find uf =
      match !uf with
      | Root _ -> uf
      | Link parent ->
          let root = find parent in
          uf := Link root; root

    let equal uf uf' = find uf == find uf'

    let unite uf uf' =
      let root = find uf in
      let root' = find uf' in
      if root != root' then begin
        let Root (rank, elts) = !root in
        let Root (rank', elts') = !root' in
        let root, root' =
          if rank <= rank'
          then root, root'
          else root', root in
        root := Link root';
        root' := Root (rank' + (if rank = rank' then 1 else 0), Set.union elts elts')
      end

    let elements uf =
      match find uf with
      | { contents = Root (_, elts) } -> elts
  end
end


