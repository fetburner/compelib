(* 素集合データ構造 *)
module Make
  (Set : sig
    type t
    val union : t -> t -> t
  end)
= struct
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
      match !root, !root' with
      | Link _, _
      | _, Link _ -> failwith "unite"
      | Root (rank, elts), Root (rank', elts') ->
          let root, root' =
            if rank <= rank'
            then root, root'
            else root', root in
          root := Link root';
          root' := Root (rank' + (if rank = rank' then 1 else 0), Set.union elts elts')
    end

  let elements uf =
    match find uf with
    | { contents = Link _ } -> failwith "elements"
    | { contents = Root (_, elts) } -> elts
end

