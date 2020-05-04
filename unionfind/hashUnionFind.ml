(* ハッシュテーブル版素集合データ構造 *)
module UnionFind = struct
  module Make
    (Elt : sig
      (* 集合の要素の型 *)
      type t
      val hash : t -> int
      val compare : t -> t -> int
    end) :
    sig
      type t
      module Class : sig
        (* 集合の識別子 *)
        type t
        val compare : t -> t -> int
      end
      (* n要素の素集合データ構造を作る *)
      val make : int -> t
      (* 要素がどの集合に属するか調べる *)
      val find : t -> Elt.t -> Class.t
      (* 与えられた集合同士を合併する
         破壊的操作であるため注意 *)
      val unite : t -> Class.t -> Class.t -> unit
      (* 与えられた集合に属する要素の数を求める *)
      val cardinal : t -> Class.t -> int
    end =
    struct
      module Hash = Hashtbl.Make (struct
        include Elt
        let equal i j = (Elt.compare i j = 0)
      end)

      type node = Leaf of int | Link of Elt.t
      type t = node Hash.t

      module Class = Elt

      let make = Hash.create

      let cardinal uf x =
        match Hash.find_opt uf x with
        | None -> 1
        | Some (Leaf x) -> x
        | Some (Link _) -> raise (Invalid_argument "cardinal")

      let rec find uf x =
        match Hash.find_opt uf x with
        | None | Some (Leaf _) -> x
        | Some (Link y) ->
            let z = find uf y in
            Hash.replace uf x (Link z); z

      let unite uf x y =
        if 0 <> Class.compare x y then begin
          let x, y =
            if cardinal uf x <= cardinal uf y then x, y else y, x in
          Hash.replace uf x (Leaf (cardinal uf x + cardinal uf y));
          Hash.replace uf y (Link x)
        end
    end
end

