(* 永続素集合データ構造 *)
module PersistentUnionFind :
  sig
    module type S =
      sig
        type t
        type elt
        module Class : sig
          (* 集合の識別子 *)
          type t
          val compare : t -> t -> int
        end
        (* 要素がどの集合に属するか調べる *)
        val find : elt -> t -> Class.t
        (* 与えられた集合同士を合併する *)
        val union : Class.t -> Class.t -> t -> t
        (* 与えられた集合に属する要素の数を求める *)
        val cardinal : Class.t -> t -> int
      end
    (* 永続ハッシュテーブルを用いた実装 *)
    module ByHashtbl : sig
      module Make :
        functor (Elt : Map.OrderedType) ->
        sig
          include S
          (* n要素が異なる集合に属した素集合データ構造を作成 *)
          val make : int -> t
        end with type elt = Elt.t
    end
    (* Mapを用いた実装 *)
    module ByMap : sig
      module Make :
        functor (Elt : Map.OrderedType) ->
        sig
          include S
          (* 全ての要素が異なる集合に属した素集合データ構造を作成 *)
          val make : unit -> t
        end with type elt = Elt.t
    end
  end
= struct
    module type S =
      sig
        type t
        type elt
        module Class : sig
          type t
          val compare : t -> t -> int
        end
        val find : elt -> t -> Class.t
        val union : Class.t -> Class.t -> t -> t
        val cardinal : Class.t -> t -> int
      end

    module Core
      (Elt : sig
        type t
        val compare : t -> t -> int
      end)
      (EMap : sig
        type 'a t
        val add : Elt.t -> 'a -> 'a t -> 'a t
        val find_opt : Elt.t -> 'a t -> 'a option
      end)
    = struct
      type node = Leaf of int | Link of Elt.t
      type elt = Elt.t
      type t = node EMap.t ref

      module Class = Elt

      let cardinal x uf =
        match EMap.find_opt x !uf with
        | None -> 1
        | Some (Leaf x) -> x
        | Some (Link _) -> raise (Invalid_argument "cardinal")

      let rec find x uf =
        match EMap.find_opt x !uf with
        | None | Some (Leaf _) -> x
        | Some (Link y) ->
            let z = find y uf in
            uf := EMap.add x (Link z) !uf; z

      let union x y uf =
        if Class.compare x y = 0
        then uf
        else begin
          let x, y = 
            if cardinal x uf <= cardinal y uf then x, y else y, x in
          ref @@
          EMap.add y (Link x) @@
          EMap.add x (Leaf (cardinal x uf + cardinal y uf)) !uf
        end
    end

    module ByHashtbl = struct
      module Make (Elt : Map.OrderedType) = struct
        include (Core (Elt) (struct
          type 'a t = (Elt.t, 'a) PHashtbl.t
          let add x y h = PHashtbl.replace h x y
          let find_opt x h = try Some (PHashtbl.find h x) with Not_found -> None
        end))

        let make n = ref (PHashtbl.create n)
      end
    end

    module ByMap = struct
      module Make (Elt : Map.OrderedType) = struct
        module EMap = Map.Make (Elt)
        include (Core (Elt) (EMap))
        let make () = ref EMap.empty
      end
    end
end