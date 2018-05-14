(* 永続素集合データ構造 *)
module PUnionFindFn
  (* 永続ハッシュテーブルの実装 *)
  (PHashtbl : sig
    type 'a t
    type key
    val create : int -> 'a t
    val find : 'a t -> key -> 'a
    val replace : 'a t -> key -> 'a -> 'a t
    val compare_key : key -> key -> int
  end) :
sig
  type t
  type class_

  (* n要素の永続素集合データ構造を作るが，ハッシュなのでn要素以上も入れられる *)
  val make : int -> t
  val find : t -> PHashtbl.key -> class_
  val union : t -> PHashtbl.key -> PHashtbl.key -> t

  val compare_class : class_ -> class_ -> int
end =
struct
  type t =
    { rank : int PHashtbl.t;
      mutable parent : PHashtbl.key PHashtbl.t }
  type class_ = PHashtbl.key

  let make n = { rank = PHashtbl.create n; parent = PHashtbl.create n }

  let rec find parent i k =
    let j = try PHashtbl.find parent i with Not_found -> i in
    if PHashtbl.compare_key i j = 0 then k parent i
    else find parent j (fun parent r -> k (PHashtbl.replace parent i r) r)
  let find ({ parent } as h) i =
    find parent i (fun parent r -> h.parent <- parent; r)

  let union uf x y =
    let cx = find uf x in
    let cy = find uf y in
    if PHashtbl.compare_key cx cy = 0 then uf
    else begin
      let rx = try PHashtbl.find uf.rank x with Not_found -> 0 in
      let ry = try PHashtbl.find uf.rank y with Not_found -> 0 in
      match compare rx ry with
      | -1 -> { uf with parent = PHashtbl.replace uf.parent cx cy }
      | 1 ->  { uf with parent = PHashtbl.replace uf.parent cy cx }
      | 0 ->
          { parent = PHashtbl.replace uf.parent cy cx;
            rank = PHashtbl.replace uf.rank cx (rx + 1) }
    end

  let compare_class = PHashtbl.compare_key
end

(* 永続ハッシュテーブルによる素集合データ構造 *)
module PUnionFindHashtbl
  (Index : sig
    type t
    val compare : t -> t -> int
  end)
= PUnionFindFn
  (struct
    type 'a t = (Index.t, 'a) PHashtbl.t
    type key = Index.t
    let create n = PHashtbl.create n
    let find = PHashtbl.find
    let replace = PHashtbl.replace
    let compare_key = Index.compare
  end)

(* Mapによる素集合データ構造 *)
module PUnionFindMap
  (Index : sig
    type t
    val compare : t -> t -> int
  end)
= PUnionFindFn
  (struct
    module IMap = Map.Make (Index)
    type 'a t = 'a IMap.t
    type key = Index.t
    let create _ = IMap.empty
    let find m i = IMap.find i m
    let replace m i x = IMap.add i x m
    let compare_key = Index.compare
  end)
