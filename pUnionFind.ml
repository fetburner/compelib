module PUnionFind
  (Index : sig
    type t
    val compare : t -> t -> int
  end) :
sig
  (* persistent union-find data structure *)
  type t
  type class_

  val make : int -> t
  val find : t -> Index.t -> class_
  val union : t -> Index.t -> Index.t -> t

  val compare_class : class_ -> class_ -> int
end =
struct
  type t =
    { rank : (Index.t, int) PHashtbl.t;
      mutable parent : (Index.t, Index.t) PHashtbl.t }
  type class_ = Index.t

  let make n = { rank = PHashtbl.create n; parent = PHashtbl.create n }

  let rec find parent i k =
    let j = try PHashtbl.find parent i with Not_found -> i in
    if Index.compare i j = 0 then k parent i
    else find parent j (fun parent r -> k (PHashtbl.replace parent i r) r)
  let find ({ parent } as h) i =
    find parent i (fun parent r -> h.parent <- parent; r)

  let union uf x y =
    let cx = find uf x in
    let cy = find uf y in
    if Index.compare cx cy = 0 then uf
    else begin
      let rx = try PHashtbl.find uf.rank x with Not_found -> 0 in
      let ry = try PHashtbl.find uf.rank y with Not_found -> 0 in
      if rx < ry then
        { uf with parent = PHashtbl.replace uf.parent cx cy }
      else
        { parent = PHashtbl.replace uf.parent cy cx;
          rank = PHashtbl.replace uf.rank cx (rx + 1) }
    end

  let compare_class = Index.compare
end
