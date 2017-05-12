type t = { rank : int PArray.t; mutable parent : int PArray.t }
type class_ = int

let make n = { rank = PArray.make n 0; parent = PArray.init n (fun i -> i) }

let rec find parent i k =
  let j = PArray.get parent i in
  if i = j then k parent i
  else find parent j (fun parent r -> k (PArray.set parent i r) r)
let find ({ parent } as h) i = find parent i (fun parent r ->
  h.parent <- parent;
  r)

let union uf x y =
  let cx = find uf x in
  let cy = find uf y in
  if cx = cy then uf
  else begin
    let rx = PArray.get uf.rank x in
    let ry = PArray.get uf.rank y in
    match compare rx ry with
    | -1 -> { uf with parent = PArray.set uf.parent cx cy }
    | 1 -> { uf with parent = PArray.set uf.parent cy cx }
    | 0 -> { rank = PArray.set uf.rank cx (cx + 1); parent = PArray.set uf.parent cy cx }
  end

