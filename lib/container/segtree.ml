module type Monoid = sig
  type t

  val e : t
  val op : t -> t -> t
end

module Make (M : Monoid) = struct
  type elt = M.t
  type t = elt array

  let make m = Array.make (2 * m) M.e
  let leaves segtree = Array.length segtree / 2

  let rec initialize_ancestors i j n d =
    if 0 < n then begin
      for k = 0 to n - 1 do
        d.(i + k) <- M.op d.(j + 2 * k) d.(j + 2 * k + 1)
      done;
      initialize_ancestors (i + n) i (n / 2) d
    end

  let of_list l =
    let segtree = make (List.length l) in
    List.iteri (Array.set segtree) l;
    initialize_ancestors (leaves segtree) 0 (leaves segtree / 2) segtree;
    segtree

  let init m f =
    let segtree = make m in
    for i = 0 to m - 1 do
      segtree.(i) <- f i
    done;
    initialize_ancestors m 0 (m / 2) segtree;
    segtree

  let product segtree = SegtreeQueries.product_iter M.e M.op (Array.get segtree) (leaves segtree)
  let upper_bound segtree l p = SegtreeQueries.upper_bound_iter M.e M.op p (Array.get segtree) (leaves segtree) l
  let lower_bound segtree r p = SegtreeQueries.lower_bound_iter M.e M.op p (Array.get segtree) (leaves segtree) r

  let rec update_ancestors i j k n d =
    if k < n then begin
      d.(i + k) <- M.op d.(j + 2 * k) d.(j + 2 * k + 1);
      update_ancestors (i + n) i (k / 2) (n / 2) d
    end
  let update segtree i f =
    segtree.(i) <- f segtree.(i);
    update_ancestors (leaves segtree) 0 (i / 2) (leaves segtree / 2) segtree

  let get = Array.get
  let set segtree i x = update segtree i @@ Fun.const x
end
