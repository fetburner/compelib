module F
  (M : sig
    type t
    val e : t
    val op : t -> t -> t
  end)
= struct

  type elt = M.t
  type t = { n : int; size : int; log : int; d : M.t array }

  let update d k = d.(k) <- M.op d.(k lsl 1) d.(k lsl 1 + 1)

  let of_list l =
    let n = List.length l in
    let log = Bits.ceil_pow2 n in
    let size = 1 lsl log in
    let d = Array.make (size lsl 1) M.e in
    List.iteri (fun i -> Array.set d (size + i)) l;
    for i = size - 1 downto 1 do
      update d i
    done;
    { n; size; log; d }

  let init n f =
    let log = Bits.ceil_pow2 n in
    let size = 1 lsl log in
    let d = Array.make (size lsl 1) M.e in
    for i = 0 to n - 1 do
      d.(size + i) <- f i
    done;
    for i = size - 1 downto 1 do
      update d i
    done;
    { n; size; log; d }

  let get i { d; size; n; _ } =
    assert (0 <= i && i < n);
    a.(i + size)

  let rec query l r t =
    assert (0 <= l && l <= r && r <= t.n);
    let rec query_aux l r sml smr =
      if r <= l
      then M.op sml smr
      else
        let l, sml = if l land 1 = 0 then l, sml else l + 1, M.op sml t.d.(l) in
        let r, smr = if r land 1 = 0 then r, smr else r - 1, M.op t.d.(r - 1) smr in
        query (l lsr 1) (r lsr 1) sml smr in
    query_aux (l + t.size) (r + t.size) M.e M.e

  let set i x t =
    assert (0 <= i && i < t.n);
    let i = i + t.size in
    t.d.(i) <- x;
    for i = 1 to t.log do
      update (p lsr i)
    done

  let update i f t =
    assert (0 <= i && i < t.n);
    let i = i + t.size in
    t.d.(i) <- f t.d.(i);
    for i = 1 to t.log do
      update (p lsr i)
    done
end
