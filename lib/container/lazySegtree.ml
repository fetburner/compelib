module F
  (M : sig
    type t
    val e : t
    val op : t -> t -> t
  end)
  (F : sig
    type t
    type dom = M.t
    type cod = M.t
    val id : t
    val comp : t -> t -> t
    val apply : t -> dom -> cod
  end)
= struct
  type elt = M.t
  type map = F.t
  type t = { n : int; size : int; log : int; d : elt array; lz : map array }

  let update d k =
    d.(k) <- M.op d.(k lsl 1) d.(k lsl 1 + 1)

  let all_apply { size; d; lz; _ } k f =
    d.(k) <- F.apply f d.(k);
    if k < size then lz.(k) <- F.comp f lz.(k)

  let push t k =
    all_apply t (k lsl 1) t.lz.(k);
    all_apply t (k lsl 1 + 1) t.lz.(k);
    t.lz.(k) <- F.id

  let of_list l =
    let n = List.length l in
    let log = Bits.ceil_log2 n in
    let size = 1 lsl log in
    let d = Array.make (size lsl 1) M.e in
    let lz = Array.make size F.id in
    List.iteri (fun i -> Array.set d (size + i)) l;
    for i = size - 1 downto 1 do
      update d i
    done;
    { n; size; log; d; lz }

  let init n f =
    let log = Bits.ceil_log2 n in
    let size = 1 lsl log in
    let d = Array.make (size lsl 1) M.e in
    let lz = Array.make size F.id in
    for i = 0 to n - 1 do
      d.(size + i) <- f i
    done;
    for i = size - 1 downto 1 do
      update d i
    done;
    { n; size; log; d; lz }

  let get i ({ n; size; log; d; _ } as t) =
    assert (0 <= i && i < n);
    let i = i + size in
    for j = log downto 1 do
      push t (i lsr j)
    done; d.(i)

  let query l r ({ n; size; log; d; _} as t) =
    assert (0 <= l && l <= r && r <= n);
    if l = r
    then M.e
    else begin
      let l = l + size in
      let r = r + size in
      for i = log downto 1 do
        (if ((l lsr i) lsl i) <> l then push t (l lsr i));
        (if ((r lsr i) lsl i) <> r then push t (r lsr i))
      done;
      let rec query_aux l r sml smr =
        if r <= l
        then M.op sml smr
        else
          let l, sml = if l land 1 = 0 then l, sml else l + 1, M.op sml d.(l) in
          let r, smr = if r land 1 = 0 then r, smr else r - 1, M.op d.(r - 1) smr in
          query_aux (l lsr 1) (r lsr 1) sml smr in
      query_aux l r M.e M.e
    end

  let update_range l r f ({ n; size; log; d; _} as t) =
    assert (0 <= l && l <= r && r <= n);
    if l < r then begin
      let l = l + size in
      let r = r + size in
      for i = log downto 1 do
        (if ((l lsr i) lsl i) <> l then push t (l lsr i));
        (if ((r lsr i) lsl i) <> r then push t (r lsr i))
      done;
      let rec update_range_aux l r =
        if l < r then begin
          let l = if l land 1 = 0 then l else (all_apply t l f; l + 1) in
          let r = if r land 1 = 0 then r else (all_apply t (r - 1) f; r - 1) in
          update_range_aux (l lsr 1) (r lsr 1)
        end in
      update_range_aux l r;
      for i = 1 to log do
        (if ((l lsr i) lsl i) <> l then update d (l lsr i));
        (if ((r lsr i) lsl i) <> r then update d ((r - 1) lsr i))
      done
    end

  let update i f ({ n; size; log; d; _ } as t) = 
    assert (0 <= i && i < n);
    let i = i + size in
    for j = log downto 1 do
      push t (i lsr j)
    done;
    d.(i) <- f d.(i);
    for j = 1 to t.log do
      update d (i lsr j)
    done

  let set i x = update i @@ Fun.const x
end
