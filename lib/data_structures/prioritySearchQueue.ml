module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module Make (Key : OrderedType) (Priority : OrderedType)
: sig
  type key = Key.t
  type priority = Priority.t

  type t
  val is_empty : t -> bool
  val cardinal : t -> int
  val find_opt : key -> t -> priority option
  val mem : key -> t -> bool
  val min_binding_opt : t -> (key * priority) option
  val empty : t
  val singleton : key -> priority -> t
  val add : key -> priority -> t -> t
  val update : key -> (priority option -> priority option) -> t -> t
  val remove : key -> t -> t
  val remove_min_binding : t -> t
  val of_list : (key * priority) list -> t
  val fold : (key -> priority -> 'a -> 'a) -> t -> 'a -> 'a
  val bindings : t -> (key * priority) list
end = struct
  type key = Key.t
  type priority = Priority.t

  type loser_tree =
    | Start
    | LLoser of int * Key.t * Priority.t * loser_tree * Key.t * loser_tree
    | RLoser of int * Key.t * Priority.t * loser_tree * Key.t * loser_tree

  type t =
    | Void
    | Winner of Key.t * Priority.t * loser_tree * Key.t

  let is_empty = function
    | Void -> true
    | Winner (_, _, _, _) -> false

  let cardinal' = function
    | Start -> 0
    | LLoser (s, _, _, _, _, _)
    | RLoser (s, _, _, _, _, _) -> s

  let cardinal = function
    | Void -> 0
    | Winner (_, _, t, _) -> cardinal' t

  let omega = 4

  let left = function
    | Start -> raise (Invalid_argument "left")
    | LLoser (_, _, _, l, _, _)
    | RLoser (_, _, _, l, _, _) -> l

  let right = function
    | Start -> raise (Invalid_argument "right")
    | LLoser (_, _, _, _, _, r)
    | RLoser (_, _, _, _, _, r) -> r

  let max_key = function
    | Void -> raise (Invalid_argument "max_key")
    | Winner (_, _, _, m) -> m

  let lloser k p l m r = LLoser (1 + cardinal' l + cardinal' r, k, p, l, m, r)
  let rloser k p l m r = RLoser (1 + cardinal' l + cardinal' r, k, p, l, m, r)

  let lsingle_left k1 p1 t1 m1 = function
    | Start -> raise (Invalid_argument "lsingle_left")
    | LLoser (_, k2, p2, t2, m2, t3) ->
        if 0 <= Priority.compare p2 p1
        then lloser k1 p1 (rloser k2 p2 t1 m1 t2) m2 t3
        else lloser k2 p2 (lloser k1 p1 t1 m1 t2) m2 t3
    | RLoser (_, k2, p2, t2, m2, t3) ->
        rloser k2 p2 (lloser k1 p1 t1 m1 t2) m2 t3

  let rsingle_left k1 p1 t1 m1 = function
    | Start -> raise (Invalid_argument "rsingle_left")
    | LLoser (_, k2, p2, t2, m2, t3) ->
        rloser k1 p1 (rloser k2 p2 t1 m1 t2) m2 t3
    | RLoser (_, k2, p2, t2, m2, t3) ->
        rloser k2 p2 (rloser k1 p1 t1 m1 t2) m2 t3

  let lsingle_right k1 p1 t1 m2 t3 =
    match t1 with
    | Start -> raise (Invalid_argument "lsingle_right")
    | LLoser (_, k2, p2, t1, m1, t2) ->
        lloser k2 p2 t1 m1 (lloser k1 p1 t2 m2 t3)
    | RLoser (_, k2, p2, t1, m1, t2) ->
        lloser k1 p1 t1 m1 (lloser k2 p2 t2 m2 t3)

  let rsingle_right k1 p1 t1 m2 t3 =
    match t1 with
    | Start -> raise (Invalid_argument "rsingle_right")
    | LLoser (_, k2, p2, t1, m1, t2) ->
        lloser k2 p2 t1 m1 (rloser k1 p1 t2 m2 t3)
    | RLoser (_, k2, p2, t1, m1, t2) ->
        if 0 <= Priority.compare p2 p1
        then rloser k1 p1 t1 m1 (lloser k2 p2 t2 m2 t3)
        else rloser k2 p2 t1 m1 (rloser k1 p1 t2 m2 t3)

  let ldouble_left k1 p1 t1 m1 = function
    | Start -> raise (Invalid_argument "ldouble_left")
    | LLoser (_, k2, p2, t2, m2, t3) ->
        lsingle_left k1 p1 t1 m1 (lsingle_right k2 p2 t2 m2 t3)
    | RLoser (_, k2, p2, t2, m2, t3) ->
        lsingle_left k1 p1 t1 m1 (rsingle_right k2 p2 t2 m2 t3)

  let ldouble_right k1 p1 t1 m2 t3 =
    match t1 with
    | Start -> raise (Invalid_argument "ldouble_right")
    | LLoser (_, k2, p2, t1, m1, t2) ->
        lsingle_right k1 p1 (lsingle_left k2 p2 t1 m1 t2) m2 t3
    | RLoser (_, k2, p2, t1, m1, t2) ->
        lsingle_right k1 p1 (rsingle_left k2 p2 t1 m1 t2) m2 t3

  let rdouble_left k1 p1 t1 m1 = function
    | Start -> raise (Invalid_argument "rdouble_left")
    | LLoser (_, k2, p2, t2, m2, t3) ->
        rsingle_left k1 p1 t1 m1 (lsingle_right k2 p2 t2 m2 t3)
    | RLoser (_, k2, p2, t2, m2, t3) ->
        rsingle_left k1 p1 t1 m1 (rsingle_right k2 p2 t2 m2 t3)

  let rdouble_right k1 p1 t1 m2 t3 =
    match t1 with
    | Start -> raise (Invalid_argument "rdouble_right")
    | LLoser (_, k2, p2, t1, m1, t2) ->
        rsingle_right k1 p1 (lsingle_left k2 p2 t1 m1 t2) m2 t3
    | RLoser (_, k2, p2, t1, m1, t2) ->
        rsingle_right k1 p1 (rsingle_left k2 p2 t1 m1 t2) m2 t3

  let lbalance_left k p l m r =
    if cardinal' (left r) < cardinal' (right r)
    then lsingle_left k p l m r
    else ldouble_left k p l m r

  let lbalance_right k p l m r =
    if cardinal' (left l) > cardinal' (right l)
    then lsingle_right k p l m r
    else ldouble_right k p l m r

  let rbalance_left k p l m r =
    if cardinal' (left r) < cardinal' (right r)
    then rsingle_left k p l m r
    else rdouble_left k p l m r

  let rbalance_right k p l m r =
    if cardinal' (left l) > cardinal' (right l)
    then rsingle_right k p l m r
    else rdouble_right k p l m r

  let lbalance k p l m r =
    if cardinal' l + cardinal' r < 2
    then lloser k p l m r
    else if cardinal' r > omega * cardinal' l
    then lbalance_left k p l m r
    else if cardinal' l > omega * cardinal' r
    then lbalance_right k p l m r
    else lloser k p l m r

  let rbalance k p l m r =
    if cardinal' l + cardinal' r < 2
    then rloser k p l m r
    else if cardinal' r > omega * cardinal' l
    then rbalance_left k p l m r
    else if cardinal' l > omega * cardinal' r
    then rbalance_right k p l m r
    else rloser k p l m r

  let rec play t t' =
    match t, t' with
    | Void, _ -> t'
    | _, Void -> t
    | Winner (k, p, t, m), Winner (k', p', t', m') ->
        if 0 <= Priority.compare p' p
        then Winner (k, p, rbalance k' p' t m t', m')
        else Winner (k', p', lbalance k p t m t', m')

  let find_opt k =
    let rec find_opt_aux k' p' = function
      | Start ->
          if Key.compare k k' = 0
          then Some p'
          else None
      | RLoser (_, k'', p'', l, m, r) ->
          if 0 <= Key.compare m k
          then find_opt_aux k' p' l
          else find_opt_aux k'' p'' r
      | LLoser (_, k'', p'', l, m, r) ->
          if 0 <= Key.compare m k
          then find_opt_aux k'' p'' l
          else find_opt_aux k' p' r in
    function
      | Void -> None
      | Winner (k', p', t, _) -> find_opt_aux k' p' t

  let mem k t = Option.is_some @@ find_opt k t

  let min_binding_opt = function
    | Void -> None
    | Winner (k, p, _, _) -> Some (k, p)

  let empty = Void

  let singleton k p = Winner (k, p, Start, k)

  let add k p =
    let rec add_aux k' p' m' = function
      | Start ->
          let cmp = Key.compare k k' in
          if cmp < 0
          then play (singleton k p) (singleton k' p')
          else if cmp = 0
          then singleton k p
          else play (singleton k' p') (singleton k p)
      | RLoser (_, k'', p'', l, m, r) ->
          if 0 <= Key.compare m k
          then play (add_aux k' p' m l) (Winner (k'', p'', r, m'))
          else play (Winner (k', p', l, m)) (add_aux k'' p'' m' r)
      | LLoser (_, k'', p'', l, m, r) ->
          if 0 <= Key.compare m k
          then play (add_aux k'' p'' m l) (Winner (k', p', r, m'))
          else play (Winner (k'', p'', l, m)) (add_aux k' p' m' r) in
    function
      | Void -> singleton k p
      | Winner (k', p', t, m) -> add_aux k' p' m t

  let update k f =
    let exception Not_modified in
    let rec update_aux k' p' m' = function
      | Start ->
          let cmp = Key.compare k k' in
          if cmp < 0
          then
            begin match f None with
            | None -> raise Not_modified
            | Some p -> play (singleton k p) (singleton k' p')
            end
          else if cmp = 0
          then
            begin match f (Some p') with
            | None -> empty
            | Some p -> if p == p' then raise Not_modified else singleton k p
            end
          else
            begin match f None with
            | None -> raise Not_modified
            | Some p -> play (singleton k' p') (singleton k p)
            end
      | RLoser (_, k'', p'', l, m, r) ->
          if 0 <= Key.compare m k
          then play (update_aux k' p' m l) (Winner (k'', p'', r, m'))
          else play (Winner (k', p', l, m)) (update_aux k'' p'' m' r)
      | LLoser (_, k'', p'', l, m, r) ->
          if 0 <= Key.compare m k
          then play (update_aux k'' p'' m l) (Winner (k', p', r, m'))
          else play (Winner (k'', p'', l, m)) (update_aux k' p' m' r) in
    function
      | Void as t0 ->
          begin match f None with
          | None -> t0
          | Some p -> singleton k p
          end
      | (Winner (k', p', t, m')) as t0 ->
          try update_aux k' p' m' t with Not_modified -> t0

  let remove k =
    let exception Not_modified in
    let rec remove_aux k' p' m' = function
      | Start ->
          if Key.compare k k' = 0
          then empty
          else raise Not_modified
      | RLoser (_, k'', p'', l, m, r) ->
          if 0 <= Key.compare m k
          then play (remove_aux k' p' m l) (Winner (k'', p'', r, m'))
          else play (Winner (k', p', l, m)) (remove_aux k'' p'' m' r)
      | LLoser (_, k'', p'', l, m, r) ->
          if 0 <= Key.compare m k
          then play (remove_aux k'' p'' m l) (Winner (k', p', r, m'))
          else play (Winner (k'', p'', l, m)) (remove_aux k' p' m' r) in
    function
      | Void as t0 -> t0
      | (Winner (k', p', t, m')) as t0 ->
          try remove_aux k' p' m' t with Not_modified -> t0

  let rec second_best m' = function
    | Start -> Void
    | LLoser (_, k, p, l, m, r) -> play (Winner (k, p, l, m)) (second_best m' r)
    | RLoser (_, k, p, l, m, r) -> play (second_best m l) (Winner (k, p, r, m'))

  let remove_min_binding = function
    | Void as t -> t
    | Winner (_, _, t, m) -> second_best m t

  let of_list = List.fold_left (fun t (k, p) -> add k p t) empty

  let fold f =
    let rec fold_aux k p acc = function
      | Start -> f k p acc
      | RLoser (_, k', p', l, _, r) ->
          fold_aux k p (fold_aux k' p' acc r) l
      | LLoser (_, k', p', l, _, r) ->
          fold_aux k' p' (fold_aux k p acc r) l in
    fun t acc ->
      match t with
      | Void -> acc
      | Winner (k, p, t, _) -> fold_aux k p acc t

  let bindings t = fold (fun k p -> List.cons (k, p)) t []
end

