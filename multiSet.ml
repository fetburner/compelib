(* 多重集合 *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    (* 空集合 *)
    val empty : t
    (* 要素数 *)
    val cardinal : t -> int
    (* 要素xをn個追加 *)
    val add : elt -> int -> t -> t
    (* 要素xをn個削除 要素がn個以上存在しない場合は全て削除 *)
    val remove : elt -> int -> t -> t
    (* 要素xの数を数える *)
    val count : elt -> t -> int
    (* xより小さい要素の数を数える *)
    val count_lt : elt -> t -> int
    (* xより大きい要素の数を数える *)
    val count_gt : elt -> t -> int
    (* 昇順に見た時のn番目の要素 範囲外ならNot_foundを投げる *)
    val nth_inc : int -> t -> elt
    (* 降順に見た時のn番目の要素 範囲外ならNot_foundを投げる *)
    val nth_dec : int -> t -> elt
    (* 昇順に見てn要素を取り出す n要素に満たなければ全ての要素を昇順に列挙する *)
    val take_inc : int -> t -> elt list
    (* 降順に見てn要素を取り出す n要素に満たなければ全ての要素を降順に列挙する *)
    val take_dec : int -> t -> elt list
    (* 全ての要素を昇順に列挙する *)
    val elements_inc : t -> elt list
    (* 全ての要素を降順に列挙する *)
    val elements_dec : t -> elt list
    (* 昇順に要素を畳み込む *)
    val fold_inc : ('a -> elt -> 'a) -> 'a -> t -> 'a
    (* 降順に要素を畳み込む *)
    val fold_dec : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (* 最小の要素 空集合が与えられた場合はNot_foundを投げる *)
    val min_elt : t -> elt
    (* 最大の要素 空集合が与えられた場合はNot_foundを投げる *)
    val max_elt : t -> elt
  end

module Make (Ord : OrderedType) : S with type elt = Ord.t =
  struct
    type elt = Ord.t

    (* MapからパクったのでAVL木の変種（高さの差を2以下まで許容する） *)
    type t =
      | Empty
      | Node of body
    and body =
      { left : t;
        data : elt;
        count : int;
        right : t;
        height : int;
        cardinal : int }

    let empty = Empty

    let height = function
      | Empty -> 0
      | Node { height } -> height

    let cardinal = function
      | Empty -> 0
      | Node { cardinal } -> cardinal

    let create ~left ~data ~count ~right =
      Node
        { left; data; count; right;
          height = 1 + max (height left) (height right);
          cardinal = count + cardinal left + cardinal right }

    let balance ~left:l ~data:x ~count:d ~right:r =
      let hl = height l in
      let hr = height r in
      if hl > hr + 2 then begin
        match l with
        | Empty -> invalid_arg "MultiSet.balance" | Node { left = ll; data = lv; count = ld; right = lr } ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
              | Empty -> invalid_arg "MultiSet.balance"
              | Node { left = lrl; data = lrv; count = lrd; right = lrr } ->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
        | Empty -> invalid_arg "MultiSet.balance"
        | Node { left = rl; data = rv; count = rd; right = rr } ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
              | Empty -> invalid_arg "MultiSet.balance"
              | Node { left = rll; data = rlv; count = rld; right = rlr } ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else create l x d r

    let rec add x n = function
      | Empty -> create Empty x 1 Empty
      | Node { left; data = y; count; right } ->
          match Ord.compare x y with
          | 0 ->
              create left x (count + n) right
          | c when c < 0 ->
              balance (add x n left) y count right
          | _ ->
              balance left y count (add x n right)

    (* 最小の要素とその数を返す *)
    let rec count_min_elt = function
      | Empty -> raise Not_found
      | Node { left = Empty; data; count } -> (data, count)
      | Node { left } -> count_min_elt left

    (* 最小の要素を全て削除する *)
    let rec clear_min_elt = function
      | Empty -> invalid_arg "MultiSet.clear_min_elt"
      | Node { left = Empty; right } -> right
      | Node { left; data; count; right } -> balance (clear_min_elt left) data count right

    let merge t1 t2 =
      match t1, t2 with
      | Empty, t
      | t, Empty -> t
      | Node _, Node _ ->
          let (x, c) = count_min_elt t2 in
          balance t1 x c (clear_min_elt t2)

    let rec remove x n = function
      | Empty -> Empty
      | Node { left; data = y; count; right } ->
          match Ord.compare x y with
          | 0 ->
              if count <= n then
                merge left right
              else
                create left y (count - n) right
          | c when c < 0 ->
              balance (remove x n left) y count right
          | _ ->
              balance left y count (remove x n right)

    let rec count x = function
      | Empty -> 0
      | Node { left; data = y; count = n; right } ->
          match Ord.compare x y with
          | 0 -> n
          | c when c < 0 ->
              count x left
          | _ ->
              count x right

    let rec count_lt x = function
      | Empty -> 0
      | Node { left; data = y; count; right } ->
          match Ord.compare x y with
          | 0 ->
              cardinal left
          | c when c < 0 ->
              count_lt x left
          | _ ->
              count_lt x right + count + cardinal left

    let rec count_gt x = function
      | Empty -> 0
      | Node { left; data = y; count; right } ->
          match Ord.compare x y with
          | 0 ->
              cardinal right
          | c when 0 < c ->
              count_gt x right
          | _ ->
              count_gt x left + count + cardinal right

    let rec nth_inc n = function
      | Empty -> raise Not_found
      | Node { left; data; count; right } ->
          if n < cardinal left then
            nth_inc n left
          else if n < cardinal left + count then
            data
          else
            nth_inc (n - cardinal left - count) right

    let rec nth_dec n = function
      | Empty -> raise Not_found
      | Node { left; data; count; right } ->
          if n < cardinal right then
            nth_dec n right
          else if n < cardinal right + count then
            data
          else
            nth_dec (n - cardinal right - count) left

    let rec elements_inc acc = function
      | Empty -> acc
      | Node { left; data; count; right } ->
          elements_inc
            (Array.fold_left (fun acc x -> x :: acc)
              (elements_inc acc right)
              (Array.make count data)) left

    let rec elements_dec acc = function
      | Empty -> acc
      | Node { left; data; count; right } ->
          elements_dec
            (Array.fold_left (fun acc x -> x :: acc)
              (elements_dec acc left)
              (Array.make count data)) right

    let rec take_inc n = function
      | Empty -> []
      | Node { left; data; count; right } ->
          if n < cardinal left then
            take_inc n left
          else if n < cardinal left + count then
            elements_inc (Array.to_list (Array.make (n - cardinal left) data)) left
          else
            elements_inc
              (Array.fold_left (fun acc x -> x :: acc)
                (take_inc (n - cardinal left - count) right)
                (Array.make count data)) left

    let rec take_dec n = function
      | Empty -> []
      | Node { left; data; count; right } ->
          if n < cardinal right then
            take_dec n right
          else if n < cardinal right + count then
            elements_dec (Array.to_list (Array.make (n - cardinal right) data)) right
          else
            elements_dec
              (Array.fold_left (fun acc x -> x :: acc)
                (take_dec (n - cardinal right - count) left)
                (Array.make count data)) right

    let elements_inc = elements_inc []
    let elements_dec = elements_dec []

    let rec fold_inc f acc = function
      | Empty -> acc
      | Node { left; data; count; right } ->
          fold_inc f
            (Array.fold_left f
              (fold_inc f acc left)
              (Array.make count data)) right

    let rec fold_dec f acc = function
      | Empty -> acc
      | Node { left; data; count; right } ->
          fold_dec f
            (Array.fold_right f
              (Array.make count data)
              (fold_dec f acc right)) left
    let fold_dec f t acc = fold_dec f acc t

    let rec min_elt t = fst (count_min_elt t)

    let rec max_elt = function
      | Empty -> raise Not_found
      | Node { data; right = Empty } -> data
      | Node { right } -> max_elt right
  end
