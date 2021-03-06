module F
  (Int : sig
    type t
    val of_int : int -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
  end)
= struct
  let rec perm acc n k =
    if k <= 0
    then acc
    else perm (let open Int in acc * of_int n) (n - 1) (k - 1)
  let perm n k = if k < 0 || n < k then Int.of_int 0 else perm (Int.of_int 1) n k

  let comb n k =
    let rec comb_aux nCi k i =
      if k <= i
      then nCi
      else let open Int in comb_aux (nCi * of_int (n - i) / of_int (i + 1)) k (i + 1) in
    if k < 0 || n < k then Int.of_int 0 else comb_aux (Int.of_int 1) (min k (n - k)) 0

  let comb_memo n =
    let i = ref 0 in
    (* 組み合わせの数を覚えておく配列
       !i以下の要素は既に組み合わせの数をメモしている *)
    let a = Array.make (n / 2 + 1) (Int.of_int 1) in
    (* 途中までの計算結果をメモしつつ，組み合わせの数nCkを求める
       nを部分適用した関数を覚えておけば，途中までの計算結果はシェアされる *)
    let rec comb_memo_aux k =
      if k <= !i
      then a.(k)
      else begin
        incr i;
        let open Int in 
        a.(!i) <- a.(!i - 1) * of_int (n - !i + 1) / of_int !i;
        comb_memo_aux k
      end in
    fun k -> if k < 0 || n < k then Int.of_int 0 else comb_memo_aux @@ min k @@ n - k

  let repcomb n k = comb (n + k - 1) k
end
