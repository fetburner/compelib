(* エラトステネスのふるい *)
let sieve :
  (* 素数かどうかを判定する自然数の上界 *)
  int ->
  (* 素数ならtrueを返す関数 *)
  (int -> bool)
  = fun n ->
    (* 今ふるおうとしている整数 *)
    let p = ref 3 in
    (* pがどのインデックスに対応するか *)
    let i = ref 0 in
    (* pの2乗 *)
    let pp = ref 9 in
    (* p^2がどのインデックスに対応するか *)
    let ii = ref 3 in
    (*
     * 2の倍数は無視し，3, 5, 7, 9 ... が格納されているとみなす
     * よって添字から格納されている数へ変換する式は 2i + 3
     * 格納されている数から添字へ変換する式は (x - 3) / 2
     *)
    let m = (n lsr 1) - 1 in
    let a = Array.make m true in
    let rec sieve_aux k =
      if k < !pp
      then a.((k - 3) lsr 1)
      else begin
        let rec mark i =
          if i < m then (a.(i) <- false; mark (i + !p)) in
        (if a.(!i) then mark !ii);
        incr i;
        p := !p + 2;
        pp := !pp + (!p lsl 1);
        ii := !ii + (!p lsl 1) - 2;
        sieve_aux k
      end in
    function
      | k when k <= 1 -> false
      | 2 -> true
      | k -> k land 1 = 1 && sieve_aux k;;


(* sample code *)

(* 呼び出しごとに途中結果がシェアされるので，こういうことしても平気 *)
List.filter (sieve 1000000) (List.init 1000000 Fun.id);;

(* 入力が小さい時の挙動も調べておきたい *)
List.init 3 (sieve 3);;
List.init 4 (sieve 4);;
List.init 5 (sieve 5);;
List.init 6 (sieve 6);;
