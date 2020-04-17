(* エラトステネスのふるい *)
let sieve :
  (* 素数かどうかを判定する自然数の上界 *)
  int ->
  (* 素数ならtrueを返す関数 *)
  (int -> bool)
  = fun n ->
    (*
     * 2の倍数は無視し，3, 5, 7, 9 ... が格納されているとみなす
     * よって添字から格納されている数へ変換する式は 2i + 3
     * 格納されている数から添字へ変換する式は (x - 3) / 2
     *)
    let m = n / 2 - 1 in
    let a = Array.make m true in
    for i = 0 to (int_of_float (sqrt @@ float_of_int n) - 3) / 2 do
      if a.(i) then begin
        let p = 2 * i + 3 in
        let rec loop i =
          if i < m then begin
            a.(i) <- false;
            loop (i + p)
          end in
        loop ((p * p - 3) / 2)
      end
    done;
    function 
      | 0 | 1 -> false
      | 2 -> true
      | n -> n mod 2 = 1 && a.((n - 3) / 2)

(* sample code *)

let p = sieve 100;;
List.init 100 (fun i -> i)
  |> List.filter p;;
let p = sieve 3;;
List.init 3 p;;
let p = sieve 4;;
List.init 4 p;;
let p = sieve 5;;
List.init 5 p;;
let p = sieve 6;;
List.init 6 p;;
