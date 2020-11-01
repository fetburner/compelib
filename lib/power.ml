(* compute exponentiation *)
let rec power ( * ) e m n =
  if n <= 0 then e
  else power ( * ) (if n land 1 = 0 then e else m * e) (m * m) (n lsr 1)

(* test *)
(let n = 1000000000 in
power ( *. ) 1. (1. +. 1. /. float_of_int n) n)

let fib n =
  let
    ((a, _),
     (_, _)) =
    power
      (fun
        ((a, b),
         (c, d))
        ((a', b'),
         (c', d')) ->
           ((a * a' + b * c', a * b' + b * d'),
            (c * a' + d * c', c * b' + d * d')))
      ((1, 0),
       (0, 1))
      ((1, 1),
       (1, 0)) n in
  a;;
Array.init 10 fib;;
