let%test _ =
  let n = 1000000000 in
  Float.abs (Compelib.Misc.power ( *. ) 1. (1. +. 1. /. float_of_int n) n -. 2.71828) <= 1e-5

let fib n =
  fst @@
  Compelib.Misc.power
    (fun (fn1, fn_1) (fm1, fm_1) ->
      let fnfm = (fn1 - fn_1) * (fm1 - fm_1) in
      (fn1 * fm1 + fnfm, fnfm + fn_1 * fm_1)) (1, 1) (1, 0) n

let%test _ = List.init 10 fib = [1; 1; 2; 3; 5; 8; 13; 21; 34; 55]
