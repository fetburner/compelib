(* fold f init n = f^n(init) *)
let rec foldn f init = function
  | 0 -> init
  | n -> foldn f (f init) (n - 1)

let rec gcd n m =
  if m = 0 then n
  else gcd m (n mod m)
