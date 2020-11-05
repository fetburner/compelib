(* compute exponentiation *)
let rec power ( * ) e m n =
  if n <= 0 then e
  else power ( * ) (if n land 1 = 0 then e else m * e) (m * m) (n lsr 1);;
