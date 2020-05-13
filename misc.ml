(* fold f init n = f^n(init) *)
let rec foldn f init = function
  | 0 -> init
  | n -> foldn f (f init) (n - 1)

let rec take n = function
  | [] -> []
  | x :: xs ->
      if n = 0 then []
      else x :: take (n - 1) xs

let rec floor_sqrt acc acc_x_2_x_r sq_acc_minus_z r sq_r =
  if r = 0 then acc
  else
    let sq_acc_minus_z' = sq_acc_minus_z + acc_x_2_x_r + sq_r in
    ( if sq_acc_minus_z' <= 0 then
        floor_sqrt (acc + r) ((acc_x_2_x_r lsr 1) + sq_r) sq_acc_minus_z'
      else
        floor_sqrt acc (acc_x_2_x_r lsr 1) sq_acc_minus_z) (r lsr 1) (sq_r lsr 2)
let floor_sqrt z = floor_sqrt 0 0 (~-z) (1 lsl 30) (1 lsl 60)

let rec fold_tournament dir f = function
  | [x] -> x
  | x :: xs ->
      fold_tournament (not dir) f @@
      match
        List.fold_left (fun (acc, prev) x ->
          match prev with
          | None -> (acc, Some x)
          | Some y -> ((if dir then f y x else f x y) :: acc, None)) ([], Some x) xs
      with 
      | (acc, None) -> acc
      | (acc, Some x) -> x :: acc
(* fold_tournament ( * ) [x1; x2; x3; x4; x5; x6; x7; x8 ... ] = (... (((x1 * x2) * (x3 * x4)) * ((x5 * x6) * (x7 * x8))) ...) * ( ... ) *)
let fold_tournament f xs = fold_tournament true f xs

let list_of_string s = List.init (String.length s) (String.get s)
let array_of_string s = Array.init (String.length s) (String.get s)

exception Exodus
let call_cc f =
  let return = ref (fun () -> raise Not_found) in
  try f (fun x -> (return := fun () -> x); raise Exodus)
  with Exodus -> !return ()
