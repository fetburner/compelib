let rec popcnt acc = function
  | 0 -> acc
  | n -> popcnt (acc + n land 1) (n lsr 1)

let%test _ =
  Random.self_init ();
  List.for_all (fun n -> popcnt 0 n = Compelib.Bits.popcnt n) @@
  List.init 100000 @@ fun _ -> Random.bits ()

let%test _ =
  List.for_all (fun n ->
    let log = Compelib.Bits.ceil_log2 n in
    1 lsl (log - 1) < n && n <= 1 lsl log) @@
  List.init 100000 succ

let%test _ =
  List.for_all (fun n ->
    let log = Compelib.Bits.floor_log2 n in
    1 lsl log <= n && n < 1 lsl (log + 1)) @@ 
  List.init 100000 succ
