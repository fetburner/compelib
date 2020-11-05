let%test _ =
  Compelib.InversionNumber.sort_count compare 7 (List.to_seq [3; 1; 4; 1; 5; 9; 2]) @@
  fun _ invs xs -> invs = 7 && xs = [1; 1; 2; 3; 4; 5; 9]
