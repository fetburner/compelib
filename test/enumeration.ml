let%test _ =
  List.of_seq (Compelib.Enumeration.perm 2 [1; 2; 3; 4]) =
  [[2; 1]; [3; 1]; [4; 1]; [1; 2]; [3; 2]; [4; 2];
   [1; 3]; [2; 3]; [4; 3]; [1; 4]; [2; 4]; [3; 4]]

let%test _ = List.of_seq (Compelib.Enumeration.perm 0 [1; 2; 3]) = [[]]

let%test _ =
  List.of_seq (Compelib.Enumeration.perm 3 [1; 2; 3]) =
  [[3; 2; 1]; [2; 3; 1]; [3; 1; 2];
   [1; 3; 2]; [2; 1; 3]; [1; 2; 3]]

let%test _ = List.of_seq (Compelib.Enumeration.perm 4 [1; 2; 3]) = []


let%test _ = List.of_seq (Compelib.Enumeration.comb 2 [1; 2; 3]) = [[2; 1]; [3; 1]; [3; 2]]
let%test _ = List.of_seq (Compelib.Enumeration.comb 3 [1; 2; 3]) = [[3; 2; 1]]
let%test _ = List.of_seq (Compelib.Enumeration.comb 0 [1; 2; 3]) = [[]]
let%test _ = List.of_seq (Compelib.Enumeration.comb 4 [1; 2; 3]) = []


let%test _ =
  List.of_seq (Compelib.Enumeration.repcomb 2 [1; 2; 3]) =
  [[1; 1]; [2; 1]; [3; 1]; [2; 2]; [3; 2]; [3; 3]]

let%test _ =
  List.of_seq (Compelib.Enumeration.repcomb 3 [1; 2; 3]) =
  [[1; 1; 1]; [2; 1; 1]; [3; 1; 1]; [2; 2; 1]; [3; 2; 1];
   [3; 3; 1]; [2; 2; 2]; [3; 2; 2]; [3; 3; 2]; [3; 3; 3]]

let%test _ = List.of_seq (Compelib.Enumeration.repcomb 0 [1; 2; 3]) = [[]]
let%test _ = List.of_seq (Compelib.Enumeration.repcomb 4 [1; 2; 3]) = [];;
