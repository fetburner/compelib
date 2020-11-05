module Int = struct
  type t = int
  let zero = 0
  let ( + ) = ( + )
  let inf = max_int
  let neg_inf = min_int
  let compare = compare
end

module G = Compelib.WarshallFloyd.F (Int)

let d = G.warshall_floyd 5
  { G.fold = fun f -> List.fold_right f
  [ (0, 1, 4); (0, 4, 3);
    (1, 0, 4); (1, 2, 2);
    (2, 1, 2); (2, 3, 3); (2, 4, 2);
    (3, 2, 3); (3, 4, 7);
    (4, 0, 3); (4, 2, 2); (4, 3, 7) ] }

let%test _ =
  List.init 5 (fun i -> List.init 5 (d i)) =
  [[0; 4; 5; 8; 3];
   [4; 0; 2; 5; 4];
   [5; 2; 0; 3; 2];
   [8; 5; 3; 0; 5];
   [3; 4; 2; 5; 0]]
