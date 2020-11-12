module G = Compelib.WarshallFloyd.F
  (struct
    type t = int
    type vertex = int
    let iter f n =
      for i = 0 to n - 1 do f i done
  end)
  (struct
    type t = int
    let min = min
    let ( + ) = ( + )
    let is_finite = ( <> ) max_int
  end)
  (struct
    type t = int array array
    type key = int
    type elt = int
    let get d i j = d.(i).(j)
    let set d i j x = d.(i).(j) <- x
  end)

let d = Array.make_matrix 5 5 max_int

let () =
  for i = 0 to 4 do
    d.(i).(i) <- 0
  done;
  List.iter (fun (u, v, c) -> d.(u).(v) <- c)
  [ (0, 1, 4); (0, 4, 3);
    (1, 0, 4); (1, 2, 2);
    (2, 1, 2); (2, 3, 3); (2, 4, 2);
    (3, 2, 3); (3, 4, 7);
    (4, 0, 3); (4, 2, 2); (4, 3, 7) ];
  G.shortest_path 5 d

let%test _ =
  List.init 5 (fun i -> List.init 5 (fun j -> d.(i).(j))) =
  [[0; 4; 5; 8; 3];
   [4; 0; 2; 5; 4];
   [5; 2; 0; 3; 2];
   [8; 5; 3; 0; 5];
   [3; 4; 2; 5; 0]]
