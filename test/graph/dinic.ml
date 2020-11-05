(* 蟻本p. 188のグラフで試す *)
module G = Compelib.Dinic.FlowNetwork
(struct
  type t = int
  let inf = max_int
  let zero = 0
  let ( + ) = ( + )
  let ( - ) = ( - )
  let compare = compare
end)

let (f, e) = G.max_flow 5
  { fold = fun f -> List.fold_right f 
    [ (0, 1, 10, 0); (0, 2, 2, 0);
      (1, 2, 6, 0); (1, 3, 6, 0);
      (2, 4, 5, 0);
      (3, 2, 3, 0); (3, 4, 8, 0) ] } 0 4
let%test _ = f = 11
let%test _ =
  e.fold List.cons [] =
  [(0, 2, 2); (0, 1, 9); (1, 3, 6); (1, 2, 3); (2, 4, 5); (3, 4, 6)]
