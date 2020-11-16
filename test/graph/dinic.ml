(* 蟻本p. 188のグラフで試す *)
module G = Compelib.Dinic.F
(struct
  type t = int
  let equal = ( = )
end)
(struct
  type t = int
  let inf = max_int
  let zero = 0
  let ( + ) = ( + )
  let ( - ) = ( - )
  let compare = compare
end)
(struct
  type 'a t = 'a array
  type key = int
  type size = int
  let make = Array.make
  let get = Array.get
  let set = Array.set
  let blit src dst = Array.iteri (Array.set dst) src
end)

let es =
  [ (0, 1, 10); (0, 2, 2);
    (1, 2, 6); (1, 3, 6);
    (2, 4, 5);
    (3, 2, 3); (3, 4, 8) ]
let fs = Array.make (List.length es) (fun () -> 0)

let f = G.max_flow 5 (fun f ->
  List.iteri (fun i (src, dst, c) ->
    fs.(i) <- f ~src ~dst c) es) ~src:0 ~dst:4

let%test _ = f = 11
let%test _ =
  Array.map (fun f -> f ()) fs =
  [| 9; 2; 3; 6; 5; 0; 6 |]
