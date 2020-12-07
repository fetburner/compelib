module G = Compelib.Bfs.F (struct
  type t = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Genarray.t
  type elt = int
  type key = int array
  type size = int array
  let make n = let d = Bigarray.Genarray.create Bigarray.int Bigarray.c_layout n in Bigarray.Genarray.fill d max_int; d
  let get = Bigarray.Genarray.get
  let set = Bigarray.Genarray.set
end)

let maze =
  [|"......";
    ".#####";
    "..#.#.";
    "..##..";
    "#....."|]

let d = G.shortest_path
  (module struct
    module Vertex = struct
      type t = int array
      type set = int array
      let universe = [| 6; 5 |]
      let iter_adjacency [| i; j |] f =
        List.iter (fun ([| i; j |] as v) ->
          match maze.(j).[i] = '.' with
          | false | exception (Invalid_argument _) -> ()
          | true -> f v)
        [ [| i + 1; j |]; [| i - 1; j |]; [| i; j + 1 |]; [| i; j - 1 |] ]
    end
  end) [| 0; 0 |]

let%test _ = d [| 5; 0 |] = 5
let%test _ = d [| 3; 2 |] = max_int
