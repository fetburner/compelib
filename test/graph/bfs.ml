let maze =
  [|"......";
    ".#####";
    "..#.#.";
    "..##..";
    "#....."|]

let d =
  let d = Bigarray.Genarray.create Bigarray.int Bigarray.c_layout [| 6; 5 |] in
  Bigarray.Genarray.fill d max_int;
  Compelib.Bfs.shortest_path
  (module struct
    module Vertex = struct
      type t = int array
      let get_distance = Bigarray.Genarray.get d
      let set_distance = Bigarray.Genarray.set d
      let iter_adjacencies [| i; j |] f =
        List.iter (fun ([| i; j |] as v) ->
          match maze.(j).[i] = '.' with
          | false | exception (Invalid_argument _) -> ()
          | true -> f v)
        [ [| i + 1; j |]; [| i - 1; j |]; [| i; j + 1 |]; [| i; j - 1 |] ]
    end
  end) [| 0; 0 |]

let%test _ = d [| 5; 0 |] = 5
let%test _ = d [| 3; 2 |] = max_int
