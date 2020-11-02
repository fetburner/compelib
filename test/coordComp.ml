module CC = Compelib.CoordComp.CoordComp (struct
  type t = int
  let compare = compare
end)

let l = [3; 1; 4; 1]
let n, f, g = CC.compress l
let%test "compress [3; 1; 4; 1] cardinal" = n = List.length (List.sort_uniq compare l)
let%test "compress [3; 1; 4; 1] compression" = List.map f l = [1; 0; 2; 0]
let%test "compress [3; 1; 4; 1] decompression" = List.init n g = List.sort_uniq compare l

let l = [100; 100; 100]
let n, f, g = CC.compress l
let%test "compress [100; 100; 100] cardinal" = n = List.length (List.sort_uniq compare l)
let%test "compress [100; 100; 100] compression" = List.map f l = [0; 0; 0]
let%test "compress [100; 100; 100] decompression" = List.init n g = List.sort_uniq compare l
