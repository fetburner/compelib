let bindings t = Compelib.PHashtbl.fold (fun i d acc -> (i, d) :: acc) t []

let h1 = Compelib.PHashtbl.create 10
let b1 = bindings h1

let%test _ = b1 = []

let h2 = Compelib.PHashtbl.remove h1 0
let b2 = bindings h2

let%test _ = bindings h1 = b1
let%test _ = b2 = []

let h3 = Compelib.PHashtbl.add h2 0 1
let b3 = bindings h3

let%test _ = bindings h1 = b1
let%test _ = bindings h2 = b2
let%test _ = b3 = [(0, 1)]

let h4 = Compelib.PHashtbl.remove h3 0
let b4 = bindings h4

let%test _ = bindings h1 = b1
let%test _ = bindings h2 = b2
let%test _ = bindings h3 = b3
let%test _ = b4 = []

let h5 = Compelib.PHashtbl.add h3 0 2
let b5 = bindings h5

let%test _ = bindings h1 = b1
let%test _ = bindings h2 = b2
let%test _ = bindings h3 = b3
let%test _ = bindings h4 = b4
let%test _ = b5 = [(0, 1); (0, 2)]
