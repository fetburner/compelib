(* n is the initial size of hash table *)
let memoize n f =
  let dp = Hashtbl.create n in
  let rec get x =
    try Hashtbl.find dp x with
    | Not_found ->
        let result = f get x in
        Hashtbl.add dp x result;
        result in
  get

(* continuation passing style *)
let memoize_cps n f =
  let dp = Hashtbl.create n in
  let rec get x k =
    try k @@ Hashtbl.find dp x with
    | Not_found ->
        f get x @@ fun y ->
          Hashtbl.add dp x y; k y in get
