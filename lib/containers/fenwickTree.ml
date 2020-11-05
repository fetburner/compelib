(* 可換モノイド *)
module type CommutativeMonoid = sig
  type t
  val e : t
  val op : t -> t -> t
end

(* 0-indexedなBIT *)
module Make (CM : CommutativeMonoid) = struct
  type t = CM.t array

  let make n = Array.make n CM.e

  let initialize a =
    for i = 0 to Array.length a - 1 do
      if i lor (i + 1) < Array.length a then
        a.(i lor (i + 1)) <- CM.op a.(i) a.(i lor (i + 1))
    done

  let init n f =
    let a = Array.init n f in
    initialize a; a

  let of_list l =
    let a = Array.of_list l in
    initialize a; a

  let rec accumulate a i x =
    if i < Array.length a then begin
      a.(i) <- CM.op x a.(i);
      accumulate a (i lor (i + 1)) x
    end

  let rec query acc a i =
    if i < 0
    then acc
    else query (CM.op acc a.(i)) a ((i land (i + 1)) - 1)
  let query a i = query CM.e a (i - 1)
end
