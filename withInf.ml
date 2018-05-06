(* 無限大の要素を付け足してトロピカル半環を作る *)
module WithInf
  (S : sig
    type t
    val zero : t
    val ( + ) : t -> t -> t
    val compare : t -> t -> int
  end) :
sig
  (* Noneで無限大を表す *)
  type t = S.t option
  val inf : t
  val zero : t
  val ( + ) : t -> t -> t
  val compare : t -> t -> int
end =
struct
  type t = S.t option
  let inf = None
  let zero = Some S.zero
  let ( + ) x y =
    match x, y with
    | None, _ -> None
    | _, None -> None
    | Some m, Some n -> Some (S.( + ) m n)
  let compare x y =
    match x, y with
    | None, None -> 0
    | None, _ -> 1
    | _, None -> -1
    | Some m, Some n -> S.compare m n
end
