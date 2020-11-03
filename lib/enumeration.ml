type 'a mutable_stream = (unit -> 'a node) ref
 and 'a node =
   | Nil
   | Cons of 'a * 'a mutable_stream

let perm n xs =
  let rec mutable_stream_of_list xs =
    ref @@ fun () ->
    match xs with
    | [] -> Nil
    | x :: xs -> Cons (x, mutable_stream_of_list xs) in
  let head = mutable_stream_of_list xs in
  let rec perm_aux xs n ptr acc () =
    if n <= 0
    then Seq.Cons (xs, acc)
    else
      match !ptr () with
      | Nil -> acc ()
      | (Cons (x, next)) as here ->
          (* ストリームからxを取り除く *)
          ptr := !next;
          (* まずxを取り除いたストリームからn-1要素を選ぶ *)
          perm_aux (x :: xs) (n - 1) head
            (fun () ->
              (* n-1要素を選び終わったタイミングで，ストリームにxを戻す *)
              ptr := (fun () -> here);
              perm_aux xs n next acc ()) () in
  perm_aux [] n head Seq.empty

let rec comb xs n ys acc () =
  match n, ys with
  | 0, _ -> Seq.Cons (xs, acc)
  | _, [] -> acc ()
  | n, y :: ys -> comb (y :: xs) (n - 1) ys (comb xs n ys acc) ()
(* 与えられたリストからn要素を選ぶ組み合わせをストリームとして列挙 *)
let comb : int -> 'a list -> 'a list Seq.t = fun n xs ->
  if n < 0 then Seq.empty else comb [] n xs Seq.empty

let rec repcomb xs n ys acc () =
  match n, ys with
  | 0, _ -> Seq.Cons (xs, acc)
  | _, [] -> acc ()
  | n, y :: ys' -> repcomb (y :: xs) (n - 1) ys (repcomb xs n ys' acc) ()
(* 与えられたリストから重複を許してn要素を選ぶ組み合わせをストリームとして列挙 *)
let repcomb : int -> 'a list -> 'a list Seq.t = fun n xs ->
  if n < 0 then Seq.empty else repcomb [] n xs Seq.empty;;

(* sample code *)
List.of_seq @@ perm 2 [1; 2; 3; 4];;
List.of_seq @@ perm 0 [1; 2; 3];;
List.of_seq @@ perm 3 [1; 2; 3];;
List.of_seq @@ perm 4 [1; 2; 3];;

List.of_seq @@ comb 2 [1; 2; 3];;
List.of_seq @@ comb 3 [1; 2; 3];;
List.of_seq @@ comb 0 [1; 2; 3];;
List.of_seq @@ comb 4 [1; 2; 3];;

List.of_seq @@ repcomb 2 [1; 2; 3];;
List.of_seq @@ repcomb 3 [1; 2; 3];;
List.of_seq @@ repcomb 0 [1; 2; 3];;
List.of_seq @@ repcomb 4 [1; 2; 3];;

(* 添字の違う要素は区別される *)
List.of_seq @@ perm 2 [1; 1; 2];;
List.of_seq @@ comb 2 [1; 1; 2];;
List.of_seq @@ repcomb 2 [1; 1; 2];;

