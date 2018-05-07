(* sorting *)
structure ListMergeSort : sig
  val sort : ('a * 'a -> bool) -> 'a list -> 'a list
end = struct
  fun revMerge op<= [] ys acc = List.revAppend (ys, acc)
    | revMerge op<= xs [] acc = List.revAppend (xs, acc)
    | revMerge op<= (x :: xs) (y :: ys) acc =
        if x <= y
        then revMerge op<= xs (y :: ys) (x :: acc)
        else revMerge op<= (x :: xs) ys (y :: acc)

  fun sort' op<= op> 2 (x :: y :: _) =
        if x <= y then [x, y] else [y, x]
    | sort' op<= op> 3 (x :: y :: z :: _) =
        if x <= y then
          if y <= z then [x, y, z]
          else if x <= z then [x, z, y]
          else [z, x, y]
        else 
          if x <= z then [y, x, z]
          else if y <= z then [y, z, x]
          else [z, y, x]
    | sort' op<= op> n xs =
        revMerge op>
          (sort' op> op<= (n div 2) xs)
          (sort' op> op<= ((n + 1) div 2) (List.drop (xs, n div 2))) []

  fun sort op> [] = []
    | sort op> [x] = [x]
    | sort op> (xs as _ :: _ :: _) = sort' (not o op>) op> (length xs) xs

end;

(* sample code *)
ListMergeSort.sort op> [3, 1, 4, 1, 5];
ListMergeSort.sort (fn ((x, _), (y, _)) => x > y) [(1, 1), (2, 2), (1, 3), (2, 4), (1, 5)];
