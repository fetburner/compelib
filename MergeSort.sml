(* sorting *)
local
  fun revMerge' op<= [] ys acc = List.revAppend (ys, acc)
    | revMerge' op<= xs [] acc = List.revAppend (xs, acc)
    | revMerge' op<= (x :: xs) (y :: ys) acc =
        if x <= y then revMerge' op<= xs (y :: ys) (x :: acc)
        else revMerge' op<= (x :: xs) ys (y :: acc)

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
        revMerge' op>
          (sort' op> op<= (n div 2) xs)
          (sort' op> op<= ((n + 1) div 2) (List.drop (xs, n div 2))) []
in
  fun revMerge cmp xs ys =
    revMerge' (fn (x, y) => cmp (x, y) <> GREATER) xs ys []

  fun sort cmp [] = []
    | sort cmp [x] = [x]
    | sort cmp (xs as _ :: _ :: _) =
        sort'
          (fn (x, y) => cmp (x, y) <> GREATER)
          (fn (x, y) => cmp (x, y) = GREATER) (length xs) xs
end
