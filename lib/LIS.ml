module LIS (ESet : Set.S) = struct
  let lis as_ = ESet.cardinal @@ List.fold_left (fun l a ->
    match ESet.split a l with
    | _, true, _ -> l
    | _, false, lr ->
        if ESet.is_empty lr then ESet.add a l
        else ESet.add a (ESet.remove (ESet.min_elt lr) l)) ESet.empty as_
end

(* example *)
module IntLIS = LIS (Set.Make (struct type t = int let compare = compare end));;

IntLIS.lis [4; 1; 6; 2; 8; 5; 7; 3]
