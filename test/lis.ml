module IntLis = Compelib.Lis.F (Set.Make (Int));;

let%test _ = IntLis.lis [4; 1; 6; 2; 8; 5; 7; 3] = 4
