module RegExp : sig
  type t
  val empty_set : t
  val empty_str : t
  val char : Char.t -> t
  val app : t -> t -> t
  val neg : t -> t
  val inter : t -> t -> t
  val union : t -> t -> t
  val star : t -> t

  val matches : t -> Char.t list -> bool
end = struct
  type t =
    { sort : sort;
      is_nullable : bool;
      derive : char -> t }
  and sort =
    | EmptySet
    | EmptyStr
    | Other

  let rec empty_set =
    { sort = EmptySet;
      is_nullable = false;
      derive = fun _ -> empty_set }

  let empty_str =
    { sort = EmptyStr;
      is_nullable = true;
      derive = fun _ -> empty_set }

  let char c =
    { sort = Other;
      is_nullable = false;
      derive = fun c' -> if c = c' then empty_str else empty_set }

  let rec neg re =
    { sort = Other;
      is_nullable = not re.is_nullable;
      derive = fun c -> neg (re.derive c) }

  let rec inter re1 re2 =
    { sort = Other;
      is_nullable = re1.is_nullable && re2.is_nullable;
      derive = fun c -> inter (re1.derive c) (re2.derive c) }

  let rec union re1 re2 =
    match re1, re2 with
    | { sort = EmptySet; _ }, _ -> re2
    | _, { sort = EmptySet; _ } -> re1
    | _, _ ->
        { sort = Other;
          is_nullable = re1.is_nullable || re2.is_nullable;
          derive = fun c -> union (re1.derive c) (re2.derive c) }

  let rec app re1 re2 =
    match re1, re2 with
    | { sort = EmptyStr; _ }, _ -> re2
    | { sort = EmptySet; _ }, _ -> empty_set
    | _, _ ->
        { sort = Other;
          is_nullable = re1.is_nullable && re2.is_nullable;
          derive = fun c ->
            union
              (app (re1.derive c) re2)
              (if re1.is_nullable then re2.derive c else empty_set) }

  let star = function
    | { sort = EmptySet; _ }
    | { sort = EmptyStr; _ } -> empty_str
    | re0 ->
        let rec re =
          { sort = Other;
            is_nullable = true;
            derive = fun c -> app (re0.derive c) re } in re

  let matches re s =
    (List.fold_left (fun re -> re.derive) re s).is_nullable
end

(*
let () =
  let s = read_line () in
  print_endline @@
  if
    let open RegExp in
    matches
      (* ((dream(er)?)|(erase(r)?))* *)
      (star @@
        union
          (app (char 'd') @@ app (char 'r') @@ app (char 'e') @@ app (char 'a') @@ app (char 'm') @@
            union empty_str @@ app (char 'e') @@ char 'r')
          (app (char 'e') @@ app (char 'r') @@ app (char 'a') @@ app (char 's') @@ app (char 'e') @@
            (union empty_str @@ char 'r'))) @@
      Array.to_list @@ Array.init (String.length s) (String.get s)
  then "YES"
  else "NO"
*)
