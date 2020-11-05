module PHashtbl : sig
  (* 永続ハッシュテーブル（は？） *)
  type ('a, 'b) t

  val create : ?random:bool -> int -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  val remove : ('a, 'b) t -> 'a -> ('a, 'b) t
  val replace : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  val find : ('a, 'b) t -> 'a -> 'b
  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  val length : ('a, 'b) t -> int
end = struct
  type ('a, 'b) t = ('a, 'b) data ref
  (* 編集による差分 *)
  and ('a, 'b) data =
      Hashtbl of ('a, 'b) Hashtbl.t
    | Add of ('a, 'b) t * 'a * 'b
    | Remove of ('a, 'b) t * 'a

  let create ?(random=false) n = ref (Hashtbl (Hashtbl.create ~random n))

  (* ハッシュを使う前に，差分を解消して一番上に持ってくる *)
  let rec reroot k = function
    | { contents = Hashtbl h } -> k h
    | { contents = Add (t', i, d) } as t ->
        reroot (fun h ->
          (* 辺を逆向きに張り替える *)
          t' := Remove (t, i);
          Hashtbl.add h i d;
          k h) t'
    | { contents = Remove (t', i) } as t ->
        reroot (fun h ->
          (* 辺を逆向きに張り替える *)
          t' := Add (t, i, Hashtbl.find h i);
          Hashtbl.remove h i;
          k h) t'
  let reroot k t =
    reroot (fun h -> t := Hashtbl h; k h) t

  (* 破壊的でなかった操作は，差分を解消してからやるだけ *)
  let find t i = reroot (fun h -> Hashtbl.find h i) t
  let iter f t = reroot (Hashtbl.iter f) t
  let fold f t d = reroot (fun h -> Hashtbl.fold f h d) t
  let length t = reroot Hashtbl.length t

  (* 破壊的だった操作は，履歴を残しておく必要がある *)
  let add t i d =
    reroot (fun h ->
      let t' = ref (Hashtbl h) in
      t := Remove (t', i);
      Hashtbl.add h i d; t') t

  let remove t i =
    reroot (fun h ->
      try
        let t' = ref (Hashtbl h) in
        t := Add (t', i, Hashtbl.find h i);
        Hashtbl.remove h i; t'
      (* 要素が存在しなかった場合，何もしないので履歴を残す必要もない *)
      with Not_found -> t) t

  (* 基本操作の組み合わせで表現できるもの *)
  let replace t i d = add (remove t i) i d
end

(* sample code *)

let bindings t = PHashtbl.fold (fun i d acc -> (i, d) :: acc) t [];;

(* 単相になってしまうが，内部では代入しまくってるので仕方あるまい *)
let h1 = PHashtbl.create 10;;
bindings h1;;
let h2 = PHashtbl.remove h1 0;;
bindings h1;;
bindings h2;;
let h3 = PHashtbl.add h2 0 1;;
bindings h1;;
bindings h2;;
bindings h3;;
let h4 = PHashtbl.remove h3 0;;
bindings h1;;
bindings h2;;
bindings h3;;
bindings h4;;
let h5 = PHashtbl.add h3 0 2;;
bindings h1;;
bindings h2;;
bindings h3;;
bindings h4;;
bindings h5;;
