module F (VSet : Set.S) : sig
  (* トポロジカルソート *)
  val sort :
    (* 頂点のリスト *)
    VSet.elt list ->
    (* 隣接リスト *)
    (VSet.elt -> VSet.elt list) ->
    VSet.elt list

  (* 強連結成分分解 *)
  val scc :
    (* 頂点のリスト *)
    VSet.elt list ->
    (* 隣接リスト *)
    (VSet.elt -> VSet.elt list) ->
    VSet.elt list list
end
