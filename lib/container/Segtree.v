Require Import Recdef.
From mathcomp Require Import all_ssreflect zify.

Lemma double_uphalf n : (uphalf n).*2 = odd n + n.
Proof. lia. Qed.

Lemma double_half n : n./2.*2 = n - odd n.
Proof. lia. Qed.

Section Segtree.
  Variable A : eqType.
  Variable idm : A.
  Hypothesis mul : Monoid.law idm.
  Local Notation "x * y" := (mul x y).

  Lemma accumulated_product f g l : forall r,
    (forall i, l <= i < r -> f i = g i.*2 * g i.*2.+1) ->
    \big[mul/idm]_(l <= i < r) f i = \big[mul/idm]_(l.*2 <= i < r.*2) g i.
  Proof.
    elim => [ | r IHr Hacc ].
    - by rewrite !big_geq.
    - case (leqP l r) => ?.
      + rewrite doubleS !big_nat_recr; try lia.
        by rewrite IHr => [ | ? ? ]; rewrite Hacc ?Monoid.mulmA //; lia.
      + by rewrite !big_geq //; lia.
  Qed.

  Lemma left_segment f l :
    (if odd l then f l else idm) = \big[mul/idm]_(l <= i < odd l + l) f i.
  Proof.
    case (odd l).
    - by rewrite big_nat1.
    - by rewrite big_geq.
  Qed.

  Corollary left_segment_acc f l lp :
    (if odd l then lp * f l else lp) = lp * \big[mul/idm]_(l <= i < odd l + l) f i.
  Proof.
    by rewrite -left_segment (fun_if (mul lp)) Monoid.mulm1.
  Qed.

  Lemma right_segment f r :
    (if odd r then f r.-1 else idm) = \big[mul/idm]_(r - odd r <= i < r) f i.
  Proof.
    case (boolP (odd r)) => [ /odd_gt0 /prednK {3}<- | ].
    - by rewrite subn1 big_nat1.
    - by rewrite big_geq //; lia.
  Qed.

  Corollary right_segment_acc f r rp :
    (if odd r then f r.-1 * rp else rp) = (\big[mul/idm]_(r - odd r <= i < r) f i) * rp.
  Proof.
    by rewrite -right_segment (fun_if (mul^~rp)) Monoid.mul1m.
  Qed.

  Fixpoint valid_segtree leaves rest_nodes :=
    if rest_nodes is parents :: rest_nodes'
    then
      (size parents == (size leaves)./2) &&
      [forall i : 'I_(size leaves)./2,
        nth idm parents i == nth idm leaves i.*2 * nth idm leaves i.*2.+1] &&
      valid_segtree parents rest_nodes'
    else leaves == [::].

  Fixpoint encode (nodes : seq (seq A)) :=
    if nodes is leaves :: rest_nodes
    then encode rest_nodes ++ leaves
    else [::].

  Fixpoint product_rec l r lp rp leaves rest_nodes :=
    if r <= l
    then lp * rp
    else
      let lp := if odd l then lp * nth idm leaves l else lp in
      let rp := if odd r then nth idm leaves r.-1 * rp else rp in
      if rest_nodes is parents :: rest_nodes'
      then product_rec (uphalf l) r./2 lp rp parents rest_nodes'
      else lp * rp.

  Definition product l r '(leaves, rest_nodes) :=
    product_rec l r idm idm leaves rest_nodes.

  Lemma product_rec_correct : forall rest_nodes l r lp rp leaves,
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    product_rec l r lp rp leaves rest_nodes
      = lp * (\big[mul/idm]_(l <= i < r) nth idm leaves i) * rp.
  Proof.
    elim => /= [ | parents ? IH ] l r ? ? leaves
         => [ /eqP -> /= | /andP [ /andP [ /eqP Hsize /forallP Hacc ] ? ] ] ?;
      case (leqP r l) => [ ? | Hlr ].
    - by rewrite big_geq ?Monoid.mulm1.
    - lia.
    - by rewrite big_geq // Monoid.mulm1.
    - rewrite IH ?Hsize ?half_leq // left_segment_acc right_segment_acc
        (accumulated_product (nth idm parents) (nth idm leaves)) => [ | i ? ].
      { rewrite double_half double_uphalf !Monoid.mulmA.
        congr (_ * _). rewrite -!Monoid.mulmA. congr (_ * _).
        by rewrite -!big_cat_nat //; lia. }
      have Hbound : i < (size leaves)./2 by lia.
      by have /eqP := Hacc (Ordinal Hbound).
  Qed.

  Theorem product_correct l r leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    product l r (leaves, rest_nodes) = \big[mul/idm]_(l <= i < r) nth idm leaves i.
  Proof.
    rewrite /product => /product_rec_correct Hvalid /Hvalid ->.
    by rewrite Monoid.mulm1 Monoid.mul1m.
  Qed.

  Function product_rec' segtree m n l r lp rp {measure id r} :=
    if r <= l
    then lp * rp
    else
      product_rec' segtree m./2 (n - m./2) (uphalf l) r./2
        (if odd l then lp * segtree (n + l) else lp)
        (if odd r then segtree (n + r.-1) * rp else rp).
  Proof. by lia. Defined.

  Definition product' segtree m n l r :=
    product_rec' segtree m n l r idm idm.

  Lemma product_rec_correspondence segtree : forall rest_nodes leaves l r lp rp,
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    product_rec l r lp rp leaves rest_nodes
      = product_rec' segtree (size leaves) (size (encode rest_nodes)) l r lp rp.
  Proof.
    elim => /= [ | ? ? IH ] ? l r ? ?
         => [ /eqP -> /= | /andP [ /andP [ /eqP Hsize ? ] ? ] ] Hbound Hsegtree;
      rewrite product_rec'_equation;
      case (leqP r l) => //= Hlr.
    - lia.
    - rewrite !Hsegtree; try lia.
      rewrite nth_cat ltnNge leq_addr /=
              nth_cat ltnNge leq_addr /= !addKn size_cat Hsize addnK
              IH ?Hsize => //= [ | i Hi ]; try lia.
      by rewrite Hsegtree ?nth_cat ?size_cat Hsize ?(leq_trans Hi (leq_addr _ _)) ?Hi.
  Qed.

  Lemma product_correspondence rest_nodes leaves segtree l r :
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    product' segtree (size leaves) (size (encode rest_nodes)) l r 
      = product l r (leaves, rest_nodes).
  Proof.
    by rewrite /product' /product => /product_rec_correspondence Hvalid /Hvalid Hbound /Hbound.
  Qed.

  Theorem product'_correct rest_nodes leaves segtree l r :
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    product' segtree (size leaves) (size (encode rest_nodes)) l r 
      = \big[mul/idm]_(l <= i < r) nth idm leaves i.
  Proof.
    move => ? ? ?.
    by rewrite product_correspondence // product_correct.
  Qed.

  Variable P : pred A.

  Fixpoint upper_bound_rec {B} l r p leaves rest_nodes (cont : nat -> A -> B) :=
    if r <= l
    then cont l p
    else
      let p' := if odd l then p * nth idm leaves l else p in
      if odd l && ~~ P p'
      then cont l p
      else
        if rest_nodes is parents :: rest_nodes'
        then upper_bound_rec (uphalf l) r./2 p' parents rest_nodes' (fun m p =>
          if r <= m.*2
          then cont m.*2 p
          else
            let p' := p * nth idm leaves m.*2 in
            if P p'
            then cont m.*2.+1 p'
            else cont m.*2 p)
        else cont l p.

  Definition upper_bound l r leaves rest_nodes :=
    upper_bound_rec l r idm leaves rest_nodes pair.

  Lemma upper_bound_rec_correct B : forall rest_nodes m l r p leaves cont,
    valid_segtree leaves rest_nodes ->
    (forall k, k <= size leaves ->
               P (p * \big[mul/idm]_(l <= i < k) nth idm leaves i) = (k <= m)) ->
    r <= size leaves ->
    m <= r ->
    l <= m ->
    @upper_bound_rec B l r p leaves rest_nodes cont
      = cont m (p * \big[mul/idm]_(l <= i < m) nth idm leaves i).
  Proof.
    elim => /= [ | parents ? IH ] m l r ? leaves ?
         => [ /eqP -> ? | /andP [ /andP [ /eqP Hsize /forallP Hacc ] ? ] Hub ? ? ? ].
    { do 3 rewrite leqn0 => /eqP ->.
      by rewrite /= big_geq // Monoid.mulm1. }
    case: ifPn => ?.
    { rewrite (@anti_leq l m); try lia.
      by rewrite big_geq // Monoid.mulm1. }
    rewrite left_segment_acc Hub; try lia.
    case: ifPn => ?.
    { rewrite (@anti_leq l m); try lia.
      by rewrite big_geq // Monoid.mulm1. }
    rewrite (IH m./2) => // [ | k ? | | | ]; try lia.
    { rewrite (accumulated_product (nth idm parents) (nth idm leaves)) => [ | i ? ].
      - rewrite double_half double_uphalf -Monoid.mulmA -big_cat_nat; try lia.
        case: ifPn => ?.
        + by have -> : m - odd m = m by lia.
        + rewrite -(@big_nat1 _ idm mul _ (nth idm leaves)) -Monoid.mulmA -big_cat_nat //; try lia.
          rewrite Hub; try lia.
          case: ifPn => ?.
          * by have -> : (m - odd m).+1 = m by lia.
          * by have -> : m - odd m = m by lia.
      - have Hbound : i < (size leaves)./2 by lia.
        by have /eqP := (Hacc (Ordinal Hbound)). }
    rewrite (accumulated_product (nth idm parents) (nth idm leaves)) ?double_uphalf => [ | i ? ].
    - case (leqP k.*2 (odd l + l)) => ?.
      + rewrite (@big_geq _ _ _ (odd l + l)); try lia.
        by rewrite Monoid.mulm1 Hub; lia.
      + rewrite -Monoid.mulmA -big_cat_nat; try lia.
        rewrite Hub; lia.
    - have Hbound : i < (size leaves)./2 by lia.
      by have /eqP := (Hacc (Ordinal Hbound)).
  Qed.

  Corollary upper_bound_correct m l r leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    (forall k, k <= size leaves ->
               P (\big[mul/idm]_(l <= i < k) nth idm leaves i) = (k <= m)) ->
    r <= size leaves ->
    m <= r ->
    l <= m ->
    upper_bound l r leaves rest_nodes = (m, \big[mul/idm]_(l <= i < m) nth idm leaves i).
  Proof.
    rewrite /upper_bound => ? Hub ? ? ?.
    by rewrite (upper_bound_rec_correct _ _ m) => // [ | ? /Hub ] /[1! Monoid.mul1m].
  Qed.

  Fixpoint upper_bound_rec' {B} segtree i m n l r p (cont : nat -> A -> B) :=
    if i is i.+1
    then
      if r <= l
      then cont l p
      else
        let p' := if odd l then p * segtree (n + l) else p in
        if odd l && ~~ P p'
        then cont l p
        else upper_bound_rec' segtree i m./2 (n - m./2) (uphalf l) r./2 p' (fun k p =>
          if r <= k.*2
          then cont k.*2 p
          else
            let p' := p * segtree (n + k.*2) in
            if P p'
            then cont k.*2.+1 p'
            else cont k.*2 p)
    else cont l p.

  Definition upper_bound' segtree m n l r := upper_bound_rec' segtree m m n l r idm pair.

  Lemma upper_bound_rec_correspondence B segtree : forall rest_nodes leaves i l r p (cont cont' : nat -> A -> B),
    valid_segtree leaves rest_nodes ->
    size leaves < 2 ^ i ->
    r <= size leaves ->
    l <= r ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    (forall k p, k <= r -> cont k p = cont' k p) ->
    upper_bound_rec l r p leaves rest_nodes cont
      = upper_bound_rec' segtree i (size leaves) (size (encode rest_nodes)) l r p cont'.
  Proof.
    elim => /= [ | parents rest_nodes IH ] leaves i l r p ? ?
         => [ /eqP -> | /andP [ /andP [ /eqP Hsize Heq ] ? ] ];
    case: i => [ /[1! expn0] /[1! ltnS] /ltac:(do 3 rewrite leqn0 => /eqP ->) ? -> //
               | i /[1! expnS] ? ] /=.
    { by (do 2 rewrite leqn0 => /eqP ->) => ? ->. }
    move => /= ? ? Hsegtree Hcont.
    case: ifPn => [ ? /[1! Hcont] // | ? ].
    rewrite Hsegtree ?nth_cat size_cat; try lia.
    rewrite ltnNge leq_addr /= addKn Hsize addnK -Hsize.
    case: ifPn => [ /[1! Hcont] // | ? ].
    apply /IH => //= [ | | | ? ? | ? ? ? ]; try lia.
    - rewrite Hsegtree ?nth_cat size_cat; try lia.
      case: ifPn => //. lia.
    - case: ifPn => // [ /[1! Hcont] // /ltac:(lia) | ? ].
      rewrite Hsegtree ?nth_cat size_cat; try lia.
      rewrite ltnNge leq_addr /= addKn !Hcont //; lia.
  Qed.

  Corollary upper_bound_correspondence l r segtree leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    l <= r ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    upper_bound l r leaves rest_nodes
      = upper_bound' segtree (size leaves) (size (encode rest_nodes)) l r.
  Proof.
    rewrite /upper_bound /upper_bound' => ? ? ? ?.
    apply /upper_bound_rec_correspondence => //.
    elim: (size leaves) => // i ?.
    rewrite expnS. lia.
  Qed.

  Corollary upper_bound'_correct m l r segtree leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    (forall k, k <= size leaves ->
               P (\big[mul/idm]_(l <= i < k) nth idm leaves i) = (k <= m)) ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    r <= size leaves ->
    m <= r ->
    l <= m ->
    upper_bound' segtree (size leaves) (size (encode rest_nodes)) l r
      = (m, \big[mul/idm]_(l <= i < m) nth idm leaves i).
  Proof.
    move => ? ? ? ? ? ?.
    rewrite -upper_bound_correspondence //; try lia.
    exact /upper_bound_correct.
  Qed.

  Fixpoint lower_bound_rec {B} l r p leaves rest_nodes (cont : nat -> A -> B) :=
    if r <= l
    then cont r p
    else
      let p' := if odd r then nth idm leaves r.-1 * p else p in
      if odd r && ~~ P p'
      then cont r p
      else
        if rest_nodes is parents :: rest_nodes'
        then lower_bound_rec (uphalf l) r./2 p' parents rest_nodes' (fun m p =>
          if m.*2 <= l
          then cont m.*2 p
          else
            let p' := nth idm leaves m.*2.-1 * p in
            if P p'
            then cont m.*2.-1 p'
            else cont m.*2 p)
        else cont r p.

  Definition lower_bound l r leaves rest_nodes := lower_bound_rec l r idm leaves rest_nodes pair.

  Lemma lower_bound_rec_correct B : forall rest_nodes m l r p leaves cont,
    valid_segtree leaves rest_nodes ->
    (forall k, P ((\big[mul/idm]_(k <= i < r) nth idm leaves i) * p) = (m <= k)) ->
    r <= size leaves ->
    m <= r ->
    l <= m ->
    @lower_bound_rec B l r p leaves rest_nodes cont
      = cont m ((\big[mul/idm]_(m <= i < r) nth idm leaves i) * p).
  Proof.
    elim => /= [ | parents ? IH ] m l r ? leaves ?
         => [ /eqP -> ? | /andP [ /andP [ /eqP Hsize /forallP Hacc ] ? ] Hlb ? ? ? ].
    { do 3 rewrite leqn0 => /eqP ->.
      by rewrite /= big_geq // Monoid.mul1m. }
    case: ifPn => ?.
    { rewrite (@anti_leq r m); try lia.
      by rewrite big_geq // Monoid.mul1m. }
    rewrite right_segment_acc Hlb; try lia.
    case: ifPn => ?.
    { rewrite (@anti_leq r m); try lia.
      by rewrite big_geq // Monoid.mul1m. }
    rewrite (IH (uphalf m)) => // [ | k | | | ]; try lia.
    { rewrite (accumulated_product (nth idm parents) (nth idm leaves)) => [ | i ? ].
      - rewrite double_half double_uphalf Monoid.mulmA -big_cat_nat; try lia.
        case: ifPn => ?.
        + by have -> : odd m + m = m by lia.
        + rewrite -(@big_nat1 _ idm mul _ (nth idm leaves)) Monoid.mulmA prednK -?big_cat_nat ?Hlb; try lia.
          case: ifPn => ?.
          * by have -> : (odd m + m).-1 = m by lia.
          * by have -> : odd m + m = m by lia.
      - have Hbound : i < (size leaves)./2 by lia.
        by have /eqP := (Hacc (Ordinal Hbound)). }
    rewrite (accumulated_product (nth idm parents) (nth idm leaves)) ?double_half => [ | i ? ].
    - case (leqP (r - odd r) k.*2) => ?.
      + rewrite (@big_geq _ _ _ k.*2); try lia.
        by rewrite Monoid.mul1m Hlb; lia.
      + rewrite Monoid.mulmA -big_cat_nat; try lia.
        rewrite Hlb; lia.
    - have Hbound : i < (size leaves)./2 by lia.
      by have /eqP := (Hacc (Ordinal Hbound)).
  Qed.

  Corollary lower_bound_correct rest_nodes m l r leaves :
    valid_segtree leaves rest_nodes ->
    (forall k, P ((\big[mul/idm]_(k <= i < r) nth idm leaves i)) = (m <= k)) ->
    r <= size leaves ->
    m <= r ->
    l <= m ->
    lower_bound l r leaves rest_nodes
      = (m, (\big[mul/idm]_(m <= i < r) nth idm leaves i)).
  Proof.
    rewrite /lower_bound => ? Hlb ? ? ?.
    by rewrite (lower_bound_rec_correct _ _ m) => // [ | ? ] /[1! Monoid.mulm1] => [ | /[1! Hlb] ].
  Qed.

  Function lower_bound_rec' {B} segtree i m n l r p (cont : nat -> A -> B) :=
    if i is i.+1
    then
      if r <= l
      then cont r p
      else
        let p' := if odd r then segtree (n + r.-1) * p else p in
        if odd r && ~~ P p'
        then cont r p
        else lower_bound_rec' segtree i m./2 (n - m./2) (uphalf l) r./2 p' (fun k p =>
          if k.*2 <= l
          then cont k.*2 p
          else
            let p' := segtree (n + k.*2.-1) * p in
            if P p'
            then cont k.*2.-1 p'
            else cont k.*2 p)
    else cont r p.

  Definition lower_bound' segtree m n l r := lower_bound_rec' segtree m m n l r idm pair.

  Lemma lower_bound_rec_correspondence B segtree : forall rest_nodes leaves i l r p (cont cont' : nat -> A -> B),
    valid_segtree leaves rest_nodes ->
    size leaves < 2 ^ i ->
    r <= size leaves ->
    l <= r ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    (forall k p, k <= r -> cont k p = cont' k p) ->
    lower_bound_rec l r p leaves rest_nodes cont
      = lower_bound_rec' segtree i (size leaves) (size (encode rest_nodes)) l r p cont'.
  Proof.
    elim => /= [ | parents rest_nodes IH ] leaves i l r p ? ?
         => [ /eqP -> | /andP [ /andP [ /eqP Hsize Heq ] ? ] ];
    case: i => [ /[1! expn0] /[1! ltnS] /ltac:(do 3 rewrite leqn0 => /eqP ->) ? -> //
               | i /[1! expnS] ? ] /=.
    - by (do 2 rewrite leqn0 => /eqP ->) => ? ->.
    - move => /= ? ? Hsegtree Hcont.
      case: ifPn => [ ? /[1! Hcont] // | ? ].
      rewrite Hsegtree ?nth_cat size_cat; try lia.
      rewrite ltnNge leq_addr /= addKn Hsize addnK -Hsize.
      case: ifPn => [ /[1! Hcont] // | ? ].
      apply /IH => //= [ | | | ? ? | ? ? ? ]; try lia.
      + rewrite Hsegtree ?nth_cat size_cat; try lia.
        case: ifPn => //. lia.
      + case: ifPn => // [ /[1! Hcont] // /ltac:(lia) | ? ].
        rewrite Hsegtree ?nth_cat size_cat; try lia.
        rewrite ltnNge leq_addr /= addKn !Hcont //; lia.
  Qed.

  Corollary lower_bound_correspondence l r segtree leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    l <= r ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    lower_bound l r leaves rest_nodes
      = lower_bound' segtree (size leaves) (size (encode rest_nodes)) l r.
  Proof.
    rewrite /lower_bound /lower_bound' => ? ? ? ?.
    apply /lower_bound_rec_correspondence => //.
    elim: (size leaves) => // i ?.
    rewrite expnS. lia.
  Qed.

  Corollary lower_bound'_correct m l r segtree leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    (forall k, P ((\big[mul/idm]_(k <= i < r) nth idm leaves i)) = (m <= k)) ->
    (forall i,
      i < size (encode rest_nodes) + size leaves ->
      segtree i = nth idm (encode rest_nodes ++ leaves) i) ->
    r <= size leaves ->
    m <= r ->
    l <= m ->
    lower_bound' segtree (size leaves) (size (encode rest_nodes)) l r
      = (m, (\big[mul/idm]_(m <= i < r) nth idm leaves i)).
  Proof.
    move => ? ? ? ? ? ?.
    rewrite -lower_bound_correspondence //; try lia.
    exact /lower_bound_correct.
  Qed.
End Segtree.
