Require Import Recdef.
From mathcomp Require Import all_ssreflect zify.

Lemma double_uphalf n : (uphalf n).*2 = odd n + n.
Proof. lia. Qed.

Lemma double_half n : n./2.*2 = n - odd n.
Proof. lia. Qed.

Definition lsb n := nat_of_bool (odd n).

Section Segtree.
  Variable A : Set.
  Hypothesis eqA : A -> A -> bool.
  Hypothesis eqAP : Equality.axiom (T:=A) eqA.
  Variable idm : A.
  Variable mul : A -> A -> A.
  Hypothesis mulA : associative mul.
  Hypothesis mul1m : left_id idm mul.
  Hypothesis mulm1 : right_id idm mul.

  Definition A_eqMixin := EqMixin eqAP.
  Canonical A_eqType := Eval hnf in EqType _ A_eqMixin.
  Canonical mul_monoid := Monoid.Law mulA mul1m mulm1.
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
    case: ifP.
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
    case: ifP => [ /odd_gt0 /prednK {3}<- | ].
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
    then leaves ++ encode rest_nodes
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
      case: ifP => ?.
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

  Function product_iter_rec segtree m n l r lp rp {measure id r} :=
    if r <= l
    then lp * rp
    else
      product_iter_rec segtree m./2 (n + m) (uphalf l) r./2
        (if odd l then lp * segtree (n + l) else lp)
        (if odd r then segtree (n + r.-1) * rp else rp).
  Proof. by lia. Defined.

  Definition product_iter segtree m l r :=
    product_iter_rec segtree m 0 l r idm idm.

  Lemma product_rec_correspondence segtree : forall rest_nodes leaves n l r lp rp,
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree (n + i) = nth idm (leaves ++ encode rest_nodes) i) ->
    product_rec l r lp rp leaves rest_nodes
      = product_iter_rec segtree (size leaves) n l r lp rp.
  Proof.
    elim => /= [ | ? ? IH ] leaves ? l r ? ?
         => [ /eqP -> /= | /andP [ /andP [ /eqP Hsize ? ] ? ] ] Hbound Hsegtree;
      rewrite product_iter_rec_equation;
      case (leqP r l) => //= Hlr.
    - lia.
    - rewrite -Hsize !Hsegtree; try lia.
      rewrite nth_cat (_ : l < size leaves); try lia.
      rewrite nth_cat (_ : r.-1 < size leaves); try lia.
      apply /IH => // [ | ? ? ].
      + lia.
      + rewrite -addnA Hsegtree.
        * by rewrite nth_cat ltnNge leq_addr /= addKn.
        * rewrite size_cat. lia.
  Qed.

  Lemma product_correspondence rest_nodes leaves segtree l r :
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree i = nth idm (leaves ++ encode rest_nodes) i) ->
    product_iter segtree (size leaves) l r = product l r (leaves, rest_nodes).
  Proof.
    by rewrite /product_iter /product => /product_rec_correspondence Hvalid /Hvalid Hbound /(Hbound _ 0).
  Qed.

  Theorem product_iter_correct rest_nodes leaves segtree l r :
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree i = nth idm (leaves ++ encode rest_nodes) i) ->
    product_iter segtree (size leaves) l r
      = \big[mul/idm]_(l <= i < r) nth idm leaves i.
  Proof.
    move => ? ? ?.
    by rewrite (product_correspondence rest_nodes) // product_correct.
  Qed.

  Variable P : pred A.

  Fixpoint upper_bound_rec {B} l p leaves rest_nodes (cont : nat -> A -> B) :=
    if size leaves <= l
    then cont l p
    else
      let p' := if odd l then p * nth idm leaves l else p in
      if odd l && ~~ P p'
      then cont l p
      else
        if rest_nodes is parents :: rest_nodes'
        then upper_bound_rec (uphalf l) p' parents rest_nodes' (fun m p =>
          if size leaves <= m.*2
          then cont m.*2 p
          else
            let p' := p * nth idm leaves m.*2 in
            if P p'
            then cont m.*2.+1 p'
            else cont m.*2 p)
        else cont l p.

  Definition upper_bound l leaves rest_nodes :=
    upper_bound_rec l idm leaves rest_nodes pair.

  Lemma upper_bound_rec_correct B : forall rest_nodes m l p leaves cont,
    valid_segtree leaves rest_nodes ->
    (forall k, k <= size leaves ->
          P (p * \big[mul/idm]_(l <= i < k) nth idm leaves i) = (k <= m)) ->
    m <= size leaves ->
    l <= m ->
    @upper_bound_rec B l p leaves rest_nodes cont
      = cont m (p * \big[mul/idm]_(l <= i < m) nth idm leaves i).
  Proof.
    elim => /= [ | parents ? IH ] m l ? leaves ?
         => [ /eqP -> ? | /andP [ /andP [ /eqP Hsize /forallP Hacc ] ? ] Hub ? ? ].
    { do 2 rewrite leqn0 => /eqP ->.
      by rewrite /= big_geq // Monoid.mulm1. }
    case: ifPn => ?.
    { rewrite (@anti_leq l m); try lia.
      by rewrite big_geq // Monoid.mulm1. }
    rewrite left_segment_acc Hub; try lia.
    case: ifPn => ?.
    { rewrite (@anti_leq l m); try lia.
      by rewrite big_geq // Monoid.mulm1. }
    rewrite (IH m./2) => // [ | k ? | | ]; try lia.
    { rewrite (accumulated_product (nth idm parents) (nth idm leaves)) => [ | i ? ].
      - rewrite double_half double_uphalf -Monoid.mulmA -big_cat_nat; try lia.
        case: ifPn => ?.
        + by have -> : m - odd m = m by lia.
        + rewrite -(@big_nat1 _ idm mul_monoid _ (nth idm leaves)) -Monoid.mulmA -big_cat_nat //; try lia.
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

  Corollary upper_bound_correct m l leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    (forall k, k <= size leaves ->
          P (\big[mul/idm]_(l <= i < k) nth idm leaves i) = (k <= m)) ->
    m <= size leaves ->
    l <= m ->
    upper_bound l leaves rest_nodes = (m, \big[mul/idm]_(l <= i < m) nth idm leaves i).
  Proof.
    rewrite /upper_bound => ? Hub ? ?.
    by rewrite (upper_bound_rec_correct _ _ m) => // [ | ? /Hub ] /[1! Monoid.mul1m].
  Qed.

  Function upper_bound_iter_cont segtree m n s l p {measure id s} :=
    if s <= 1
    then (l, p)
    else
      let m := m.*2 + lsb s in
      let n := n - m in
      if m <= l.*2
      then upper_bound_iter_cont segtree m n s./2 l.*2 p
      else
        let p' := p * segtree (n + l.*2) in
        if P p'
        then upper_bound_iter_cont segtree m n s./2 l.*2.+1 p'
        else upper_bound_iter_cont segtree m n s./2 l.*2 p.
  Proof.
    - lia.
    - lia.
    - lia.
  Qed.

  Function upper_bound_iter_rec segtree m n s l p {measure id m} :=
    if m <= l
    then upper_bound_iter_cont segtree m n s l p
    else
      if odd l
      then
        let p' := p * segtree (n + l) in
        if P p'
        then upper_bound_iter_rec segtree m./2 (n + m) (s.*2 + lsb m) l./2.+1 p'
        else upper_bound_iter_cont segtree m n s l p
      else upper_bound_iter_rec segtree m./2 (n + m) (s.*2 + lsb m) l./2 p.
  Proof.
    - lia.
    - lia.
  Defined.

  Definition upper_bound_iter segtree m l :=
    upper_bound_iter_rec segtree m 0 1 l idm.

  Lemma upper_bound_iter_rec_equation' segtree m n s l p :
    upper_bound_iter_rec segtree m n s l p
      = if m <= l
        then upper_bound_iter_cont segtree m n s l p
        else
          let p' := if odd l then p * segtree (n + l) else p in
          if odd l && ~~ P p'
          then upper_bound_iter_cont segtree m n s l p
          else upper_bound_iter_rec segtree m./2 (n + m) (s.*2 + odd m) (uphalf l) p'.
  Proof.
    rewrite upper_bound_iter_rec_equation uphalf_half.
    case: ifP => // ?.
    by case: (boolP (odd l)) => //= /[1! if_neg].
  Qed.

  Lemma upper_bound_rec_correspondence segtree : forall rest_nodes leaves n s l p cont,
    valid_segtree leaves rest_nodes ->
    0 < s ->
    l <= size leaves ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree (n + i) = nth idm (leaves ++ encode rest_nodes) i) ->
    (forall l p, l <= size leaves ->
            cont l p = upper_bound_iter_cont segtree (size leaves) n s l p) ->
    upper_bound_rec l p leaves rest_nodes cont
      = upper_bound_iter_rec segtree (size leaves) n s l p.
  Proof.
    elim => /= [ | parents rest_nodes IH ] leaves ? s l ? ? /[1! upper_bound_iter_rec_equation']
         => /= [ /eqP -> /= ? /[1! leqn0] /eqP -> ? -> //
              | /andP [ /andP [ /eqP Hsize Heq ] ? ] ? ? Hsegtree Hcont ].
    case: ifPn => [ ? /[1! Hcont] // | ? ].
    rewrite Hsegtree ?size_cat; try lia.
    rewrite -Hsize nth_cat (_: l < size leaves); try lia.
    case: ifPn => [ /[1! Hcont] // | ? ].
    apply /IH => //= [ | | ? ? | k ? ? ]; try lia.
    - rewrite -addnA Hsegtree ?size_cat; try lia.
      by rewrite nth_cat ltnNge leq_addr addKn.
    - rewrite upper_bound_iter_cont_equation Hsize.
      have -> : (s.*2 + odd (size leaves) <= 1) = false by lia.
      have -> : (size leaves)./2.*2 + odd (s.*2 + odd (size leaves)) = size leaves by lia.
      have -> : (s.*2 + odd (size leaves))./2 = s by lia.
      rewrite addnK Hsegtree ?nth_cat ?size_cat; try lia.
      case (leqP (size leaves) k.*2) => [ ? /[1! Hcont] // /ltac:(lia) | ? ].
      by case: ifP => ? /[1! Hcont] //; lia.
  Qed.

  Corollary upper_bound_iter_correspondence l segtree leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    l <= size leaves ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree i = nth idm (leaves ++ encode rest_nodes) i) ->
    upper_bound l leaves rest_nodes = upper_bound_iter segtree (size leaves) l.
  Proof.
    rewrite /upper_bound /upper_bound_iter => ? ? ?.
    apply /upper_bound_rec_correspondence => // ? ? ?.
    by rewrite upper_bound_iter_cont_equation.
  Qed.

  Corollary upper_bound_iter_correct m l segtree leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    (forall k, k <= size leaves ->
          P (\big[mul/idm]_(l <= i < k) nth idm leaves i) = (k <= m)) ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree i = nth idm (leaves ++ encode rest_nodes) i) ->
    m <= size leaves ->
    l <= m ->
    upper_bound_iter segtree (size leaves) l = (m, \big[mul/idm]_(l <= i < m) nth idm leaves i).
  Proof.
    move => ? ? ? ? ?.
    rewrite -(upper_bound_iter_correspondence _ _ _ rest_nodes) //; try lia.
    exact /upper_bound_correct.
  Qed.

  Fixpoint lower_bound_rec {B} r p leaves rest_nodes (cont : nat -> A -> B) :=
    if r <= 0
    then cont r p
    else
      let p' := if odd r then nth idm leaves r.-1 * p else p in
      if odd r && ~~ P p'
      then cont r p
      else
        if rest_nodes is parents :: rest_nodes'
        then lower_bound_rec r./2 p' parents rest_nodes' (fun m p =>
          if m.*2 <= 0
          then cont m.*2 p
          else
            let p' := nth idm leaves m.*2.-1 * p in
            if P p'
            then cont m.*2.-1 p'
            else cont m.*2 p)
        else cont r p.

  Definition lower_bound r leaves rest_nodes := lower_bound_rec r idm leaves rest_nodes pair.

  Lemma lower_bound_rec_correct B : forall rest_nodes m r p leaves cont,
    valid_segtree leaves rest_nodes ->
    (forall k, P ((\big[mul/idm]_(k <= i < r) nth idm leaves i) * p) = (m <= k)) ->
    r <= size leaves ->
    m <= r ->
    @lower_bound_rec B r p leaves rest_nodes cont
      = cont m ((\big[mul/idm]_(m <= i < r) nth idm leaves i) * p).
  Proof.
    elim => /= [ | parents ? IH ] m r ? leaves ?
         => [ /eqP -> ? | /andP [ /andP [ /eqP Hsize /forallP Hacc ] ? ] Hlb ? ? ].
    { do 2 rewrite leqn0 => /eqP ->.
      by rewrite /= big_geq // Monoid.mul1m. }
    case: ifPn => ?.
    { rewrite (@anti_leq r m); try lia.
      by rewrite big_geq // Monoid.mul1m. }
    rewrite right_segment_acc Hlb; try lia.
    case: ifPn => ?.
    { rewrite (@anti_leq r m); try lia.
      by rewrite big_geq // Monoid.mul1m. }
    rewrite (IH (uphalf m)) => // [ | k | | ]; try lia.
    { rewrite (accumulated_product (nth idm parents) (nth idm leaves)) => [ | i ? ].
      - rewrite double_half double_uphalf Monoid.mulmA -big_cat_nat; try lia.
        case: ifPn => ?.
        + by have -> : odd m + m = m by lia.
        + rewrite -(@big_nat1 _ idm mul_monoid _ (nth idm leaves)) Monoid.mulmA prednK -?big_cat_nat ?Hlb; try lia.
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

  Corollary lower_bound_correct rest_nodes m r leaves :
    valid_segtree leaves rest_nodes ->
    (forall k, P ((\big[mul/idm]_(k <= i < r) nth idm leaves i)) = (m <= k)) ->
    r <= size leaves ->
    m <= r ->
    lower_bound r leaves rest_nodes = (m, \big[mul/idm]_(m <= i < r) nth idm leaves i).
  Proof.
    rewrite /lower_bound => ? Hlb ? ?.
    by rewrite (lower_bound_rec_correct _ _ m) => // [ | ? ] /[1! Monoid.mulm1] => [ | /[1! Hlb] ].
  Qed.

  Function lower_bound_iter_cont segtree m n s r p {measure id s} :=
    if s <= 1
    then (r, p)
    else
      let m := m.*2 + lsb s in
      let n := n - m in
      if r.*2 <= 0
      then lower_bound_iter_cont segtree m n s./2 r.*2 p
      else
        let p' := segtree (n + r.*2.-1) * p in
        if P p'
        then lower_bound_iter_cont segtree m n s./2 r.*2.-1 p'
        else lower_bound_iter_cont segtree m n s./2 r.*2 p.
  Proof.
    - lia.
    - lia.
    - lia.
  Defined.

  Function lower_bound_iter_rec segtree m n s r p {measure id r} :=
    if r <= 0
    then lower_bound_iter_cont segtree m n s r p
    else
      if odd r
      then
        let p' := segtree (n + r.-1) * p in
        if P p'
        then lower_bound_iter_rec segtree m./2 (n + m) (s.*2 + lsb m) r./2 p'
        else lower_bound_iter_cont segtree m n s r p
      else lower_bound_iter_rec segtree m./2 (n + m) (s.*2 + lsb m) r./2 p.
  Proof.
    - lia.
    - lia.
  Defined.

  Definition lower_bound_iter segtree m r :=
    lower_bound_iter_rec segtree m 0 1 r idm.

  Lemma lower_bound_iter_rec_equation' segtree m n s r p :
    lower_bound_iter_rec segtree m n s r p
      = if r <= 0
        then lower_bound_iter_cont segtree m n s r p
        else
          let p' := if odd r then segtree (n + r.-1) * p else p in
          if odd r && ~~ P p'
          then lower_bound_iter_cont segtree m n s r p
          else lower_bound_iter_rec segtree m./2 (n + m) (s.*2 + odd m) r./2 p'.
  Proof.
    rewrite lower_bound_iter_rec_equation /=.
    congr (if _ then _ else _).
    case (boolP (odd r)) => //= ?.
    by rewrite if_neg.
  Qed.

  Lemma lower_bound_rec_correspondence segtree : forall rest_nodes leaves n s r p cont,
    valid_segtree leaves rest_nodes ->
    0 < s ->
    r <= size leaves ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree (n + i) = nth idm (leaves ++ encode rest_nodes) i) ->
    (forall k p, k <= r -> cont k p = lower_bound_iter_cont segtree (size leaves) n s k p) ->
    lower_bound_rec r p leaves rest_nodes cont
      = lower_bound_iter_rec segtree (size leaves) n s r p.
  Proof.
    elim => /= [ | parents rest_nodes IH ] leaves n s r p ? /[1! lower_bound_iter_rec_equation']
         => /= [ /eqP -> ? /[1! leqn0] /eqP -> ? -> //
              | /andP [ /andP [ /eqP Hsize Heq ] ? ] ? ? Hsegtree Hcont ].
    case: ifPn => [ ? /[1! Hcont] // | ? ].
    rewrite Hsegtree ?size_cat; try lia.
    rewrite -Hsize nth_cat (_ : r.-1 < size leaves); try lia.
    case: ifPn => [ /[1! Hcont] // | ? ].
    apply /IH => //= [ | | ? ? | k ? ? ]; try lia.
    + rewrite -addnA Hsegtree ?size_cat; try lia.
      by rewrite nth_cat ltnNge leq_addr addKn.
    + rewrite lower_bound_iter_cont_equation Hsize.
      have -> : s.*2 + odd (size leaves) <= 1 = false by lia.
      have -> : (size leaves)./2.*2 + odd (s.*2 + odd (size leaves)) = size leaves by lia.
      have -> : (s.*2 + odd (size leaves))./2 = s by lia.
      rewrite addnK Hsegtree ?size_cat; try lia.
      rewrite nth_cat (_ : k.*2.-1 < size leaves); try lia.
      case: ifPn => [ ? /[1! Hcont] // /ltac:(lia) | ? ].
      by case: ifP => ? /[1! Hcont] //; lia.
  Qed.

  Corollary lower_bound_correspondence r segtree leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    r <= size leaves ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree i = nth idm (leaves ++ encode rest_nodes) i) ->
    lower_bound r leaves rest_nodes = lower_bound_iter segtree (size leaves) r.
  Proof.
    rewrite /lower_bound /lower_bound_iter => ? ? ?.
    exact /lower_bound_rec_correspondence.
  Qed.

  Corollary lower_bound_iter_correct m r segtree leaves rest_nodes :
    valid_segtree leaves rest_nodes ->
    (forall k, P ((\big[mul/idm]_(k <= i < r) nth idm leaves i)) = (m <= k)) ->
    (forall i, i < size leaves + size (encode rest_nodes) ->
          segtree i = nth idm (leaves ++ encode rest_nodes) i) ->
    r <= size leaves ->
    m <= r ->
    lower_bound_iter segtree (size leaves) r = (m, \big[mul/idm]_(m <= i < r) nth idm leaves i).
  Proof.
    move => ? ? ? ? ?.
    rewrite -(lower_bound_correspondence _ _ _ rest_nodes) //; try lia.
    exact /lower_bound_correct.
  Qed.
End Segtree.

Extract Inductive prod => "( * )" [ "" ].
Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive nat => int [ "0" "succ" ]
 "(fun fO fS n -> if n=0 then fO () else fS (n-1))".
Extract Inlined Constant negb => "not".
Extract Inlined Constant leq => "( <= )".
Extract Inlined Constant addn => "( + )".
Extract Inlined Constant subn => "( - )".
Extract Inlined Constant Nat.pred => "pred".
Extract Constant half => "fun n -> n / 2".
Extract Constant double => "fun n -> n * 2".
Extract Constant lsb => "fun n -> n mod 2".
Extract Constant odd => "fun n -> n mod 2 = 1".
Extract Constant uphalf => "fun n -> (n + 1) / 2".
Extraction "segtreeQueries.ml" product_iter upper_bound_iter lower_bound_iter.
