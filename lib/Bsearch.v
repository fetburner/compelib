Require Import Bool ZArith Lia Program ssreflect.

Open Scope Z_scope.

Program Fixpoint upper_bound' l r
  (p : forall n, l <= n < r -> bool)
  (Hacc : @Acc _ (ltof _ (fun p => Z.to_nat (snd p - fst p))) (l, r))
  (_ : exists m,
    l <= m < r /\
    (forall n H, p n H = true -> n <= m) /\
    (forall n H, n <= m -> p n H = true)) :
  { m | l <= m < r /\
    (forall n H, p n H = true -> n <= m) /\
    (forall n H, n <= m -> p n H = true) } :=
  match Hacc with
  | Acc_intro _ Hacc' =>
      match Z_le_gt_dec r (Z.succ l) with
      | left _ => exist _ l _
      | right _ =>
          let m := (l + r) / 2 in
          ( if p m _ as b return p m _ = b -> _
            then fun _ =>
              let (n, _) := upper_bound' m r (fun n _ => p n _) (Hacc' _ _) _ in
              exist _ n _
            else fun _ =>
              let (n, _) := upper_bound' l m (fun n _ => p n _) (Hacc' _ _) _ in
              exist _ n _ ) eq_refl
      end
  end.
Next Obligation. by auto with zarith. Qed.
Next Obligation.
  by split;
  [ apply /Z.div_le_lower_bound
  | apply /Z.div_lt_upper_bound ]; lia.
Qed.
Next Obligation.
  suff [ ? ? ] : (l + r) / 2 < r /\ l <= (l + r) / 2 by apply /Z2Nat.inj_lt => /=; lia.
  by split;
  [ apply /Z.div_lt_upper_bound
  | apply /Z.div_le_lower_bound ]; lia.
Qed.
Next Obligation.
  suff : (l + r) / 2 < r by lia.
  by apply Z.div_lt_upper_bound; lia.
Qed.
Next Obligation.
  suff -> : n = H by [].
  apply /Z.le_antisymm.
  - by eapply l0, H3, Z.le_refl.
  - by eapply H2, e, Z.le_refl.
  Unshelve.
  + by [].
  + case (Z_le_gt_dec ((l + r) / 2) H) => [ Hle | ]; auto with zarith.
    by move: (e) (H0) => ->.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
  case (Z_le_gt_dec ((l + r) / 2) H) => [ Hle | ]; auto with zarith.
  by move: (e) (H0) => ->.
Qed.
Next Obligation.
  suff [ ? ? ] : Z.succ l <= (l + r) / 2 /\ (l + r) / 2 <= r by apply /Z2Nat.inj_lt => /=; lia.
  by split;
  [ apply /Z.div_le_lower_bound
  | apply /Z.div_le_upper_bound ]; lia.
Qed.
Next Obligation.
  suff : Z.succ l <= (l + r) / 2 by lia.
  by apply /Z.div_le_lower_bound; lia.
Qed.
Next Obligation.
  suff -> : n = H by [].
  have ? : H <= n by eapply H2, e, Z.le_refl.
  have ? : n <= H by eapply l0, H3, Z.le_refl.
  by lia.
  Unshelve.
  - by eauto.
  - by lia.
Qed.
Next Obligation. by eauto 8. Qed.

Program Definition upper_bound l r p :
  (exists2 m, l <= m < r & forall n H, p n H = true <-> n <= m) ->
  { m | l <= m < r & forall n H, p n H = true <-> n <= m } := fun _ =>
  let (m, _) := upper_bound' l r p (well_founded_ltof _ _ _) _ in
  exist2 _ _ m _ _.
Next Obligation.
  exists H. (repeat split) => // ? ? /H1; by apply.
Qed.
Next Obligation.
  split.
  - exact /H1.
  - exact /H2.
Qed.

Program Fixpoint lower_bound' l r
  (p : forall n, l < n <= r -> bool)
  (Hacc : @Acc _ (ltof _ (fun p => Z.to_nat (snd p - fst p))) (l, r))
  (_ : exists m,
    l < m <= r /\
    (forall n H, p n H = true -> m <= n) /\
    (forall n H, m <= n -> p n H = true)) :
  { m | l < m <= r /\
    (forall n H, p n H = true -> m <= n) /\
    (forall n H, m <= n -> p n H = true) } :=
  match Hacc with
  | Acc_intro _ Hacc' =>
      match Z_le_gt_dec r (Z.succ l) with
      | left _ => exist _ r _
      | right _ =>
          let m := (l + r) / 2 in
          ( if p m _ as b return p m _ = b -> _
            then fun _ =>
              let (n, _) := lower_bound' l m (fun n _ => p n _) (Hacc' _ _) _ in
              exist _ n _
            else fun _ =>
              let (n, _) := lower_bound' m r (fun n _ => p n _) (Hacc' _ _) _ in
              exist _ n _ ) eq_refl
      end
  end.
Next Obligation. auto with zarith. Qed.
Next Obligation.
  split.
  - suff : Z.succ l <= (l + r) / 2 by lia.
    by apply /Z.div_le_lower_bound; lia.
  - by apply /Z.div_le_upper_bound; lia.
Qed.
Next Obligation.
  suff [ ? ? ] : (l + r) / 2 <= r /\ Z.succ l <= (l + r) / 2 by apply /Z2Nat.inj_lt => /=; lia.
  split;
  [ apply /Z.div_le_upper_bound
  | apply /Z.div_le_lower_bound ]; lia.
Qed.
Next Obligation.
  suff : Z.succ l <= (l + r) / 2 by lia.
  by apply /Z.div_le_lower_bound; lia.
Qed.
Next Obligation.
  suff -> : n = H by [].
  apply /Z.le_antisymm.
  - by eapply H2, e, Z.le_refl.
  - by eapply l0, H3, Z.le_refl.
  Unshelve.
  + case (Z_le_gt_dec H ((l + r) / 2)) => [ Hle | ]; auto with zarith.
    by move: (e) (H0) => ->.
  + by [].
Qed.
Next Obligation.
  exists H. repeat split; eauto.
  case (Z_le_gt_dec H ((l + r) / 2)) => [ Hle | ]; auto with zarith.
  by move: (e) (H0) => ->.
Qed.
Next Obligation.
  suff [ ? ? ] : l <= (l + r) / 2 /\ (l + r) / 2 < r by apply Z2Nat.inj_lt => /=; lia.
  split;
  [ apply /Z.div_le_lower_bound
  | apply /Z.div_lt_upper_bound ]; lia.
Qed.
Next Obligation.
  suff : (l + r) / 2 < r by lia.
  by apply /Z.div_lt_upper_bound; lia.
Qed.
Next Obligation.
  suff -> : n = H by [].
  have ? : H <= n by eapply l0, H3, Z.le_refl.
  have ? : n <= H by eapply H2, e, Z.le_refl.
  by lia.
  Unshelve.
  - by auto.
  - by lia.
Qed.
Next Obligation. by eauto 8. Qed.

Program Definition lower_bound l r p :
  (exists2 m, l < m <= r & forall n H, p n H = true <-> m <= n) ->
  { m | l < m <= r & forall n H, p n H = true <-> m <= n } := fun _ =>
  let (m, _) := lower_bound' l r p (well_founded_ltof _ _ _) _ in
  exist2 _ _ m _ _.
Next Obligation.
  exists H. (repeat split) => // ? ? /H1; by apply.
Qed.
Next Obligation.
  split.
  - exact /H1.
  - exact /H2.
Qed.

Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inductive positive => int ["(fun n -> 2 * n + 1)" "(( * ) 2)" "1"] "(fun fI fO f1 n -> if n = 1 then f1 () else if n mod 2 = 0 then fO (n / 2) else fI (n / 2))".
Extract Inductive Z => int ["0" "" "( ~- )"] "(fun fO fpos fneg n -> if n = 0 then fO () else if 0 < n then fpos n else fneg (~-n))".

Extract Inlined Constant Z.succ => "succ".
Extract Inlined Constant Z.add => "( + )".
Extract Inlined Constant Z.sub => "( - )".
Extract Inlined Constant Z.div => "( / )".
Extract Inlined Constant Z_le_gt_dec => "( <= )".
Extraction "bsearch.ml" lower_bound upper_bound.
