Require Import Bool ZArith Lia Program.

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
      if Z_le_gt_dec r (Z.succ l)
      then exist _ l _
      else
        let m := (l + r) / 2 in
        ( if p m _ as b return p m _ = b -> _
          then fun _ =>
            let (n, _) := upper_bound' m r (fun n _ => p n _) (Hacc' _ _) _ in
            exist _ n _
          else fun _ =>
            let (n, _) := upper_bound' l m (fun n _ => p n _) (Hacc' _ _) _ in
            exist _ n _ ) eq_refl
  end.
Next Obligation. auto with zarith. Qed.
Next Obligation.
  split.
  - apply (Z.div_le_lower_bound _ 2); lia.
  - apply (Z.div_lt_upper_bound _ 2); lia.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Z.div_lt_upper_bound; lia. }
  assert (l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; lia. }
  apply Z2Nat.inj_lt; simpl; lia.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Z.div_lt_upper_bound; lia. }
  lia.
Qed.
Next Obligation.
  replace n with H; auto with zarith.
  assert (n <= H) by eapply l0, H4, Z.le_refl.
  assert (H <= n) by eapply H3, e, Z.le_refl.
  lia.
  Unshelve.
  - auto.
  - destruct (Z_le_gt_dec ((l + r) / 2) H) as [ Hle | ]; auto with zarith.
    generalize H1. erewrite (e _ _ Hle). congruence.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
  destruct (Z_le_gt_dec ((l + r) / 2) H) as [ Hle | ]; auto with zarith.
  generalize H1. erewrite (e _ _ Hle). congruence.
Qed.
Next Obligation.
  assert (Z.succ l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; lia. }
  assert ((l + r) / 2 <= r).
  { apply Z.div_le_upper_bound; lia. }
  apply Z2Nat.inj_lt; simpl; lia.
Qed.
Next Obligation.
  assert (Z.succ l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; lia. }
  lia.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (H <= n) by eapply H3, e, Z.le_refl.
  assert (n <= H) by eapply l0, H4, Z.le_refl.
  lia.
  Unshelve.
  - eauto.
  - lia.
Qed.
Next Obligation. eauto 8. Qed.

Definition upper_bound l r p :=
  upper_bound' l r p (well_founded_ltof _ _ _).

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
      if Z_le_gt_dec r (Z.succ l)
      then exist _ r _
      else
        let m := (l + r) / 2 in
        ( if p m _ as b return p m _ = b -> _
          then fun _ =>
            let (n, _) := lower_bound' l m (fun n _ => p n _) (Hacc' _ _) _ in
            exist _ n _
          else fun _ =>
            let (n, _) := lower_bound' m r (fun n _ => p n _) (Hacc' _ _) _ in
            exist _ n _ ) eq_refl
  end.
Next Obligation. auto with zarith. Qed.
Next Obligation.
  split.
  - assert (Z.succ l <= (l + r) / 2) by (apply (Z.div_le_lower_bound _ 2); lia). lia.
  - apply (Z.div_le_upper_bound _ 2); lia.
Qed.
Next Obligation.
  assert ((l + r) / 2 <= r).
  { apply Z.div_le_upper_bound; lia. }
  assert (Z.succ l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; lia. }
  apply Z2Nat.inj_lt; simpl; lia.
Qed.
Next Obligation.
  assert (Z.succ l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; lia. }
  lia.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (n <= H) by eapply H3, e, Z.le_refl.
  assert (H <= n) by eapply l0, H4, Z.le_refl.
  lia.
  Unshelve.
  - destruct (Z_le_gt_dec H ((l + r) / 2)) as [ Hle | ]; try lia.
    generalize H1. erewrite (e _ _ Hle). congruence.
  - eauto.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
  destruct (Z_le_gt_dec H ((l + r) / 2)) as [ Hle | ]; try lia.
  generalize H1. erewrite (e _ _ Hle). congruence.
Qed.
Next Obligation.
  assert (l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; lia. }
  assert ((l + r) / 2 < r).
  { apply Z.div_lt_upper_bound; lia. }
  apply Z2Nat.inj_lt; simpl; omega.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Z.div_lt_upper_bound; lia. }
  lia.
Qed.
Next Obligation.
  replace n with H; auto.
  assert (H <= n) by eapply l0, H4, Z.le_refl.
  assert (n <= H) by eapply H3, e, Z.le_refl.
  lia.
  Unshelve.
  - auto.
  - auto with zarith.
Qed.
Next Obligation. eauto 8. Qed.

Definition lower_bound l r p :=
  lower_bound' l r p (well_founded_ltof _ _ _).

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
