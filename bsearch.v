Require Import Bool ZArith Omega Program.

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
Next Obligation.
  repeat split; intros; try omega.
  eapply H3. omega.
Qed.
Next Obligation.
  split.
  - apply (Z.div_le_lower_bound _ 2); omega.
  - apply (Z.div_lt_upper_bound _ 2); omega.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Z.div_lt_upper_bound; omega. }
  assert (l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; omega. }
  unfold ltof. simpl in *. apply Z2Nat.inj_lt; omega.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Z.div_lt_upper_bound; omega. }
  simpl in *. omega.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (n <= H) by eapply l0, H4, Z.le_refl.
  assert (H <= n) by eapply H3, e, Z.le_refl.
  omega.
  Unshelve.
  - eauto.
  - destruct (Z_le_gt_dec ((l + r) / 2) H) as [ Hle | ]; simpl in *; try omega.
    generalize H1. erewrite (e _ _ Hle). inversion 1.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
  destruct (Z_le_gt_dec ((l + r) / 2) H) as [ Hle | ]; simpl in *; try omega.
  generalize H1. erewrite (e _ _ Hle). inversion 1.
Qed.
Next Obligation.
  assert (Z.succ l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; omega. }
  assert ((l + r) / 2 <= r).
  { apply Z.div_le_upper_bound; omega. }
  unfold ltof. simpl in *. apply Z2Nat.inj_lt; omega.
Qed.
Next Obligation.
  assert (Z.succ l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; omega. }
  unfold ltof. simpl in *. omega.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (H <= n) by eapply H3, e, Z.le_refl.
  assert (n <= H) by eapply l0, H4, Z.le_refl.
  omega.
  Unshelve.
  - eauto.
  - generalize (l0 _ _ H1). omega.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
Qed.

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
Next Obligation.
  repeat split; intros; try omega.
  eapply H3. omega.
Qed.
Next Obligation.
  split.
  - assert (Z.succ l <= (l + r) / 2) by (apply (Z.div_le_lower_bound _ 2); omega). omega.
  - apply (Z.div_le_upper_bound _ 2); omega.
Qed.
Next Obligation.
  assert ((l + r) / 2 <= r).
  { apply Z.div_le_upper_bound; omega. }
  assert (Z.succ l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; omega. }
  unfold ltof. simpl in *. apply Z2Nat.inj_lt; omega.
Qed.
Next Obligation.
  assert (Z.succ l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; omega. }
  simpl in *. omega.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (n <= H) by eapply H3, e, Z.le_refl.
  assert (H <= n) by eapply l0, H4, Z.le_refl.
  omega.
  Unshelve.
  - destruct (Z_le_gt_dec H ((l + r) / 2)) as [ Hle | ]; simpl in *; try omega.
    generalize H1. erewrite (e _ _ Hle). inversion 1.
  - eauto.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
  destruct (Z_le_gt_dec H ((l + r) / 2)) as [ Hle | ]; simpl in *; try omega.
  generalize H1. erewrite (e _ _ Hle). inversion 1.
Qed.
Next Obligation.
  assert (l <= (l + r) / 2).
  { apply Z.div_le_lower_bound; omega. }
  assert ((l + r) / 2 < r).
  { apply Z.div_lt_upper_bound; omega. }
  unfold ltof. simpl in *. apply Z2Nat.inj_lt; omega.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Z.div_lt_upper_bound; omega. }
  unfold ltof. simpl in *. omega.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (H <= n) by eapply l0, H4, Z.le_refl.
  assert (n <= H) by eapply H3, e, Z.le_refl.
  omega.
  Unshelve.
  - eauto.
  - generalize (l0 _ _ H1). omega.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
Qed.

Definition lower_bound l r p :=
  lower_bound' l r p (well_founded_ltof _ _ _).

Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inductive positive => int ["(fun n -> 2 * n + 1)" "(( * ) 2)" "1"] "(fun fI fO f1 n -> if n = 1 then f1 () else if n mod 2 = 0 then fO (n / 2) else fI (n / 2))".
Extract Inductive Z => int ["0" "" "( ~- )"] "(fun fO fpos fneg n -> if n = 0 then fO () else if 0 < n then fpos n else fneg (~-n))".

Extract Constant Z.add => "( + )".
Extract Constant Z.sub => "( - )".
Extract Constant Z.div => "( / )".
Extract Constant Z_le_gt_dec => "( <= )".
Extraction "bsearch.ml" lower_bound upper_bound.
