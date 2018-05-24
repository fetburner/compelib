Require Import Bool Arith Omega Program.

Program Fixpoint upper_bound' l r
  (p : forall n, l <= n < r -> bool)
  (Hacc : @Acc _ (ltof _ (fun p => snd p - fst p)) (l, r))
  (_ : exists m,
    l <= m < r /\
    (forall n H, p n H = true -> n <= m) /\
    (forall n H, n <= m -> p n H = true)) :
  { m | l <= m < r /\
    (forall n H, p n H = true -> n <= m) /\
    (forall n H, n <= m -> p n H = true) } :=
  match Hacc with
  | Acc_intro _ Hacc' =>
      if le_gt_dec r (S l)
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
  - apply (Nat.div_le_lower_bound _ 2); omega.
  - apply (Nat.div_lt_upper_bound _ 2); omega.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Nat.div_lt_upper_bound; omega. }
  unfold ltof. simpl in *. omega.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Nat.div_lt_upper_bound; omega. }
  simpl in *. omega.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (n <= H) by eapply l0, H4, le_n.
  assert (H <= n) by eapply H3, e, le_n.
  omega.
  Unshelve.
  - eauto.
  - destruct (le_gt_dec ((l + r) / 2) H) as [ Hle | ]; simpl in *; try omega.
    generalize H1. erewrite (e _ _ Hle). inversion 1.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
  destruct (le_gt_dec ((l + r) / 2) H) as [ Hle | ]; simpl in *; try omega.
  generalize H1. erewrite (e _ _ Hle). inversion 1.
Qed.
Next Obligation.
  assert (l < (l + r) / 2).
  { apply Nat.div_le_lower_bound; omega. }
  unfold ltof. simpl in *. omega.
Qed.
Next Obligation.
  assert (l < (l + r) / 2).
  { apply Nat.div_le_lower_bound; omega. }
  unfold ltof. simpl in *. omega.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (H <= n) by eapply H3, e, le_n.
  assert (n <= H) by eapply l0, H4, le_n.
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
  (Hacc : @Acc _ (ltof _ (fun p => snd p - fst p)) (l, r))
  (_ : exists m,
    l < m <= r /\
    (forall n H, p n H = true -> m <= n) /\
    (forall n H, m <= n -> p n H = true)) :
  { m | l < m <= r /\
    (forall n H, p n H = true -> m <= n) /\
    (forall n H, m <= n -> p n H = true) } :=
  match Hacc with
  | Acc_intro _ Hacc' =>
      if le_gt_dec r (S l)
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
  - apply (Nat.div_le_lower_bound _ 2); omega.
  - apply (Nat.div_le_upper_bound _ 2); omega.
Qed.
Next Obligation.
  assert (l < (l + r) / 2).
  { apply Nat.div_le_lower_bound; omega. }
  unfold ltof. simpl in *. omega.
Qed.
Next Obligation.
  assert (l < (l + r) / 2).
  { apply Nat.div_le_lower_bound; omega. }
  simpl in *. omega.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (n <= H) by eapply H3, e, le_n.
  assert (H <= n) by eapply l0, H4, le_n.
  omega.
  Unshelve.
  - destruct (le_gt_dec H ((l + r) / 2)) as [ Hle | ]; simpl in *; try omega.
    generalize H1. erewrite (e _ _ Hle). inversion 1.
  - eauto.
Qed.
Next Obligation.
  exists H. repeat split; eauto.
  destruct (le_gt_dec H ((l + r) / 2)) as [ Hle | ]; simpl in *; try omega.
  generalize H1. erewrite (e _ _ Hle). inversion 1.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Nat.div_lt_upper_bound; omega. }
  unfold ltof. simpl in *. omega.
Qed.
Next Obligation.
  assert ((l + r) / 2 < r).
  { apply Nat.div_lt_upper_bound; omega. }
  unfold ltof. simpl in *. omega.
Qed.
Next Obligation.
  replace n with H; eauto.
  assert (H <= n) by eapply l0, H4, le_n.
  assert (n <= H) by eapply H3, e, le_n.
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
Extract Inductive nat => int ["0" "succ"] "(fun fO fS n -> if n = 0 then fO () else fS (n-1))".

Extract Constant plus => "( + )".
Extract Constant minus => "( - )".
Extract Constant Nat.div => "( / )".
Extract Constant le_gt_dec => "( <= )".
Extraction "bsearch.ml" lower_bound upper_bound.
