Require Import Arith Omega Recdef.

Section LowerBound.
  Variable P : nat -> Prop.
  Variable threshold : nat.
  Hypothesis P_dec : forall n, { P n } + { ~P n }.
  Hypothesis P_monotone1 : forall n, threshold <= n -> P n.
  Hypothesis P_monotone2 : forall n, P n -> threshold <= n.

  Function lower_bound_aux offset n { wf lt n } :=
    match n with
    | O => offset
    | _ =>
      let m := Nat.div2 n in
      if P_dec (offset + m) then lower_bound_aux offset m
      else lower_bound_aux (S m + offset) (n - S m)
    end.
  Proof.
    - intros. apply Nat.lt_div2. omega.
    - intros. omega.
    - apply lt_wf.
  Defined.

  Lemma lower_bound_aux_spec n : forall offset error,
    threshold = offset + error ->
    error <= n ->
    lower_bound_aux offset n = offset + error.
  Proof.
    induction n as [[| n'] IHn] using lt_wf_ind;
      intros offset error ? ?;
      subst;
      rewrite lower_bound_aux_equation.
    - omega.
    - destruct (P_dec (offset + Nat.div2 (S n'))) as [ HP | ].
      + apply IHn; eauto.
        * apply Nat.lt_div2.
          omega.
        * specialize (P_monotone2 _ HP).
          omega.
      + destruct (le_dec (offset + error) (offset + Nat.div2 (S n'))) as [ Hle | ].
        * apply P_monotone1 in Hle.
          congruence.
        * rewrite IHn with (error0 := error - S (Nat.div2 (S n'))); try omega.
  Qed.

  Definition lower_bound alpha beta :=
    lower_bound_aux alpha (beta - alpha).

  Theorem lower_bound_spec alpha beta :
    alpha <= threshold <= beta ->
    lower_bound alpha beta = threshold.
  Proof.
    unfold lower_bound.
    intros ?.
    rewrite lower_bound_aux_spec with (error := threshold - alpha); try omega.
  Qed.
End LowerBound.

Section UpperBound.
  Variable P : nat -> Prop.
  Variable threshold : nat.
  Hypothesis P_dec : forall n, { P n } + { ~P n }.
  Hypothesis P_monotone1 : forall n, n <= threshold -> P n.
  Hypothesis P_monotone2 : forall n, P n -> n <= threshold.
  
  Program Definition upper_bound := lower_bound (fun n => ~P (S n)) _.
  Next Obligation.
    destruct (P_dec (S n)); eauto.
  Qed.

  Lemma upper_bound_spec alpha beta :
    alpha <= threshold <= beta ->
    upper_bound alpha beta = threshold.
  Proof.
    unfold upper_bound.
    intros ?.
    rewrite lower_bound_spec with (threshold := threshold); try omega.
    - intros n ?.
      destruct (P_dec (S n)) as [ HP | ].
      + specialize (P_monotone2 _ HP).
        omega.
      + eauto.
    - intros n Hp.
      destruct (le_dec (S n) threshold) as [ Hle | ].
      + exfalso.
        eauto.
      + omega.
  Qed.
End UpperBound.

(* sqrt 4 *)
Eval compute in
  (lower_bound
     (fun n => 4 <= n * n)
     (fun n => match le_dec 4 (n * n) with left H => left H | right H => right H end) 0 10).

Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inductive nat => int ["0" "succ"] "(fun fO fS n -> if n = 0 then fO () else fS (n-1))".

Extract Constant plus => "( + )".
Extract Constant minus => "( - )".
Extract Constant Nat.div2 => "(fun x -> x / 2)".
Extraction "bsearch.ml" lower_bound upper_bound.
