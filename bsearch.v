Require Import Arith Div2 Omega Recdef.

Function bsearch_aux (p : nat -> bool) offset n { wf lt n } :=
  match n with
  | O => offset
  | _ =>
      let m := div2 n in
      if p (offset + m) then bsearch_aux p offset m
      else bsearch_aux p (S m + offset) (n - S m)
  end.
Proof.
  + intros. apply lt_div2. omega.
  + intros. omega.
  + apply lt_wf.
Defined.

Lemma bsearch_aux_spec : forall p n offset error,
  (forall n, offset + error <= n -> p n = true) ->
  (forall n, p n = true -> offset + error <= n) ->
  error <= n ->
  bsearch_aux p offset n = offset + error.
Proof.
  intros p n.
  induction n as [[| n'] IHn] using lt_wf_ind;
    intros offset error H H' ?;
    rewrite bsearch_aux_equation.
  - omega.
  - remember (p (offset + div2 (S n'))) as b.
    symmetry in Heqb.
    destruct b.
    + apply IHn; eauto.
      * apply lt_div2.
        omega.
      * specialize (H' _ Heqb).
        omega.
    + destruct (le_dec (offset + error) (offset + div2 (S n'))) as [ Hle | ].
      * apply H in Hle.
        congruence.
      * { rewrite IHn with (error := error - S (div2 (S n'))); try omega.
          - intros ? ?.
            apply H.
            omega.
          - intros ? Hp.
            specialize (H' _ Hp).
            omega. }
Qed.

Definition bsearch p alpha beta :=
  bsearch_aux p alpha (beta - alpha).

Theorem bsearch_spec : forall p threshold,
  (forall n, threshold <= n -> p n = true) ->
  (forall n, p n = true -> threshold <= n) ->
  forall alpha beta,
  alpha <= threshold <= beta ->
  bsearch p alpha beta = threshold.
Proof.
  unfold bsearch.
  intros ? ? H H' alpha beta ?.
  rewrite bsearch_aux_spec with (error := threshold - alpha); try omega.
  - intros ? ?.
    apply H.
    omega.
  - intros ? Htrue.
    specialize (H' _ Htrue).
    omega.
Qed.

Definition bsearch' p alpha beta :=
  bsearch (fun x => negb (p (S x))) alpha beta.

Lemma bsearch'_spec : forall p threshold,
  (forall n, n <= threshold -> p n = true) ->
  (forall n, p n = true -> n <= threshold) ->
  forall alpha beta,
  alpha <= threshold <= beta ->
  bsearch' p alpha beta = threshold.
Proof.
  unfold bsearch'.
  intros ? ? H H' ? ? ?.
  rewrite bsearch_spec with (threshold := threshold); try omega.
  - intros n ?.
    remember (p (S n)) as b.
    symmetry in Heqb.
    destruct b; simpl.
    + specialize (H' _ Heqb).
      omega.
    + reflexivity.
  - intros n Hp.
    destruct (le_dec (S n) threshold) as [ Hle | ].
    + rewrite (H _ Hle) in *.
      simpl in Hp.
      congruence.
    + omega.
Qed.

(* sqrt 4 *)
Eval compute in
  (bsearch
    (fun n =>
      if le_dec 4 (n * n) then true
      else false) 0 10).

Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive nat => int ["0" "succ"] "(fun fO fS n -> if n = 0 then fO () else fS (n-1))".

Extract Constant plus => "( + )".
Extract Constant minus => "( - )".
Extract Constant div2 => "(fun x -> x / 2)".
Extract Constant negb => "not".
Extraction "bsearch.ml" bsearch bsearch'.
