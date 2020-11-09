Require Import ssreflect Nat Arith Lia Program.

Lemma iter_S A f : forall m x,
  @Nat.iter m A f (f x) = Nat.iter (S m) f x.
Proof. elim => //= ? IH ?. by rewrite IH. Qed.

Lemma iter_plus A f n x : forall m,
  @Nat.iter (m + n) A f x = Nat.iter m f (Nat.iter n f x).
Proof. by elim => //= ? ->. Qed.

Lemma iter_mult A f n x : forall m,
  @Nat.iter (m * n) A f x = @Nat.iter m _ (@Nat.iter n _ f) x.
Proof. elim => //= ?. by rewrite iter_plus => ->. Qed.

Section CycleDetection.
  Variable A : Set.
  Variable eq_dec : forall x y : A, { x = y } + { x <> y }.
  Variable f : A -> A.

  Definition eventually_periodic m0 l n : Prop :=
    l > 0 /\ forall m, m0 <= m -> iter (l + m) f n = iter m f n.

  Lemma tortoise_and_hare g u v
    (Hex : exists n, @Nat.iter n _ f u = @Nat.iter n _ g v) :
    { '(n, w) | w = @Nat.iter n _ f u /\ w = @Nat.iter n _ g v /\ forall n', @Nat.iter n' _ f u = @Nat.iter n' _ g v -> n <= n' }.
  Proof.
    refine (@Fix _
      (fun n m => S m = n /\ (forall m, m < n -> @Nat.iter m _ f u <> @Nat.iter m _ g v)) _
      (fun n =>
        forall u', u' = @Nat.iter n _ f u ->
        forall v', v' = @Nat.iter n _ g v ->
        (forall m, m < n -> @Nat.iter m _ f u <> @Nat.iter m _ g v) ->
        { '(n, w) | w = @Nat.iter n _ f u /\ w = @Nat.iter n _ g v /\ forall n', @Nat.iter n' _ f u = @Nat.iter n' _ g v -> n <= n' })
      (fun n tortoise_and_hare u' Hu v' Hv Hneq =>
         match eq_dec u' v' with
         | left _ => exist _ (n, u') (conj _ (conj _ (fun n' => _)))
         | right _ => tortoise_and_hare (S n) (conj _ _) (f u') _ (g v') _ _
         end) 0 u _ v _ _); subst; eauto.
    - case Hex => n Hmeet m.
      remember (n - m) as d.
      generalize dependent m.
      induction d as [ | ? ] => m ?; constructor => ? [ ? Hnp ]; subst.
      + (have : (n < S m) by lia) => /Hnp. congruence.
      + apply /IHd. lia.
    - by case (le_lt_dec n n') => // Hlt /(Hneq _ Hlt).
    - move => m /le_S_n Hle.
      by case (le_lt_dec n m) => [ /(@Nat.le_antisymm _ _ Hle) -> | /Hneq ].
    - move => m /le_S_n Hle.
      by case (le_lt_dec n m) => [ /(@Nat.le_antisymm _ _ Hle) -> | /Hneq ].
    - lia.
  Defined.

  Theorem detect_cycle v
    (Hperiod : exists m l, eventually_periodic m l v /\ forall m' l', eventually_periodic m' l' v -> m <= m' /\ l <= l') :
    {'(m, l) | eventually_periodic m l v /\ forall m' l', eventually_periodic m' l' v -> m <= m' /\ l <= l' }.
  Proof.
    case (tortoise_and_hare (fun x => f (f x)) (f v) (f (f v))) => [ | [ m u ] Hm ].
    { move: Hperiod => [ mu' [ l' [ [ Hpos Hperiod ] Hleast ] ] ].
      exists ((mu' + 2) * l' - 1).
      set j := (mu' + 2) * l' - 1.
      have -> : (fun x => f (f x)) = @iter 2 _ f by [].
      rewrite -iter_mult Nat.mul_comm /= Nat.add_0_r !iter_S.
      have -> : S (S (j + j)) = (mu' + 2) * l' + S j by lia.
      elim: (mu' + 2) => [ // | ? ? /= ].
      rewrite -plus_assoc Hperiod //. nia. }
    case (tortoise_and_hare f u v) => [ | [ mu w ] Hmu ].
    { exists (S m).
      move: Hm => [ -> [ ] ].
      have -> : (fun x => f (f x)) = Nat.iter 2 f by [].
      by rewrite -iter_plus !iter_S -!iter_mult -(Nat.mul_comm 2) /= Nat.add_0_r Nat.add_succ_r. }
    case (tortoise_and_hare id (f w) w) => [ | [ l ? ] Hl ].
    { exists m.
      move: Hm Hmu => [ -> [ ? ? ] ] [ -> [ Heq ? ] ].
      have -> : id = Nat.iter 0 f by [].
      by rewrite -iter_mult -(Nat.mul_comm 0) Heq !iter_S -iter_plus Nat.add_comm iter_plus -iter_S Heq. }
    exists (mu, S l).
    move: Hperiod => [ mu' [ l' [ [ Hpos Hperiod ] Hleast ] ] ].
    have /Hleast [ ? Hlel ] : eventually_periodic mu (S l) v.
    { split => [ | m0 ? ]; auto with arith.
      have -> : m0 = (m0 - mu) + mu by lia.
      have -> : S l + (m0 - mu + mu) = (m0 - mu) + (S l + mu) by lia.
      move: Hmu Hl => [ -> [ -> ? ] ] [ -> [ ] ].
      rewrite !iter_plus -iter_S => ->.
      have -> : id = Nat.iter 0 f by [].
      by rewrite -iter_mult Nat.mul_0_r. }
    have /Hleast [ Hlemu Hlem ] : eventually_periodic mu (S m) v.
    { split => [ | m0 ? ]; auto with arith.
      have -> : m0 = (m0 - mu) + mu by lia.
      have -> : S m + (m0 - mu + mu) = (m0 - mu) + (S (mu + m)) by lia.
      move: Hm Hmu => [ -> [ ? ? ] ] [ -> [ ] ].
      by rewrite -iter_plus iter_S !(@iter_plus _ _ _ _ (m0 - mu)) => ->. }
    case (zerop (S m mod l')) => Hmod.
    - have : mu <= mu' => [ | /(le_antisym _ _ Hlemu) ? ]; subst.
      { apply /(proj2 (proj2 Hmu)).
        move: Hm => [ -> [ ? ? ] ].
        rewrite iter_S -iter_plus Nat.add_comm (Nat.div_mod (S m) l'). { lia. }
        rewrite Hmod -plus_n_O mult_comm.
        elim: (S m / l') => //= ? IH.
        by rewrite -plus_assoc iter_plus IH -iter_plus Hperiod. }
      have : S l <= l' => [ | /(le_antisym _ _ Hlel) <- // ].
      { rewrite (S_pred_pos l') //.
        apply /le_n_S /(proj2 (proj2 Hl)).
        have -> : id = Nat.iter 0 f by [].
        move: Hm Hmu => [ -> [ ? ? ] ] [ -> [ ? ? ] ].
        rewrite 2!iter_S -S_pred_pos // -iter_mult Nat.mul_0_r -!iter_plus -plus_assoc Hperiod //.
        lia. }
    - have : forall i, i * l' <= S m -> iter (S m - i * l' + mu) f v = iter mu f v => [ | /(_ (S m / l')) ].
      { elim => // [ ? | i IH ? ].
        - move: Hm Hmu => [ -> [ ? ? ] ] [ -> [ ] ].
          by rewrite Nat.mul_0_l Nat.sub_0_r Nat.add_comm iter_plus iter_S /= iter_S.
        - rewrite -IH. { lia. }
          have->: S m - i * l' + mu = l' + (S m - S i * l' + mu) by lia.
          rewrite Hperiod //. lia. }
      rewrite (mult_comm _ l') -Nat.mod_eq => [ | Hperiod' ]. { lia. }
      have : eventually_periodic mu (S m mod l') v => [ | /Hleast [ ? /le_not_lt [ ] ] ].
      { split => [ // | m0 ? ].
        have->: m0 = (m0 - mu) + mu by lia.
        have->: S m mod l' + (m0 - mu + mu) = (m0 - mu) + (S m mod l' + mu) by lia.
        rewrite !(@iter_plus _ _ _ _ (m0 - mu)) Hperiod' //.
        apply /Nat.mul_div_le. lia. }
      apply /Nat.mod_upper_bound. lia.
  Defined.
End CycleDetection.

Extract Inductive nat => "int" [ "0" "Stdlib.succ" ].
Extract Inductive prod => "(*)" [ "(,)" ].
Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inlined Constant id => "Fun.id".
Extract Inlined Constant add => "( + )".
Extraction "cycleDetection.ml" detect_cycle.
