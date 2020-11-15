Require Import ssreflect ssrbool List Program.

Module Type Char.
  Parameter t : Set.
  Parameter eq_dec : forall c c' : t, { c = c' } + { c <> c' }.
End Char.

Module F (C : Char).
  Definition character := C.t.

  Inductive sort :=
    | Other
    | EmptySet
    | UniversalSet.

  CoInductive t := regexp (sort : sort) (is_nullable : bool) (derive : character -> t) : t.

  Definition frob t :=
    match t with
    | regexp sort is_nullable derive =>
      regexp sort is_nullable derive
    end.

  Lemma frob_eq : forall t, t = frob t.
  Proof. by case. Qed.

  Fixpoint matches re s :=
    match re, s with
    | regexp _ is_nullable _, nil => is_nullable
    | regexp _ _ derive, c :: s => matches (derive c) s
    end.

  CoInductive well_sorted : t -> Prop :=
    | well_sorted_other is_nullable derive :
        (forall c, well_sorted (derive c)) ->
        well_sorted (regexp Other is_nullable derive)
    | well_sorted_empty_set is_nullable derive :
        let re := regexp EmptySet is_nullable derive in
        (forall c, well_sorted (derive c)) ->
        (forall s, ~~ matches re s) ->
        well_sorted re
    | well_sorted_universal_set is_nullable derive :
        let re := regexp UniversalSet is_nullable derive in
        (forall c, well_sorted (derive c)) ->
        (forall s, matches re s) ->
        well_sorted re.

  CoFixpoint empty_set := regexp EmptySet false (fun _ => empty_set).

  Corollary empty_set_eq : empty_set = regexp EmptySet false (fun _ => empty_set).
  Proof. exact /frob_eq. Qed.

  Lemma empty_set_spec : forall s, ~~ matches empty_set s.
  Proof. by elim. Qed.

  Lemma empty_set_well_sorted : well_sorted empty_set.
  Proof.
    cofix empty_set_well_sorted.
    rewrite empty_set_eq.
    apply /well_sorted_empty_set => ? //.
    by rewrite -empty_set_eq empty_set_spec.
  Qed.

  CoFixpoint universal_set := regexp UniversalSet true (fun _ => universal_set).

  Lemma universal_set_eq : universal_set = regexp UniversalSet true (fun _ => universal_set).
  Proof. exact /frob_eq. Qed.

  Lemma universal_set_spec : forall s, matches universal_set s.
  Proof. by elim. Qed.

  Lemma universal_set_well_sorted : well_sorted universal_set.
  Proof.
    cofix universal_set_well_sorted.
    rewrite universal_set_eq.
    apply /well_sorted_universal_set => ? //.
    by rewrite -universal_set_eq universal_set_spec.
  Qed.

  Definition empty_str := regexp Other true (fun _ => empty_set).

  Lemma empty_str_spec : forall s, matches empty_str s <-> s = nil.
  Proof.
    case => //= ? ?.
    by rewrite (negbTE (empty_set_spec _)).
  Qed.

  Lemma empty_str_well_sorted : well_sorted empty_str.
  Proof.
    apply well_sorted_other => // ?.
    exact empty_set_well_sorted.
  Qed.

  Definition char c := regexp Other false (fun c' =>
    match C.eq_dec c c' with
    | left _ => empty_str
    | right _ => empty_set
    end).

  Lemma char_spec c : forall s, matches (char c) s <-> s = cons c nil.
  Proof.
    case => //= c' ?.
    case (C.eq_dec c c') => [ -> | ? ].
    - split => [ /empty_str_spec -> // | ].
      inversion 1.
      exact /empty_str_spec.
    - rewrite (negbTE (empty_set_spec _)).
      split => //.
      by inversion 1; subst.
  Qed.

  Lemma char_well_sorted c : well_sorted (char c).
  Proof.
    apply /well_sorted_other => // c'.
    case (C.eq_dec c c') => ?.
    + exact /empty_str_well_sorted.
    + exact /empty_set_well_sorted.
  Qed.

  CoFixpoint neg re :=
    match re with
    | regexp EmptySet _ _ => universal_set
    | regexp UniversalSet _ _ => empty_set
    | regexp _ is_nullable derive =>
      regexp Other (~~ is_nullable) (fun c => neg (derive c))
    end.

  Lemma neg_eq re : neg re =
    match re with
    | regexp EmptySet _ _ => universal_set
    | regexp UniversalSet _ _ => empty_set
    | regexp _ is_nullable derive =>
      regexp Other (~~ is_nullable) (fun c => neg (derive c))
    end.
  Proof.
    rewrite (frob_eq (neg re)).
    move: re => [ [ ] ? ? ] //=.
    - by rewrite -universal_set_eq.
    - by rewrite -empty_set_eq.
  Qed.

  Lemma neg_spec : forall s re,
    well_sorted re ->
    matches (neg re) s = ~~ matches re s.
  Proof.
    elim => [ | c s IH ]; inversion 1; subst => //=.
    - by move: (H1 nil) => /= ->.
    - by move: (H1 nil) => /= ->.
    - exact /IH.
    - by move: (H1 (c :: s)) (universal_set_spec s) => /= -> ->.
    - by move: (H1 (c :: s)) (negbTE (empty_set_spec s)) => /= -> ->.
  Qed.

  Lemma neg_well_sorted : forall re,
    well_sorted re ->
    well_sorted (neg re).
  Proof.
    cofix neg_well_sorted.
    inversion 1; subst; rewrite neg_eq.
    - apply /well_sorted_other => ?.
      exact /neg_well_sorted.
    - rewrite /re0.
      exact /universal_set_well_sorted.
    - rewrite /re0.
      exact /empty_set_well_sorted.
  Qed.

  CoFixpoint inter re re' :=
    match re, re' with
    | regexp EmptySet _ _, _ => empty_set
    | regexp UniversalSet _ _, _ => re'
    | _, regexp EmptySet _ _ => empty_set
    | _, regexp UniversalSet _ _ => re
    | regexp Other is_nullable derive, regexp Other is_nullable' derive' =>
      regexp Other (is_nullable && is_nullable') (fun c => inter (derive c) (derive' c))
    end.

  Lemma inter_eq re re' : inter re re' =
    match re, re' with
    | regexp EmptySet _ _, _ => empty_set
    | regexp UniversalSet _ _, _ => re'
    | _, regexp EmptySet _ _ => empty_set
    | _, regexp UniversalSet _ _ => re
    | regexp Other is_nullable derive, regexp Other is_nullable' derive' =>
      regexp Other (is_nullable && is_nullable') (fun c => inter (derive c) (derive' c))
    end.
  Proof.
    rewrite (frob_eq (inter re re')).
    move: re re' => [ [ ] ? ? ] [ [ ] ? ? ] //=;
    by rewrite -empty_set_eq.
  Qed.

  Lemma inter_spec : forall s,
    forall re, well_sorted re ->
    forall re', well_sorted re' ->
    matches (inter re re') s = matches re s && matches re' s.
  Proof.
    elim => [ | c s IH ]; inversion 1 => /= ?.
    - inversion 1 => //=.
      + by move: (negbTE (H4 nil)) andbF => /= ->.
      + by move: (H4 nil) andbT => /= ->.
    - by move: (negbTE (H1 nil)) => /= ->.
    - by move: (H1 nil) => /= ->.
    - inversion 1 => //=.
      + by rewrite IH.
      + move: (negbTE (H4 (c :: s))) => /= ->.
        by rewrite (negbTE (empty_set_spec _)) andbF.
      + by move: (H4 (c :: s)) andbT => /= ->.
    - move: (negbTE (H1 (c :: s))) => /= ->.
      by rewrite (negbTE (empty_set_spec _)).
    - by move: (H1 (c :: s)) => /= ->.
  Qed.

  Lemma inter_well_sorted :
    forall re, well_sorted re ->
    forall re', well_sorted re' ->
    well_sorted (inter re re').
  Proof.
    cofix inter_well_sorted.
    inversion 1 => [ | ? | ? ].
    - inversion 1.
      + rewrite inter_eq.
        apply /well_sorted_other => ?.
        exact /inter_well_sorted.
      + rewrite inter_eq /re0.
        exact /empty_set_well_sorted.
      + rewrite inter_eq /re0.
        exact /well_sorted_other.
    - rewrite inter_eq /re0 => ?.
      exact /empty_set_well_sorted.
    - by rewrite inter_eq /re0.
  Qed.

  CoFixpoint union re re' :=
    match re, re' with
    | regexp EmptySet _ _, _ => re'
    | regexp UniversalSet _ _, _ => universal_set
    | _, regexp EmptySet _ _ => re
    | _, regexp UniversalSet _ _ => universal_set
    | regexp Other is_nullable derive, regexp Other is_nullable' derive' =>
      regexp Other (is_nullable || is_nullable') (fun c => union (derive c) (derive' c))
    end.

  Lemma union_eq re re' : union re re' =
    match re, re' with
    | regexp EmptySet _ _, _ => re'
    | regexp UniversalSet _ _, _ => universal_set
    | _, regexp EmptySet _ _ => re
    | _, regexp UniversalSet _ _ => universal_set
    | regexp Other is_nullable derive, regexp Other is_nullable' derive' =>
      regexp Other (is_nullable || is_nullable') (fun c => union (derive c) (derive' c))
    end.
  Proof.
    rewrite (frob_eq (union re re')).
    move: re re' => [ [ ] ? ? ] [ [ ] ? ? ] //=;
    by rewrite -universal_set_eq.
  Qed.

  Lemma union_spec : forall s,
    forall re, well_sorted re ->
    forall re', well_sorted re' ->
    matches (union re re') s = matches re s || matches re' s.
  Proof.
    elim => [ | c s IH ]; inversion 1 => /= ?.
    - inversion 1 => //=.
      + by move: (negbTE (H4 nil)) orbF => /= ->.
      + by move: (H4 nil) orbT => /= -> ->.
    - by move: (negbTE (H1 nil)) => /= ->.
    - by move: (H1 nil) => /= ->.
    - inversion 1 => //=.
      + by rewrite IH.
      + by move: (negbTE (H4 (c :: s))) orbF => /= ->.
      + move: (H4 (c :: s)) => /= ->.
        by rewrite universal_set_spec orbT.
    - by move: (negbTE (H1 (c :: s))) => /= ->.
    - move: (H1 (c :: s)) => /= ->.
      by rewrite universal_set_spec.
  Qed.

  Lemma union_well_sorted :
    forall re, well_sorted re ->
    forall re', well_sorted re' ->
    well_sorted (union re re').
  Proof.
    cofix union_well_sorted.
    inversion 1 => [ | ? | ? ].
    - inversion 1.
      + rewrite union_eq.
        apply /well_sorted_other => ?.
        exact /union_well_sorted.
      + rewrite union_eq /re0.
        exact /well_sorted_other.
      + rewrite union_eq /re0.
        exact /universal_set_well_sorted.
    - by rewrite union_eq /re0.
    - rewrite union_eq /re0 => ?.
      exact /universal_set_well_sorted.
  Qed.

  (* union (app re1 re2) re3 *)
  CoFixpoint union_app re1 re2 re3 :=
    match re1, re2, re3 with
    | regexp EmptySet _ _, _, _ => re3
    | regexp UniversalSet _ _, regexp _ true _, _ => universal_set
    | _, _, regexp UniversalSet _ _ => universal_set
    | regexp _ is_nullable1 derive1,
      regexp _ is_nullable2 derive2,
      regexp _ is_nullable3 derive3 =>
      regexp Other (is_nullable1 && is_nullable2 || is_nullable3) (fun c =>
        if is_nullable1
        then union_app (derive1 c) re2 (union (derive2 c) (derive3 c))
        else union_app (derive1 c) re2 (derive3 c))
    end.

  Lemma union_app_eq re1 re2 re3 : union_app re1 re2 re3 =
    match re1, re2, re3 with
    | regexp EmptySet _ _, _, _ => re3
    | regexp UniversalSet _ _, regexp _ true _, _ => universal_set
    | _, _, regexp UniversalSet _ _ => universal_set
    | regexp _ is_nullable1 derive1,
      regexp _ is_nullable2 derive2,
      regexp _ is_nullable3 derive3 =>
      regexp Other (is_nullable1 && is_nullable2 || is_nullable3) (fun c =>
        if is_nullable1
        then union_app (derive1 c) re2 (union (derive2 c) (derive3 c))
        else union_app (derive1 c) re2 (derive3 c))
    end.
  Proof.
    rewrite (frob_eq (union_app re1 re2 re3)).
    move: re1 re2 re3 => [ [ ] ? ? ] [ ? [ ] ? ] [ [ ] ? ? ] //=;
    by rewrite -universal_set_eq.
  Qed.

  Lemma union_app_spec : forall s,
    forall re1, well_sorted re1 ->
    forall re2, well_sorted re2 ->
    forall re3, well_sorted re3 ->
    matches (union_app re1 re2 re3) s <->
    ((exists s1 s2, s = s1 ++ s2 /\ matches re1 s1 /\ matches re2 s2) \/ matches re3 s).
  Proof.
    elim => [ | c s IH ].
    - have H :
        forall sort1 is_nullable1 derive1,
        forall sort2 is_nullable2 derive2,
        forall is_nullable3,
        is_nullable1 && is_nullable2 || is_nullable3 <->
        (exists s1 s2, nil = s1 ++ s2 /\ matches (regexp sort1 is_nullable1 derive1) s1 /\ matches (regexp sort2 is_nullable2 derive2) s2) \/ is_nullable3.
      { split => [ /orP [ /andP [ -> -> ] | -> ] | [ [ [ | ? ? ] [ [ | ? ? ] [ //= ? [ -> -> ] ] ] ] | -> ] ]; eauto.
        - left. by exists nil, nil.
        - by rewrite orbT. }
      inversion 1 => [ [ ? ? derive2 ] Hws2 | ? ? ? | [ ? [ ] derive2 ] Hws2 ].
      + have ? : forall c, well_sorted (derive2 c) by inversion Hws2.
        inversion 1.
        * exact /H.
        * exact /H.
        * split; eauto.
      + rewrite /re.
        move: (negbTE (H2 nil)) => /= ->.
        split => [ | [ [ [ | ? ? ] [ [ | ? ? ] [ //= ? [ // ] ] ] ] | ] ]; eauto.
      + split => //= ?.
        left. by exists nil, nil.
      + have ? : forall c, well_sorted (derive2 c) by inversion Hws2.
        inversion 1.
        * exact /H.
        * exact /H.
        * split; eauto.
    - have H :
        forall sort1 is_nullable1 derive1, (forall c, well_sorted (derive1 c)) ->
        forall sort2 is_nullable2 derive2, well_sorted (regexp sort2 is_nullable2 derive2) -> (forall c, well_sorted (derive2 c)) ->
        forall derive3, (forall c, well_sorted (derive3 c)) ->
        matches
          (if is_nullable1
           then union_app (derive1 c) (regexp sort2 is_nullable2 derive2) (union (derive2 c) (derive3 c))
           else union_app (derive1 c) (regexp sort2 is_nullable2 derive2) (derive3 c)) s <->
        ((exists s1 s2, c :: s = s1 ++ s2 /\ matches (regexp sort1 is_nullable1 derive1) s1 /\ matches (regexp sort2 is_nullable2 derive2) s2) \/ matches (derive3 c) s).
      { move => ? is_nullable1 derive1 Hws1' ? ? derive2 Hws2 Hws2' derive3 Hws3'.
        case is_nullable1.
        - split => [ /(IH _ (Hws1' _) _ Hws2 _ (union_well_sorted _ (Hws2' _) _ (Hws3' _))) | H ].
          + rewrite union_spec => // [ [ [ s1 [ s2 [ -> [ H1 H2 ] ] ] ] | /orP [ ? | -> ] ] ]; eauto.
            * left. by exists (c :: s1), s2.
            * left. by exists nil, (c :: s).
          + apply /IH => //.
            * exact /union_well_sorted.
            * rewrite union_spec //.
              move: H => [ [ [ [ ? /= [ <- [ ? /= -> ] ] ] | c' s' [ s2 [ ] ] ] ] | -> ]; eauto.
              -- inversion 1; subst => [ /= [ ? ? ] ].
                 left. by exists s', s2.
              -- rewrite orbT. eauto.
        - split => [ /(IH _ (Hws1' _) _ Hws2 _ (Hws3' _)) [ [ s1 [ s2 [ -> [ H1 H2 ] ] ] ] | -> ] | H ]; eauto.
          + left. by exists (c :: s1), s2.
          + apply /IH => //.
            move: H => [ [ [ [ ? [ ? [ // ] ] ] | ? s1 [ s2 [ ] ] ] ] | -> ]; eauto.
            inversion 1 => /= [ [ ? ? ] ].
            left. by exists s1, s2. }
      inversion 1 => [ [ ? ? derive2 ] Hws2 | ? ? ? | [ ? [ ] derive2 ] Hws2 ].
      + have ? : forall c, well_sorted (derive2 c) by inversion Hws2.
        inversion 1.
        * exact /H.
        * exact /H.
        * rewrite H5 /= universal_set_spec.
          split; eauto.
      + split => [ | [ [ ? [ ? [ ? [ ] ] ] ] | ] ]; eauto.
        by rewrite (negbTE (H2 _)).
      + split => ?.
        * left. exists (c :: s), nil. by rewrite app_nil_r.
        * exact /universal_set_spec.
      + have ? : forall c, well_sorted (derive2 c) by inversion Hws2.
        inversion 1.
        * exact /H.
        * exact /H.
        * rewrite H6 /= universal_set_spec.
          split; eauto.
  Qed.

  Lemma union_app_well_sorted :
    forall re1, well_sorted re1 ->
    forall re2, well_sorted re2 ->
    forall re3, well_sorted re3 ->
    well_sorted (union_app re1 re2 re3).
  Proof.
    cofix union_app_well_sorted.
    have H :
      forall is_nullable1 derive1, (forall c, well_sorted (derive1 c)) ->
      forall sort2 is_nullable2 derive2, well_sorted (regexp sort2 is_nullable2 derive2) -> (forall c, well_sorted (derive2 c)) ->
      forall is_nullable3 derive3, (forall c, well_sorted (derive3 c)) ->
      well_sorted (regexp Other (is_nullable1 && is_nullable2 || is_nullable3) (fun c =>
        if is_nullable1
        then union_app (derive1 c) (regexp sort2 is_nullable2 derive2) (union (derive2 c) (derive3 c))
        else union_app (derive1 c) (regexp sort2 is_nullable2 derive2) (derive3 c))).
    { move => is_nullable1 ? Hws1' ? ? ? Hws2 Hws2' ? ? Hws3'.
      apply /well_sorted_other => ?.
      case is_nullable1.
      - apply /union_app_well_sorted => //.
        exact /union_well_sorted.
      - exact /union_app_well_sorted. }
    inversion 1 => [ [ ? ? derive2 ] Hws2 | ? ? ? | [ ? [ ? ? ? ? | derive2 Hws2 ] ] ]; subst.
    - have ? : forall c, well_sorted (derive2 c) by inversion Hws2.
      inversion 1; subst; rewrite union_app_eq.
      + exact /H.
      + exact /H.
      + exact /universal_set_well_sorted.
    - by rewrite union_app_eq.
    - rewrite union_app_eq.
      exact /universal_set_well_sorted.
    - have ? : forall c, well_sorted (derive2 c) by inversion Hws2.
      inversion 1; subst; rewrite union_app_eq.
      + exact /H.
      + exact /H.
      + exact /universal_set_well_sorted.
  Qed.

  Definition app re1 re2 := union_app re1 re2 empty_set.

  Corollary app_spec : forall s,
    forall re1, well_sorted re1 ->
    forall re2, well_sorted re2 ->
    matches (app re1 re2) s <->
    (exists s1 s2, s = s1 ++ s2 /\ matches re1 s1 /\ matches re2 s2).
  Proof.
    move => ? ? Hws1 ? Hws2.
    split => [ /(union_app_spec _ _ Hws1 _ Hws2 _ empty_set_well_sorted) [ ] // | ? ].
    - by rewrite (negbTE (empty_set_spec _)).
    - apply /union_app_spec; eauto.
      exact /empty_set_well_sorted.
  Qed.

  Corollary app_well_sorted :
    forall re1, well_sorted re1 ->
    forall re2, well_sorted re2 ->
    well_sorted (app re1 re2).
  Proof.
    move => ? ? ? ?.
    apply /union_app_well_sorted => //.
    exact /empty_set_well_sorted.
  Qed.

  (* app re1 (star re2) *)
  CoFixpoint app_star re1 re2 :=
    match re1, re2 with
    | regexp EmptySet _ _, _ => empty_set
    | regexp UniversalSet _ _, _ => universal_set
    | regexp Other is_nullable1 derive1,
      regexp _ _ derive2 =>
      regexp Other is_nullable1 (fun c =>
        if is_nullable1
        then app_star (union (derive1 c) (derive2 c)) re2
        else app_star (derive1 c) re2)
    end.

  Lemma app_star_eq re1 re2 : app_star re1 re2 =
    match re1, re2 with
    | regexp EmptySet _ _, _ => empty_set
    | regexp UniversalSet _ _, _ => universal_set
    | regexp Other is_nullable1 derive1,
      regexp _ _ derive2 =>
      regexp Other is_nullable1 (fun c =>
        if is_nullable1
        then app_star (union (derive1 c) (derive2 c)) re2
        else app_star (derive1 c) re2)
    end.
  Proof.
    rewrite (frob_eq (app_star re1 re2)).
    move: re1 re2 => [ [ ] ? ? ] [ ? ? ? ] //=;
    by rewrite -?universal_set_eq -?empty_set_eq.
  Qed.

  Inductive matches_star re : list character -> Prop :=
    | matches_star_nil : matches_star re nil
    | matches_star_app s t :
        matches re s ->
        matches_star re t ->
        matches_star re (s ++ t).

  Lemma matches_star_cons_elim : forall re s,
    matches_star re s ->
    forall sort is_nullable derive,
    re = regexp sort is_nullable derive ->
    forall c s', s = c :: s' ->
    exists s1 s2, s' = s1 ++ s2 /\ matches (derive c) s1 /\ matches_star re s2.
  Proof.
    induction 1 => // ? ? ? ? ? ?; subst.
    destruct s => /=; inversion 1; subst.
    - exact /IHmatches_star.
    - by repeat eexists.
  Qed.

  Lemma app_star_spec : forall s,
    forall re1, well_sorted re1 ->
    forall re2, well_sorted re2 ->
    matches (app_star re1 re2) s <->
    exists s1 s2, s = s1 ++ s2 /\ matches re1 s1 /\ matches_star re2 s2.
  Proof.
    elim => [ | c s IH ].
    - have H :
        forall sort1 (is_nullable1 : bool) derive1,
        forall sort2 is_nullable2 derive2,
        is_nullable1 <->
        (exists s1 s2, nil = s1 ++ s2 /\ matches (regexp sort1 is_nullable1 derive1) s1 /\ matches_star (regexp sort2 is_nullable2 derive2) s2).
      { split => [ -> | [ [ | ? ? ] [ [ | ? ? ] [ //= ? [ -> ] ] ] ] ] //.
        exists nil, nil. repeat split. exact /matches_star_nil. }
      inversion 1 => [ [ ? ? derive2 ] Hws2 | ? | ? ].
      + exact /H.
      + rewrite /re.
        move: (negbTE (H2 nil)) => /= ->.
        by split => [ | [ [ | ? ? ] [ [ | ? ? ] [ ? [ ] ] ] ] ].
      + split => [ ? | [ [ | ? ? ] [ [ | ? ? ] [ ? [ ] ] ] ] ] //=.
        exists nil, nil. repeat split; eauto. exact /matches_star_nil.
    - have H :
        forall sort1 is_nullable1 derive1, (forall c, well_sorted (derive1 c)) ->
        forall sort2 is_nullable2 derive2, well_sorted (regexp sort2 is_nullable2 derive2) -> (forall c, well_sorted (derive2 c)) ->
        matches
          (if is_nullable1
           then app_star (union (derive1 c) (derive2 c)) (regexp sort2 is_nullable2 derive2)
           else app_star (derive1 c) (regexp sort2 is_nullable2 derive2)) s <->
        exists s1 s2, c :: s = s1 ++ s2 /\ matches (regexp sort1 is_nullable1 derive1) s1 /\ matches_star (regexp sort2 is_nullable2 derive2) s2.
      { move => ? [ ] ? Hws1' ? ? ? Hws2 Hws2'.
        - split => [ /(IH _ (union_well_sorted _ (Hws1' _) _ (Hws2' _)) _ Hws2) [ s1 [ s2 [ -> [ ] ] ] ] | [ [ /= [ ? [ <- [ ? /(fun H => matches_star_cons_elim _ _ H _ _ _ eq_refl _ _ eq_refl) [ s1 [ s2 [ ? [ Hderive ? ] ] ] ] ] ] ] | /= c' s1 [ s2 [ ] ] ] ] ].
          + rewrite union_spec => // /orP [ ] ? ?.
            * by exists (c :: s1), s2.
            * exists nil, ((c :: s1) ++ s2).
              repeat split.
              exact /matches_star_app.
          + apply /(IH _ (union_well_sorted _ _ _ _)) => //.
            exists s1, s2.
            repeat split => //.
            by rewrite union_spec // Hderive orbT.
          + inversion 1; subst => [ [ Hderive ? ] ].
            apply /(IH _ (union_well_sorted _ _ _ _)) => //.
            exists s1, s2.
            repeat split => //.
            by rewrite union_spec // Hderive.
        - split => [ /(IH _ (Hws1' _) _ Hws2) [ s1 [ s2 [ -> [ ? ? ] ] ] ] | [ [ /= [ ? [ ? [ // ] ] ] | ? ? [ ? [ /= ] ] ] ] ].
          + by exists (c :: s1), s2.
          + inversion 1. subst => [ [ ? ? ] ].
            apply /IH; eauto. }
      inversion 1 => [ [ ? ? derive2 ] Hws2 | ? | ? ].
      + have ? : forall c, well_sorted (derive2 c) by inversion Hws2.
        exact /H.
      + split => [ /= | [ ? [ ? [ ? [ ] ] ] ] ].
        * by rewrite (negbTE (empty_set_spec _)).
        * by rewrite (negbTE (H2 _)).
      + split => ? /=.
        * exists (c :: s), nil.
          rewrite app_nil_r H2.
          repeat split.
          exact /matches_star_nil.
        * exact /universal_set_spec.
  Qed.

  Lemma app_star_well_sorted :
    forall re1, well_sorted re1 ->
    forall re2, well_sorted re2 ->
    well_sorted (app_star re1 re2).
  Proof.
    cofix app_star_well_sorted.
    have H :
      forall is_nullable1 derive1, (forall c, well_sorted (derive1 c)) ->
      forall sort2 is_nullable2 derive2, well_sorted (regexp sort2 is_nullable2 derive2) -> (forall c, well_sorted (derive2 c)) ->
      well_sorted (regexp Other is_nullable1 (fun c =>
        if is_nullable1
        then app_star (union (derive1 c) (derive2 c)) (regexp sort2 is_nullable2 derive2)
        else app_star (derive1 c) (regexp sort2 is_nullable2 derive2))).
    { move => [ ] ? Hws1' ? ? ? Hws2 Hws2';
      apply /well_sorted_other => // ?;
      apply /app_star_well_sorted => //.
      exact /union_well_sorted. }
    inversion 1 => [ [ ? ? derive2 ] Hws2 | ? ? | ? ? ]; rewrite app_star_eq /=.
    - apply /H => //.
      by inversion Hws2.
    - exact /empty_set_well_sorted.
    - exact /universal_set_well_sorted.
  Qed.

  Definition star := app_star empty_str.

  Corollary star_spec : forall s,
    forall re, well_sorted re ->
    matches (star re) s <-> matches_star re s.
  Proof.
    move => ? ? Hws.
    split => [ /(app_star_spec _ _ empty_str_well_sorted _ Hws) [ [ | ? ? ] /= [ ? [ -> [ ] ] ] ] | ? ] //.
    - by rewrite (negbTE (empty_set_spec _)).
    - apply /app_star_spec => //.
      { exact /empty_str_well_sorted. }
      exists nil. by repeat eexists.
  Qed.

  Corollary star_well_sorted :
    forall re, well_sorted re ->
    well_sorted (star re).
  Proof. exact /app_star_well_sorted /empty_str_well_sorted. Qed.
End F.

Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inductive list => "list" ["[]" "(::)"].
Extract Inlined Constant negb => "not".
Extract Inlined Constant andb => "(&&)".
Extract Inlined Constant orb => "(||)".
Extraction "regexp.ml" F.
