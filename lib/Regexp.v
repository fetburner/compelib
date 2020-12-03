Require Import ssreflect ssrbool List Program.

Module Type Char.
  Parameter t : Set.
  Parameter eq_dec : forall c c' : t, { c = c' } + { c <> c' }.
End Char.

Module F (C : Char).
  Definition character := C.t.

  CoInductive t : Set :=
    | empty_set
    | regexp (is_nullable : bool) (derive : character -> t).

  Definition frob t :=
    match t with
    | empty_set => empty_set
    | regexp is_nullable derive => regexp is_nullable derive
    end.

  Lemma frob_eq : forall t, t = frob t.
  Proof. by case. Qed.

  Fixpoint matches re s :=
    match re, s with
    | empty_set, _ => false
    | regexp is_nullable _, nil => is_nullable
    | regexp _ derive, c :: s => matches (derive c) s
    end.

  Definition is_nullable re :=
    match re with
    | empty_set => false
    | regexp is_nullable _ => is_nullable
    end.

  Lemma is_nullable_spec : forall re, is_nullable re = matches re nil.
  Proof. by case. Qed.

  Definition derive re :=
    match re with
    | empty_set => fun _ => empty_set
    | regexp _ derive => derive
    end.

  Lemma derive_spec : forall re c s, matches re (c :: s) = matches (derive re c) s.
  Proof. by case => //= ? []. Qed.

  Lemma empty_set_spec : forall s, ~~ matches empty_set s.
  Proof. by case. Qed.

  Definition empty_str := regexp true (fun _ => empty_set).

  Lemma empty_str_spec : forall s, matches empty_str s <-> s = nil.
  Proof.
    case => //= ? ?.
    by rewrite (negbTE (empty_set_spec _)).
  Qed.

  Definition char c := regexp false (fun c' =>
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

  CoFixpoint inter re re' :=
    match re, re' with
    | empty_set, _ => empty_set
    | _, empty_set => empty_set
    | regexp is_nullable derive, regexp is_nullable' derive' =>
      regexp (is_nullable && is_nullable') (fun c => inter (derive c) (derive' c))
    end.

  Lemma inter_eq re re' : inter re re' =
    match re, re' with
    | empty_set, _ => empty_set
    | _, empty_set => empty_set
    | regexp is_nullable derive, regexp is_nullable' derive' =>
      regexp (is_nullable && is_nullable') (fun c => inter (derive c) (derive' c))
    end.
  Proof.
    rewrite (frob_eq (inter re re')).
    by move: re re' => [ | ? ? ] [ | ? ? ] /=.
  Qed.

  Lemma inter_spec : forall s re re',
    matches (inter re re') s = matches re s && matches re' s.
  Proof.
    elim => [ | ? ? ? ] [ | ? ? ] [ | ? ? ] //=;
    by rewrite ?andbT ?andbF.
  Qed.

  CoFixpoint union re re' :=
    match re, re' with
    | empty_set, _ => re'
    | _, empty_set => re
    | regexp is_nullable derive, regexp is_nullable' derive' =>
      regexp (is_nullable || is_nullable') (fun c => union (derive c) (derive' c))
    end.

  Lemma union_eq re re' : union re re' =
    match re, re' with
    | empty_set, _ => re'
    | _, empty_set => re
    | regexp is_nullable derive, regexp is_nullable' derive' =>
      regexp (is_nullable || is_nullable') (fun c => union (derive c) (derive' c))
    end.
  Proof.
    rewrite (frob_eq (union re re')).
    by move: re re' => [ | ? ? ] [ | ? ? ] /=.
  Qed.

  Lemma union_spec : forall s re re',
    matches (union re re') s = matches re s || matches re' s.
  Proof.
    elim => [ | ? ? ? ] [ | ? ? ] [ | ? ? ] //=;
    by rewrite ?orbT ?orbF.
  Qed.

  (* union (app re1 re2) re3 *)
  CoFixpoint union_app re1 re2 re3 :=
    match re1, re2 with
    | empty_set, _ => re3
    | _, empty_set => re3
    | regexp is_nullable1 derive1, regexp is_nullable2 derive2 =>
      regexp (is_nullable1 && is_nullable2 || is_nullable re3) (fun c =>
        if is_nullable1
        then union_app (derive1 c) re2 (union (derive2 c) (derive re3 c))
        else union_app (derive1 c) re2 (derive re3 c))
    end.

  Lemma union_app_eq re1 re2 re3 : union_app re1 re2 re3 =
    match re1, re2 with
    | empty_set, _ => re3
    | _, empty_set => re3
    | regexp is_nullable1 derive1, regexp is_nullable2 derive2 =>
      regexp (is_nullable1 && is_nullable2 || is_nullable re3) (fun c =>
        if is_nullable1
        then union_app (derive1 c) re2 (union (derive2 c) (derive re3 c))
        else union_app (derive1 c) re2 (derive re3 c))
    end.
  Proof.
    rewrite (frob_eq (union_app re1 re2 re3)).
    by move: re1 re2 re3 => [ | ? ? ] [ | ? ? ] [ | ? ? ] /=.
  Qed.

  Lemma union_app_spec : forall s re1 re2 re3,
    matches (union_app re1 re2 re3) s <->
    ((exists s1 s2, s = s1 ++ s2 /\ matches re1 s1 /\ matches re2 s2) \/ matches re3 s).
  Proof.
    elim => [ | c s IH ].
    - have H : forall re1 re2 re3,
        is_nullable re1 && is_nullable re2 || is_nullable re3 <->
        (exists s1 s2, nil = s1 ++ s2 /\ matches re1 s1 /\ matches re2 s2) \/ is_nullable re3.
      { split => [ /orP [ /andP [ ? ? ] | -> ] | [ [ [ | ? ? ] [ [ | ? ? ] [ // ? [ ] ] ] ] | -> ] ]; eauto.
        - left. by exists nil, nil.
        - by rewrite !is_nullable_spec => -> ->.
        - by rewrite orbT. }
      move => [ | is_nullable1 derive1 ].
      + split => /= [ -> | [ [ [ | ? ? ] [ [ | ? ? ] [ // ? [ ] ] ] ] | -> ] ]; eauto.
      + move => [ | is_nullable2 derive2 re3 ].
        * split => /= [ -> | [ [ [ | ? ? ] [ [ | ? ? ] [ // ? [ ] ] ] ] | -> ] ]; eauto.
        * { rewrite -(is_nullable_spec re3) /=.
            split => [ /orP [ /andP [ ? ? ] | -> ] | [ [ [ | ? ? ] [ [ | ? ? ] [ // ? [ ] ] ] ] | -> ] ]; eauto.
            - left. by exists nil, nil.
            - by rewrite -!is_nullable_spec => /= -> ->.
            - by rewrite orbT. }
    - move => [ | is_nullable1 derive1 ].
      + split => /= [ -> | [ [ ? [ ? [ ? [ ] ] ] ] | -> ] ]; eauto.
        by rewrite (negbTE (empty_set_spec _)).
      + move => [ | is_nullable2 derive2 re3 ].
        * split => /= [ -> | [ [ ? [ ? [ ? [ ] ] ] ] | -> ] ]; eauto.
          by rewrite (negbTE (empty_set_spec _)).
        * rewrite (derive_spec re3) /=.
          { (split; case is_nullable1) => [ /IH [ [ s1 [ s2 [ -> [ ? ? ] ] ] ] | ] | /IH [ [ s1 [ s2 [ -> [ ] ] ] ] | ] | H | H ]; eauto.
            - left. exists (c :: s1), s2. by rewrite derive_spec.
            - rewrite union_spec => /orP [ | -> ]; eauto.
              left. exists nil, (c :: s). by rewrite derive_spec.
            - left. exists (c :: s1), s2. by rewrite derive_spec.
            - apply /IH.
              case H.
              + move => [ [ [ [ | ? ? ] [ // ] ] | ? ? [ ? [ ] ] ] ];
                inversion 1; subst => [ [ ] ]; rewrite derive_spec => ? Hre2; eauto 6.
                right. by rewrite union_spec Hre2.
              + rewrite union_spec => ->.
                rewrite orbT. eauto.
            - apply /IH.
              case H; eauto.
              move => [ [ [ [ | ? ? ] [ // ] ] | ? ? [ ? [ ] ] ] ];
              inversion 1; subst => [ [ ] ]; rewrite derive_spec; eauto 6. }
  Qed.

  Definition app re1 re2 := union_app re1 re2 empty_set.

  Corollary app_spec s re1 re2 :
    matches (app re1 re2) s <->
    (exists s1 s2, s = s1 ++ s2 /\ matches re1 s1 /\ matches re2 s2).
  Proof.
    split => [ /union_app_spec [ ] // | ? ].
    - by rewrite (negbTE (empty_set_spec _)).
    - apply /union_app_spec; eauto.
  Qed.

  (* app re1 (star re2) *)
  CoFixpoint app_star re1 re2 :=
    match re1 with
    | empty_set => empty_set
    | regexp is_nullable1 derive1 =>
      regexp is_nullable1 (fun c =>
        if is_nullable1
        then app_star (union (derive1 c) (derive re2 c)) re2
        else app_star (derive1 c) re2)
    end.

  Lemma app_star_eq re1 re2 : app_star re1 re2 =
    match re1 with
    | empty_set => empty_set
    | regexp is_nullable1 derive1 =>
      regexp is_nullable1 (fun c =>
        if is_nullable1
        then app_star (union (derive1 c) (derive re2 c)) re2
        else app_star (derive1 c) re2)
    end.
  Proof.
    rewrite (frob_eq (app_star re1 re2)).
    by move: re1 re2 => [ | ? ? ] [ | ? ? ] /=.
  Qed.

  Inductive matches_star re : list character -> Prop :=
    | matches_star_nil : matches_star re nil
    | matches_star_app s t :
        matches re s ->
        matches_star re t ->
        matches_star re (s ++ t).

  Lemma matches_star_cons_elim : forall re s,
    matches_star re s ->
    forall c s', s = c :: s' ->
    exists s1 s2, s' = s1 ++ s2 /\ matches (derive re c) s1 /\ matches_star re s2.
  Proof.
    induction 1 => // ? ?.
    destruct s => /=; inversion 1; subst.
    - exact /IHmatches_star.
    - repeat eexists; eauto.
      by rewrite -derive_spec.
  Qed.

  Lemma app_star_spec : forall s re1 re2,
    matches (app_star re1 re2) s <->
    exists s1 s2, s = s1 ++ s2 /\ matches re1 s1 /\ matches_star re2 s2.
  Proof.
    elim => [ | c s IH ] [ | is_nullable1 derive1 ] re2 /=.
    - split => [ // | [ ? [ ? [ ? [ ] ] ] ] ].
      by rewrite (negbTE (empty_set_spec _)).
    - split => [ -> | [ [ | ? ? ] [ [ | ? ? ] [ ? [ // ] ] ] ] ].
      exists nil, nil. repeat split. exact /matches_star_nil.
    - split => [ // | [ ? [ ? [ ? [ ] ] ] ] ].
      by rewrite (negbTE (empty_set_spec _)).
    - case is_nullable1;
      split => [ /IH [ s1 [ s2 [ -> [ ] ] ] ] | H ].
      + rewrite union_spec => /orP [ ] ? ?.
        * exists (c :: s1), s2. by rewrite derive_spec.
        * exists nil, ((c :: s1) ++ s2).
          repeat split. apply /matches_star_app => //. by rewrite derive_spec.
      + apply /IH.
        move: H => [ [ [ [ | ? ? ] [ ] ] | ? ? [ ? [ ] ] ] ]; inversion 1; subst => [ [ ] ].
        * move => ? /(matches_star_cons_elim _ _) /(_ _ _ eq_refl) [ s1 [ s2 [ -> [ Hre2 ? ] ] ] ].
          repeat eexists; eauto. by rewrite union_spec Hre2 orbT.
        * rewrite derive_spec => /= Hre1 ?.
          repeat eexists; eauto. by rewrite union_spec Hre1.
      + exists (c :: s1), s2. by rewrite derive_spec.
      + apply /IH.
        move: H => [ [ [ ? [ ] ] | ? ? [ ? [ ] ] ] ]; inversion 1; subst => [ [ ] ] //.
        rewrite derive_spec /=.
        repeat eexists; eauto.
  Qed.

  Definition star := app_star empty_str.

  Corollary star_spec s re :
    matches (star re) s <-> matches_star re s.
  Proof.
    split => [ /app_star_spec [ [ | ? ? ] /= [ ? [ -> [ ] ] ] ] | ? ] //.
    - by rewrite (negbTE (empty_set_spec _)).
    - apply /app_star_spec. by exists nil, s.
  Qed.
End F.

Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive sumbool => "bool" ["true" "false"].
Extract Inductive list => "list" ["[]" "(::)"].
Extract Inlined Constant negb => "not".
Extract Inlined Constant andb => "(&&)".
Extract Inlined Constant orb => "(||)".
Extraction "regexp.ml" F.
