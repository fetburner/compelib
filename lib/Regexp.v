Require Import ssreflect ssrbool List Program.

Module Type Char.
  Parameter t : Set.
  Parameter eq_dec : forall c c' : t, { c = c' } + { c <> c' }.
End Char.

Module F (C : Char).
  Definition character := C.t.

  CoInductive t : Set :=
    | empty_set
    | universal_set
    | regexp (is_nullable : bool) (derive : character -> t).

  Definition frob t :=
    match t with
    | empty_set => empty_set
    | universal_set => universal_set
    | regexp is_nullable derive => regexp is_nullable derive
    end.

  Lemma frob_eq : forall t, t = frob t.
  Proof. by case. Qed.

  Fixpoint matches re s :=
    match re, s with
    | empty_set, _ => false
    | universal_set, _ => true
    | regexp is_nullable _, nil => is_nullable
    | regexp _ derive, c :: s => matches (derive c) s
    end.

  Definition is_nullable re :=
    match re with
    | empty_set => false
    | universal_set => true
    | regexp is_nullable _ => is_nullable
    end.

  Lemma is_nullable_spec : forall re, is_nullable re = matches re nil.
  Proof. by case. Qed.

  Definition derive re :=
    match re with
    | empty_set => fun _ => empty_set
    | universal_set => fun _ => universal_set
    | regexp _ derive => derive
    end.

  Lemma derive_spec : forall re c s, matches re (c :: s) = matches (derive re c) s.
  Proof. by case => //= ? []. Qed.

  Lemma empty_set_spec : forall s, ~~ matches empty_set s.
  Proof. by case. Qed.

  Lemma universal_set_spec : forall s, matches universal_set s.
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

  CoFixpoint neg re :=
    match re with
    | empty_set => universal_set
    | universal_set => empty_set
    | regexp is_nullable derive => regexp (~~ is_nullable) (fun c => neg (derive c))
    end.

  Lemma neg_eq re : neg re =
    match re with
    | empty_set => universal_set
    | universal_set => empty_set
    | regexp is_nullable derive => regexp (~~ is_nullable) (fun c => neg (derive c))
    end.
  Proof.
    rewrite (frob_eq (neg re)).
    by case re.
  Qed.

  Lemma neg_spec : forall s re,
    matches (neg re) s = ~~ matches re s.
  Proof. by elim => [ | ? ? ? ] [ | | ? ? ] /=. Qed.

  CoFixpoint inter re re' :=
    match re, re' with
    | empty_set, _ => empty_set
    | universal_set, _ => re'
    | _, empty_set => empty_set
    | _, universal_set => re
    | regexp is_nullable derive, regexp is_nullable' derive' =>
      regexp (is_nullable && is_nullable') (fun c => inter (derive c) (derive' c))
    end.

  Lemma inter_eq re re' : inter re re' =
    match re, re' with
    | empty_set, _ => empty_set
    | universal_set, _ => re'
    | _, empty_set => empty_set
    | _, universal_set => re
    | regexp is_nullable derive, regexp is_nullable' derive' =>
      regexp (is_nullable && is_nullable') (fun c => inter (derive c) (derive' c))
    end.
  Proof.
    rewrite (frob_eq (inter re re')).
    by move: re re' => [ | | ? ? ] [ | | ? ? ] /=.
  Qed.

  Lemma inter_spec : forall s re re',
    matches (inter re re') s = matches re s && matches re' s.
  Proof.
    elim => [ | ? ? ? ] [ | | ? ? ] [ | | ? ? ] //=;
    by rewrite ?andbT ?andbF.
  Qed.

  CoFixpoint union re re' :=
    match re, re' with
    | empty_set, _ => re'
    | universal_set, _ => universal_set
    | _, empty_set => re
    | _, universal_set => universal_set
    | regexp is_nullable derive, regexp is_nullable' derive' =>
      regexp (is_nullable || is_nullable') (fun c => union (derive c) (derive' c))
    end.

  Lemma union_eq re re' : union re re' =
    match re, re' with
    | empty_set, _ => re'
    | universal_set, _ => universal_set
    | _, empty_set => re
    | _, universal_set => universal_set
    | regexp is_nullable derive, regexp is_nullable' derive' =>
      regexp (is_nullable || is_nullable') (fun c => union (derive c) (derive' c))
    end.
  Proof.
    rewrite (frob_eq (union re re')).
    by move: re re' => [ | | ? ? ] [ | | ? ? ] /=.
  Qed.

  Lemma union_spec : forall s re re',
    matches (union re re') s = matches re s || matches re' s.
  Proof.
    elim => [ | ? ? ? ] [ | | ? ? ] [ | | ? ? ] //=;
    by rewrite ?orbT ?orbF.
  Qed.

  (* union (app re1 re2) re3 *)
  CoFixpoint union_app re1 re2 re3 :=
    match re1, re2, re3 with
    | empty_set, _, _ => re3
    | _, empty_set, _ => re3
    | universal_set, regexp true _, _ => universal_set
    | regexp true _, universal_set, _ => universal_set
    | universal_set, universal_set, _ => universal_set
    | _, _, universal_set => universal_set
    | _, _, _ =>
      regexp (is_nullable re1 && is_nullable re2 || is_nullable re3) (fun c =>
        if is_nullable re1
        then union_app (derive re1 c) re2 (union (derive re2 c) (derive re3 c))
        else union_app (derive re1 c) re2 (derive re3 c))
    end.

  Lemma union_app_eq re1 re2 re3 : union_app re1 re2 re3 =
    match re1, re2, re3 with
    | empty_set, _, _ => re3
    | _, empty_set, _ => re3
    | universal_set, regexp true _, _ => universal_set
    | regexp true _, universal_set, _ => universal_set
    | universal_set, universal_set, _ => universal_set
    | _, _, universal_set => universal_set
    | _, _, _ =>
      regexp (is_nullable re1 && is_nullable re2 || is_nullable re3) (fun c =>
        if is_nullable re1
        then union_app (derive re1 c) re2 (union (derive re2 c) (derive re3 c))
        else union_app (derive re1 c) re2 (derive re3 c))
    end.
  Proof.
    rewrite (frob_eq (union_app re1 re2 re3)).
    by move: re1 re2 re3 => [ | | [ ] ? ] [ | | [ ] ? ] [ | | ? ? ] /=.
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
      move => [ | | [ ] derive1 ] /=.
      + split => [ -> | [ [ [ | ? ? ] [ [ | ? ? ] [ // ? [ ] ] ] ] | -> ] ]; eauto.
      + move => [ | | [ ] derive2 ] /=.
        * split => [ -> | [ [ [ | ? ? ] [ [ | ? ? ] [ // ? [ ] ] ] ] | -> ] ]; eauto.
        * split; eauto. left. exists nil, nil. by rewrite !universal_set_spec.
        * split; eauto. left. exists nil, nil. by rewrite !universal_set_spec.
        * { move => [ | | is_nullable3 derive3 ].
            - exact /(H universal_set (regexp false derive2) empty_set).
            - split; eauto.
            - exact /(H universal_set (regexp false derive2) (regexp is_nullable3 derive3)). }
      + move => [ | | is_nullable2 derive2 ].
        * split => [ -> | [ [ [ | ? ? ] [ [ | ? ? ] [ // ? [ ] ] ] ] | -> ] ]; eauto.
        * split; eauto. left. exists nil, nil. by rewrite !universal_set_spec.
        * { move => [ | | is_nullable3 derive3 ].
            - exact /(H (regexp true derive1) (regexp is_nullable2 derive2) empty_set).
            - split; eauto.
            - exact /(H (regexp true derive1) (regexp is_nullable2 derive2) (regexp is_nullable3 derive3)). }
      + move => [ | | is_nullable2 derive2 ].
        * split => [ -> | [ [ [ | ? ? ] [ [ | ? ? ] [ // ? [ ] ] ] ] | -> ] ]; eauto.
        * { move => [ | | is_nullable3 derive3 ].
            - exact /(H (regexp false derive1) universal_set empty_set).
            - split; eauto.
            - exact /(H (regexp false derive1) universal_set (regexp is_nullable3 derive3)). }
        * { move => [ | | is_nullable3 derive3 ].
            - exact /(H (regexp false derive1) (regexp is_nullable2 derive2) empty_set).
            - split; eauto.
            - exact /(H (regexp false derive1) (regexp is_nullable2 derive2) (regexp is_nullable3 derive3)). }
    - have H : forall re1 re2 re3,
        matches
          (if is_nullable re1
           then union_app (derive re1 c) re2 (union (derive re2 c) (derive re3 c))
           else union_app (derive re1 c) re2 (derive re3 c)) s <->
        (exists s1 s2 : list character, c :: s = s1 ++ s2 /\ matches re1 s1 /\ matches re2 s2) \/
        matches (derive re3 c) s.
      { (split; case (@idP (is_nullable re1)) => [ | /negP ] His_nullable) => [ /IH [ [ s1 [ s2 [ -> [ ? ? ] ] ] ] | ] | /IH [ [ s1 [ s2 [ -> [ ] ] ] ] | ] | H | H ]; eauto.
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
          inversion 1; subst => [ [ ] ]; rewrite derive_spec; eauto 6.
          by rewrite -is_nullable_spec (negbTE His_nullable). }
      move => [ | | [ ] derive1 ].
      + split => /= [ -> | [ [ ? [ ? [ ? [ ] ] ] ] | -> ] ]; eauto.
        by rewrite (negbTE (empty_set_spec _)).
      + move => [ | | [ ] derive2 ].
        * split => /= [ -> | [ [ ? [ ? [ ? [ ] ] ] ] | -> ] ]; eauto.
          by rewrite (negbTE (empty_set_spec _)).
        * split => ? //=.
          left. exists nil, (c :: s). by rewrite !universal_set_spec.
        * split => ? //=.
          left. exists (c :: s), nil. by rewrite app_nil_r universal_set_spec.
        * { move => [ | | is_nullable3 derive3 ].
            - move: (H universal_set (regexp false derive2) empty_set).
              by rewrite /= (negbTE (empty_set_spec _)).
            - rewrite union_app_eq !universal_set_spec.
              split; eauto.
            - exact /(H universal_set (regexp false derive2) (regexp is_nullable3 derive3)). }
      + move => [ | | is_nullable2 derive2 ].
        * split => /= [ -> | [ [ ? [ ? [ ? [ ] ] ] ] | -> ] ]; eauto.
          by rewrite (negbTE (empty_set_spec _)).
        * split => ? //=.
          left. exists nil, (c :: s). by rewrite universal_set_spec.
        * { move => [ | | is_nullable3 derive3 ].
            - move: (H (regexp true derive1) (regexp is_nullable2 derive2) empty_set).
              by rewrite /= (negbTE (empty_set_spec _)).
            - split; eauto.
            - exact /(H (regexp true derive1) (regexp is_nullable2 derive2) (regexp is_nullable3 derive3)). }
      + move => [ | | is_nullable2 derive2 ].
        * split => /= [ -> | [ [ ? [ ? [ ? [ ] ] ] ] | -> ] ]; eauto.
          by rewrite (negbTE (empty_set_spec _)).
        * { move => [ | | is_nullable3 derive3 ].
            - move: (H (regexp false derive1) universal_set empty_set).
              by rewrite /= (negbTE (empty_set_spec _)).
            - split; eauto.
            - exact /(H (regexp false derive1) universal_set (regexp is_nullable3 derive3)). }
        * { move => [ | | is_nullable3 derive3 ].
            - move: (H (regexp false derive1) (regexp is_nullable2 derive2) empty_set).
              by rewrite /= (negbTE (empty_set_spec _)).
            - split; eauto.
            - exact /(H (regexp false derive1) (regexp is_nullable2 derive2) (regexp is_nullable3 derive3)). }
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
    | universal_set => universal_set
    | regexp is_nullable1 derive1 =>
      regexp is_nullable1 (fun c =>
        if is_nullable1
        then app_star (union (derive1 c) (derive re2 c)) re2
        else app_star (derive1 c) re2)
    end.

  Lemma app_star_eq re1 re2 : app_star re1 re2 =
    match re1 with
    | empty_set => empty_set
    | universal_set => universal_set
    | regexp is_nullable1 derive1 =>
      regexp is_nullable1 (fun c =>
        if is_nullable1
        then app_star (union (derive1 c) (derive re2 c)) re2
        else app_star (derive1 c) re2)
    end.
  Proof.
    rewrite (frob_eq (app_star re1 re2)).
    by move: re1 re2 => [ | | ? ? ] [ | | ? ? ] /=.
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
    elim => [ | c s IH ] [ | | is_nullable1 derive1 ] re2 /=.
    - split => [ // | [ ? [ ? [ ? [ ] ] ] ] ].
      by rewrite (negbTE (empty_set_spec _)).
    - split => // ?.
      exists nil, nil. repeat split. exact /matches_star_nil.
    - split => [ -> | [ [ | ? ? ] [ [ | ? ? ] [ ? [ // ] ] ] ] ].
      exists nil, nil. repeat split. exact /matches_star_nil.
    - split => [ // | [ ? [ ? [ ? [ ] ] ] ] ].
      by rewrite (negbTE (empty_set_spec _)).
    - split => // ?.
      exists (c :: s), nil. rewrite app_nil_r. repeat split. exact /matches_star_nil.
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
