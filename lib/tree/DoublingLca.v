Require Import Program.
From mathcomp Require Import all_ssreflect.

Lemma eq_bool : forall b1 b2 : bool, b1 <-> b2 -> b1 = b2.
Proof.
  move => [ ] ? [ H12 H21 ]; apply /Logic.eq_sym.
  - exact /H12.
  - by apply /negbTE /negP => /H21.
Qed.

Module Type Vertex.
  Parameter t : Set.
  Parameter eq_op : t -> t -> bool.
  Parameter eqP : Equality.axiom eq_op.
End Vertex.

Module F (V : Vertex).
  Definition vertex_eqMixin := EqMixin V.eqP.
  Canonical vertex_eqType := Eval hnf in EqType _ vertex_eqMixin.

  Section Lca.
    Variable root : V.t.
    Variable parent : V.t -> V.t.
    Hypothesis height : V.t -> nat.
    Hypothesis height_spec : forall n v, (iter n parent v == root) = (height v <= n).

    Lemma height_root_eq v : (height v == 0) = (v == root).
    Proof. by rewrite -leqn0 -height_spec. Qed.

    Corollary height_root : height root = 0.
    Proof. apply /eqP. by rewrite height_root_eq. Qed.

    Corollary parent_root : parent root = root.
    Proof.
      move: (height_spec 1 root).
      by rewrite /= height_root => /eqP ->.
    Qed.

    Lemma height_parent v : height (parent v) = (height v).-1.
    Proof.
      case (leqP (height v) 0) => [ | /prednK Hpredn ].
      - rewrite leqn0 height_root_eq => /eqP ->.
        by rewrite parent_root height_root.
      - apply /anti_leq /andP.
        split.
        + by rewrite -height_spec -iterSr Hpredn height_spec.
        + by rewrite -ltnS Hpredn -height_spec iterSr height_spec.
    Qed.

    Lemma height_iter_parent v : forall n, height (iter n parent v) = height v - n.
    Proof.
      elim => [ | ? ].
      - by rewrite subn0.
      - by rewrite iterS height_parent subnS => ->.
    Qed.

    (* u is an ancestor of v *)
    Definition ancestor u v := iter (height v - height u) parent v == u.

    Lemma ancestor_iter_parent n v : ancestor (iter n parent v) v.
    Proof.
      rewrite /ancestor height_iter_parent.
      case (leqP n (height v)) => [ /subKn -> // | /ltnW Hleq ].
      have -> : height v - n = 0 by apply /eqP; rewrite subn_eq0.
      move: height_spec (Hleq) => <- /eqP ->.
      by rewrite subn0 height_spec.
    Qed.

    Corollary ancestor_parent v : ancestor (parent v) v.
    Proof. exact /(ancestor_iter_parent 1). Qed.

    Lemma ancestor_height u v : ancestor u v -> height u <= height v.
    Proof.
      case (leqP (height u) (height v)) => // Hlt.
      have : height v - height u = 0 by apply /eqP; rewrite subn_eq0 ltnW.
      rewrite /ancestor => -> /= /eqP Heq.
      by move: Heq ltnn Hlt => -> ->.
    Qed.

    Lemma ancestor_refl : reflexive ancestor.
    Proof.
      move => ?.
      by rewrite /ancestor subnn.
    Qed.

    Lemma ancestor_trans : transitive ancestor.
    Proof.
      move => ? ? ? Huv Hvw.
      move: (ancestor_height _ _ Huv) (ancestor_height _ _ Hvw) (Huv) (Hvw) => ? ? /eqP <- /eqP <-.
      by rewrite /ancestor !height_iter_parent -iterD !subKn // addnBAC // subnKC.
    Qed.

    Lemma ancestor_antisymmetric : antisymmetric ancestor.
    Proof.
      move => u v /andP [ ].
      rewrite /ancestor.
      by (case (leqP (height u) (height v)) => [ | /ltnW ];
          rewrite -subn_eq0 => /eqP ->) => /= [ ? /eqP | /eqP ].
    Qed.

    Definition common_ancestor u v w := ancestor w u && ancestor w v.
    Definition lowest_common_ancestor u v w := forall w', common_ancestor u v w' = ancestor w' w.

    Lemma common_ancestor_comm u v w : common_ancestor u v w = common_ancestor v u w.
    Proof. by rewrite /common_ancestor andbC. Qed.

    Corollary lowest_common_ancestor_comm u v w :
      lowest_common_ancestor u v w <-> lowest_common_ancestor v u w.
    Proof. split => H ?; by rewrite common_ancestor_comm H. Qed.

    Lemma common_ancestor_climb u v w :
      common_ancestor u v w = common_ancestor u (iter (height v - height u) parent v) w.
    Proof.
      case (leqP (height w) (height u)) => [ Hwu | ].
      - congr (ancestor w u && _).
        rewrite /ancestor height_iter_parent -minnE.
        case (leqP (height v) (height u)) => [ | /ltnW Huv ].
        + by rewrite -subn_eq0 => /eqP ->.
        + have ? := leq_trans Hwu Huv.
          by rewrite -iterD addnBA // addnBAC // addnC -addnBAC // -addnBA // subnn addn0.
      - by rewrite /common_ancestor ltnNge => /(contra (ancestor_height _ _)) /negbTE ->.
    Qed.

    Corollary lowest_common_ancestor_climb u v w :
      lowest_common_ancestor u v w <-> lowest_common_ancestor u (iter (height v - height u) parent v) w.
    Proof.
      split => ? ?.
      - by rewrite -common_ancestor_climb.
      - by rewrite common_ancestor_climb.
    Qed.

    Variable climb2exp : nat -> V.t -> V.t.
    Hypothesis climb2exp_spec : forall n v, climb2exp n v = iter (2 ^ n) parent v.

    Fixpoint climb e p n v :=
      if e is e'.+1
      then
        let p' := p./2 in
        if n < p'
        then climb e' p' n v
        else climb e' p' (n - p') (climb2exp e' v)
      else v.

    Lemma climb_spec : forall e n v,
      n < 2 ^ e ->
      climb e (2 ^ e) n v = iter n parent v.
    Proof.
      elim => /= [ ? ? | e IH n ? ].
      - by rewrite expn0 -subn_eq0 subn1 succnK => /eqP ->.
      - rewrite expnS mul2n half_double -addnn => Hltn.
        case (leqP (2 ^ e) n) => [ Hleq | /IH // ].
        rewrite climb2exp_spec IH.
        * by rewrite -iterD subnK.
        * by rewrite ltn_subLR.
    Qed.

    Fixpoint bisect e u v :=
      if e is e'.+1
      then
        let u' := climb2exp e' u in
        let v' := climb2exp e' v in
        if V.eq_op u' v'
        then bisect e' u v
        else bisect e' u' v'
      else climb2exp 0 u.

    Lemma bisect_spec : forall e u v,
      u != v ->
      height u = height v ->
      iter (2 ^ e) parent u = iter (2 ^ e) parent v ->
      lowest_common_ancestor u v (bisect e u v).
    Proof.
      elim => /= [ u v Huv Hheight Hparent w' | e IH u v Huv Hheight ].
      - rewrite climb2exp_spec /= /common_ancestor /ancestor -Hheight height_parent -predn_sub.
        case (posnP (height u - height w')) => [ Hzero | /prednK <- ].
        { rewrite Hzero /=.
          apply /eq_bool.
          split => [ /andP [ /eqP <- /eqP Heq ] | /eqP /(f_equal height) ].
          - by move: Heq eq_refl Huv => -> ->.
          - rewrite height_parent => Hw'.
            move: subn1 Hw' Hzero => <- <-.
            case (posnP (height u)) => [ /eqP Hroot | /subKn -> // ].
            move: height_root_eq (Hroot) Huv => -> /eqP ->.
            by move: Hheight height_root_eq Hroot eq_refl => -> -> /eqP -> ->. }
        by rewrite succnK !iterSr Hparent andbb.
      - rewrite expnS mul2n -addnn !iterD !climb2exp_spec.
        case (@V.eqP (iter (2 ^ e) parent u) (iter (2 ^ e) parent v)) => [ ? ? | Hneq Heq w' ].
        + exact /IH.
        + have /(_ w') <- : lowest_common_ancestor
            (iter (2 ^ e) parent u) (iter (2 ^ e) parent v)
            (bisect e (iter (2 ^ e) parent u) (iter (2 ^ e) parent v)).
          { apply /IH => //.
            - exact /eqP.
            - by rewrite !height_iter_parent Hheight. }
          rewrite /common_ancestor /ancestor !height_iter_parent -!iterD !(subnAC _ (2 ^ e)) -Hheight.
          case (leqP (2 ^ e) (height u - height w')) => [ /subnK -> // | /ltnW Hleq ].
          move: subn_eq0 (Hleq) add0n Hneq => <- /eqP -> -> Hneq.
          apply /eq_bool.
          split => [ | /andP [ /eqP <- /eqP Hcontra ] ].
          { move: (subnK Hleq) Hneq => <-.
            rewrite !iterD => Hneq /andP [ ].
            rewrite eq_sym => /eqP /(eq_trans _) Htrans /eqP /Htrans Hcontra.
            by move: Hcontra (Hneq) => -> /(_ erefl). }
          by move: Hcontra Hneq => -> /(_ erefl).
    Qed.

    Definition lca_aux e u v :=
      if V.eq_op u v
      then u
      else bisect e u v.

    Lemma lca_aux_spec : forall e u v,
      height u = height v ->
      height u <= 2 ^ e ->
      lowest_common_ancestor u v (lca_aux e u v).
    Proof.
      move => e u v Hheight Hu.
      rewrite /lca_aux.
      case (@V.eqP u v) => [ <- ? | /eqP ? ].
      - by rewrite /common_ancestor andbb.
      - apply /bisect_spec => //.
        move: height_spec (Hu) => <- /eqP ->.
        by move: Hheight height_spec Hu => -> <- /eqP ->.
    Qed.

    Definition lca e p u v :=
      if height u <= height v
      then lca_aux e u (climb e p (height v - height u) v)
      else lca_aux e v (climb e p (height u - height v) u).

    Theorem lca_spec : forall e u v,
      height u < 2 ^ e ->
      height v < 2 ^ e ->
      lowest_common_ancestor u v (lca e (2 ^ e) u v).
    Proof.
      move => e u v Hu Hv.
      rewrite /lca !climb_spec.
      - case (leqP (height u) (height v)) => [ /minn_idPr ? | /ltnW /minn_idPl ? ].
        + apply /lowest_common_ancestor_climb /lca_aux_spec.
          * by rewrite height_iter_parent -minnE.
          * exact /ltnW.
        + apply /lowest_common_ancestor_comm /lowest_common_ancestor_climb /lca_aux_spec.
          * by rewrite height_iter_parent -minnE minnC.
          * exact /ltnW.
      - exact /(leq_ltn_trans (leq_subr _ _)).
      - exact /(leq_ltn_trans (leq_subr _ _)).
    Qed.
  End Lca.
End F.

Extract Constant half => "(Fun.flip ( lsr ) 1)".
Extract Inlined Constant leq => "( <= )".
Extract Inlined Constant subn => "( - )".
Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive reflect => "bool" ["true" "false"].
Extract Inductive list => "list" ["[]" "(::)"].
Extract Inductive nat => int ["0" "succ"] "(fun fO fS n -> if n = 0 then fO () else fS (n-1))".

Extraction "doublingLca.ml" F uphalf.
