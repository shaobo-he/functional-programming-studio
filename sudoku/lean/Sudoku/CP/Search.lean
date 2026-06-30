import Sudoku.CP.Propagation

set_option autoImplicit false
set_option maxRecDepth 4096
set_option maxHeartbeats 1000000
set_option linter.unusedVariables false

namespace Sudoku.CP

def score (g : Grid) (i : Cell) : Nat :=
  let n := (getOptions g i).length
  if n > 1 then n else 10

def undecided (g : Grid) (i : Cell) : Bool :=
  (getOptions g i).length > 1

def gridDigit (g : Grid) (i : Cell) : Digit :=
  match getOptions g i with
  | d :: _ => d
  | [] => 0

def unitHasDigit (g : Grid) (u : List Cell) (d : Digit) : Bool :=
  u.any (fun i => gridDigit g i == d)

def validGrid (g : Grid) : Bool :=
  cells.all (fun a =>
    cells.all (fun b =>
      if peerB a b then gridDigit g a != gridDigit g b else true)) &&
  units.all (fun u => digits.all (fun d => unitHasDigit g u d))

def CoversUndecided (todo : List Cell) (g : Grid) : Prop :=
  ∀ i, undecided g i = true → i ∈ todo

theorem length_le_of_nodup_subset {xs ys : Options}
    (hn : xs.Nodup) (hsub : ∀ d, d ∈ xs → d ∈ ys) :
    xs.length ≤ ys.length := by
  induction xs generalizing ys with
  | nil =>
      simp
  | cons x xs ih =>
      cases hn with
      | cons hnot hnodup =>
          have hxys : x ∈ ys := hsub x (by simp)
          have hsubTail : ∀ d, d ∈ xs → d ∈ ys.erase x := by
            intro d hd
            have hne : d ≠ x := by
              intro hdx
              exact (hnot d hd) hdx.symm
            exact (List.mem_erase_of_ne hne).mpr (hsub d (by simp [hd]))
          have ihle := ih hnodup hsubTail
          have hlen := List.length_erase_of_mem (a := x) (l := ys) hxys
          have hpos : 0 < ys.length := List.length_pos_of_mem hxys
          rw [hlen] at ihle
          simp at ihle ⊢
          omega

theorem length_le_of_narrows {g' g : Grid} (hn : NodupGrid g')
    (h : NarrowsGrid g' g) (i : Cell) :
    (getOptions g' i).length ≤ (getOptions g i).length :=
  length_le_of_nodup_subset (hn i) (fun d hd => h i d hd)

theorem undecided_eq_true {g : Grid} {i : Cell} :
    undecided g i = true ↔ 1 < (getOptions g i).length := by
  unfold undecided
  simp

theorem undecided_false_of_length_le_one {g : Grid} {i : Cell}
    (hlen : (getOptions g i).length ≤ 1) : undecided g i = false := by
  unfold undecided
  simp [Nat.not_lt.mpr hlen]

theorem undecided_of_narrows {g' g : Grid} (hn : NodupGrid g')
    (h : NarrowsGrid g' g) {i : Cell} :
    undecided g' i = true → undecided g i = true := by
  intro hui
  have hlt : 1 < (getOptions g' i).length := undecided_eq_true.mp hui
  have hle := length_le_of_narrows hn h i
  exact undecided_eq_true.mpr (by omega)

theorem covers_erase_after_decided {todo : List Cell} {g g' : Grid} {i : Cell}
    (hc : CoversUndecided todo g) (hn' : NodupGrid g') (hnar : NarrowsGrid g' g)
    (hi : undecided g' i = false) :
    CoversUndecided (todo.erase i) g' := by
  intro j hj
  have hjOld : undecided g j = true := undecided_of_narrows hn' hnar hj
  have hjTodo : j ∈ todo := hc j hjOld
  have hji : j ≠ i := by
    intro h
    subst j
    rw [hi] at hj
    simp at hj
  exact (List.mem_erase_of_ne hji).mpr hjTodo

theorem assign_not_undecided {g out : Grid} {i : Cell} {d : Digit}
    (hnout : NodupGrid out) (hout : assign g i d = some out) :
    undecided out i = false := by
  unfold assign at hout
  rw [propagate.eq_2] at hout
  let opts := getOptions g i
  by_cases hhas : hasDigit opts d = true
  · by_cases hlen : 1 < opts.length
    · simp [opts, hhas, hlen] at hout
      have hnar : NarrowsGrid out (setOptions g i [d]) :=
        propagate_narrows _ _ _ hout
      have hle := length_le_of_narrows hnout hnar i
      have hleOne : (getOptions out i).length ≤ 1 := by
        simpa [getOptions_setOptions_same] using hle
      exact undecided_false_of_length_le_one hleOne
    · simp [opts, hhas, hlen] at hout
      simp [propagate] at hout
      subst out
      exact undecided_false_of_length_le_one (by
        simpa [opts] using Nat.le_of_not_gt hlen)
  · simp [opts, hhas] at hout

def liftBest {i : Cell} {rest : List Cell} :
    {j : Cell // j ∈ rest} → {j : Cell // j ∈ i :: rest}
  | ⟨j, hj⟩ => ⟨j, by simp [hj]⟩

def bestIndexIn (g : Grid) : (todo : List Cell) → Option {i : Cell // i ∈ todo}
  | [] => none
  | i :: rest =>
      let here : Option {j : Cell // j ∈ i :: rest} :=
        if undecided g i then some ⟨i, by simp⟩ else none
      let there := (bestIndexIn g rest).map liftBest
      match here, there with
      | none, none => none
      | some h, none => some h
      | none, some t => some t
      | some h, some t =>
          if score g h.val ≤ score g t.val then some h else some t

theorem bestIndexIn_none_not_undecided {g : Grid} {todo : List Cell}
    (hbest : bestIndexIn g todo = none) :
    ∀ i, i ∈ todo → undecided g i = false := by
  induction todo with
  | nil =>
      intro i hi
      simp at hi
  | cons x xs ih =>
      intro i hi
      cases hx : undecided g x
      · cases hb : bestIndexIn g xs with
        | none =>
            cases hi with
            | head =>
                exact hx
            | tail _ htail =>
                exact ih hb i htail
        | some picked =>
            simp [bestIndexIn, hx, hb] at hbest
      · cases hb : bestIndexIn g xs <;> simp [bestIndexIn, hx, hb] at hbest
        case some picked =>
          split at hbest <;> simp at hbest

theorem list_all_true_of_forall {α : Type} (p : α → Bool) :
    ∀ xs : List α, (∀ x, x ∈ xs → p x = true) → xs.all p = true
  | [], _ => rfl
  | x :: xs, h => by
      simp [List.all, h x (by simp),
        list_all_true_of_forall p xs (fun y hy => h y (by simp [hy]))]

theorem list_all_true_get {α : Type} {p : α → Bool} :
    ∀ {xs : List α} {x : α}, xs.all p = true → x ∈ xs → p x = true
  | [], _, hall, hx => by
      simp at hx
  | y :: ys, x, hall, hx => by
      simp [List.all] at hall
      cases hx with
      | head =>
          exact hall.1
      | tail _ htail =>
          exact hall.2 x htail

theorem list_any_true_of_mem {α : Type} {p : α → Bool} :
    ∀ {xs : List α} {x : α}, x ∈ xs → p x = true → xs.any p = true
  | [], _, hx, _ => by
      simp at hx
  | y :: ys, x, hx, hp => by
      cases hx with
      | head =>
          simp [List.any, hp]
      | tail _ htail =>
          have htailAny : ys.any p = true := list_any_true_of_mem htail hp
          simp [List.any, htailAny]

theorem exists_of_list_any_true {α : Type} {p : α → Bool} :
    ∀ {xs : List α}, xs.any p = true → ∃ x, x ∈ xs ∧ p x = true
  | [], h => by
      simp at h
  | x :: xs, h => by
      simp [List.any] at h
      cases h with
      | inl hx =>
          exact ⟨x, by simp, hx⟩
      | inr hxs =>
          rcases hxs with ⟨y, hy, hpy⟩
          exact ⟨y, by simp [hy], hpy⟩

theorem solvedGrid_true_of_lengths {g : Grid}
    (h : ∀ i, (getOptions g i).length = 1) : solvedGrid g = true := by
  unfold solvedGrid
  apply list_all_true_of_forall
  intro i _hi
  simp [h i]

theorem length_one_of_solvedGrid {g : Grid} (h : solvedGrid g = true) (i : Cell) :
    (getOptions g i).length = 1 := by
  unfold solvedGrid at h
  have hi := list_all_true_get h (mem_cells i)
  simpa using hi

theorem gridDigit_eq_of_solved_admits {g : Grid} {s : Sol}
    (hsolved : solvedGrid g = true) (hadm : Admits s g) (i : Cell) :
    gridDigit g i = s i := by
  have hlen := length_one_of_solvedGrid hsolved i
  have hmem := hadm i
  unfold gridDigit
  cases hopts : getOptions g i with
  | nil =>
      simp [hopts] at hmem
  | cons d rest =>
      cases rest with
      | nil =>
          simp [hopts] at hmem ⊢
          exact hmem.symm
      | cons e tail =>
          simp [hopts] at hlen

theorem validGrid_complete {g : Grid} {s : Sol}
    (hsol : IsSolution s) (hsolved : solvedGrid g = true) (hadm : Admits s g) :
    validGrid g = true := by
  unfold validGrid
  have hgd : ∀ i, gridDigit g i = s i :=
    gridDigit_eq_of_solved_admits hsolved hadm
  have hpeers :
      cells.all (fun a =>
        cells.all (fun b =>
          if peerB a b then gridDigit g a != gridDigit g b else true)) = true := by
    apply list_all_true_of_forall
    intro a _ha
    apply list_all_true_of_forall
    intro b _hb
    cases hp : peerB a b
    · simp
    · have hneq : s a ≠ s b := hsol.1 a b hp
      simp [hgd a, hgd b, hneq]
  have hunits :
      units.all (fun u => digits.all (fun d => unitHasDigit g u d)) = true := by
    apply list_all_true_of_forall
    intro u hu
    apply list_all_true_of_forall
    intro d _hd
    rcases hsol.2 u hu d with ⟨i, hiu, hsid⟩
    unfold unitHasDigit
    apply list_any_true_of_mem hiu
    simp [hgd i, hsid]
  rw [hpeers, hunits]
  rfl

theorem exists_undecided_of_not_solved_admits {g : Grid} {s : Sol}
    (hadm : Admits s g) (hnot : ¬ solvedGrid g = true) :
    ∃ i, undecided g i = true := by
  by_cases hex : ∃ i, undecided g i = true
  · exact hex
  · have hall : ∀ i, (getOptions g i).length = 1 := by
      intro i
      have hpos : 0 < (getOptions g i).length := List.length_pos_of_mem (hadm i)
      have hnlt : ¬ 1 < (getOptions g i).length := by
        intro hlt
        exact hex ⟨i, undecided_eq_true.mpr hlt⟩
      omega
    exact False.elim (hnot (solvedGrid_true_of_lengths hall))

theorem length_erase_lt_of_mem {α : Type} [BEq α] [LawfulBEq α]
    {x : α} {xs : List α} (hmem : x ∈ xs) :
    (xs.erase x).length < xs.length := by
  have hlen := List.length_erase_of_mem (a := x) (l := xs) hmem
  have hpos : 0 < xs.length := List.length_pos_of_mem hmem
  omega

mutual
  def searchTodo : List Cell → Grid → Option Grid
    | todo, g =>
        if solvedGrid g then
          if validGrid g then some g else none
        else
          match bestIndexIn g todo with
          | none => none
          | some picked =>
              tryValues todo g picked.val picked.property (getOptions g picked.val)
  termination_by todo _g => (2 * todo.length + 1, 0)

  def tryValues (todo : List Cell) (g : Grid) (i : Cell) (himem : i ∈ todo) :
      List Digit → Option Grid
    | [] => none
    | d :: rest =>
        match assign g i d with
        | none => tryValues todo g i himem rest
        | some g' =>
            match searchTodo (todo.erase i) g' with
            | some solved => some solved
            | none => tryValues todo g i himem rest
  termination_by vals => (2 * todo.length, vals.length)
decreasing_by
  · exact Prod.Lex.right (2 * todo.length) (Nat.lt_succ_self rest.length)
  · exact Prod.Lex.left _ _ (by
      have hlen : (todo.erase i).length < todo.length := length_erase_lt_of_mem himem
      omega)
  · exact Prod.Lex.right (2 * todo.length) (Nat.lt_succ_self rest.length)
end

theorem searchTodo_complete :
    ∀ (todo : List Cell) (g : Grid) (s : Sol),
      IsSolution s → Admits s g → NodupGrid g → CoversUndecided todo g →
        ∃ out, searchTodo todo g = some out := by
  intro todo g
  refine searchTodo.induct
    (motive1 := fun todo g =>
      ∀ (s : Sol), IsSolution s → Admits s g → NodupGrid g → CoversUndecided todo g →
        ∃ out, searchTodo todo g = some out)
    (motive2 := fun todo g i _himem vals =>
      ∀ (s : Sol), IsSolution s → Admits s g → NodupGrid g → CoversUndecided todo g →
        s i ∈ vals → ∃ out, tryValues todo g i _himem vals = some out)
    ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_ todo g
  · intro todo g hsolved hvalid s _hsol _hadm _hn _hc
    exact ⟨g, by simp [searchTodo, hsolved, hvalid]⟩
  · intro todo g hsolved hnotValid s hsol hadm _hn _hc
    have hvalid : validGrid g = true := validGrid_complete hsol hsolved hadm
    exact False.elim (hnotValid hvalid)
  · intro todo g hnot hbest s _hsol hadm _hn hc
    rcases exists_undecided_of_not_solved_admits hadm hnot with ⟨i, hui⟩
    have hiTodo : i ∈ todo := hc i hui
    have hfalse := bestIndexIn_none_not_undecided hbest i hiTodo
    rw [hfalse] at hui
    simp at hui
  · intro todo g hnot picked hpick ih s hsol hadm hn hc
    have hmem : s picked.val ∈ getOptions g picked.val := hadm picked.val
    rcases ih s hsol hadm hn hc hmem with ⟨out, hout⟩
    refine ⟨out, ?_⟩
    simpa [searchTodo, hnot, hpick] using hout
  · intro todo g i himem s _hsol _hadm _hn _hc hmem
    simp at hmem
  · intro todo g i himem d rest hassign ih s hsol hadm hn hc hmem
    cases List.mem_cons.mp hmem with
    | inl hEq =>
        rcases assign_admits hsol hadm hn hEq with ⟨g', hg', _hadm', _hn'⟩
        rw [hassign] at hg'
        simp at hg'
    | inr hRest =>
        rcases ih s hsol hadm hn hc hRest with ⟨out, hout⟩
        refine ⟨out, ?_⟩
        simpa [tryValues, hassign] using hout
  · intro todo g i himem d rest g' hassign solved hsearch _ih s _hsol _hadm _hn _hc _hmem
    exact ⟨solved, by simp [tryValues, hassign, hsearch]⟩
  · intro todo g i himem d rest g' hassign hsearch ihSearch ihRest s hsol hadm hn hc hmem
    cases List.mem_cons.mp hmem with
    | inl hEq =>
        rcases assign_admits hsol hadm hn hEq with ⟨gSol, hgSol, hadm', hn'⟩
        rw [hassign] at hgSol
        injection hgSol with hgEq
        subst gSol
        have hnar : NarrowsGrid g' g := assign_narrows hassign
        have hdec : undecided g' i = false := assign_not_undecided hn' hassign
        have hc' : CoversUndecided (todo.erase i) g' :=
          covers_erase_after_decided hc hn' hnar hdec
        rcases ihSearch s hsol hadm' hn' hc' with ⟨out, hout⟩
        rw [hsearch] at hout
        simp at hout
    | inr hRest =>
        rcases ihRest s hsol hadm hn hc hRest with ⟨out, hout⟩
        refine ⟨out, ?_⟩
        simpa [tryValues, hassign, hsearch] using hout

def searchGrid (g : Grid) : Option Grid :=
  searchTodo cells g

theorem searchGrid_complete {g : Grid} {s : Sol}
    (hsol : IsSolution s) (hadm : Admits s g) (hn : NodupGrid g) :
    ∃ out, searchGrid g = some out := by
  have hc : CoversUndecided cells g := by
    intro i _hi
    exact mem_cells i
  simpa [searchGrid] using searchTodo_complete cells g s hsol hadm hn hc

theorem validGrid_sound {g : Grid} (hvalid : validGrid g = true) :
    IsSolution (gridDigit g) := by
  unfold validGrid at hvalid
  simp [unitHasDigit] at hvalid
  constructor
  · intro a b hp heq
    have hpair := hvalid.1 a (mem_cells a) b (mem_cells b)
    cases hpair with
    | inl hfalse =>
        rw [hp] at hfalse
        simp at hfalse
    | inr hneq =>
        exact hneq heq
  · intro u hu d
    exact hvalid.2 u hu d (mem_digits d)

theorem searchTodo_sound_valid :
    ∀ (todo : List Cell) (g out : Grid),
      searchTodo todo g = some out → solvedGrid out = true ∧ validGrid out = true := by
  intro todo g
  refine searchTodo.induct
    (motive1 := fun todo g =>
      ∀ out, searchTodo todo g = some out → solvedGrid out = true ∧ validGrid out = true)
    (motive2 := fun todo g i himem vals =>
      ∀ out, tryValues todo g i himem vals = some out →
        solvedGrid out = true ∧ validGrid out = true)
    ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_ todo g
  · intro todo g hsolved hvalid out hout
    simp [searchTodo, hsolved, hvalid] at hout
    subst out
    exact ⟨hsolved, hvalid⟩
  · intro todo g hsolved hnotValid out hout
    simp [searchTodo, hsolved, hnotValid] at hout
  · intro todo g hnot hbest out hout
    simp [searchTodo, hnot, hbest] at hout
  · intro todo g hnot picked hpick ih out hout
    have htry : tryValues todo g picked.val picked.property (getOptions g picked.val) = some out := by
      simpa [searchTodo, hnot, hpick] using hout
    exact ih out htry
  · intro todo g i himem out hout
    simp [tryValues] at hout
  · intro todo g i himem d rest hassign ih out hout
    have hrest : tryValues todo g i himem rest = some out := by
      simpa [tryValues, hassign] using hout
    exact ih out hrest
  · intro todo g i himem d rest g' hassign solved hsearch ih out hout
    simp [tryValues, hassign, hsearch] at hout
    subst out
    exact ih solved hsearch
  · intro todo g i himem d rest g' hassign hsearch ihSearch ihRest out hout
    have hrest : tryValues todo g i himem rest = some out := by
      simpa [tryValues, hassign, hsearch] using hout
    exact ihRest out hrest

theorem searchGrid_sound {g out : Grid} (h : searchGrid g = some out) :
    IsSolution (gridDigit out) := by
  have hs := searchTodo_sound_valid cells g out (by simpa [searchGrid] using h)
  exact validGrid_sound hs.2

end Sudoku.CP
