import Std

set_option autoImplicit false
set_option maxRecDepth 4096
set_option maxHeartbeats 1000000
set_option linter.unusedVariables false

namespace Sudoku.NorvigWF

abbrev Digit := Fin 9
abbrev Cell := Fin 81
abbrev Options := List Digit
abbrev Grid := Vector Options 81

instance : Inhabited Digit := ⟨0, by decide⟩
instance : Inhabited Cell := ⟨0, by decide⟩

def digits : List Digit := List.finRange 9
def cells : List Cell := List.finRange 81
def fullOptions : Options := digits
def initialGrid : Grid := Vector.replicate 81 fullOptions

theorem mem_cells (i : Cell) : i ∈ cells := by
  unfold cells
  exact List.mem_finRange i

theorem mem_digits (d : Digit) : d ∈ digits := by
  unfold digits
  exact List.mem_finRange d

theorem nodup_cells : cells.Nodup := by
  native_decide

def digitToNat (d : Digit) : Nat := d.val + 1

def natToDigit? : Nat → Option Digit
  | 1 => some ⟨0, by decide⟩
  | 2 => some ⟨1, by decide⟩
  | 3 => some ⟨2, by decide⟩
  | 4 => some ⟨3, by decide⟩
  | 5 => some ⟨4, by decide⟩
  | 6 => some ⟨5, by decide⟩
  | 7 => some ⟨6, by decide⟩
  | 8 => some ⟨7, by decide⟩
  | 9 => some ⟨8, by decide⟩
  | _ => none

def rowOf (i : Cell) : Nat := i.val / 9
def colOf (i : Cell) : Nat := i.val % 9

def cellOfRowCol (r c : Fin 9) : Cell :=
  ⟨r.val * 9 + c.val, by
    have hr : r.val < 9 := r.isLt
    have hc : c.val < 9 := c.isLt
    omega⟩

def cellOfBox (br bc dr dc : Fin 3) : Cell :=
  cellOfRowCol
    ⟨br.val * 3 + dr.val, by
      have hbr : br.val < 3 := br.isLt
      have hdr : dr.val < 3 := dr.isLt
      omega⟩
    ⟨bc.val * 3 + dc.val, by
      have hbc : bc.val < 3 := bc.isLt
      have hdc : dc.val < 3 := dc.isLt
      omega⟩

def rowUnit (r : Fin 9) : List Cell :=
  (List.finRange 9).map (fun c => cellOfRowCol r c)

def colUnit (c : Fin 9) : List Cell :=
  (List.finRange 9).map (fun r => cellOfRowCol r c)

def boxUnit (br bc : Fin 3) : List Cell :=
  (List.finRange 3).flatMap (fun dr =>
    (List.finRange 3).map (fun dc => cellOfBox br bc dr dc))

def rows : List (List Cell) :=
  (List.finRange 9).map rowUnit

def cols : List (List Cell) :=
  (List.finRange 9).map colUnit

def boxes : List (List Cell) :=
  (List.finRange 3).flatMap (fun br =>
    (List.finRange 3).map (fun bc => boxUnit br bc))

def units : List (List Cell) := rows ++ cols ++ boxes

def rowFinOfCell (i : Cell) : Fin 9 :=
  ⟨rowOf i, by
    unfold rowOf
    have h : i.val < 81 := i.isLt
    omega⟩

def colFinOfCell (i : Cell) : Fin 9 :=
  ⟨colOf i, Nat.mod_lt _ (by decide : 0 < 9)⟩

def boxRowOfCell (i : Cell) : Fin 3 :=
  ⟨rowOf i / 3, by
    have hrow : rowOf i < 9 := (rowFinOfCell i).isLt
    omega⟩

def boxColOfCell (i : Cell) : Fin 3 :=
  ⟨colOf i / 3, by
    have hcol : colOf i < 9 := (colFinOfCell i).isLt
    omega⟩

def unitsOf (i : Cell) : List (List Cell) :=
  [rowUnit (rowFinOfCell i), colUnit (colFinOfCell i),
    boxUnit (boxRowOfCell i) (boxColOfCell i)]

def peersOf (i : Cell) : List Cell :=
  ((rowUnit (rowFinOfCell i) ++ colUnit (colFinOfCell i) ++
      boxUnit (boxRowOfCell i) (boxColOfCell i)).eraseDups).erase i

def getOptions (g : Grid) (i : Cell) : Options := g[i.val]

def setOptions (g : Grid) (i : Cell) (opts : Options) : Grid :=
  g.set i.val opts i.isLt

theorem getOptions_setOptions_same (g : Grid) (i : Cell) (opts : Options) :
    getOptions (setOptions g i opts) i = opts := by
  unfold getOptions setOptions
  simp

theorem getOptions_setOptions_ne (g : Grid) {i j : Cell} (opts : Options)
    (hij : j ≠ i) :
    getOptions (setOptions g i opts) j = getOptions g j := by
  unfold getOptions setOptions
  have hval : i.val ≠ j.val := by
    intro h
    apply hij
    exact Fin.ext h.symm
  simp [hval]

def hasDigit (opts : Options) (d : Digit) : Bool :=
  opts.contains d

def eraseDigit (opts : Options) (d : Digit) : Options :=
  opts.erase d

def singletonDigit? : Options → Option Digit
  | [d] => some d
  | _ => none

def solvedGrid (g : Grid) : Bool :=
  cells.all (fun i => (getOptions g i).length == 1)

def sumByCells (f : Cell → Nat) : List Cell → Nat
  | [] => 0
  | i :: rest => f i + sumByCells f rest

@[irreducible] def candidateCount (g : Grid) : Nat :=
  sumByCells (fun i => (getOptions g i).length) cells

inductive Task where
  | assign : Cell → Digit → Task
  | elim : Cell → Digit → Task
  | checkUnit : List Cell → Digit → Task
deriving Repr

def assignWeight (g : Grid) (i : Cell) : Nat :=
  (getOptions g i).length + 1

def maxAssignWeight (g : Grid) : List Cell → Nat
  | [] => 0
  | i :: rest => max (assignWeight g i) (maxAssignWeight g rest)

def taskWeight (g : Grid) : Task → Nat
  | Task.elim _ _ => 1
  | Task.assign i _ => assignWeight g i
  | Task.checkUnit u _ => maxAssignWeight g u + 1

def workWeight (g : Grid) : List Task → Nat
  | [] => 0
  | t :: rest => taskWeight g t + workWeight g rest

theorem workWeight_append (g : Grid) (xs ys : List Task) :
    workWeight g (xs ++ ys) = workWeight g xs + workWeight g ys := by
  induction xs with
  | nil =>
      simp [workWeight]
  | cons x xs ih =>
      simp [workWeight, ih, Nat.add_assoc]

theorem workWeight_elim_map (g : Grid) (i : Cell) (xs : List Digit) :
    workWeight g (xs.map (fun d => Task.elim i d)) = xs.length := by
  induction xs with
  | nil =>
      simp [workWeight]
  | cons x xs ih =>
      simp [workWeight, taskWeight, ih, Nat.add_comm]

theorem assignWeight_le_maxAssignWeight {g : Grid} {u : List Cell} {i : Cell}
    (hi : i ∈ u) : assignWeight g i ≤ maxAssignWeight g u := by
  induction u with
  | nil =>
      simp at hi
  | cons x xs ih =>
      simp [maxAssignWeight] at hi ⊢
      cases hi with
      | inl h =>
          subst h
          exact Nat.le_max_left _ _
      | inr h =>
          exact Nat.le_trans (ih h) (Nat.le_max_right _ _)

def assignTasks (g : Grid) (i : Cell) (d : Digit) : List Task :=
  ((getOptions g i).erase d).map (fun other => Task.elim i other)

def r1Tasks (i : Cell) (d : Digit) : List Task :=
  (peersOf i).map (fun p => Task.elim p d)

def r2Tasks (i : Cell) (d : Digit) : List Task :=
  (unitsOf i).map (fun u => Task.checkUnit u d)

def assignR2Tasks (i : Cell) (removed : List Digit) : List Task :=
  removed.flatMap (fun other => r2Tasks i other)

def possiblePlaces (g : Grid) (u : List Cell) (d : Digit) : List Cell :=
  u.filter (fun i => hasDigit (getOptions g i) d)

theorem sumByCells_congr {f h : Cell → Nat} :
    ∀ xs, (∀ i, i ∈ xs → f i = h i) → sumByCells f xs = sumByCells h xs
  | [], _ => rfl
  | x :: xs, heq => by
      simp [sumByCells, heq x (by simp)]
      exact sumByCells_congr xs (fun i hi => heq i (by simp [hi]))

theorem sumByCells_decrease_one {f h : Cell → Nat} :
    ∀ (x : Cell) (xs : List Cell), xs.Nodup → x ∈ xs →
      (∀ y, y ∈ xs → y ≠ x → h y = f y) →
      h x < f x →
      sumByCells h xs < sumByCells f xs
  | x, [], _hnodup, hmem, _, _ => by
      simp at hmem
  | x, y :: ys, hnodup, hmem, hsame, hlt => by
      cases hnodup with
      | cons hnotmem hnodupTail =>
      simp [sumByCells] at hmem ⊢
      cases hmem with
      | inl hyx =>
          subst hyx
          have htail : sumByCells h ys = sumByCells f ys :=
            sumByCells_congr ys (fun z hz =>
              hsame z (by simp [hz]) (by
                intro hzx
                subst hzx
                exact (hnotmem z hz) rfl))
          omega
      | inr hxmem =>
          have hy_ne_x : y ≠ x := by
            intro hyx
            subst hyx
            exact (hnotmem y hxmem) rfl
          have hsame_y : h y = f y := hsame y (by simp) hy_ne_x
          have hrec : sumByCells h ys < sumByCells f ys :=
            sumByCells_decrease_one x ys hnodupTail hxmem
              (fun z hz hzx => hsame z (by simp [hz]) hzx) hlt
          omega

theorem candidateCount_setOptions_erase_lt {g : Grid} {i : Cell} {d : Digit}
    (hhas : hasDigit (getOptions g i) d = true) :
    candidateCount (setOptions g i (eraseDigit (getOptions g i) d)) <
      candidateCount g := by
  unfold candidateCount
  apply sumByCells_decrease_one i cells nodup_cells (mem_cells i)
  · intro y _hy hyne
    simp [getOptions_setOptions_ne, hyne]
  · simp [getOptions_setOptions_same, eraseDigit, hasDigit] at hhas ⊢
    have hlen := List.length_erase_of_mem (l := getOptions g i) hhas
    have hpos : 0 < (getOptions g i).length := List.length_pos_of_mem hhas
    omega

theorem candidateCount_setOptions_singleton_lt {g : Grid} {i : Cell} {d a b : Digit}
    {tail : List Digit} (hopts : getOptions g i = a :: b :: tail) :
    candidateCount (setOptions g i [d]) < candidateCount g := by
  unfold candidateCount
  apply sumByCells_decrease_one i cells nodup_cells (mem_cells i)
  · intro y _hy hyne
    simp [getOptions_setOptions_ne, hyne]
  · rw [getOptions_setOptions_same]
    simp [hopts]

theorem candidateCount_setOptions_singleton_lt_of_length {g : Grid} {i : Cell}
    {d : Digit} (hlt : 1 < (getOptions g i).length) :
    candidateCount (setOptions g i [d]) < candidateCount g := by
  unfold candidateCount
  apply sumByCells_decrease_one i cells nodup_cells (mem_cells i)
  · intro y _hy hyne
    simp [getOptions_setOptions_ne, hyne]
  · rw [getOptions_setOptions_same]
    simp
    exact hlt

/-! ## Semantic model and CP preservation facts -/

abbrev Sol := Cell → Digit

def sameBlock (a b : Cell) : Bool :=
  (rowOf a / 3 == rowOf b / 3) && (colOf a / 3 == colOf b / 3)

def peerB (a b : Cell) : Bool :=
  (a != b) && ((rowOf a == rowOf b) || (colOf a == colOf b) || sameBlock a b)

def IsSolution (s : Sol) : Prop :=
  (∀ a b, peerB a b = true → s a ≠ s b) ∧
  (∀ u, u ∈ units → ∀ d, ∃ i, i ∈ u ∧ s i = d)

def Admits (s : Sol) (g : Grid) : Prop :=
  ∀ i, s i ∈ getOptions g i

def NodupGrid (g : Grid) : Prop :=
  ∀ i, (getOptions g i).Nodup

def NarrowsGrid (g' g : Grid) : Prop :=
  ∀ i d, d ∈ getOptions g' i → d ∈ getOptions g i

def TaskOk (s : Sol) : Task → Prop
  | Task.assign i d => s i = d
  | Task.elim i d => s i ≠ d
  | Task.checkUnit u _d => u ∈ units

def TasksOk (s : Sol) (ts : List Task) : Prop :=
  ∀ t, t ∈ ts → TaskOk s t

theorem hasDigit_eq_true {opts : Options} {d : Digit} :
    hasDigit opts d = true ↔ d ∈ opts := by
  unfold hasDigit
  simp

theorem not_hasDigit_eq_true {opts : Options} {d : Digit} :
    hasDigit opts d = false ↔ d ∉ opts := by
  unfold hasDigit
  simp

theorem mem_erase_of_mem_ne {opts : Options} {d e : Digit}
    (he : e ∈ opts) (hne : e ≠ d) : e ∈ eraseDigit opts d := by
  unfold eraseDigit
  exact (List.mem_erase_of_ne hne).mpr he

theorem nodupGrid_setOptions_erase {g : Grid} {i : Cell} {d : Digit}
    (hn : NodupGrid g) :
    NodupGrid (setOptions g i (eraseDigit (getOptions g i) d)) := by
  intro j
  by_cases hji : j = i
  · subst j
    rw [getOptions_setOptions_same]
    unfold eraseDigit
    exact List.Nodup.erase d (hn i)
  · rw [getOptions_setOptions_ne _ _ hji]
    exact hn j

theorem admits_setOptions_erase {s : Sol} {g : Grid} {i : Cell} {d : Digit}
    (hadm : Admits s g) (hne : s i ≠ d) :
    Admits s (setOptions g i (eraseDigit (getOptions g i) d)) := by
  intro j
  by_cases hji : j = i
  · subst j
    rw [getOptions_setOptions_same]
    exact mem_erase_of_mem_ne (hadm i) hne
  · rw [getOptions_setOptions_ne _ _ hji]
    exact hadm j

theorem nodupGrid_setOptions_singleton {g : Grid} {i : Cell} {d : Digit}
    (hn : NodupGrid g) :
    NodupGrid (setOptions g i [d]) := by
  intro j
  by_cases hji : j = i
  · subst j
    rw [getOptions_setOptions_same]
    simp
  · rw [getOptions_setOptions_ne _ _ hji]
    exact hn j

theorem admits_setOptions_singleton {s : Sol} {g : Grid} {i : Cell} {d : Digit}
    (hadm : Admits s g) (hsi : s i = d) :
    Admits s (setOptions g i [d]) := by
  intro j
  by_cases hji : j = i
  · subst j
    rw [getOptions_setOptions_same]
    simp [hsi]
  · rw [getOptions_setOptions_ne _ _ hji]
    exact hadm j

theorem narrows_refl (g : Grid) : NarrowsGrid g g := by
  intro _ _ h
  exact h

theorem narrows_trans {g'' g' g : Grid}
    (h1 : NarrowsGrid g'' g') (h2 : NarrowsGrid g' g) :
    NarrowsGrid g'' g := by
  intro i d hd
  exact h2 i d (h1 i d hd)

theorem narrows_setOptions_erase (g : Grid) (i : Cell) (d : Digit) :
    NarrowsGrid (setOptions g i (eraseDigit (getOptions g i) d)) g := by
  intro j e he
  by_cases hji : j = i
  · subst j
    rw [getOptions_setOptions_same] at he
    unfold eraseDigit at he
    exact List.mem_of_mem_erase he
  · rw [getOptions_setOptions_ne _ _ hji] at he
    exact he

theorem narrows_setOptions_singleton_of_mem {g : Grid} {i : Cell} {d : Digit}
    (hd : d ∈ getOptions g i) : NarrowsGrid (setOptions g i [d]) g := by
  intro j e he
  by_cases hji : j = i
  · subst j
    rw [getOptions_setOptions_same] at he
    simp at he
    simpa [he] using hd
  · rw [getOptions_setOptions_ne _ _ hji] at he
    exact he

theorem singleton_admitted_eq {s : Sol} {g : Grid} {i : Cell} {d : Digit}
    (hadm : Admits s g) (hopts : getOptions g i = [d]) : s i = d := by
  have hm : s i ∈ [d] := by simpa [hopts] using hadm i
  simpa using hm

theorem mem_possiblePlaces {g : Grid} {u : List Cell} {d : Digit} {i : Cell} :
    i ∈ possiblePlaces g u d ↔ i ∈ u ∧ d ∈ getOptions g i := by
  unfold possiblePlaces
  rw [List.mem_filter]
  constructor
  · intro h
    exact ⟨h.1, hasDigit_eq_true.mp h.2⟩
  · intro h
    exact ⟨h.1, hasDigit_eq_true.mpr h.2⟩

theorem peer_of_mem_peersOf {i p : Cell} (hp : p ∈ peersOf i) :
    peerB i p = true := by
  revert i p
  native_decide

theorem unitsOf_mem_units {i : Cell} {u : List Cell} (hu : u ∈ unitsOf i) :
    u ∈ units := by
  revert i u
  native_decide

theorem taskOk_of_mem_assignTasks {s : Sol} {g : Grid} {i : Cell} {d : Digit}
    (hsi : s i = d) (hnodup : (getOptions g i).Nodup) :
    ∀ t, t ∈ assignTasks g i d → TaskOk s t := by
  intro t ht
  unfold assignTasks at ht
  rcases List.mem_map.mp ht with ⟨other, hother, rfl⟩
  simp [TaskOk]
  intro hbad
  have hmem : other ∈ (getOptions g i).erase d := hother
  have hne : other ≠ d := by
    intro h
    subst h
    exact List.Nodup.not_mem_erase hnodup hmem
  apply hne
  rw [← hsi]
  exact hbad.symm

theorem taskOk_of_mem_r1Tasks {s : Sol} {g : Grid} {i : Cell} {d : Digit}
    (hsol : IsSolution s) (hadm : Admits s g) (hopts : getOptions g i = [d]) :
    ∀ t, t ∈ r1Tasks i d → TaskOk s t := by
  intro t ht
  unfold r1Tasks at ht
  rcases List.mem_map.mp ht with ⟨p, hp, rfl⟩
  simp [TaskOk]
  have hsid : s i = d := singleton_admitted_eq hadm hopts
  have hneq : s i ≠ s p := hsol.1 i p (peer_of_mem_peersOf hp)
  intro hpd
  exact hneq (by simp [hsid, hpd])

theorem taskOk_of_mem_r2Tasks {s : Sol} {i : Cell} {d : Digit} :
    ∀ t, t ∈ r2Tasks i d → TaskOk s t := by
  intro t ht
  unfold r2Tasks at ht
  rcases List.mem_map.mp ht with ⟨u, hu, rfl⟩
  exact unitsOf_mem_units hu

theorem taskOk_of_mem_assignR2Tasks {s : Sol} {i : Cell} {removed : List Digit} :
    ∀ t, t ∈ assignR2Tasks i removed → TaskOk s t := by
  intro t ht
  unfold assignR2Tasks at ht
  simp at ht
  rcases ht with ⟨d, _hd, htd⟩
  exact taskOk_of_mem_r2Tasks t htd

theorem tasksOk_append {s : Sol} {xs ys : List Task}
    (hx : TasksOk s xs) (hy : TasksOk s ys) :
    TasksOk s (xs ++ ys) := by
  intro t ht
  have hmem := List.mem_append.mp ht
  cases hmem with
  | inl h => exact hx t h
  | inr h => exact hy t h

theorem tasksOk_cons {s : Sol} {t : Task} {ts : List Task}
    (ht : TaskOk s t) (hts : TasksOk s ts) : TasksOk s (t :: ts) := by
  intro q hq
  cases hq with
  | head => exact ht
  | tail _ h => exact hts q h

theorem tasksOk_tail {s : Sol} {t : Task} {ts : List Task}
    (h : TasksOk s (t :: ts)) : TasksOk s ts := by
  intro q hq
  exact h q (by simp [hq])


def propagate : List Task → Grid → Option Grid
  | [], g => some g
  | Task.assign i d :: rest, g =>
      let opts := getOptions g i
      if hasDigit opts d then
        if 1 < opts.length then
          let removed := eraseDigit opts d
          let g' := setOptions g i [d]
          propagate (r1Tasks i d ++ assignR2Tasks i removed ++ rest) g'
        else
          propagate rest g
      else
        none
  | Task.checkUnit u d :: rest, g =>
      match hplaces : possiblePlaces g u d with
      | [] => none
      | [only] => propagate (Task.assign only d :: rest) g
      | _ :: _ :: _ => propagate rest g
  | Task.elim i d :: rest, g =>
      let opts := getOptions g i
      if hhas : hasDigit opts d then
        let opts' := eraseDigit opts d
        let g' := setOptions g i opts'
        match opts' with
        | [] => none
        | [single] => propagate (r1Tasks i single ++ r2Tasks i d ++ rest) g'
        | _ :: _ :: _ => propagate (r2Tasks i d ++ rest) g'
      else
        propagate rest g
termination_by ts g => (candidateCount g, workWeight g ts)
decreasing_by
  · -- an effective batched assignment strictly shrinks the chosen cell.
    rename_i hlen
    exact Prod.Lex.left _ _ (candidateCount_setOptions_singleton_lt_of_length
      (g := g) (i := i) (d := d) (by
        simpa [opts] using hlen))
  · -- assigning an already-singleton cell consumes one task.
    exact Prod.Lex.right (candidateCount g) (by
      simp [workWeight, taskWeight, assignWeight])
  · -- hidden-single unit check expands to one assignment, so queue weight drops.
    exact Prod.Lex.right (candidateCount g) (by
      simp [workWeight, taskWeight]
      have hmemPlaces : only ∈ possiblePlaces g u d := by
        simp [hplaces]
      have hmemU : only ∈ u := by
        unfold possiblePlaces at hmemPlaces
        exact (List.mem_filter.mp hmemPlaces).1
      exact Nat.lt_succ_of_le (assignWeight_le_maxAssignWeight (g := g) hmemU))
  · -- non-forcing unit check consumes one task.
    exact Prod.Lex.right (candidateCount g) (by
      simp [workWeight, taskWeight])
  · -- effective singleton elimination strictly decreases the candidate count.
    exact Prod.Lex.left _ _ (candidateCount_setOptions_erase_lt (g := g) (i := i) (d := d) (by
      simpa [opts] using hhas))
  · -- effective non-singleton elimination strictly decreases the candidate count.
    exact Prod.Lex.left _ _ (candidateCount_setOptions_erase_lt (g := g) (i := i) (d := d) (by
      simpa [opts] using hhas))
  · -- no-op elimination consumes one task.
    exact Prod.Lex.right (candidateCount g) (by
      simp [workWeight, taskWeight])

theorem propagate_admits :
    ∀ (ts : List Task) (g : Grid) (s : Sol),
      IsSolution s → Admits s g → NodupGrid g → TasksOk s ts →
        ∃ g', propagate ts g = some g' ∧ Admits s g' ∧ NodupGrid g' := by
  refine propagate.induct
    (motive := fun ts g =>
      ∀ (sol : Sol), IsSolution sol → Admits sol g → NodupGrid g → TasksOk sol ts →
        ∃ g', propagate ts g = some g' ∧ Admits sol g' ∧ NodupGrid g')
    ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_
  · intro g sol _hsol hadm hn _hok
    exact ⟨g, by simp [propagate], hadm, hn⟩
  · intro i d rest g opts hhas hlen removed g' ih sol hsol hadm hn hok
    have hassign : TaskOk sol (Task.assign i d) := hok (Task.assign i d) (by simp)
    have hadm' : Admits sol g' := by
      dsimp [g']
      exact admits_setOptions_singleton hadm hassign
    have hn' : NodupGrid g' := by
      dsimp [g']
      exact nodupGrid_setOptions_singleton hn
    have hopts : getOptions g' i = [d] := by
      dsimp [g']
      rw [getOptions_setOptions_same]
    have hrest : TasksOk sol rest := tasksOk_tail hok
    have hr1 : TasksOk sol (r1Tasks i d) :=
      taskOk_of_mem_r1Tasks hsol hadm' hopts
    have hr2 : TasksOk sol (assignR2Tasks i removed) :=
      taskOk_of_mem_assignR2Tasks
    have hnew : TasksOk sol (r1Tasks i d ++ assignR2Tasks i removed ++ rest) :=
      tasksOk_append (tasksOk_append hr1 hr2) hrest
    rcases ih sol hsol hadm' hn' hnew with ⟨out, hout, hadmOut, hnOut⟩
    refine ⟨out, ?_, hadmOut, hnOut⟩
    rw [propagate.eq_2]
    simp [opts, hhas, hlen]
    simpa [removed, g', List.append_assoc] using hout
  · intro i d rest g opts hhas hnotLong ih sol hsol hadm hn hok
    have hrest : TasksOk sol rest := tasksOk_tail hok
    rcases ih sol hsol hadm hn hrest with ⟨out, hout, hadmOut, hnOut⟩
    refine ⟨out, ?_, hadmOut, hnOut⟩
    rw [propagate.eq_2]
    simp [opts, hhas, hnotLong, hout]
  · intro i d rest g opts hnot sol _hsol hadm _hn hok
    have hassign : TaskOk sol (Task.assign i d) := hok (Task.assign i d) (by simp)
    have hsi : sol i = d := hassign
    have hmem : d ∈ opts := by
      rw [← hsi]
      simpa [opts] using hadm i
    exact False.elim (hnot (hasDigit_eq_true.mpr hmem))
  · intro u d rest g hplaces sol hsol hadm hn hok
    have htask : TaskOk sol (Task.checkUnit u d) := hok (Task.checkUnit u d) (by simp)
    change u ∈ units at htask
    have hu : u ∈ units := htask
    rcases hsol.2 u hu d with ⟨i, hiu, hsid⟩
    have hmem : i ∈ possiblePlaces g u d := by
      exact mem_possiblePlaces.mpr ⟨hiu, by simpa [hsid] using hadm i⟩
    simp [hplaces] at hmem
  · intro u d rest g only hplaces ih sol hsol hadm hn hok
    have htask : TaskOk sol (Task.checkUnit u d) := hok (Task.checkUnit u d) (by simp)
    change u ∈ units at htask
    have hu : u ∈ units := htask
    have hrest : TasksOk sol rest := tasksOk_tail hok
    have honly : sol only = d := by
      rcases hsol.2 u hu d with ⟨i, hiu, hsid⟩
      have hmem : i ∈ possiblePlaces g u d := by
        exact mem_possiblePlaces.mpr ⟨hiu, by simpa [hsid] using hadm i⟩
      have hieq : i = only := by simpa [hplaces] using hmem
      simpa [← hieq] using hsid
    have hnew : TasksOk sol (Task.assign only d :: rest) :=
      tasksOk_cons honly hrest
    rcases ih sol hsol hadm hn hnew with ⟨out, hout, hadmOut, hnOut⟩
    refine ⟨out, ?_, hadmOut, hnOut⟩
    rw [propagate.eq_3, hplaces]
    simpa using hout
  · intro u d rest g head head₁ tail hplaces ih sol hsol hadm hn hok
    have hrest : TasksOk sol rest := tasksOk_tail hok
    rcases ih sol hsol hadm hn hrest with ⟨out, hout, hadmOut, hnOut⟩
    refine ⟨out, ?_, hadmOut, hnOut⟩
    rw [propagate.eq_3, hplaces]
    exact hout
  · intro i d rest g opts hhas opts' hempty sol hsol hadm hn hok
    have htask : TaskOk sol (Task.elim i d) := hok (Task.elim i d) (by simp)
    have hadm' : Admits sol (setOptions g i (eraseDigit (getOptions g i) d)) :=
      admits_setOptions_erase hadm htask
    have hmemEmpty : sol i ∈ ([] : Options) := by
      simpa [getOptions_setOptions_same, hempty, opts, opts'] using hadm' i
    simp at hmemEmpty
  · intro i d rest g opts hhas opts' g' single hsingle ih sol hsol hadm hn hok
    have htask : TaskOk sol (Task.elim i d) := hok (Task.elim i d) (by simp)
    have hadm' : Admits sol g' := by
      dsimp [g']
      simpa [opts, opts'] using admits_setOptions_erase (g := g) (i := i) (d := d) hadm htask
    have hn' : NodupGrid g' := by
      dsimp [g']
      simpa [opts, opts'] using nodupGrid_setOptions_erase (g := g) (i := i) (d := d) hn
    have hopts : getOptions g' i = [single] := by
      dsimp [g']
      rw [getOptions_setOptions_same]
      simpa using hsingle
    have hrest : TasksOk sol rest := tasksOk_tail hok
    have hr1 : TasksOk sol (r1Tasks i single) :=
      taskOk_of_mem_r1Tasks hsol hadm' hopts
    have hr2 : TasksOk sol (r2Tasks i d) :=
      taskOk_of_mem_r2Tasks
    have hnew : TasksOk sol (r1Tasks i single ++ r2Tasks i d ++ rest) :=
      tasksOk_append (tasksOk_append hr1 hr2) hrest
    simpa [propagate, hhas, hsingle, opts, opts', g'] using ih sol hsol hadm' hn' hnew
  · intro i d rest g opts hhas opts' g' head head₁ tail hmany ih sol hsol hadm hn hok
    have htask : TaskOk sol (Task.elim i d) := hok (Task.elim i d) (by simp)
    have hadm' : Admits sol g' := by
      dsimp [g']
      simpa [opts, opts'] using admits_setOptions_erase (g := g) (i := i) (d := d) hadm htask
    have hn' : NodupGrid g' := by
      dsimp [g']
      simpa [opts, opts'] using nodupGrid_setOptions_erase (g := g) (i := i) (d := d) hn
    have hrest : TasksOk sol rest := tasksOk_tail hok
    have hr2 : TasksOk sol (r2Tasks i d) :=
      taskOk_of_mem_r2Tasks
    have hnew : TasksOk sol (r2Tasks i d ++ rest) :=
      tasksOk_append hr2 hrest
    simpa [propagate, hhas, hmany, opts, opts', g'] using ih sol hsol hadm' hn' hnew
  · intro i d rest g opts hnot ih sol hsol hadm hn hok
    have hrest : TasksOk sol rest := tasksOk_tail hok
    rcases ih sol hsol hadm hn hrest with ⟨out, hout, hadmOut, hnOut⟩
    refine ⟨out, ?_, hadmOut, hnOut⟩
    rw [propagate.eq_4]
    simp [opts, hnot, hout]

theorem propagate_narrows :
    ∀ (ts : List Task) (g out : Grid),
      propagate ts g = some out → NarrowsGrid out g := by
  refine propagate.induct
    (motive := fun ts g => ∀ out, propagate ts g = some out → NarrowsGrid out g)
    ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_ ?_
  · intro g out hout
    simp [propagate] at hout
    subst out
    exact narrows_refl g
  · intro i d rest g opts hhas hlen removed g' ih out hout
    have houtRec :
        propagate (r1Tasks i d ++ assignR2Tasks i removed ++ rest) g' = some out := by
      rw [propagate.eq_2] at hout
      simp [opts, hhas, hlen] at hout
      simpa [removed, g', List.append_assoc] using hout
    have hset : NarrowsGrid g' g := by
      dsimp [g']
      exact narrows_setOptions_singleton_of_mem (by
        simpa [opts] using hasDigit_eq_true.mp hhas)
    exact narrows_trans (ih out houtRec) hset
  · intro i d rest g opts hhas hnotLong ih out hout
    have houtRec : propagate rest g = some out := by
      rw [propagate.eq_2] at hout
      simpa [opts, hhas, hnotLong] using hout
    exact ih out houtRec
  · intro i d rest g opts hnot out hout
    rw [propagate.eq_2] at hout
    simp [opts, hnot] at hout
  · intro u d rest g hplaces out hout
    rw [propagate.eq_3, hplaces] at hout
    simp at hout
  · intro u d rest g only hplaces ih out hout
    have houtRec : propagate (Task.assign only d :: rest) g = some out := by
      rw [propagate.eq_3, hplaces] at hout
      simpa using hout
    exact ih out houtRec
  · intro u d rest g head head₁ tail hplaces ih out hout
    have houtRec : propagate rest g = some out := by
      rw [propagate.eq_3, hplaces] at hout
      simpa using hout
    exact ih out houtRec
  · intro i d rest g opts hhas opts' hempty out hout
    rw [propagate.eq_4] at hout
    simp [opts, hhas, opts', hempty] at hout
  · intro i d rest g opts hhas opts' g' single hsingle ih out hout
    have houtRec : propagate (r1Tasks i single ++ r2Tasks i d ++ rest) g' = some out := by
      rw [propagate.eq_4] at hout
      simp [opts, hhas, opts', hsingle] at hout
      simpa [g', hsingle, List.append_assoc] using hout
    have hset : NarrowsGrid g' g := by
      dsimp [g']
      simpa [opts, opts'] using narrows_setOptions_erase g i d
    exact narrows_trans (ih out houtRec) hset
  · intro i d rest g opts hhas opts' g' head head₁ tail hmany ih out hout
    have houtRec : propagate (r2Tasks i d ++ rest) g' = some out := by
      rw [propagate.eq_4] at hout
      simpa [opts, hhas, opts', g', hmany] using hout
    have hset : NarrowsGrid g' g := by
      dsimp [g']
      simpa [opts, opts'] using narrows_setOptions_erase g i d
    exact narrows_trans (ih out houtRec) hset
  · intro i d rest g opts hnot ih out hout
    have houtRec : propagate rest g = some out := by
      rw [propagate.eq_4] at hout
      simpa [opts, hnot] using hout
    exact ih out houtRec

def assign (g : Grid) (i : Cell) (d : Digit) : Option Grid :=
  propagate [Task.assign i d] g

theorem assign_admits {g : Grid} {i : Cell} {d : Digit} {s : Sol}
    (hsol : IsSolution s) (hadm : Admits s g) (hn : NodupGrid g) (hsi : s i = d) :
    ∃ g', assign g i d = some g' ∧ Admits s g' ∧ NodupGrid g' := by
  have hok : TasksOk s [Task.assign i d] := by
    intro t ht
    simp at ht
    subst t
    exact hsi
  simpa [assign] using propagate_admits [Task.assign i d] g s hsol hadm hn hok

theorem assign_narrows {g out : Grid} {i : Cell} {d : Digit}
    (h : assign g i d = some out) : NarrowsGrid out g := by
  simpa [assign] using propagate_narrows [Task.assign i d] g out h

def eliminate (g : Grid) (i : Cell) (d : Digit) : Option Grid :=
  propagate [Task.elim i d] g

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

def foldOption {α β : Type} (xs : List α) (init : β)
    (f : β → α → Option β) : Option β :=
  match xs with
  | [] => some init
  | x :: rest =>
      match f init x with
      | none => none
      | some next => foldOption rest next f

def applyGivens (nums : List Nat) : Option Grid :=
  if nums.length = 81 then
    foldOption (cells.zip nums) initialGrid (fun acc pair =>
      let i := pair.1
      let n := pair.2
      if n = 0 then
        some acc
      else
        match natToDigit? n with
        | none => none
        | some d => assign acc i d)
  else
    none

def isSpace (c : Char) : Bool :=
  c == ' ' || c == '\n' || c == '\t' || c == '\r'

def digitChar? (c : Char) : Option Nat :=
  if c == '0' then some 0
  else if c == '1' then some 1
  else if c == '2' then some 2
  else if c == '3' then some 3
  else if c == '4' then some 4
  else if c == '5' then some 5
  else if c == '6' then some 6
  else if c == '7' then some 7
  else if c == '8' then some 8
  else if c == '9' then some 9
  else none

def parseChars (chars : List Char) (acc : List Nat) (cur : Nat)
    (inToken : Bool) : Option (List Nat) :=
  match chars with
  | [] => some (if inToken then acc ++ [cur] else acc)
  | c :: rest =>
      if isSpace c then
        parseChars rest (if inToken then acc ++ [cur] else acc) 0 false
      else
        match digitChar? c with
        | none => none
        | some d => parseChars rest acc (cur * 10 + d) true

def parseNumbers (s : String) : Option (List Nat) :=
  parseChars s.toList [] 0 false

def solveText (s : String) : Option Grid := do
  let nums ← parseNumbers s
  let grid ← applyGivens nums
  searchGrid grid

def cellString (g : Grid) (i : Cell) : String :=
  match getOptions g i with
  | d :: _ => toString (digitToNat d)
  | [] => "0"

def cellOfNat (n : Nat) : Cell :=
  ⟨n % 81, Nat.mod_lt _ (by decide : 0 < 81)⟩

def gridToString (g : Grid) : String :=
  String.join <|
    (List.range 9).map (fun r =>
      String.join ((List.range 9).map (fun c =>
        cellString g (cellOfNat (r * 9 + c)) ++ " ")) ++ "\n")

def main : IO UInt32 := do
  let input ← (← IO.getStdin).readToEnd
  match solveText input with
  | some grid => IO.print (gridToString grid)
  | none => IO.println "false"
  return 0

end Sudoku.NorvigWF
