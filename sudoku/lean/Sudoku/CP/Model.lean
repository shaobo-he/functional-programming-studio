import Std

set_option autoImplicit false
set_option maxRecDepth 4096
set_option maxHeartbeats 1000000
set_option linter.unusedVariables false

namespace Sudoku.CP

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

end Sudoku.CP
