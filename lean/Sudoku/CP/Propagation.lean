import Sudoku.CP.Model

set_option autoImplicit false
set_option maxRecDepth 4096
set_option maxHeartbeats 1000000
set_option linter.unusedVariables false

namespace Sudoku.CP

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

end Sudoku.CP
