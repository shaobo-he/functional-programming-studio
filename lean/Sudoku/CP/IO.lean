import Sudoku.CP.Search

set_option autoImplicit false
set_option maxRecDepth 4096
set_option maxHeartbeats 1000000
set_option linter.unusedVariables false

namespace Sudoku.CP

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

end Sudoku.CP
