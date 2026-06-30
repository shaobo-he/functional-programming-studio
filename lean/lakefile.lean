import Lake
open Lake DSL

package functionalProgrammingStudio where

@[default_target]
lean_lib Sudoku where

@[default_target]
lean_exe sudokuSolve where
  root := `Main
