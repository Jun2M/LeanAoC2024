import Mathlib

def input : IO String := IO.FS.readFile "LeanAoC2024/inputs/Day2.txt"
def parse := (·.splitOn "\n" |>.map (·.splitOn " " |>.map String.toInt!)) <$> input

def a : List Int → Bool :=
  λ s => (s.Sorted (· < ·) ∨ s.Sorted (· > ·)) ∧ s.Chain' (fun a b => (a - b).natAbs ≤ 3)

#eval (fun L =>
L.countP a
) <$> parse

#eval (fun L =>
L.countP (λ s => s.sublistsLen (s.length - 1)
|>.any a)
) <$> parse
