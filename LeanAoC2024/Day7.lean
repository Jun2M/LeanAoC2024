import Mathlib.Data.Nat.Digits

def input : IO String := IO.FS.readFile "LeanAoC2024/inputs/Day7.txt"

def f : List Int → List Int
| [] => []
| [x] => [x]
| x :: xs => ((f xs).map (· + x)) ++ ((f xs).map (· * x))

#eval (·.splitOn "\n"
|>.map (·.splitOn ": ")
|>.map (fun L => (L[0]!.toInt!, L[1]!.splitOn " " |>.map (·.toInt!)))
|>.map (fun ⟨x, xs⟩ => if x ∈ f xs.reverse then x else 0)
|>.sum
) <$> input

def g : List Int → List Int
| [] => []
| [x] => [x]
| x :: xs => ((g xs).map (· + x)) ++ ((g xs).map (· * x)) ++ ((g xs).map (
  let n := (10:ℕ).digits x.natAbs |>.length
  · * 10^n + x))

#eval (·.splitOn "\n"
|>.map (·.splitOn ": ")
|>.map (fun L => (L[0]!.toInt!, L[1]!.splitOn " " |>.map (·.toInt!)))
|>.map (fun ⟨x, xs⟩ => if x ∈ g xs.reverse then x else 0)
|>.sum
) <$> input
