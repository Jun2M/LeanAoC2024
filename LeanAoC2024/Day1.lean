def input : IO String := IO.FS.readFile "LeanAoC2024/inputs/Day1.txt"
def parse := (·.splitOn "\n" |>.map (·.splitOn "   " |>.map String.toInt!)) <$> input

/-
2176849
-/
#eval (fun L =>
(L.map (·[0]!)).mergeSort
|>.zip ((L.map (·[1]!)).mergeSort)
|>.map (λ s => (s.1 - s.2).natAbs)
|>.sum) <$> parse

/-
23384288
-/
#eval (fun L =>
L.map (·[0]!)
|>.map (λ x => x * (L.map (·[1]!)).count x)
|>.sum) <$> parse
