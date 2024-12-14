import Mathlib.Data.Nat.Digits
import Mathlib.Data.Finsupp.Multiset

def input : IO String := IO.FS.readFile "LeanAoC2024/inputs/Day10.txt"

def blink (s : List Nat) : List Nat := Id.run $ do
let mut s' := []
for i in s do
  if i = 0
  then s' := 1 :: s'
  else
  let d := Nat.log 10 i
  if d % 2 = 1
  then
    let a := d / 2 + 1
    s' := i / 10^a :: i % 10^a :: s'
  else s' := 2024 * i :: s'
return s'

#eval (fun s =>
(s.splitOn " "
|>.map (·.toInt!.natAbs))
|>.map (blink^[25] [·] |>.length)
|>.sum
) <$> input


-- Part two not done (75??)
