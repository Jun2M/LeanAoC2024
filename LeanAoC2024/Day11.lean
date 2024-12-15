import Mathlib.Data.Nat.Digits
import Mathlib.Data.Finsupp.Multiset
import Mathlib.Data.Matrix.Basic

def input : IO String := IO.FS.readFile "LeanAoC2024/inputs/Day11.txt"

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

def L := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 24, 26, 28, 32, 36, 40, 42, 45, 48, 56, 57, 58, 60, 67, 72, 75, 77, 80,
  84, 86, 91, 94, 96, 544, 2024, 2048, 2457, 2608, 2867, 2880, 3277, 3686, 4048, 4256, 4510, 4840,
  6032, 6072, 7558, 8096, 9184, 9456, 10120, 12144, 14168, 16192, 18216, 22285, 37344, 1101056,
  20482880, 24579456, 28676032, 32772608, 36869184, 45104840, 75584256, 2228537344]

def L' := [[1], [2024], [4048], [6072], [8096], [10120], [12144], [14168], [16192], [18216], [1, 0], [2, 0], [2, 4], [2, 6],
  [2, 8], [3, 2], [3, 6], [4, 0], [4, 2], [4, 5], [4, 8], [5, 6], [5, 7], [5, 8], [6, 0], [6, 7], [7, 2], [7, 5],
  [7, 7], [8, 0], [8, 4], [8, 6], [9, 1], [9, 4], [9, 6], [1101056], [20, 24], [20, 48], [24, 57], [26, 8], [28, 67],
  [28, 80], [32, 77], [36, 86], [40, 48], [42, 56], [45, 10], [48, 40], [60, 32], [60, 72], [75, 58], [80, 96],
  [91, 84], [94, 56], [20482880], [24579456], [28676032], [32772608], [36869184], [45104840], [75584256], [2228537344],
  [2048, 2880], [2457, 9456], [2867, 6032], [3277, 2608], [3686, 9184], [4510, 4840], [7558, 4256], [22285, 37344]]

-- set_option pp.deepTerms true
-- #eval L|>.map (blink [·])
-- #eval ((List.range 7).map (fun x => blink^[x] [720]) |>.flatten) ++ L |>.mergeSort (· < ·) |>.dedup

def L0 := L.map (fun _ => 0)
def M := L'.map (fun s => L.map (s.count · ))

def linearOp (v : List ℕ) : List ℕ :=
  M.zip v
  |>.map (fun (row, x) => row.map (· * x))
  |>.foldl (·.zipWith (·+·) ·) (List.replicate L.length 0)

def blinkCount (sl : List Nat × List Nat) : (List Nat) × (List Nat) := Id.run $ do
  let (s, l) := sl
  let l' := blink l
  let s' := (linearOp s).zipWith (· + ·) (L.map (l'.count ·))
  let l'l := l'.removeAll L
  return (s', l'l)

#eval (fun s =>
let s' := s.splitOn " "
|>.map (·.toInt!.natAbs)
let (a1, b1) := blinkCount^[40] (L0, s')
let (b', f) := b1.dedup.map (fun x => (x, b1.count x)) |>.unzip
b'.map (fun x => (blinkCount^[35] (L0, [x])))
  |>.zip f
  |>.map (fun (((a1, b1) : (List Nat) × (List Nat)), f) => (a1.sum + b1.length) * f)
  |>.sum
+ (linearOp^[35] a1).sum
) <$> input
