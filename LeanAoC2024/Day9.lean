def input : IO String := IO.FS.readFile "LeanAoC2024/inputs/Day9.txt"

def findIndexSum (l : List Nat) (s : Nat) : Nat := Id.run $ do
let mut i := 0
let mut sum := 0
let mut l' := l
while sum < s do
  sum := sum + l'.head!
  i := i + 1
  l' := l'.tail
return i

def List.undense (l : List (Nat × Nat)) : List Nat := Id.run $ do
let mut s := []
for (x, lenx) in l do
  s := s ++ List.replicate lenx x
return s

def List.replaceOdd (l : List Nat) (r : List Nat) : List Nat := Id.run $ do
let mut r' := r
let mut s := []
for x in l do
  if x % 2 = 1
  then s := s ++ [r'.head!]; r' := r'.tail
  else s := s ++ [x]
return s

#eval (fun s =>
let s' := s.toList.map (fun x => x.toNat - 48)
let s'' := if s'.length % 2 = 1 then s' else s'.dropLast
let n := s'.enum.filter (fun (i, _) => i % 2 = 0) |>.map (·.snd) |>.sum
let i := findIndexSum s'' n
let offset := (s''.take i).sum - n
let l' := (s''.enum.take i |>.undense).reverse
let l := l'.drop offset |>.reverse
let leftover := l'.take offset |>.reverse
let r := (s''.enum.drop i
  |>.reverse
  |>.filter (fun (i, _) => i % 2 = 0)
  |>.map (fun (i, x) => List.replicate x i)
  |>.flatten)
  ++ leftover
l.replaceOdd r
|>.map (· / 2)
|>.enum
|>.map (fun (i, x) => i * x)
|>.sum
) <$> input


-- Part two not done
