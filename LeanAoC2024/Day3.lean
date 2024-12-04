def input : IO (String) := IO.FS.readFile "LeanAoC2024/inputs/Day3.txt"

def a : String → Int := λ s =>
s.splitOn "mul("
|>.filter (·.front.isDigit)
|>.map (fun s => (s.takeWhile (·.isDigit) |>.toInt!, s.dropWhile (·.isDigit)))
|>.filter (fun (a, b) => b.startsWith ",")
|>.map (fun (a, b) => (a, b.drop 1))
|>.map (fun (a, b) => (a, b.takeWhile (·.isDigit) |>.toInt!, b.dropWhile (·.isDigit)))
|>.filter (fun (a, b, c) => c.startsWith ")")
|>.map (fun (a, b, c) => a * b)
|>.sum

#eval a <$> input

#eval a <$> (fun L =>
L.splitOn "do"
|>.foldl (fun (i, r) s =>
  if s.startsWith "()"
  then (1, r.append s)
  else if s.startsWith "n't()"
  then (0, r)
  else if i == 1
  then (i, r.append s)
  else (i, r)) (1, "")
|>.snd
) <$> input
