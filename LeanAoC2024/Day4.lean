def input : IO String := IO.FS.readFile "LeanAoC2024/inputs/Day4.txt"
def lines := (·.splitOn "\n") <$> input


#eval (fun L =>
L.map (·.splitOn "X"
|>.filter (·.startsWith "MAS")
|>.length
) |>.sum
) <$> lines

#eval (fun L =>
L.map (·.splitOn "S"
|>.filter (·.startsWith "AMX")
|>.length
) |>.sum
) <$> lines

#eval (fun L =>
L.map (·.find (· == "X".front))
) <$> lines

-- Not completed
