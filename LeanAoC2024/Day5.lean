def input : IO String := IO.FS.readFile "LeanAoC2024/inputs/Day5.txt"
def rc := (fun str =>
((str.splitOn "\n\n")[0]!, (str.splitOn "\n\n")[1]!)
) <$> input
def RulesCases := (fun prod =>
prod.map (fun str =>
str.splitOn "\n"
|>.map (fun str =>
  let L := str.splitOn "|"
  (L[0]!.toInt!, L[1]!.toInt!)
))
(fun str =>
str.splitOn "\n"
|>.map (·.splitOn ",")
|>.map (·.map (·.toInt!))
)) <$> rc

#eval RulesCases

def verify (rule : Int × Int) (case : List Int) : Bool :=
let i := case.indexOf rule.1
let j := case.indexOf rule.2
i ≤ j ∨ rule.1 ∉ case ∨ rule.2 ∉ case

partial def sort (rules : List (Int × Int)) (case : List Int) := Id.run do
for rule in rules do
  let i := case.indexOf rule.1
  let j := case.indexOf rule.2
  if ¬ (i ≤ j ∨ rule.1 ∉ case ∨ rule.2 ∉ case) then
  let c := case.erase rule.1
  return sort rules (c.take j ++ [rule.1] ++ c.drop j)
return case[case.length/2]!


#eval (fun rscs =>
let rules := rscs.fst
let cases := rscs.snd
cases.map (fun case =>
if rules.all (verify · case)
then case[case.length/2]!
else 0
)|>.sum
) <$> RulesCases

#eval (fun rscs =>
let rules := rscs.fst
let cases := rscs.snd
cases.filter (fun case => ¬ rules.all (verify · case))
|>.map (sort rules ·)
|>.sum
) <$> RulesCases
