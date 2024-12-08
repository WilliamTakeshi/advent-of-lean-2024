def parse (s: String): Option (Nat × Nat) :=
  match s.split (· = ' ') |>.filter (fun part => part != "") with
  | [part1, part2] =>
    match part1.toNat?, part2.toNat? with
    | some n1, some n2 => some (n1, n2)
    | _, _ => none
  | _ => none

def absDiff (a b : Nat) : Nat :=
  if a ≥ b then a - b else b - a

def main : IO Unit :=
do
  let input ← IO.FS.lines "test.txt"
  let (lefts, rights) := (input.map parse).foldl (fun (lefts, rights) opt =>
    match opt with
    | some (n1, n2) => (lefts.push n1, rights.push n2)
    | none => (lefts, rights)
  ) (#[], #[])
  let sorted_lefts := lefts.qsort (· < ·)
  let sorted_rights := rights.qsort (· < ·)
  let result := (Array.zip sorted_lefts sorted_rights).foldl (fun acc p => acc + absDiff p.fst p.snd) 0
  IO.print s!"{result}"
