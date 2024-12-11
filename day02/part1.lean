def parse (s: String) : List Nat :=
  s.split (· = ' ')|>.map (·.toNat?)|>.filterMap id

def differences (l : List Nat) : List Int :=
  match l with
  | [] => []  -- Handle empty list
  | _ :: [] => [] -- Handle single-element list
  | _x :: _xs =>
      let diffs := (l.zip l.tail!).map (fun (a, b) => a - b)
      diffs

def countTrues (lst : Array Bool) : Nat :=
  lst.foldl (fun acc b => acc + if b then 1 else 0) 0

def main : IO Unit :=
do
  let input ← IO.FS.lines "test.txt"
  let safe := input.map parse |>.map differences|>.map (fun r => r.all (fun x => (x <= 3 && x >= 1)) || r.all (fun x => (x >= -3 && x <= -1)))
  let res := countTrues safe
  IO.print s!"{res}"
