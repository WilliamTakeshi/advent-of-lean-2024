def remove_at {α : Type} (lst : List α) (i : Nat) : List α :=
  match lst with
  | [] => []
  | h :: t =>
    if i = 0 then t
    else h :: remove_at t (i - 1)

def parse (s: String) : List Nat :=
  s.split (· = ' ')|>.map (·.toNat?)|>.filterMap id

def differences (l : List Nat) : List Int :=
  match l with
  | [] => []  -- Handle empty list
  | _ :: [] => [] -- Handle single-element list
  | _x :: _xs =>
      let diffs := (l.zip l.tail!).map (fun (a, b) => a - b)
      diffs

def countTrues (lst : Array (Bool × Bool)) : Nat :=
  lst.foldl (fun acc (l, r) => acc + if l||r then 1 else 0) 0

def safe_report? (differences: List Int) : Bool :=
  differences.all (fun x => (x <= 3 && x >= 1)) || differences.all (fun x => (x >= -3 && x <= -1))

def remove_one_term (lst : List Nat) : List (List Nat) :=
  lst.enum.map (λ ⟨i, _⟩ => remove_at lst i)

def can_be_safe_by_removing_one (row: List Nat) : Bool :=
  (remove_one_term row).any (λ one_removed =>
    safe_report? (differences one_removed))

def main : IO Unit :=
do
  let input ← IO.FS.lines "input.txt"
  let parsed_input := input.map parse
  let safe := parsed_input.map (λ x => safe_report? (differences x))
  let can_be_safe := parsed_input.map (λ x => can_be_safe_by_removing_one x)
  let res := countTrues (Array.zip safe can_be_safe)
  IO.print s!"{res}"
