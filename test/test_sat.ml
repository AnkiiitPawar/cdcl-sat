open Sat

let failures = ref 0

let check name ok =
  Printf.printf "%s %s\n%!" (if ok then "PASS" else "FAIL") name;
  if not ok then incr failures

let is_sat = function Solver.Sat _ -> true | Solver.Unsat -> false

let sat_and_valid nvars clauses =
  match Solver.solve nvars clauses with Solver.Sat m -> Solver.satisfies m clauses | Solver.Unsat -> false

(* Exhaustive search over all 2^n assignments: the ground truth. *)
let brute_force nvars clauses =
  let rec go mask =
    if mask = 1 lsl nvars then false
    else
      let model = Array.init (nvars + 1) (fun d -> d > 0 && (mask lsr (d - 1)) land 1 = 1) in
      Solver.satisfies model clauses || go (mask + 1)
  in
  go 0

let () =
  check "empty formula is satisfiable" (is_sat (Solver.solve 0 []));
  check "x and not x is unsatisfiable" (not (is_sat (Solver.solve 1 [ [ 1 ]; [ -1 ] ])));
  check "the empty clause is unsatisfiable" (not (is_sat (Solver.solve 2 [ [ 1; 2 ]; [] ])));
  check "a small chain of implications is solved" (sat_and_valid 3 [ [ 1; 2 ]; [ -1; 2 ]; [ -2; 3 ] ]);
  check "tautologies and duplicate literals are tolerated" (sat_and_valid 2 [ [ 1; -1 ]; [ 2; 2; 2 ] ])

let () =
  (* Differential test: 600 random formulas around the satisfiability threshold. *)
  let agree = ref true and sat_count = ref 0 in
  for seed = 1 to 600 do
    let n = 8 + (seed mod 7) in
    let m = int_of_float (float n *. (3.0 +. (float (seed mod 25) /. 10.0))) in
    let nvars, clauses = Encode.random_3sat ~nvars:n ~nclauses:m ~seed in
    let expected = brute_force nvars clauses in
    if expected then incr sat_count;
    match Solver.solve nvars clauses with
    | Solver.Sat model -> if not (expected && Solver.satisfies model clauses) then agree := false
    | Solver.Unsat -> if expected then agree := false
  done;
  check (Printf.sprintf "agrees with brute force on 600 random formulas (%d SAT)" !sat_count)
    (!agree && !sat_count > 100 && !sat_count < 500)

let () =
  let n, cl = Encode.pigeonhole ~pigeons:8 ~holes:7 in
  check "8 pigeons do not fit in 7 holes" (not (is_sat (Solver.solve n cl)));
  let n, cl = Encode.pigeonhole ~pigeons:7 ~holes:7 in
  check "7 pigeons fit in 7 holes" (sat_and_valid n cl)

let () =
  let n, cl = Encode.n_queens 8 in
  check "8 queens has a valid placement" (sat_and_valid n cl);
  List.iter
    (fun k ->
      let n, cl = Encode.n_queens k in
      check (Printf.sprintf "%d queens is impossible" k) (not (is_sat (Solver.solve n cl))))
    [ 2; 3 ]

let () =
  (* A valid solved grid from a shifting pattern, with 50 cells blanked out. *)
  let solved = Array.init 9 (fun r -> Array.init 9 (fun c -> ((((r * 3) + (r / 3) + c) mod 9) + 1))) in
  let puzzle = Array.map Array.copy solved in
  let st = Random.State.make [| 7 |] in
  let blanked = ref 0 in
  while !blanked < 50 do
    let r = Random.State.int st 9 and c = Random.State.int st 9 in
    if puzzle.(r).(c) <> 0 then (puzzle.(r).(c) <- 0; incr blanked)
  done;
  let n, cl = Encode.sudoku puzzle in
  match Solver.solve n cl with
  | Solver.Unsat -> check "sudoku is solved" false
  | Solver.Sat model ->
      let grid = Encode.decode_sudoku model in
      let digits_ok cells = List.sort compare cells = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
      let idx = List.init 9 Fun.id in
      let rows = List.for_all (fun r -> digits_ok (List.map (fun c -> grid.(r).(c)) idx)) idx in
      let cols = List.for_all (fun c -> digits_ok (List.map (fun r -> grid.(r).(c)) idx)) idx in
      let boxes =
        List.for_all
          (fun b -> digits_ok (List.map (fun k -> grid.((3 * (b / 3)) + (k / 3)).((3 * (b mod 3)) + (k mod 3))) idx))
          idx
      in
      let givens = List.for_all (fun r -> List.for_all (fun c -> puzzle.(r).(c) = 0 || puzzle.(r).(c) = grid.(r).(c)) idx) idx in
      check "sudoku is solved: rows, columns, boxes and givens all hold" (rows && cols && boxes && givens)

let () =
  let text = "c a comment\np cnf 3 2\n1 -3\n 0 2 3 -1 0\n%\n0\n" in
  let n, clauses = Dimacs.parse text in
  check "DIMACS parser handles comments, split clauses and the % terminator"
    (n = 3 && clauses = [ [ 1; -3 ]; [ 2; 3; -1 ] ])

let () =
  (* A hard-ish random instance at the 4.26 clause/variable ratio. *)
  let n, cl = Encode.random_3sat ~nvars:200 ~nclauses:852 ~seed:2026 in
  let result, stats = Solver.solve_with_stats n cl in
  let consistent = match result with Solver.Sat m -> Solver.satisfies m cl | Solver.Unsat -> true in
  check (Printf.sprintf "200-variable random 3-SAT at the threshold (%d conflicts)" stats.conflicts) consistent

let () =
  if !failures > 0 then (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
  else print_endline "all tests passed"
