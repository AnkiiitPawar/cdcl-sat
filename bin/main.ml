(* cdcl-sat FILE.cnf           solve a DIMACS file ("-" reads standard input)
   cdcl-sat --random N M SEED  solve a random 3-SAT formula with N variables and M clauses

   SAT-competition style output ("c", "s", "v" lines); exit code 10 for SAT, 20 for UNSAT. *)

open Sat

let read_all ic =
  let buf = Buffer.create 65536 and chunk = Bytes.create 65536 in
  let rec go () =
    let k = input ic chunk 0 (Bytes.length chunk) in
    if k > 0 then (
      Buffer.add_subbytes buf chunk 0 k;
      go ())
  in
  go ();
  Buffer.contents buf

let usage () =
  prerr_endline "usage: cdcl-sat FILE.cnf | cdcl-sat - | cdcl-sat --random N M SEED";
  exit 2

let () =
  let nvars, clauses =
    match Array.to_list Sys.argv with
    | [ _; "--random"; n; m; seed ] -> (
        match (int_of_string_opt n, int_of_string_opt m, int_of_string_opt seed) with
        | Some n, Some m, Some seed -> Encode.random_3sat ~nvars:n ~nclauses:m ~seed
        | _ -> usage ())
    | [ _; "-" ] -> Dimacs.parse (read_all stdin)
    | [ _; path ] ->
        let ic = open_in_bin path in
        let text = read_all ic in
        close_in ic;
        Dimacs.parse text
    | _ -> usage ()
  in
  let t0 = Sys.time () in
  let result, stats = Solver.solve_with_stats nvars clauses in
  let elapsed = Sys.time () -. t0 in
  Printf.printf "c %d variables, %d clauses\n" nvars (List.length clauses);
  Printf.printf "c decisions %d  conflicts %d  propagations %d  restarts %d  time %.3fs\n" stats.decisions
    stats.conflicts stats.propagations stats.restarts elapsed;
  (match result with
  | Solver.Sat model -> assert (Solver.satisfies model clauses)
  | Solver.Unsat -> ());
  Dimacs.print_result Format.std_formatter result;
  exit (match result with Solver.Sat _ -> 10 | Solver.Unsat -> 20)
