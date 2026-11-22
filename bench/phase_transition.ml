(* Random 3-SAT phase transition: fraction satisfiable and median solver effort as the
   clause/variable ratio sweeps through ~4.26. Writes CSV to stdout.
   dune exec bench/phase_transition.exe > docs/phase_transition.csv *)

open Sat

let median xs =
  let a = Array.of_list xs in
  Array.sort compare a;
  a.(Array.length a / 2)

let () =
  let instances = 100 in
  print_endline "n,ratio,p_sat,median_conflicts,median_ms";
  List.iter
    (fun n ->
      for step = 0 to 15 do
        let ratio = 3.0 +. (0.2 *. float_of_int step) in
        let m = int_of_float (Float.round (ratio *. float_of_int n)) in
        let sat = ref 0 and conflicts = ref [] and times = ref [] in
        for k = 1 to instances do
          let nvars, clauses = Encode.random_3sat ~nvars:n ~nclauses:m ~seed:((n * 100_000) + (step * 1000) + k) in
          let t0 = Sys.time () in
          let result, stats = Solver.solve_with_stats nvars clauses in
          times := (1000.0 *. (Sys.time () -. t0)) :: !times;
          conflicts := stats.conflicts :: !conflicts;
          match result with
          | Solver.Sat model ->
              assert (Solver.satisfies model clauses);
              incr sat
          | Solver.Unsat -> ()
        done;
        Printf.printf "%d,%.2f,%.3f,%d,%.3f\n%!" n ratio
          (float_of_int !sat /. float_of_int instances)
          (median !conflicts) (median !times)
      done)
    [ 50; 100; 150 ]
