(* DIMACS CNF: "c" comment lines, a "p cnf VARS CLAUSES" header, then clauses as
   integers terminated by 0, possibly spanning lines. A "%" line (used by the SATLIB
   benchmark files) ends the input. *)

exception Error of string

let parse (text : string) : int * int list list =
  let nvars = ref (-1) in
  let clauses = ref [] and current = ref [] in
  let lines = String.split_on_char '\n' text in
  let rec go = function
    | [] -> ()
    | line :: rest ->
        let line = String.trim line in
        if line = "" || line.[0] = 'c' then go rest
        else if line.[0] = '%' then ()
        else if line.[0] = 'p' then begin
          (match List.filter (( <> ) "") (String.split_on_char ' ' line) with
          | [ "p"; "cnf"; v; _ ] -> nvars := int_of_string v
          | _ -> raise (Error ("bad header: " ^ line)));
          go rest
        end
        else begin
          List.iter
            (fun tok ->
              if tok <> "" then
                match int_of_string_opt tok with
                | Some 0 ->
                    clauses := List.rev !current :: !clauses;
                    current := []
                | Some d -> current := d :: !current
                | None -> raise (Error ("not an integer: " ^ tok)))
            (String.split_on_char ' ' (String.map (fun c -> if c = '\t' then ' ' else c) line));
          go rest
        end
  in
  go lines;
  if !current <> [] then clauses := List.rev !current :: !clauses;
  if !nvars < 0 then raise (Error "missing 'p cnf' header");
  (!nvars, List.rev !clauses)

let print_result ppf (result : Solver.result) =
  match result with
  | Solver.Unsat -> Format.fprintf ppf "s UNSATISFIABLE@."
  | Solver.Sat model ->
      Format.fprintf ppf "s SATISFIABLE@.v";
      Array.iteri (fun d b -> if d > 0 then Format.fprintf ppf " %d" (if b then d else -d)) model;
      Format.fprintf ppf " 0@."
