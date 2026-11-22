(* Classic problems encoded as CNF (DIMACS integer literals). *)

let pairs xs =
  let rec go acc = function [] -> acc | x :: rest -> go (List.map (fun y -> (x, y)) rest @ acc) rest in
  go [] xs

let at_most_one vars = List.map (fun (a, b) -> [ -a; -b ]) (pairs vars)
let range a b = List.init (max 0 (b - a)) (fun i -> a + i)

(* p pigeons into h holes, at most one per hole: unsatisfiable when p > h, and
   famously exponential for resolution-based solvers. Variable (i, j) = pigeon i in hole j. *)
let pigeonhole ~pigeons ~holes =
  let v i j = (i * holes) + j + 1 in
  let every_pigeon_somewhere = List.map (fun i -> List.map (v i) (range 0 holes)) (range 0 pigeons) in
  let no_sharing = List.concat_map (fun j -> at_most_one (List.map (fun i -> v i j) (range 0 pigeons))) (range 0 holes) in
  (pigeons * holes, every_pigeon_somewhere @ no_sharing)

(* n queens on an n x n board: one per row, at most one per column and diagonal. *)
let queen n r c = (r * n) + c + 1

let n_queens n =
  let cells = List.concat_map (fun r -> List.map (fun c -> (r, c)) (range 0 n)) (range 0 n) in
  let rows = List.map (fun r -> List.map (queen n r) (range 0 n)) (range 0 n) in
  let attack = List.filter (fun ((r1, c1), (r2, c2)) -> r1 = r2 || c1 = c2 || abs (r1 - r2) = abs (c1 - c2)) (pairs cells) in
  let no_attacks = List.map (fun ((r1, c1), (r2, c2)) -> [ -queen n r1 c1; -queen n r2 c2 ]) attack in
  (n * n, rows @ no_attacks)

(* Sudoku: variable (r, c, d) means cell (r, c) holds digit d + 1. 0 marks a blank. *)
let cell r c d = (81 * r) + (9 * c) + d + 1

let sudoku (grid : int array array) =
  let digits = range 0 9 and idx = range 0 9 in
  let each_cell = List.concat_map (fun r -> List.map (fun c -> List.map (cell r c) digits) idx) idx in
  let one_digit = List.concat_map (fun r -> List.concat_map (fun c -> at_most_one (List.map (cell r c) digits)) idx) idx in
  let unit_cells =
    List.map (fun r -> List.map (fun c -> (r, c)) idx) idx
    @ List.map (fun c -> List.map (fun r -> (r, c)) idx) idx
    @ List.map (fun b -> List.map (fun k -> ((3 * (b / 3)) + (k / 3), (3 * (b mod 3)) + (k mod 3))) idx) idx
  in
  let each_digit_in_each_unit =
    List.concat_map (fun cells -> List.map (fun d -> List.map (fun (r, c) -> cell r c d) cells) digits) unit_cells
  in
  let givens =
    List.concat_map
      (fun r -> List.filter_map (fun c -> if grid.(r).(c) > 0 then Some [ cell r c (grid.(r).(c) - 1) ] else None) idx)
      idx
  in
  (729, each_cell @ one_digit @ each_digit_in_each_unit @ givens)

let decode_sudoku (model : bool array) =
  Array.init 9 (fun r -> Array.init 9 (fun c -> 1 + List.find (fun d -> model.(cell r c d)) (range 0 9)))

(* Uniform random 3-SAT: each clause has 3 distinct variables with random signs. *)
let random_3sat ~nvars ~nclauses ~seed =
  let st = Random.State.make [| seed |] in
  let clause () =
    let rec pick acc =
      if List.length acc = 3 then acc
      else
        let v = 1 + Random.State.int st nvars in
        if List.mem v acc then pick acc else pick (v :: acc)
    in
    List.map (fun v -> if Random.State.bool st then v else -v) (pick [])
  in
  (nvars, List.init nclauses (fun _ -> clause ()))
