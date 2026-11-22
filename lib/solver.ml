(* Conflict-driven clause learning (CDCL) SAT solver.

   Literals are ints: variable v (0-based) is 2v when positive and 2v + 1 when
   negated, so negation is [l lxor 1] and the variable is [l lsr 1].

   Main loop:
     propagate unit clauses (two watched literals per clause)
     on conflict: learn a clause by 1-UIP analysis, jump back, assert it
     otherwise:   restart if the Luby budget is spent, else decide on the
                  most active unassigned variable (VSIDS) with its saved phase *)

type stats = { mutable decisions : int; mutable conflicts : int; mutable propagations : int; mutable restarts : int }
type result = Sat of bool array (* indexed by DIMACS variable 1..n *) | Unsat

type t = {
  nvars : int;
  clauses : int array Vec.t;
  watches : int Vec.t array;  (* per literal: ids of clauses watching it *)
  assigns : int array;  (* per variable: 1 true, -1 false, 0 unassigned *)
  level : int array;
  reason : int array;  (* clause that forced the variable, -1 for decisions *)
  trail : int Vec.t;  (* assigned literals in order *)
  trail_lim : int Vec.t;  (* trail size at the start of each decision level *)
  mutable qhead : int;  (* next trail entry to propagate *)
  activity : float array;
  mutable var_inc : float;
  order : Heap.t;
  phase : bool array;  (* last polarity each variable had *)
  seen : bool array;
  mutable ok : bool;  (* false once a conflict is found at level 0 *)
  stats : stats;
}

let var l = l lsr 1
let neg l = l lxor 1
let lit_of_dimacs d = if d > 0 then 2 * (d - 1) else (2 * (-d - 1)) + 1

let create nvars =
  let activity = Array.make nvars 0.0 in
  let s =
    {
      nvars;
      clauses = Vec.create [||];
      watches = Array.init (2 * nvars) (fun _ -> Vec.create 0);
      assigns = Array.make nvars 0;
      level = Array.make nvars 0;
      reason = Array.make nvars (-1);
      trail = Vec.create ~capacity:nvars 0;
      trail_lim = Vec.create 0;
      qhead = 0;
      activity;
      var_inc = 1.0;
      order = Heap.create nvars activity;
      phase = Array.make nvars false;
      seen = Array.make nvars false;
      ok = true;
      stats = { decisions = 0; conflicts = 0; propagations = 0; restarts = 0 };
    }
  in
  for v = 0 to nvars - 1 do
    Heap.insert s.order v
  done;
  s

let value s l =
  let a = s.assigns.(var l) in
  if l land 1 = 0 then a else -a

let decision_level s = Vec.size s.trail_lim

let enqueue s l reason =
  let v = var l in
  s.assigns.(v) <- (if l land 1 = 0 then 1 else -1);
  s.level.(v) <- decision_level s;
  s.reason.(v) <- reason;
  Vec.push s.trail l

let attach s lits =
  let id = Vec.size s.clauses in
  Vec.push s.clauses lits;
  Vec.push s.watches.(lits.(0)) id;
  Vec.push s.watches.(lits.(1)) id;
  id

(* Unit propagation with two watched literals. Returns a conflicting clause id or -1.
   A clause is only visited when one of its two watched literals becomes false. *)
let propagate s =
  let conflict = ref (-1) in
  while !conflict < 0 && s.qhead < Vec.size s.trail do
    let p = Vec.get s.trail s.qhead in
    s.qhead <- s.qhead + 1;
    s.stats.propagations <- s.stats.propagations + 1;
    let false_lit = neg p in
    let ws = s.watches.(false_lit) in
    let n = Vec.size ws in
    let i = ref 0 and j = ref 0 in
    while !i < n do
      let id = Vec.get ws !i in
      incr i;
      if !conflict >= 0 then begin
        Vec.set ws !j id;
        incr j
      end
      else begin
        let c = Vec.get s.clauses id in
        if c.(0) = false_lit then begin
          c.(0) <- c.(1);
          c.(1) <- false_lit
        end;
        if value s c.(0) = 1 then begin
          (* satisfied by the other watch: keep watching *)
          Vec.set ws !j id;
          incr j
        end
        else begin
          let len = Array.length c in
          let k = ref 2 in
          while !k < len && value s c.(!k) = -1 do
            incr k
          done;
          if !k < len then begin
            (* found a non-false literal: watch it instead *)
            c.(1) <- c.(!k);
            c.(!k) <- false_lit;
            Vec.push s.watches.(c.(1)) id
          end
          else begin
            Vec.set ws !j id;
            incr j;
            if value s c.(0) = -1 then conflict := id else enqueue s c.(0) id
          end
        end
      end
    done;
    Vec.shrink ws !j
  done;
  !conflict

let bump s v =
  s.activity.(v) <- s.activity.(v) +. s.var_inc;
  if s.activity.(v) > 1e100 then begin
    for u = 0 to s.nvars - 1 do
      s.activity.(u) <- s.activity.(u) *. 1e-100
    done;
    s.var_inc <- s.var_inc *. 1e-100
  end;
  Heap.increased s.order v

(* First-UIP conflict analysis. Resolve the conflict clause with the reasons of
   current-level literals, walking the trail backwards, until only one literal of
   the current level remains: the unique implication point. The learnt clause
   asserts its negation after backjumping to the second-highest level involved. *)
let analyze s conflict =
  let learnt = Vec.create 0 in
  Vec.push learnt 0 (* slot for the asserting literal *);
  let pending = ref 0 and p = ref (-1) and index = ref (Vec.size s.trail - 1) in
  let clause = ref conflict in
  let continue = ref true in
  while !continue do
    let c = Vec.get s.clauses !clause in
    for k = (if !p = -1 then 0 else 1) to Array.length c - 1 do
      let q = c.(k) in
      let v = var q in
      if (not s.seen.(v)) && s.level.(v) > 0 then begin
        s.seen.(v) <- true;
        bump s v;
        if s.level.(v) >= decision_level s then incr pending else Vec.push learnt q
      end
    done;
    while not s.seen.(var (Vec.get s.trail !index)) do
      decr index
    done;
    p := Vec.get s.trail !index;
    decr index;
    clause := s.reason.(var !p);
    s.seen.(var !p) <- false;
    decr pending;
    if !pending <= 0 then continue := false
  done;
  Vec.set learnt 0 (neg !p);
  for k = 1 to Vec.size learnt - 1 do
    s.seen.(var (Vec.get learnt k)) <- false
  done;
  let backjump =
    if Vec.size learnt = 1 then 0
    else begin
      let best = ref 1 in
      for k = 2 to Vec.size learnt - 1 do
        if s.level.(var (Vec.get learnt k)) > s.level.(var (Vec.get learnt !best)) then best := k
      done;
      let tmp = Vec.get learnt 1 in
      Vec.set learnt 1 (Vec.get learnt !best);
      Vec.set learnt !best tmp;
      s.level.(var (Vec.get learnt 1))
    end
  in
  (Vec.to_array learnt, backjump)

let cancel_until s lvl =
  if decision_level s > lvl then begin
    let lim = Vec.get s.trail_lim lvl in
    for k = Vec.size s.trail - 1 downto lim do
      let l = Vec.get s.trail k in
      let v = var l in
      s.phase.(v) <- l land 1 = 0;
      s.assigns.(v) <- 0;
      s.reason.(v) <- -1;
      Heap.insert s.order v
    done;
    Vec.shrink s.trail lim;
    Vec.shrink s.trail_lim lvl;
    s.qhead <- lim
  end

let rec pick_branch s =
  if Heap.is_empty s.order then None
  else
    let v = Heap.pop s.order in
    if s.assigns.(v) <> 0 then pick_branch s else Some (if s.phase.(v) then 2 * v else (2 * v) + 1)

(* Clauses are added at decision level 0, before search. *)
let add_clause s dimacs =
  if s.ok then begin
    let lits = List.sort_uniq compare (List.map lit_of_dimacs dimacs) in
    let tautology = List.exists (fun l -> List.mem (neg l) lits) lits in
    let satisfied = List.exists (fun l -> value s l = 1) lits in
    if not (tautology || satisfied) then
      match List.filter (fun l -> value s l = 0) lits with
      | [] -> s.ok <- false
      | [ l ] ->
          enqueue s l (-1);
          if propagate s >= 0 then s.ok <- false
      | lits -> ignore (attach s (Array.of_list lits))
  end

(* Luby sequence 1 1 2 1 1 2 4 1 1 2 1 1 2 4 8 ...: restart intervals that are
   within a log factor of optimal when nothing is known about the instance. *)
let rec luby i =
  let rec size k = if (1 lsl k) - 1 >= i then k else size (k + 1) in
  let k = size 1 in
  if (1 lsl k) - 1 = i then 1 lsl (k - 1) else luby (i - (1 lsl (k - 1)) + 1)

let model s = Array.init (s.nvars + 1) (fun d -> d > 0 && s.assigns.(d - 1) = 1)

let search s =
  let result = ref None and restart = ref 0 in
  if not s.ok then result := Some Unsat;
  while !result = None do
    incr restart;
    let budget = 100 * luby !restart and conflicts = ref 0 and stop = ref false in
    while not !stop do
      let conflict = propagate s in
      if conflict >= 0 then begin
        s.stats.conflicts <- s.stats.conflicts + 1;
        incr conflicts;
        if decision_level s = 0 then begin
          result := Some Unsat;
          stop := true
        end
        else begin
          let learnt, backjump = analyze s conflict in
          cancel_until s backjump;
          if Array.length learnt = 1 then enqueue s learnt.(0) (-1)
          else enqueue s learnt.(0) (attach s learnt);
          s.var_inc <- s.var_inc /. 0.95
        end
      end
      else if !conflicts >= budget then begin
        s.stats.restarts <- s.stats.restarts + 1;
        cancel_until s 0;
        stop := true
      end
      else
        match pick_branch s with
        | None ->
            result := Some (Sat (model s));
            stop := true
        | Some l ->
            s.stats.decisions <- s.stats.decisions + 1;
            Vec.push s.trail_lim (Vec.size s.trail);
            enqueue s l (-1)
    done
  done;
  match !result with Some r -> r | None -> assert false

let solve_with_stats nvars clauses =
  let s = create nvars in
  List.iter (add_clause s) clauses;
  let r = search s in
  (r, s.stats)

let solve nvars clauses = fst (solve_with_stats nvars clauses)

(* Every clause has at least one true literal. *)
let satisfies model clauses =
  List.for_all (List.exists (fun d -> if d > 0 then model.(d) else not model.(-d))) clauses
