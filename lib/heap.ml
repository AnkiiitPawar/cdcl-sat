(* Binary max-heap of variables ordered by activity, with O(1) membership and
   O(log n) increase-key, as needed by VSIDS branching. *)

type t = {
  mutable heap : int array;
  mutable size : int;
  index : int array;  (* position of each variable in [heap], or -1 if absent *)
  activity : float array;  (* shared with the solver *)
}

let create n activity = { heap = Array.make (max 1 n) 0; size = 0; index = Array.make n (-1); activity }
let mem h v = h.index.(v) >= 0
let is_empty h = h.size = 0
let higher h a b = h.activity.(a) > h.activity.(b)

let swap h i j =
  let a = h.heap.(i) and b = h.heap.(j) in
  h.heap.(i) <- b;
  h.heap.(j) <- a;
  h.index.(b) <- i;
  h.index.(a) <- j

let rec up h i =
  if i > 0 then
    let parent = (i - 1) / 2 in
    if higher h h.heap.(i) h.heap.(parent) then begin
      swap h i parent;
      up h parent
    end

let rec down h i =
  let l = (2 * i) + 1 and r = (2 * i) + 2 in
  let best = ref i in
  if l < h.size && higher h h.heap.(l) h.heap.(!best) then best := l;
  if r < h.size && higher h h.heap.(r) h.heap.(!best) then best := r;
  if !best <> i then begin
    swap h i !best;
    down h !best
  end

let insert h v =
  if not (mem h v) then begin
    h.heap.(h.size) <- v;
    h.index.(v) <- h.size;
    h.size <- h.size + 1;
    up h (h.size - 1)
  end

(* Call after the activity of [v] has increased. *)
let increased h v = if mem h v then up h h.index.(v)

let pop h =
  let top = h.heap.(0) in
  h.size <- h.size - 1;
  h.index.(top) <- -1;
  if h.size > 0 then begin
    let last = h.heap.(h.size) in
    h.heap.(0) <- last;
    h.index.(last) <- 0;
    down h 0
  end;
  top
