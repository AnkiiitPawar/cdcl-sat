(* Growable array (OCaml 4.14 has no Dynarray). *)

type 'a t = { mutable data : 'a array; mutable size : int; dummy : 'a }

let create ?(capacity = 8) dummy = { data = Array.make (max 1 capacity) dummy; size = 0; dummy }
let size v = v.size
let get v i = v.data.(i)
let set v i x = v.data.(i) <- x
let last v = v.data.(v.size - 1)

let push v x =
  if v.size = Array.length v.data then begin
    let bigger = Array.make (2 * v.size) v.dummy in
    Array.blit v.data 0 bigger 0 v.size;
    v.data <- bigger
  end;
  v.data.(v.size) <- x;
  v.size <- v.size + 1

(* Drop elements from index n onwards. *)
let shrink v n =
  for i = n to v.size - 1 do
    v.data.(i) <- v.dummy
  done;
  v.size <- n

let to_array v = Array.sub v.data 0 v.size
