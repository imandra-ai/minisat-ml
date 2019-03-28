

(* used to compute next capacity *)
let[@inline] imax x y : int =
  let mask = (y-x) lsr Sys.int_size in
  (x land mask) + (y land (lnot mask))

type 'a t = {
  mutable data: 'a array;
  mutable sz: int;
}

let[@inline] make () : _ t = {data=[| |]; sz=0}
let[@inline] make_with sz pad : _ t = assert (sz>=0); {data=Array.make sz pad; sz}

let[@inline] size self : int = self.sz
let[@inline] empty self = self.sz = 0
let[@inline] capacity self : int = Array.length self.data
let[@inline] get self i = assert (i>=0 && i<self.sz); Array.unsafe_get self.data i
let[@inline] set self i x : unit = assert (i>=0 && i<self.sz); Array.unsafe_set self.data i x
let[@inline] shrink self size = if size < self.sz then self.sz <- size
let[@inline] clear self = self.sz <- 0
let[@inline] clear_dealloc self = self.sz <- 0; self.data <- [| |]

let ensure self min_cap pad : unit =
  let cap = capacity self in
  if cap < min_cap then (
    (* grow by approx. 3/2 *)
    let add =
      imax ((min_cap - cap + 1) land (lnot 1)) (((cap lsr 1) + 2) land (lnot 1))
    in
    if add > max_int-cap then raise_notrace Out_of_memory;
    let new_data = Array.make (cap+add) pad in
    Array.blit self.data 0 new_data 0 self.sz;
    self.data <- new_data;
  )

let grow_to self size pad : unit =
  if self.sz < size then (
    ensure self size pad;
    Array.fill self.data self.sz (size-self.sz) pad;
    self.sz <- size;
  )

let grow_to_with self size f : unit =
  if self.sz < size then (
    let pad = if Array.length self.data=0 then f 0 else self.data.(0) in
    ensure self size pad;
    for i = self.sz to size-1 do
      self.data.(i) <- f i
    done;
    self.sz <- size;
  )

let push self x =
  if self.sz = Array.length self.data then ensure self (self.sz+1) x;
  Array.unsafe_set self.data self.sz x;
  self.sz <- 1 + self.sz

let[@inline] last self = assert(self.sz>0); get self (self.sz-1)
let[@inline] pop self : unit = assert(self.sz>0); self.sz <- self.sz - 1

let copy_to self ~into : unit =
  into.data <- Array.sub self.data 0 self.sz;
  into.sz <- self.sz

let move_to self ~into : unit =
  into.data <- self.data;
  into.sz <- self.sz

let[@specialise] iteri f {data; sz} : unit =
  assert (sz <= Array.length data);
  for i=0 to sz-1 do
    f i (Array.unsafe_get data i)
  done

module Internal = struct
  let[@inline] data v = v.data
end

(*$inject
  module V = Minisat_vec
*)

(*$R
  let v = V.make() in
  V.push v 1;
  V.push v 2;
  assert_equal 2 (V.size v);
  V.push v 3;
  assert_equal 3 (V.last v);
  assert_equal 1 (V.get v 0);
  ()
*)
