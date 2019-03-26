
module type S = sig
  type elt
  type t

  val make : unit -> t
  val make_with : int -> t

  val size : t -> int
  val capacity : t -> int
  val ensure : t -> int -> unit
  val grow_to : t -> int -> unit

  val shrink : t -> int -> unit
  val push : t -> elt -> unit
  val pop : t -> unit
  val last : t -> elt

  val get : t -> int -> elt
  val set : t -> int -> elt -> unit

  val clear : t -> unit
  val clear_dealloc : t -> unit

  val copy_to : t -> into:t -> unit
  val move_to : t -> into:t -> unit
end

module type ARG = sig
  type t
  val pad : t
end

module[@inline] Make(T : ARG)
  : S with type elt = T.t
= struct

  (* used to compute next capacity *)
  let[@inline] imax x y : int =
    let mask = (y-x) lsr Sys.int_size in
    (x land mask) + (y land (lnot mask))

  type elt = T.t
  type t = {
    mutable data: elt array;
    mutable sz: int;
  }

  let[@inline] make () : t = {data=[| |]; sz=0}
  let[@inline] make_with sz : t = assert (sz>=0); {data=Array.make sz T.pad; sz}

  let[@inline] size self : int = self.sz
  let[@inline] capacity self : int = Array.length self.data
  let[@inline] get self i : elt = assert (i>=0 && i<self.sz); Array.unsafe_get self.data i
  let[@inline] set self i x : unit = assert (i>=0 && i<self.sz); Array.unsafe_set self.data i x
  let[@inline] shrink self size = if size < self.sz then self.sz <- size
  let[@inline] clear self = self.sz <- 0
  let[@inline] clear_dealloc self = self.sz <- 0; self.data <- [| |]

  let ensure self min_cap : unit =
    let cap = capacity self in
    if cap < min_cap then (
      (* grow by approx. 3/2 *)
      let add =
        imax ((min_cap - cap + 1) land (lnot 1)) (((cap lsr 1) + 2) land (lnot 1))
      in
      if add > max_int-cap then raise_notrace Out_of_memory;
      let new_data = Array.make (cap+add) T.pad in
      Array.blit self.data 0 new_data 0 self.sz;
      self.data <- new_data;
    )

  let grow_to self size : unit =
    if self.sz < size then (
      ensure self size;
      Array.fill self.data self.sz (size-self.sz) T.pad;
      self.sz <- size;
    )

  let[@inline] push self x =
    if self.sz = Array.length self.data then ensure self (self.sz+1);
    Array.unsafe_set self.data self.sz x;
    self.sz <- 1 + self.sz

  let[@inline] last self : elt = assert(self.sz>0); get self (self.sz-1)
  let[@inline] pop self : unit = assert(self.sz>0); self.sz <- self.sz - 1

  let copy_to self ~into : unit =
    into.data <- Array.sub self.data 0 self.sz;
    into.sz <- self.sz

  let move_to self ~into : unit =
    into.data <- self.data;
    into.sz <- self.sz
end

module Vec_int = Make(struct type t = int let pad = -1 end)
module Vec_float = Make(struct type t = float let pad = 0. end)
module Vec_bool = Make(struct type t = bool let pad = false end)

(*$inject
  module V = Minisat_vec.Vec_int
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
