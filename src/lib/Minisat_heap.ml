(*
Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
Copyright (c) 2007-2010, Niklas Sorensson

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module Vec = Minisat_vec

module type S = sig
  type t
  type elt [@@ocaml.immediate]

  val make : cmp:(elt -> elt -> int) -> t

  val empty : t -> bool
  val in_heap : t -> elt -> bool
  val get : t -> int -> int

  val increase : t -> elt -> unit
  val decrease : t -> elt -> unit
  val insert : t -> elt -> unit

  val size : t -> int
  val get : t -> int -> elt

  val update : t -> elt -> unit (** safe variant of insert/decrease/increase *)

  val remove_min : t -> elt

  (** rebuild from scratch from the given vector *)
  val build : t -> elt Minisat_vec.t -> unit

  val clear : t -> unit
  val clear_dealloc : t -> unit
end

module type ARG = sig
  type t = private int
end

module Make(T : ARG)
(*   : S with type elt = T.t *)
= struct
  type elt = T.t
  type t = {
    cmp: elt -> elt -> int;
    heap: elt Vec.t;
    indices: int Vec.t;
  }

  let[@inline] left i = i*2 + 1
  let[@inline] right i = (i+1)*2
  let[@inline] parent i = (i-1) lsr 1

  let make ~cmp : t = {cmp; heap=Vec.make(); indices=Vec.make()}

  let[@inline] empty self = Vec.size self.heap = 0
  let[@inline] in_heap self (i:elt) =
    (i:>int) < Vec.size self.indices && Vec.get self.indices (i:>int) >= 0

  let[@inline] size self = Vec.size self.heap
  let[@inline] get self i = Vec.get self.heap i

  let percolate_up {heap;indices;cmp} (i:int) : unit =
    let x = Vec.get heap i in
    let p = parent i in
    let rec loop i p =
      if i<>0 && cmp x (Vec.get heap p) < 0 then (
        Vec.set heap i (Vec.get heap p);
        Vec.set indices ((Vec.get heap p):>int) i;
        let i = p in
        let p = parent p in
        loop i p
      ) else i
    in
    let i = loop i p in
    Vec.set heap i x;
    Vec.set indices (x:>int) i;
    ()

  let percolate_down {heap;indices;cmp} (i:int) : unit =
    let size = Vec.size heap in
    let x = Vec.get heap i in
    let rec loop i =
      if left i < size then (
        (* pick side: left or right *)
        let child =
          if right i < size &&
             cmp (Vec.get heap (right i)) (Vec.get heap (left i)) < 0
          then right i else left i
        in
        if cmp (Vec.get heap child) x >= 0 then (
          i (* break *)
        ) else (
          Vec.set heap i (Vec.get heap child);
          Vec.set indices ((Vec.get heap i):>int) i;
          let i = child in
          loop i
        )
      ) else i
    in
    let i = loop i in
    Vec.set heap i x;
    Vec.set indices (x:>int) i;
    ()

  let[@inline] decrease self i = assert (in_heap self i); percolate_up self (Vec.get self.indices (i:>int))
  let[@inline] increase self i = assert (in_heap self i); percolate_down self (Vec.get self.indices (i:>int))

  let insert self (n:elt) =
    Vec.grow_to self.indices ((n:>int)+1) ~-1;
    assert (not (in_heap self n));
    (* insert as a leaf, then percolate up to preserve heap property *)
    Vec.set self.indices (n:>int) (Vec.size self.heap);
    Vec.push self.heap n;
    percolate_up self (Vec.get self.indices (n:>int))

  let update self i =
    if not (in_heap self i) then (
      insert self i
    ) else (
      percolate_up self (Vec.get self.indices (i:>int));
      percolate_down self (Vec.get self.indices (i:>int))
    )

  let remove_min self : elt =
    assert (not (empty self));
    let x = Vec.get self.heap 0 in
    (* swap first element with a leaf (the bottom right one), then fix structure *)
    let new_first = Vec.last self.heap in
    Vec.set self.heap 0 new_first;
    Vec.set self.indices (new_first:>int) 0;
    Vec.set self.indices (x:>int) ~-1;
    Vec.pop self.heap;
    if Vec.size self.heap > 1 then percolate_down self 0;
    x

  let clear self : unit =
    for i=0 to Vec.size self.heap do
      Vec.set self.indices ((Vec.get self.heap i):>int) ~-1;
    done;
    Vec.clear self.heap

  let clear_dealloc self =
    clear self;
    Vec.clear_dealloc self.heap

  let build self (ns:elt Vec.t) : unit =
    Vec.clear self.heap;
    Vec.iteri
      (fun i (x:elt) ->
         Vec.set self.indices (x:>int) i;
         Vec.push self.heap x)
      ns;
    for i= (Vec.size self.heap/2)-1 downto 0 do
      percolate_down self i
    done
end[@@inline]
