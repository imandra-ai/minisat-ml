(* Minisat-ml, adapted from Minisat by Simon Cruanes <simon@imandra.ai>
   Copyright (c) 2019-2019, Aesthetic Integration (https://imandra.ai)
*)

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
**************************************************************************************************/
 *)

type 'a t = {
  mutable data : 'a array;
  mutable sz : int
}

let[@inline] make () : _ t = { data = [||]; sz = 0 }

let[@inline] make_with sz pad : _ t =
  assert (sz >= 0);
  { data = Array.make sz pad; sz }

let[@inline] size self : int = self.sz

let[@inline] empty self = self.sz = 0

let[@inline] capacity self : int = Array.length self.data

let[@inline] get self i =
  assert (i >= 0 && i < self.sz);
  Array.unsafe_get self.data i

let[@inline] set self i x : unit =
  assert (i >= 0 && i < self.sz);
  Array.unsafe_set self.data i x

let[@inline] shrink self size = if size < self.sz then self.sz <- size

let[@inline] clear self = self.sz <- 0

let[@inline] clear_dealloc self =
  self.sz <- 0;
  self.data <- [||]

let[@inline never] ensure self min_cap pad : unit =
  let cap = capacity self in
  if cap < min_cap then (
    (* grow by approx. 3/2 *)
    let new_cap =
      min Sys.max_array_length (max min_cap (cap + (cap lsr 1) + 2))
    in
    if new_cap < min_cap then raise_notrace Out_of_memory;
    let new_data = Array.make new_cap pad in
    Array.blit self.data 0 new_data 0 self.sz;
    self.data <- new_data )

let grow_to self size pad : unit =
  if self.sz < size then (
    ensure self size pad;
    Array.fill self.data self.sz (size - self.sz) pad;
    self.sz <- size )

let grow_to_with self size f : unit =
  if self.sz < size then (
    let pad = if Array.length self.data = 0 then f 0 else self.data.(0) in
    ensure self size pad;
    for i = self.sz to size - 1 do
      self.data.(i) <- f i
    done;
    self.sz <- size )

let push self x =
  if self.sz = Array.length self.data then ensure self (self.sz + 1) x;
  Array.unsafe_set self.data self.sz x;
  self.sz <- 1 + self.sz

let[@inline] last self =
  assert (self.sz > 0);
  Array.unsafe_get self.data (self.sz - 1)

let[@inline] pop self : unit =
  assert (self.sz > 0);
  self.sz <- self.sz - 1

let[@inline] blit v1 i1 v2 i2 len = Array.blit v1.data i1 v2.data i2 len

let copy_to self ~into : unit =
  if self.sz > 0 then (
    ensure into self.sz self.data.(0);
    Array.blit self.data 0 into.data 0 self.sz );
  into.sz <- self.sz

let move_to self ~into : unit =
  into.data <- self.data;
  into.sz <- self.sz;
  self.data <- [||];
  self.sz <- 0;
  ()

let[@inline] iteri f { data; sz } : unit =
  assert (sz <= Array.length data);
  for i = 0 to sz - 1 do
    f i (Array.unsafe_get data i)
  done

let[@inline] iter f { data; sz } : unit =
  assert (sz <= Array.length data);
  for i = 0 to sz - 1 do
    f (Array.unsafe_get data i)
  done

module Infix = struct
  let ( .%[] ) = get

  let ( .%[]<- ) = set
end

include Infix

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
