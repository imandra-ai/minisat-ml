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


type 'a t

val make : unit -> 'a t
val make_with : int -> 'a -> 'a t

val empty : _ t -> bool
val size : _ t -> int
val capacity : _ t -> int
val ensure : 'a t -> int -> 'a -> unit
val grow_to : 'a t -> int -> 'a -> unit
val grow_to_with : 'a t -> int -> (int -> 'a) -> unit

val shrink : _ t -> int -> unit
val push : 'a t -> 'a -> unit
val pop : _ t -> unit
val last : 'a t -> 'a

val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit

val clear : 'a t -> unit
val clear_dealloc : 'a t -> unit

val copy_to : 'a t -> into:'a t -> unit
val move_to : 'a t -> into:'a t -> unit

val blit : 'a t -> int -> 'a t -> int -> int -> unit

val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit

module Infix : sig
  val ( .%[] ) : 'a t -> int -> 'a
  val ( .%[]<- ) : 'a t -> int -> 'a -> unit
end

include module type of Infix

module Internal : sig
  val data : 'a t -> 'a array
end
