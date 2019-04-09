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
*)

open Minisat_ml

type t

val make : refill:(Bytes.t -> int -> int -> int) -> t

val make_chan : in_channel -> t

val parse_int : t -> int

val skip_whitespace : t -> unit

val skip_line : t -> unit

val skip_metadata : t -> unit
(** Skip header, comments, etc. *)

val read_int_list : t -> int list
(** Read a clause as an integer list, for debug purpose *)

val read_int_list_list : t -> int list list
(** Read all clauses as integer lists, for debug purpose *)

val read_clause : t -> Solver.t -> Lit.t Vec.t -> bool

val parse_dimacs : t -> Solver.t -> unit

(* NOTE: we can't really do gzip without a lot of trouble around IO channels *)
