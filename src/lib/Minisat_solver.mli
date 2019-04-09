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

open Minisat_types

type t

val create : unit -> t

val set_verbosity : t -> int -> unit

val new_var : t -> Var.t
val new_var' : ?polarity:bool -> ?decision:bool -> t -> Var.t

val add_clause : t -> Lit.t Vec.t -> bool
val add_empty_clause : t -> unit

val ok : t -> bool

val n_assigns : t -> int
val n_clauses : t -> int
val n_learnts : t -> int
val n_vars : t -> int
val n_free_vars : t -> int
val n_starts : t -> int
val n_conflicts : t -> int
val n_propagations : t -> int
val n_decisions : t -> int
val n_rnd_decisions : t -> int
val n_tot_literals : t -> int
val n_max_literals : t -> int

val simplify : t -> bool

val solve : t -> assumps:Lit.t Vec.t -> bool
val solve_limited : t -> assumps:Lit.t Vec.t -> Lbool.t

val set_ccmin_mode : t -> int -> unit

val set_decision_var : t -> Var.t -> bool -> unit
(** Set whether the variable is a eligible for decisions *)

val value_var : t -> Var.t -> Lbool.t
val value_lit : t -> Lit.t -> Lbool.t

(* TODO

val set_polarity : t -> Var.t -> bool -> unit
(** Declare polarity of the given variable when it's decided *)

val model_value_var : t -> Var.t -> Lbool.t
val model_value_lit : t -> Lit.t -> Lbool.t

(** {2 Ressource constraints} *)
val set_conf_budget : t -> int -> unit
val set_prop_budget : t -> int -> unit
val budget_off : t -> unit

(** If problem is satisfiable, this contains the model *)
val model : t -> Lbool.t Vec.t

(** If problem is unsatisfiable, this contains the final conflict clause
    expressed in the assumptions *)
val conflict : t -> Lit.t Vec.t
   *)
