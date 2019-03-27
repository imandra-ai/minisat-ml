
open Minisat_types

type t

val create : unit -> t

val set_verbosity : t -> int -> unit

val new_var : t -> Var.t
val new_var' : ?polarity:bool -> ?decision:bool -> t -> Var.t

val add_clause : t -> Vec_lit.t -> bool
val add_empty_clause : t -> unit

(* TODO
val simplify : t -> bool

val solve : t -> assumps:Vec_lit.t -> bool
val solve_limited : t -> assumps:Vec_lit.t -> bool

val ok : t -> bool

val set_polarity : t -> Var.t -> bool -> unit
(** Declare polarity of the given variable when it's decided *)

val set_decision_var : t -> Var.t -> bool -> unit
(** Set whether the variable is a eligible for decisions *)

val value_var : t -> Var.t -> Lbool.t
val value_lit : t -> Lit.t -> Lbool.t

val model_value_var : t -> Var.t -> Lbool.t
val model_value_lit : t -> Lit.t -> Lbool.t

val n_assigns : t -> int
val n_clauses : t -> int
val n_learnts : t -> int
val n_vars : t -> int
val n_free_vars : t -> int

(** {2 Ressource constraints} *)
val set_conf_budget : t -> int -> unit
val set_prop_budget : t -> int -> unit
val budget_off : t -> unit

(** If problem is satisfiable, this contains the model *)
val model : t -> Vec_lbool.t

(** If problem is unsatisfiable, this contains the final conflict clause
    expressed in the assumptions *)
val conflict : t -> Vec_lit.t
   *)
