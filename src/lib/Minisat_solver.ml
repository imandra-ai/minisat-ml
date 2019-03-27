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

(* Returns a random float 0 <= x < 1. Seed must never be 0. *)
let drand (seed: float ref) : float =
  seed := !seed *. 1389796.;
  let q = int_of_float (!seed /. 2147483647.) in
  seed := !seed -. (float_of_int q *. 2147483647.);
  !seed /. 2147483647.

let[@inline] irand (seed: float ref) (size:int) : int =
  int_of_float (drand seed *. float_of_int size)

type t = {
  mutable verbosity: int;
  mutable var_decay: float;
  mutable clause_decay : float;
  mutable random_var_freq : float;
  mutable random_seed : float;
  mutable luby_restart : bool;
  mutable ccmin_mode : int; (* Controls conflict clause minimization (0=none, 1=basic, 2=deep). *)
  mutable phase_saving : int; (* Controls the level of phase saving (0=none, 1=limited, 2=full). *)
  mutable rnd_pol : bool; (* Use random polarities for branching heuristics. *)
  mutable rnd_init_act : bool;  (* Initialize variable activities with a small random value. *)
  mutable garbage_frac : float; (* The fraction of wasted memory allowed before a garbage collection is triggered. *)

  mutable restart_first : int; (* The initial restart limit. (default 100) *)
  restart_inc : float; (* The factor with which the restart limit is multiplied in each restart (default 1.5) *)
  learntsize_factor : float; (* The initial limit for learnt clauses is a factor of the original clause (default 1 / 3) *)
  learntsize_inc : float; (* The limit for learnt clauses is multiplied with this factor each restart (default 1.1) *)

  learntsize_adjust_start_confl : int;
  learntsize_adjust_inc : float;

  (* statistics: *)

  mutable solves: int;
  mutable starts: int;
  mutable decisions: int;
  mutable rnd_decisions: int;
  mutable propagations: int;
  mutable conflicts: int;
  mutable dec_vars: int;
  mutable clause_literals: int;
  mutable max_literals: int;
  mutable tot_literals: int;

  mutable ok: bool;

  ca: Clause.Alloc.t;
  clauses: Vec_cref.t; (* problem clauses *)
  learnts: Vec_cref.t; (* learnt clauses *)

  mutable cla_inc: float; (* Amount to bump next clause with. *)

  var_reason: Vec_cref.t; (* reason for the propagation of a variable *)
  var_level: Vec_int.t; (* decision level of variable *)
  var_act: Vec_float.t; (* A heuristic measurement of the activity of a variable. *)

  mutable var_inc: float; (* Amount to bump next variable with. *)

  (* watch list *)
  watches_cref : Vec_cref.t;
  watches_blocker: Vec_lit.t;

  assigns: Vec_lbool.t; (* The current assignments. *)
  polarity: Vec_bool.t; (* The preferred polarity of each variable. *)
  decision: Vec_bool.t; (* Declares if a variable is eligible for selection in the decision heuristic. *)

  trail: Vec_lit.t; (* Assignment stack; stores all assigments made in the order they were made. *)
  trail_lim: Vec_int.t; (* Separator indices for different decision levels in 'trail'. *)

  mutable qhead: int; (* Head of queue (as index into the trail) *)

  mutable simpDB_assigns: int; (* Number of top-level assignments since last execution of 'simplify()'. *)
  mutable simpDB_props : int; (* Remaining number of propagations that must be made before next execution of 'simplify()'. *)
  assumptions: Vec_lit.t; (* Current set of assumptions provided to solve by the user. *)

  (* TODO:
    Heap<VarOrderLt>    order_heap;       // A priority queue of variables ordered with respect to the variable activity.
     *)
  mutable progress_estimate: float; (* Set by 'search()'. *)
  mutable remove_satisfied: bool; (* Indicates whether possibly inefficient linear scan for satisfied clauses should be performed in 'simplify'. *)

  model: Vec_lbool.t; (* If problem is satisfiable, this vector contains the model (if any). *)
  conflict: Vec_lit.t;
  (* If problem is unsatisfiable (possibly under assumptions),
     this vector represent the final conflict clause expressed in the
     assumptions. *)

  (* Temporaries (to reduce allocation overhead). Each variable is prefixed by the method in which it is
    used, except 'seen' wich is used in several places.
  *)
  seen: Vec_bool.t;
  analyze_stack: Vec_lit.t;
  analyze_toclear: Vec_lit.t;
  add_tmp: Vec_lit.t;

  mutable max_learnts: float;
  mutable learntsize_adjust_confl : float;
  mutable learntsize_adjust_cnt : int;

  mutable conflict_budget : int; (*  -1 means no budget.*)
  mutable propagation_budget : int; (* -1 means no budget. *)
}

(* TODO *)

let add_empty_clause self = self.ok <- false

let[@inline] n_vars self : int = Vec_int.size self.var_level

let new_var_ self ~polarity ~decision : Var.t =
  let v = n_vars self in


  assert false (* TODO *)

let new_var self = new_var_ self ~polarity:false ~decision:true
let new_var' ?(polarity=false) ?(decision=true) self = new_var_ self ~polarity ~decision

let add_clause self c : bool =
  assert false (* TODO *)

let set_verbosity self v =
  assert (v>=0 && v<=2);
  self.verbosity <- v

let solve_ (self:t) : bool =
  Vec_lbool.clear self.model;
  Vec_lit.clear self.conflict;
  if self.ok then (

    true (* TODO *)
  ) else false

let create(): t =
  let s = {
    verbosity=0;
    var_decay=0.95;
    clause_decay=0.999;
    random_var_freq=0.;
    random_seed=91648253.;
    luby_restart=true;
    ccmin_mode=2;
    phase_saving=2;
    rnd_pol=false;
    rnd_init_act=false;
    garbage_frac=0.20;
    restart_first=100;
    restart_inc=2.;
    learntsize_factor=(1./.3.);
    learntsize_inc=1.1;
    learntsize_adjust_start_confl=100;
    learntsize_adjust_inc=1.5;
    solves=0;
    starts=0;
    decisions=0;
    rnd_decisions=0;
    propagations=0;
    conflicts=0;
    dec_vars=0;
    clause_literals=0;
    max_literals=0;
    tot_literals=0;

    ok=false;

    ca=Clause.Alloc.make ();
    clauses=Vec_cref.make();
    learnts=Vec_cref.make();
    cla_inc=1.;

    var_reason=Vec_cref.make();
    var_level=Vec_int.make();
    var_act=Vec_float.make();
    var_inc=1.;

    watches_cref=Vec_cref.make();
    watches_blocker=Vec_lit.make();

    assigns=Vec_lbool.make();
    polarity=Vec_bool.make();
    decision=Vec_bool.make();

    trail=Vec_lit.make();
    trail_lim=Vec_int.make();
    qhead=0;
    simpDB_assigns= -1;
    simpDB_props=0;
    assumptions=Vec_lit.make();

    progress_estimate= 0.;
    remove_satisfied=true;

    model=Vec_lbool.make();
    conflict=Vec_lit.make();
    seen=Vec_bool.make();
    analyze_stack=Vec_lit.make();
    analyze_toclear=Vec_lit.make();
    add_tmp=Vec_lit.make();

    max_learnts=0.;
    learntsize_adjust_confl=0.;
    learntsize_adjust_cnt =0;
    conflict_budget= -1;
    propagation_budget= -1;
  } in
  s
