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

module CH = Clause.Header

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
  random_seed : float ref;
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
  mutable learnt_literals: int;
  mutable clause_literals: int;
  mutable max_literals: int;
  mutable tot_literals: int;

  mutable ok: bool;

  ca: Clause.Alloc.t;
  clauses: Cref.t Vec.t; (* problem clauses *)
  learnts: Cref.t Vec.t; (* learnt clauses *)

  mutable cla_inc: float; (* Amount to bump next clause with. *)

  var_reason: Cref.t Vec.t; (* reason for the propagation of a variable *)
  var_level: int Vec.t; (* decision level of variable *)
  var_act: float Vec.t; (* A heuristic measurement of the activity of a variable. *)

  mutable var_inc: float; (* Amount to bump next variable with. *)

  (* watch list *)
  watches_cref: Cref.t Vec.t Vec.t;
  watches_blocker: Lit.t Vec.t Vec.t;
  watches_dirty: bool Vec.t;
  watches_dirties: Lit.t Vec.t;

  assigns: Lbool.t Vec.t; (* The current assignments. *)
  polarity: bool Vec.t; (* The preferred polarity of each variable. *)
  decision: bool Vec.t; (* Declares if a variable is eligible for selection in the decision heuristic. *)

  trail: Lit.t Vec.t; (* Assignment stack; stores all assigments made in the order they were made. *)
  trail_lim: int Vec.t; (* Separator indices for different decision levels in 'trail'. *)

  mutable qhead: int; (* Head of queue (as index into the trail) *)

  mutable simpDB_assigns: int; (* Number of top-level assignments since last execution of 'simplify()'. *)
  mutable simpDB_props : int; (* Remaining number of propagations that must be made before next execution of 'simplify()'. *)
  assumptions: Lit.t Vec.t; (* Current set of assumptions provided to solve by the user. *)

  order_heap: Heap.t; (* A priority queue of variables ordered with respect to the variable activity. *)

  mutable progress_estimate: float; (* Set by 'search()'. *)
  mutable remove_satisfied: bool; (* Indicates whether possibly inefficient linear scan for satisfied clauses should be performed in 'simplify'. *)

  model: Lbool.t Vec.t; (* If problem is satisfiable, this vector contains the model (if any). *)
  conflict: Lit.t Vec.t;
  (* If problem is unsatisfiable (possibly under assumptions),
     this vector represent the final conflict clause expressed in the
     assumptions. *)

  (* Temporaries (to reduce allocation overhead). Each variable is prefixed by the method in which it is
    used, except 'seen' wich is used in several places.
  *)
  seen: bool Vec.t;
  analyze_stack: Lit.t Vec.t;
  analyze_toclear: Lit.t Vec.t;
  add_tmp: Lit.t Vec.t;

  mutable max_learnts: float;
  mutable learntsize_adjust_confl : float;
  mutable learntsize_adjust_cnt : int;

  mutable conflict_budget : int; (*  -1 means no budget.*)
  mutable propagation_budget : int; (* -1 means no budget. *)
}

(* TODO *)

let[@inline] ok self = self.ok
let[@inline] n_vars self : int = Vec.size self.var_level
let[@inline] n_free_vars self : int =
  self.dec_vars -
  (if Vec.size self.trail_lim = 0 then Vec.size self.trail else Vec.get self.trail_lim 0)
let[@inline] n_assigns self : int = Vec.size self.trail
let[@inline] n_clauses self : int = Vec.size self.clauses
let[@inline] n_learnts self : int = Vec.size self.learnts

let[@inline] decision_level self : int = Vec.size self.trail_lim

let[@inline] level_var self (v:Var.t) : int = Vec.get self.var_level (v:>int)
let[@inline] level_lit self (x:Lit.t) : int = level_var self (Lit.var x)
    
let[@inline] value_var self (v:Var.t) : Lbool.t = Vec.get self.assigns (v:>int)
let[@inline] value_lit self (x:Lit.t) : Lbool.t = Lbool.xor (value_var self (Lit.var x)) (Lit.sign x)

let[@inline] abstract_level self (v:Var.t) : int =
  1 lsl (level_var self v land 31)

let set_verbosity self v =
  assert (v>=0 && v<=2);
  self.verbosity <- v

let[@inline] budget_off self : unit =
  self.conflict_budget <- -1;
  self.propagation_budget <- -1

let[@inline] within_budget self : bool =
  (self.conflict_budget < 0 || self.conflicts < self.conflict_budget) &&
  (self.propagation_budget < 0 || self.propagations < self.propagation_budget)

let add_empty_clause self = self.ok <- false

let[@inline] decision self v = Vec.get self.decision (v:Var.t:>int)

let insert_var_order self (v:Var.t) : unit =
  if not (Heap.in_heap self.order_heap (v:>int)) && decision self v then (
    Heap.insert self.order_heap (v:>int)
  )

let[@inline] watch_blocker_ self (lit:Lit.t) : _ Vec.t =
  Vec.get self.watches_blocker (lit:>int)
let[@inline] watch_cref_ self (lit:Lit.t) : _ Vec.t =
  Vec.get self.watches_cref (lit:>int)

let watch_init_ self (lit:Lit.t) : unit =
  let i = (lit:>int) in
  Vec.grow_to_with self.watches_cref (i+1) (fun _ ->Vec.make());
  Vec.grow_to_with self.watches_blocker (i+1) (fun _ ->Vec.make());
  Vec.grow_to self.watches_dirty (i+1) false;
  ()

let set_decision_var self (v:Var.t) b : unit =
  if b && not (decision self v) then self.dec_vars <- self.dec_vars+1;
  if not b && decision self v then self.dec_vars <- self.dec_vars-1;
  Vec.set self.decision (v:>int) b;
  insert_var_order self v

let new_var_ self ~polarity ~decision : Var.t =
  let v_idx = n_vars self in
  let v = Var.make v_idx in
  watch_init_ self (Lit.make_sign v false);
  watch_init_ self (Lit.make_sign v true);
  Vec.push self.assigns Lbool.undef;
  Vec.push self.var_level 0;
  Vec.push self.var_reason Cref.undef;
  Vec.push self.var_act
    (if self.rnd_init_act then drand self.random_seed *. 0.00001 else 0.);
  Vec.push self.seen false;
  Vec.push self.polarity polarity;
  Vec.push self.decision false;
  Vec.ensure self.trail (v_idx+1) Lit.undef;
  set_decision_var self v decision;
  v

let new_var self = new_var_ self ~polarity:false ~decision:true
let new_var' ?(polarity=false) ?(decision=true) self = new_var_ self ~polarity ~decision

let unchecked_enqueue self (p:Lit.t) (reason: Cref.t) : unit =
  assert (Lbool.equal Lbool.undef @@ value_lit self p);
  let v_idx = (Lit.var p :> int) in
  Vec.set self.assigns v_idx (Lbool.of_bool (not (Lit.sign p)));
  Vec.set self.var_reason v_idx reason;
  Vec.set self.var_level v_idx (decision_level self);
  Vec.push self.trail p

let[@inline] enqueue self (p:Lit.t) (from:Cref.t) : bool =
  let v = value_lit self p in
  if Lbool.equal Lbool.undef v then (
    unchecked_enqueue self p from;
    true
  ) else (
    not (Lbool.equal Lbool.false_ v)
  )

exception Early_return_true
exception Early_return_false

let attach_clause (self:t) (c:Cref.t) : unit =
  Printf.printf "attach clause %d\n" c;
  Array.iter (fun lit -> Printf.printf "  %d\n" (Lit.to_int lit)) (Clause.lits_a self.ca c);
  let h = Clause.header self.ca c in
  assert (CH.size h > 1);
  let c0 = Clause.lit self.ca c 0 in
  let c1 = Clause.lit self.ca c 1 in
  Vec.push (watch_blocker_ self (Lit.not c0)) c1;
  Vec.push (watch_cref_ self (Lit.not c0)) c;
  Vec.push (watch_blocker_ self (Lit.not c1)) c0;
  Vec.push (watch_cref_ self (Lit.not c1)) c;
  if CH.learnt h then (
    self.learnt_literals <- CH.size h + self.learnt_literals;
  ) else (
    self.clause_literals <- CH.size h + self.clause_literals;
  )

(* perform boolean propagation.
   if a conflict is detected, return the conflict clause's ref,
   otherwise [Cref.undef] *)
let propagate (self:t) : Cref.t =
  Printf.printf "propagate\n";
  assert false (* TODO *)

let add_clause self (ps:Lit.t Vec.t) : bool =
  Printf.printf "add clause\n";
  try
    if not self.ok then raise_notrace Early_return_false;
    assert (decision_level self = 0);
    Sort.sort_vec Lit.compare ps;

    (* Check if clause is satisfied and remove false/duplicate literals: *)
    let j = ref 0 and p = ref Lit.undef in
    for i=0 to Vec.size ps-1 do
      let p_i = Vec.get ps i in
      let v = value_lit self p_i in
      if Lbool.equal Lbool.true_ v || Lit.equal (Lit.not !p) p_i then (
        raise_notrace Early_return_true; (* satisfied/trivial *)
      );
      if not (Lbool.equal Lbool.false_ v) && not (Lit.equal !p p_i) then (
        (* not a duplicate *)
        Vec.set ps !j p_i;
        p := p_i;
        incr j
      )
    done;
    Vec.shrink ps !j;

    if Vec.size ps = 0 then (
      self.ok <- false;
      false
    ) else if Vec.size ps = 1 then (
      unchecked_enqueue self (Vec.get ps 0) Cref.undef;
      let confl = propagate self in
      if Cref.is_undef confl then (
        true
      ) else (
        self.ok <- false;
        false
      )
    ) else (
      let cr = Clause.Alloc.alloc self.ca ps ~learnt:false in
      Vec.push self.clauses cr;
      attach_clause self cr;
      true
    )
  with
  | Early_return_true -> true
  | Early_return_false -> false

let simplify _self : bool =
  Printf.printf "simplify\n";
  assert false (* TODO *)

let solve_ (self:t) : Lbool.t =
  Printf.printf "solve\n";
  Vec.clear self.model;
  Vec.clear self.conflict;
  if self.ok then (
    Lbool.true_ (* TODO *)
  ) else Lbool.false_

let solve self ~assumps : bool =
  budget_off self;
  Vec.copy_to assumps ~into:self.assumptions;
  Lbool.equal Lbool.true_ (solve_ self)

let solve_limited (self:t) ~assumps : Lbool.t =
  Vec.copy_to assumps ~into:self.assumptions;
  solve_ self

let create(): t =
  let var_act = Vec.make () in
  (* heap of variables, ordered by activity (higher activity comes first) *)
  let order_heap =
    Heap.make
      ~cmp:(fun v1 v2 -> compare (Vec.get var_act v2) (Vec.get var_act v1))
  in
  let s = {
    verbosity=0;
    var_decay=0.95;
    clause_decay=0.999;
    random_var_freq=0.;
    random_seed=ref 91648253.;
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
    learnt_literals=0;
    max_literals=0;
    tot_literals=0;

    ok=true;

    ca=Clause.Alloc.make ();
    clauses=Vec.make();
    learnts=Vec.make();
    cla_inc=1.;

    var_reason=Vec.make();
    var_level=Vec.make();
    var_act;
    var_inc=1.;

    watches_cref=Vec.make();
    watches_blocker=Vec.make();
    watches_dirty=Vec.make();
    watches_dirties=Vec.make();

    assigns=Vec.make();
    polarity=Vec.make();
    decision=Vec.make();

    trail=Vec.make();
    trail_lim=Vec.make();
    qhead=0;
    simpDB_assigns= -1;
    simpDB_props=0;
    assumptions=Vec.make();

    progress_estimate= 0.;
    remove_satisfied=true;

    model=Vec.make();
    conflict=Vec.make();
    seen=Vec.make();
    analyze_stack=Vec.make();
    analyze_toclear=Vec.make();
    add_tmp=Vec.make();
    order_heap;
    max_learnts=0.;
    learntsize_adjust_confl=0.;
    learntsize_adjust_cnt =0;
    conflict_budget= -1;
    propagation_budget= -1;
  } in
  s
