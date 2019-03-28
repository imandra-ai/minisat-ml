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
module Heap = Minisat_heap.Make(Var)

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

let[@inline] reason_var self (v:Var.t) : Cref.t = Vec.get self.var_reason (v:>int)
let[@inline] reason_lit self (x:Lit.t) : Cref.t = reason_var self (Lit.var x)

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
  if not (Heap.in_heap self.order_heap v) && decision self v then (
    Heap.insert self.order_heap v
  )

module Watch = struct
  type nonrec t = t
  let[@inline] blocker_ self (lit:Lit.t) : _ Vec.t =
    Vec.get self.watches_blocker (lit:>int)
  let[@inline] cref_ self (lit:Lit.t) : _ Vec.t =
    Vec.get self.watches_cref (lit:>int)

  let smudge self (lit:Lit.t) : unit =
    let i = (lit:>int) in
    if not (Vec.get self.watches_dirty i) then (
      Vec.set self.watches_dirty i true;
      Vec.push self.watches_dirties lit;
    )

  (*
  template<class Idx, class Vec, class Deleted>
  void OccLists<Idx,Vec,Deleted>::cleanAll()
  {
      for (int i = 0; i < dirties.size(); i++)
          // Dirties may contain duplicates so check here if a variable is already cleaned:
          if (dirty[toInt(dirties[i])])
              clean(dirties[i]);
      dirties.clear();
  }


  template<class Idx, class Vec, class Deleted>
  void OccLists<Idx,Vec,Deleted>::clean(const Idx& idx)
  {
      Vec& vec = occs[toInt(idx)];
      int  i, j;
      for (i = j = 0; i < vec.size(); i++)
          if (!deleted(vec[i]))
              vec[j++] = vec[i];
      vec.shrink(i - j);
      dirty[toInt(idx)] = 0;
  }
     *)

end

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

let attach_clause (self:t) (c:Cref.t) : unit =
  Printf.printf "attach clause %d\n" c;
  Array.iter (fun lit -> Printf.printf "  %d\n" (Lit.to_int lit)) (Clause.lits_a self.ca c);
  let h = Clause.header self.ca c in
  assert (CH.size h > 1);
  let c0 = Clause.lit self.ca c 0 in
  let c1 = Clause.lit self.ca c 1 in
  Vec.push (Watch.blocker_ self (Lit.not c0)) c1;
  Vec.push (Watch.cref_ self (Lit.not c0)) c;
  Vec.push (Watch.blocker_ self (Lit.not c1)) c0;
  Vec.push (Watch.cref_ self (Lit.not c1)) c;
  if CH.learnt h then (
    self.learnt_literals <- CH.size h + self.learnt_literals;
  ) else (
    self.clause_literals <- CH.size h + self.clause_literals;
  )

let detach_clause_ (self:t) ~strict (c:Cref.t) : unit =
  let h = Clause.header self.ca c in
  assert (CH.size h > 1);
  let c0 = Clause.lit self.ca c 0 in
  let c1 = Clause.lit self.ca c 1 in
  if strict then (
    assert false (* NOTE: not used internally outside of Simp, and requires eager removal *)
  ) else (
    (* Lazy detaching: *)
    Watch.smudge self (Lit.not c0);
    Watch.smudge self (Lit.not c1);
  );
  if CH.learnt h then (
    self.learnt_literals <- self.learnt_literals - (CH.size h)
  ) else (
    self.clause_literals <- self.clause_literals - (CH.size h)
  )

let[@inline] detach_clause self c : unit = detach_clause_ self ~strict:false c

let cancel_until self (level:int) : unit =
  if decision_level self > level then (
    Printf.printf "cancel-until %d\n" level;
    let offset = Vec.get self.trail_lim level in
    for c = Vec.size self.trail-1 downto offset do
      let lit_c = Vec.get self.trail c in
      let v = Lit.var lit_c in
      Vec.set self.assigns (v:>int) Lbool.undef;
      if self.phase_saving>1 ||
         (self.phase_saving=1 && c > Vec.last self.trail_lim) then (
        (* save phase *)
        Vec.set self.polarity (v:>int) (Lit.sign lit_c);
      );
      insert_var_order self v;
    done;
    self.qhead <- offset;
    Vec.shrink self.trail offset;
    Vec.shrink self.trail_lim level;
  )

let pick_branch_lit self : Lit.t =
  let next =
    (* random pick? *)
    if self.random_var_freq > 0. &&
       drand self.random_seed < self.random_var_freq &&
       not (Heap.empty self.order_heap) then (
      let v = Heap.get self.order_heap
          (irand self.random_seed (Heap.size self.order_heap)) in
      if Lbool.equal Lbool.undef (value_var self v) &&
         Vec.get self.decision (v:>int) then (
        self.rnd_decisions <- 1 + self.rnd_decisions;
      );
      v
    ) else Var.undef
  in
  let rec loop next =
    if Var.equal Var.undef next ||
       not (Lbool.equal Lbool.undef (value_var self next)) ||
       not (Vec.get self.decision (next:>int)) then (

      if Heap.empty self.order_heap then Var.undef
      else loop (Heap.remove_min self.order_heap)
    ) else next
  in
  let next = loop next in
  if Var.equal Var.undef next then (
    Lit.undef
  ) else (
    Lit.make_sign next
      (if self.rnd_pol then drand self.random_seed < 0.5
       else Vec.get self.polarity (next:>int))
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

(* is the clause locked (is it the reason a literal is propagated)? *)
let locked self (c:Cref.t) : bool =
  let c0 = Clause.lit self.ca c 0 in
  Lbool.equal Lbool.true_ (value_var self (Lit.var c0)) &&
  c = reason_var self (Lit.var c0)

let remove_clause self (c:Cref.t) : unit =
  Printf.printf "remove clause %d\n" c;
  detach_clause self c;
  if locked self c then (
    Vec.set self.var_reason ((Lit.var (Clause.lit self.ca c 0)):>int) Cref.undef;
  );
  Clause.set_mark self.ca c 1;
  Clause.Alloc.free self.ca c

let var_bump_activity self (v:Var.t) : unit =
  Printf.printf "var bump activity\n";
  () (* TODO *)

let cla_bump_activity self (c:Cref.t) : unit =
  Printf.printf "cla bump activity\n";
  () (* TODO *)

let lit_redundant self (p:Lit.t) (ab_lvl:int) : bool =
  Vec.clear self.analyze_stack;
  Vec.push self.analyze_stack p;
  let top = Vec.size self.analyze_toclear in
  try
    while Vec.size self.analyze_stack > 0 do
      (* clause that propagated a literal *)
      let c = reason_lit self (Vec.last self.analyze_stack) in
      Vec.pop self.analyze_stack;
      assert (not (Cref.is_undef c));
      let h = Clause.header self.ca c in

      for i=1 to CH.size h-1 do
        let p = Clause.lit self.ca c i in
        if not (Vec.get self.seen ((Lit.var p):>int)) && level_lit self p>0 then (
          if not (Cref.is_undef (reason_lit self p)) &&
             (abstract_level self (Lit.var p) land ab_lvl) <> 0
          then (
            Vec.set self.seen ((Lit.var p):>int) true;
            Vec.push self.analyze_stack p;
            Vec.push self.analyze_toclear p;
          ) else (
            (* cannot be eliminated, not involved in conflict.
               restore to input state + return false. *)
            for j = top to Vec.size self.analyze_toclear-1 do
              Vec.set self.seen ((Lit.var (Vec.get self.analyze_toclear j)):>int) false;
            done;
            Vec.shrink self.analyze_toclear top;
            raise_notrace Early_return_false
          )
        )
      done;
    done;
    true (* TODO *)
  with Early_return_false -> false

(* Description:
   Analyze conflict and produce a reason clause.
 
   Pre-conditions:
     * 'out_learnt' is assumed to be cleared.
     * Current decision level must be greater than root level.
 
   Post-conditions:
     * 'out_learnt[0]' is the asserting literal at level 'out_btlevel'.
     * If out_learnt.size() > 1 then 'out_learnt[1]' has the greatest decision level of the 
       rest of literals. There may be others from the same level though.
 *)
let analyze (self:t) (confl:Cref.t) (out_learnt: Lit.t Vec.t) : int =
  assert (Vec.empty out_learnt);
  Vec.push out_learnt Lit.undef; (* leave room for asserting lit *)

  let rec resolve_loop ~pathC ~p ~index ~confl : Lit.t =
    assert (not (Cref.is_undef confl));

    let h = Clause.header self.ca confl in
    if CH.learnt h then cla_bump_activity self confl;

    (* resolve with the other literals of the clause *)
    for j = (if Lit.is_undef p then 0 else 1) to CH.size h - 1 do
      let q = Clause.lit self.ca confl j in

      if not (Vec.get self.seen ((Lit.var q):>int)) && level_lit self q > 0 then (
        var_bump_activity self (Lit.var q);
        Vec.set self.seen ((Lit.var q):>int) true;

        if level_lit self q >= decision_level self then (
          pathC := !pathC + 1; (* need to resolve this away *)
        ) else (
          Vec.push out_learnt q;
        )
      );
    done;

    (* next literal to consider *)
    let index =
      let rec loop i =
        let v = Lit.var (Vec.get self.trail i) in
        if Vec.get self.seen (v:>int) then i else loop (i-1)
      in
      loop index
    in

    let p = Vec.get self.trail (index+1) in
    let confl = reason_lit self p in
    Vec.set self.seen ((Lit.var p):>int) false;
    pathC := !pathC - 1;

    if !pathC > 0 then (resolve_loop[@tailcall]) ~pathC ~p ~index ~confl
    else p
  in
  let p = resolve_loop ~pathC:(ref 0) ~p:Lit.undef ~index:(Vec.size self.trail-1) ~confl in
  Vec.set out_learnt 0 (Lit.not p);

  (* simplify conflict clause *)
  Vec.copy_to out_learnt ~into:self.analyze_toclear;
  let j = ref 0 in
  if self.ccmin_mode = 2 then (
    (* maintain an abstraction of levels involved in conflict *)
    let ab_lvl =
      let lvl = ref 0 in
      for i=1 to Vec.size out_learnt-1 do
        lvl:= !lvl lor abstract_level self (Lit.var (Vec.get out_learnt i));
      done;
      !lvl
    in

    j := 1;
    for i = 1 to Vec.size out_learnt-1 do
      let p = Vec.get out_learnt i in
      if Cref.is_undef (reason_lit self p) || not (lit_redundant self p ab_lvl) then (
        (* decision lit, or not redundant: keep *)
        Vec.set out_learnt !j p;
        j := !j + 1;
      )
    done;
  ) else if self.ccmin_mode = 1 then (
    assert false (* TODO *)
  ) else (
    j := Vec.size out_learnt;
  );

  self.max_literals <- self.max_literals + Vec.size out_learnt;
  Vec.shrink out_learnt !j;
  self.tot_literals <- self.tot_literals + Vec.size out_learnt;

  (* cleanup 'seen' *)
  Vec.iteri
    (fun _ p -> Vec.set self.seen ((Lit.var p):>int) false)
    self.analyze_toclear;
  Vec.clear self.analyze_toclear;

  (* Find correct backtrack level: *)
  if Vec.size out_learnt = 1 then (
    0
  ) else (
    let max_i = ref 1 in
    (* Find the first literal assigned at the next-highest level: *)
    for i = 2 to Vec.size out_learnt-1 do
      if level_lit self (Vec.get out_learnt i) > level_lit self (Vec.get out_learnt !max_i) then (
        max_i := i;
      )
    done;
    (* Swap-in this literal at index 1: *)
    let p = Vec.get out_learnt !max_i in
    Vec.set out_learnt !max_i (Vec.get out_learnt 1);
    Vec.set out_learnt 1 p;
    level_lit self p
  )

let satisfied self (c:Cref.t) : bool =
  Clause.exists self.ca c (fun lit -> Lbool.equal Lbool.true_ (value_lit self lit))

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
      ~cmp:(fun v1 v2 ->
          compare (Vec.get var_act (v2:>int)) (Vec.get var_act (v1:>int)))
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
