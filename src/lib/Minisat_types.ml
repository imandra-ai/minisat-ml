
(* magic for casting from bool to int *)
external __int_of_bool : bool -> int = "%identity"

module Vec = Minisat_vec
module Sort = Minisat_sort

module Var : sig
  type t = private int [@@ocaml.immediate]

  val make : int -> t
  val undef : t
  val equal : t -> t -> bool
  val to_int : t -> int
  val to_int_a : t array -> int array

  (**/**)
  module Internal : sig
    val of_int : int -> t
  end
  (**/**)
end = struct
  type t = int
  let[@inline] make x = assert (x>=0); x
  let[@inline] to_int x = x
  let[@inline] to_int_a x = x
  let[@inline] equal (x:t) y = x=y
  let undef = -1
  module Internal = struct
    let[@inline] of_int x = x
  end
end

module Lit : sig
  type t = private int

  val make_sign : Var.t -> bool -> t
  val make : Var.t -> t
  val is_undef : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val not : t -> t
  val xor : t -> bool -> t
  val sign : t -> bool
  val var : t -> Var.t
  val to_int : t -> int
  val to_int_a : t array -> int array
  val undef : t
  val error : t

  (**/**)
  module Internal : sig
    val of_int : int -> t (* unchecked conversion *)
    val of_int_a : int array -> t array (* unchecked conversion *)
  end
  (**/**)
end = struct
  type t = int
  let[@inline] make_sign v sign = let v = (v:Var.t:>int) in v + v + (__int_of_bool sign)
  let[@inline] make v = make_sign v false
  let[@inline] equal (x:t) (y:t) : bool = x=y
  let[@inline] compare (x:t) (y:t) : int = compare x y
  let[@inline] not x = x lxor 1
  let[@inline] xor x b = x lxor (__int_of_bool b)
  let[@inline] sign x = (x land 1) <> 0
  let[@inline] var x = Var.Internal.of_int (x lsr 1)
  let[@inline] to_int x = x
  let[@inline] to_int_a x = x
  let undef = -2
  let error = -1
  let[@inline] is_undef c = equal undef c
  module Internal = struct
    let[@inline] of_int x = x
    let[@inline] of_int_a x = x
  end
end

module Lbool : sig
  type t = private int
  val true_ : t
  val false_ : t
  val undef : t

  val of_int : int -> t
  val of_bool : bool -> t
  val equal : t -> t -> bool
  val not : t -> t
  val xor : t -> bool -> t
  val (&&&) : t -> t -> t
  val (|||) : t -> t -> t
  val to_int : t -> int
  val to_string : t -> string
end = struct
  type t = int

  let true_ = 0
  let false_ = 1
  let undef = 2

  let of_int x = x
  let[@inline] of_bool x = __int_of_bool (not x)
  let[@inline] not x = x lxor 1
  let[@inline] xor x b = x lxor (__int_of_bool b)
  let[@inline] to_int x = x

  let[@inline] equal (x:t) (y:t) : bool =
    (((x land 2) land (y land 2)) lor
     ((__int_of_bool ((x land 2) = 0)) land (__int_of_bool (x=y)))
    ) <> 0

  let (&&&) (x:t) y : t =
    let sel = (x lsl 1) lor (y lsl 3) in
    let v = (0xF7F755F4 lsr sel) land 3 in
    v
  let (|||) (x:t) y : t =
    let sel = (x lsl 1) lor (y lsl 3) in
    let v = (0xFCFCF400 lsr sel) land 3 in
    v

  let to_string x =
    if equal x true_ then "true" else if equal x false_ then "false"
    else if equal x undef then "undef" else assert false
end

(*$= & ~cmp:Lbool.equal ~printer:Lbool.to_string
  Lbool.undef Lbool.(not undef)
  Lbool.true_ Lbool.true_
  Lbool.false_ Lbool.false_
  Lbool.(true_ &&& true_) Lbool.true_
  Lbool.(true_ &&& false_) Lbool.false_
  Lbool.(false_ &&& false_) Lbool.false_
  Lbool.(false_ &&& true_) Lbool.false_
  Lbool.(undef &&& true_) Lbool.undef
  Lbool.(undef &&& undef) Lbool.undef
  Lbool.(false_ &&& undef) Lbool.false_
  Lbool.(false_ ||| true_) Lbool.true_
  Lbool.(true_ ||| true_) Lbool.true_
  Lbool.(true_ ||| false_) Lbool.true_
  Lbool.(false_ ||| false_) Lbool.false_
  Lbool.(false_ ||| undef) Lbool.undef
  Lbool.(undef ||| undef) Lbool.undef
  Lbool.(of_bool true) Lbool.true_
  Lbool.(of_bool false) Lbool.false_
  Lbool.(not true_) Lbool.false_
  Lbool.(not false_) Lbool.true_
  Lbool.(not undef) Lbool.undef
*)

(*$T
  not Lbool.(equal true_ undef)
  not Lbool.(equal false_ undef)
  not Lbool.(equal true_ false_)
*)

module Cref : sig
  type t = int
  val undef : t
  val is_undef : t -> bool
end = struct
  type t = int
  let undef : t = max_int
  let is_undef c = c=undef
end

exception Early_return_true
exception Early_return_false

module Clause : sig
  module Alloc : sig
    type t
    val make : ?start:int -> unit -> t 
    val wasted : t -> int
    val alloc : t -> Lit.t Vec.t -> learnt:bool -> Cref.t
    val free : t -> Cref.t -> unit
    val move_to : t -> into:t -> unit
  end

  module Header : sig
    type t [@@ocaml.immediate]
    val mark : t -> int
    val set_mark : int -> t -> t
    val learnt : t -> bool
    val has_extra : t -> bool
    val reloced : t -> bool
    val size : t -> int
  end

  val header : Alloc.t -> Cref.t -> Header.t
  val set_header : Alloc.t -> Cref.t -> Header.t -> unit
  val size : Alloc.t -> Cref.t -> int
  val lit : Alloc.t -> Cref.t -> int -> Lit.t
  val activity : Alloc.t -> Cref.t -> float
  val set_activity : Alloc.t -> Cref.t -> float -> unit
  val mark : Alloc.t -> Cref.t -> int
  val set_mark : Alloc.t -> Cref.t -> int -> unit (* 2 bits *)

  val lits_a : Alloc.t -> Cref.t -> Lit.t array
  val swap_lits: Alloc.t -> Cref.t -> int -> int -> unit
  val exists : Alloc.t -> Cref.t -> (Lit.t -> bool) -> bool
  val for_all : Alloc.t -> Cref.t -> (Lit.t -> bool) -> bool

  val reloced : Alloc.t -> Cref.t -> bool
  val relocation : Alloc.t -> Cref.t -> Cref.t
  val relocate : Alloc.t -> Cref.t -> into:Cref.t -> unit
end = struct
  (* we imitate Minisat's allocator, but cannot fit both integers and floats
     in a single array.
     Therefore we have a separate array for the activity. *)
  type alloc = {
    mutable memory: int array; (* large array *)
    mutable sz: int;
    mutable act: float array;
    mutable sz_act: int;
    mutable wasted: int;
    mutable extra_clause_field: bool;
  }

  module Header = struct
    (* layout:
       field      size     offset
       mark:      2        30 .. 32
       learnt:    1        29 .. 30
       has_extra: 1        28 .. 29
       reloced:   1        27 .. 28
       size:      27       0  .. 27
    *)
    type t = int

    let () = assert (Sys.word_size >= 32)

    let m_mark = 3 lsr 30
    let m_size = (1 lsl 27) - 1
    let m_has_extra = 1 lsl 28

    let[@inline] make size = assert (size<m_size); size
    let[@inline] mark (h:t)  = (h lsl 30) land 3
    let[@inline] set_mark h b = assert (b = b land 3); (h land (lnot m_mark)) lor (b lsr 30)
    let[@inline] learnt (h:t) : bool = ((h lsr 29) land 1) <> 0
    let[@inline] has_extra h = ((h lsr 28) land 1) <> 0
    let[@inline] set_has_extra h = h lor (1 lsl 28)
    let[@inline] reloced h = ((h lsr 27) land 1) <> 0
    let[@inline] set_reloced h = h lor (1 lsl 27)
    let[@inline] make_ ~learnt size = assert (size<m_size); if learnt then size lor (1 lsl 29) else size
    let[@inline] size (h:t) : int = h land m_size
    let[@inline] set_size h sz : t = (h land (lnot m_size)) lor sz
  end

  module Alloc = struct
    type t = alloc

    let make ?(start=1024 * 1024) () : t =
      { memory=Array.make start (-1);
        sz=0;
        act= [| |]; sz_act=0;
        wasted=0;
        extra_clause_field=false;
      }

    let[@inline] size self = self.sz
    let[@inline] wasted self = self.wasted

    let[@inline] cap_act a = Array.length a.act

    (* ensure that capacity is at least [min_cap] *)
    let ensure_ self (min_cap:int) : unit =
      if Array.length self.memory < min_cap then (
        let prev_cap = Array.length self.memory in
        let cap = ref prev_cap in
        while !cap < min_cap do
          (* NOTE: Multiply by a factor (13/8) without causing overflow, then add 2 and make the
             result even by clearing the least significant bit. The resulting sequence of capacities
             is carefully chosen to hit a maximum capacity that is close to the '2^32-1' limit when
             using 'uint32_t' as indices so that as much as possible of this space can be used.
          *)
          let delta = ((!cap lsr 1) + (!cap lsr 3) + 2) land (lnot 1) in
          cap := !cap + delta;

          if !cap <= prev_cap then raise Out_of_memory; (* overflow *)
        done;
      )

    (* ensure activity array is at least [min_cap] *)
    let ensure_act_cap_ a (min_cap:int) =
      let cap = cap_act a in
      if cap < min_cap then (
        let new_cap = min Sys.max_array_length (max min_cap (cap + 2 + cap lsr 1)) in
        if new_cap < min_cap then raise_notrace Out_of_memory; (* too big *)
        let new_act = Array.create_float new_cap in
        Array.blit new_act 0 a.act 0 a.sz_act;
        a.act <- new_act;
      )

    (* abstraction of the literals *)
    let compute_abstraction_ lits : int =
      let abs = ref 0 in
      Vec.iteri
        (fun _ lit ->
          abs := !abs lor (1 lsl ((Lit.var lit:Var.t :>int) land 31)))
        lits;
      !abs

    let alloc self (lits:Lit.t Vec.t) ~learnt : Cref.t =
      let use_extra = self.extra_clause_field || learnt in
      let size = Vec.size lits in
      let len = 1 + size + (__int_of_bool use_extra) in
      ensure_ self (self.sz + len);
      let cr = self.sz in
      self.sz <- self.sz + len;
      let header = Header.make_ ~learnt size in
      (* copy header and lits *)
      self.memory.(cr) <- header;
      Vec.iteri (fun i lit -> self.memory.(cr+i+1) <- (lit:Lit.t:>int)) lits;
      if use_extra then (
        let extra =
          if learnt then (
            (* allocate index in [self.act], set it to [0.], return the index *)
            ensure_act_cap_ self (self.sz_act+1);
            let act_idx = self.sz_act in
            self.sz_act <- 1 + self.sz_act;
            self.act.(act_idx) <- 0.;
            act_idx
          ) else (
            compute_abstraction_ lits
          )
        in
        self.memory.(1+size) <- extra;
      );
      cr

    let[@inline] free_ self size : unit = self.wasted <- size + self.wasted

    let free self (c:Cref.t) : unit =
      let h = self.memory.(c) in
      let size = 1 + Header.size h + (__int_of_bool (Header.has_extra h)) in
      free_ self size

    let move_to self ~into : unit =
      into.memory <- self.memory;
      into.sz <- self.sz;
      into.wasted <- self.wasted;
      self.memory <- [| |];
      self.wasted <- 0;
      self.sz <- 0;
      ()
  end

  (** A clause.
      The clause is actually an offset in the allocator, pointing to a slice of
      integers with the following layout:

      Cref.t
        |
        v
      [header; lit0; lit1; …; lit_{size-1}; (abstraction | activity_index)?]

      where the last slot is there only if [header.has_extra] and is
      either an offset in alloc.act if [header.learnt], or
      an abtraction of the clause literals otherwise
  *)

  let[@inline] header (a:alloc) (r:Cref.t) : Header.t = a.memory.(r)
  let[@inline] set_header (a:alloc) (r:Cref.t) (h:Header.t) : unit = a.memory.(r) <- h
  let[@inline] size a c = Header.size (header a c)

  let[@inline] get_data_ a (c:Cref.t) (i:int) : int = a.memory.(c+1+i)
  let[@inline] set_data_ a (c:Cref.t) (i:int) (x:int) : unit = a.memory.(c+1+i) <- x

  let extra_data_ a c : int =
    let h = header a c in
    assert (Header.has_extra h);
    get_data_ a c (Header.size h)

  let[@inline] lit a (c:Cref.t) i : Lit.t = Lit.Internal.of_int (get_data_ a c i)

  let activity a (c:Cref.t): float =
    let idx = extra_data_ a c in
    a.act.(idx)

  let set_activity a (c:Cref.t) (f:float) : unit =
    let idx = extra_data_ a c in
    a.act.(idx) <- f

  let[@inline] mark a c : int = Header.mark (header a c)
  let set_mark a c m : unit =
    let h = header a c in
    set_header a c (Header.set_mark m h)

  (* obtain literals *)
  let lits_a a c =
    let h = header a c in
    Lit.Internal.of_int_a (Array.sub a.memory (c+1) (Header.size h))

  let swap_lits a c i j =
    if i<>j then (
      let tmp = get_data_ a c i in
      set_data_ a c i (get_data_ a c j);
      set_data_ a c j tmp;
    )

  let[@specialise] for_all a c f : bool =
    let h = header a c in
    try
      for i = 0 to Header.size h - 1 do
        if not (f (lit a c i)) then raise_notrace Early_return_false
      done;
      true
    with Early_return_false -> false

  let[@specialise] exists a c f : bool =
    let h = header a c in
    try
      for i = 0 to Header.size h - 1 do
        if f (lit a c i) then raise_notrace Early_return_true
      done;
      false
    with Early_return_true -> true

  let[@inline] reloced a c = Header.reloced (header a c)
  let[@inline] relocation a c : Cref.t = get_data_ a c 0
  let relocate a c ~into : unit =
    set_header a c (Header.set_reloced (header a c));
    set_data_ a c 0 into (* first lit --> into *)
end

(*$inject
  module CH = Clause.Header
*)

(*$R
  let h = CH.make 4 in
  assert_equal 4 (CH.size h);
  assert_bool "not learnt" (not @@ CH.learnt h);
  *)

(*$R
  let h = CH.make_learnt 6 in
  assert_equal 6 (CH.size h);
  assert_bool "learnt" (CH.learnt h);
  *)
