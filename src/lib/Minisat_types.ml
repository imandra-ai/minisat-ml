
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

(*$T
  Lit.is_undef Lit.undef
*)

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

(*
  let[@inline] equal (x:t) (y:t) : bool =
    (((x land 2) land (y land 2)) lor
     ((__int_of_bool ((x land 2) = 0)) land (__int_of_bool (x=y)))
    ) <> 0
*)

  let[@inline] equal (x:t) (y:t) : bool =
    if x=(x land 1) then x=y else x land 2 = y land 2

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

exception Early_return_true
exception Early_return_false

module Clause : sig
  module Header : sig
    type t [@@ocaml.immediate]
    val mark : t -> int
    val set_mark : int -> t -> t
    val learnt : t -> bool
    val has_extra : t -> bool
    val reloced : t -> bool
    val size : t -> int

    val make_ : learnt:bool -> int -> t
  end

  type t = private {
    mutable header: Header.t;
    mutable offset: int; (* in the allocator *)
    mutable activity: float; (* only useful for learnt clauses *)
  }
  type clause = t

  module Alloc : sig
    type t
    val make : ?start:int -> unit -> t 
    val wasted : t -> int
    val size : t -> int
    val alloc : t -> Lit.t Vec.t -> learnt:bool -> clause
    val free : t -> clause -> unit
    val move_to : t -> into:t -> unit
  end

  val undef : t
  val is_undef : t -> bool

  val header : t -> Header.t
  val set_header : t -> Header.t -> unit
  val size : t -> int
  val lit : Alloc.t -> t -> int -> Lit.t
  val learnt : t -> bool
  val activity : t -> float
  val set_activity : t -> float -> unit
  val mark : t -> int
  val set_mark : t -> int -> unit (* 2 bits *)

  val lits_a : Alloc.t -> t -> Lit.t array
  val swap_lits: Alloc.t -> t -> int -> int -> unit
  val exists : Alloc.t -> t -> (Lit.t -> bool) -> bool
  val for_all : Alloc.t -> t -> (Lit.t -> bool) -> bool

  val reloced : t -> bool
  val reloc : Alloc.t -> t -> into:Alloc.t -> unit
  val reset_reloced : t -> unit
end = struct
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
    let[@inline] mark (h:t)  = (h lsr 30) land 3
    let[@inline] set_mark (h:t) b = assert (b = b land 3); (h land (lnot m_mark)) lor (b lsl 30)
    let[@inline] learnt (h:t) : bool = ((h lsr 29) land 1) <> 0
    let[@inline] has_extra h = ((h lsr 28) land 1) <> 0
    let[@inline] set_has_extra h = h lor (1 lsl 28)
    let[@inline] reloced h = ((h lsr 27) land 1) <> 0
    let[@inline] set_reloced h = h lor (1 lsl 27)
    let[@inline] set_not_reloced h = h land (lnot (1 lsl 27))
    let[@inline] make_ ~learnt size = assert (size<m_size); if learnt then size lor (1 lsl 29) else size
    let[@inline] size (h:t) : int = h land m_size
    let[@inline] set_size h sz : t = (h land (lnot m_size)) lor sz
  end

  (** A clause.
      The clause is a header+activity, and an offset in the allocator,
      pointing to a slice of integers in [allocator.memory] with the following
      layout:

       t
         |
         v
      [lit0; lit1; …; lit_{size-1}; abstraction?]

      where the last slot is there only if [header.has_extra] and is
      an abtraction of the clause literals.
  *)
  type t = {
    mutable header: Header.t;
    mutable offset: int; (* in the allocator *)
    mutable activity: float; (* only useful for learnt clauses *)
  }

  (* we imitate Minisat's allocator, but with a proper type for clauses
     as it's going to save a ton of bound checks and accesses.
     Besides we can't put activity in the allocator itself *)
  type alloc = {
    mutable memory: int array; (* large array *)
    mutable sz: int;
    mutable wasted: int;
    mutable extra_clause_field: bool;
  }
  type clause = t

  module Alloc = struct
    type t = alloc

    let make ?(start=1024 * 1024) () : t =
      { memory=Array.make start (-1);
        sz=0;
        wasted=0;
        extra_clause_field=false;
      }

    let[@inline] size self = self.sz
    let[@inline] wasted self = self.wasted

    (* ensure that capacity is at least [min_cap] *)
    let ensure_ self (min_cap:int) : unit =
      if Array.length self.memory < min_cap then (
        let prev_cap = Array.length self.memory in
        (*Printf.printf "clause.alloc.ensure %d (cap %d)\n" min_cap prev_cap; *)
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

          (* realloc now *)
          let memory = Array.make !cap ~-1 in
          Array.blit self.memory 0 memory 0 self.sz;
          self.memory <- memory;
        done;
      )

    (* abstraction of the literals *)
    let compute_abstraction_ lits offset size : int =
      let abs = ref 0 in
      for i = 0 to size-1 do
        let lit = lits.(offset+i) in
        abs := !abs lor (1 lsl ((Lit.var lit:Var.t :>int) land 31))
      done;
      !abs

    let alloc_ self (lits:Lit.t array) lits_offset (size:int) ~learnt : clause =
      let use_extra = not learnt && self.extra_clause_field in
      let len = 1 + size + (__int_of_bool use_extra) in
      ensure_ self (self.sz + len);
      (*Printf.printf "alloc lits (len %d) offset %d size %d (self: len %d sz %d)\n"
        (Array.length lits) offset size (Array.length self.memory) self.sz;*)
      let offset = self.sz in
      self.sz <- self.sz + len;
      let header = Header.make_ ~learnt size in
      let header = if use_extra then Header.set_has_extra header else header in
      (* copy lits *)
      Array.blit (Lit.to_int_a lits) lits_offset self.memory offset size;
      if use_extra then (
        assert (not learnt);
        let extra = compute_abstraction_ lits lits_offset size in
        self.memory.(offset+size) <- extra;
      );
      { offset; header; activity=0.}

    let alloc self (lits:Lit.t Vec.t) ~learnt : clause =
      alloc_ self (Vec.Internal.data lits) 0 (Vec.size lits) ~learnt

    let[@inline] free_ self size : unit = self.wasted <- size + self.wasted

    let free self (c:clause) : unit =
      let size = Header.size c.header + (__int_of_bool (Header.has_extra c.header)) in
      free_ self size

    let move_to self ~into : unit =
      (*Printf.printf "ca.move_to\n";*)
      into.memory <- self.memory;
      into.sz <- self.sz;
      into.wasted <- self.wasted;
      self.memory <- [| |];
      self.wasted <- 0;
      self.sz <- 0;
      ()
  end

  let undef = {header=Header.make_ ~learnt:false 0; offset=max_int; activity=nan}
  let[@inline] is_undef c = c == undef

  let[@inline] header (c:t) : Header.t = c.header
  let[@inline] set_header (c:t) (h:Header.t) : unit = c.header <- h
  let[@inline] size c = Header.size c.header
  let[@inline] learnt c = Header.learnt c.header

  let[@inline] get_data_ a (c:t) (i:int) : int = Array.unsafe_get a.memory (c.offset+i)
  let[@inline] set_data_ a (c:t) (i:int) (x:int) : unit = Array.unsafe_set a.memory (c.offset+i) x

  let extra_data_ a c : int =
    assert (Header.has_extra c.header);
    get_data_ a c (Header.size c.header)

  let[@inline] lit a (c:t) i : Lit.t = Lit.Internal.of_int (get_data_ a c i)

  let[@inline] activity (c:t): float = c.activity
  let[@inline] set_activity (c:t) (f:float) : unit = c.activity <- f

  let[@inline] mark c : int = Header.mark (header c)
  let[@inline] set_mark c m : unit = c.header <- Header.set_mark c.header m

  (* obtain literals *)
  let lits_a a c =
    Lit.Internal.of_int_a (Array.sub a.memory c.offset (size c))

  let swap_lits a c i j =
    if i<>j then (
      let tmp = get_data_ a c i in
      set_data_ a c i (get_data_ a c j);
      set_data_ a c j tmp;
    )

  let[@specialise] for_all a c f : bool =
    try
      for i = 0 to Header.size c.header - 1 do
        if not (f (lit a c i)) then raise_notrace Early_return_false
      done;
      true
    with Early_return_false -> false

  let[@specialise] exists a c f : bool =
    try
      for i = 0 to Header.size c.header - 1 do
        if f (lit a c i) then raise_notrace Early_return_true
      done;
      false
    with Early_return_true -> true

  let[@inline] reloced c = Header.reloced c.header
  let set_reloced c = set_header c (Header.set_reloced c.header)
  let reset_reloced c = set_header c (Header.set_not_reloced c.header)

  let reloc (a:Alloc.t) (c:t) ~into : unit =
    if not (Header.reloced c.header) then (
      (* allocate copy of [c] *)
      (*Printf.printf "reloc c%d [len %d]\n" c (Header.size h);*)
      let c2: t =
        Alloc.alloc_ into
          (Lit.Internal.of_int_a a.memory) c.offset (Header.size c.header)
          ~learnt:(Header.learnt c.header) in
      set_reloced c;
      c.offset <- c2.offset;
      (* TODO: check that if [has_extra h] then abstraction is recomputed properly *)
      ()
    )
end

(*$inject
  module CH = Clause.Header
*)

(*$R
  let h = CH.make_ ~learnt:false 4 in
  assert_equal 4 (CH.size h);
  assert_bool "not learnt" (not @@ CH.learnt h);
  *)

(*$R
  let h = CH.make_ ~learnt:true 6 in
  assert_equal 6 (CH.size h);
  assert_bool "learnt" (CH.learnt h);
  *)
