
(* magic for casting from bool to int *)
external __int_of_bool : bool -> int = "%identity"

module Vec = Minisat_vec

module Var : sig
  type t = private int [@@ocaml.immediate]

  val make : int -> t
  val undef : t
  val to_int : t -> int

  (**/**)
  val pad : t
  val __make_unsafe : int -> t
  (**/**)
end = struct
  type t = int
  let[@inline] __make_unsafe x = x
  let[@inline] make x = assert (x>=0); x
  let[@inline] to_int x = x
  let undef = -1
  let pad = undef
end

module Lit : sig
  type t = private int

  val make_sign : Var.t -> bool -> t
  val make : Var.t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val not : t -> t
  val xor : t -> bool -> t
  val sign : t -> bool
  val var : t -> Var.t
  val to_int : t -> int
  val undef : t
  val error : t

  (**/**)
  val __make_unsafe : int -> t
  val pad : t
  (**/**)
end = struct
  type t = int
  let[@inline] __make_unsafe x = x
  let[@inline] make_sign v sign = let v = (v:Var.t:>int) in v + v + (__int_of_bool sign)
  let[@inline] make v = make_sign v false
  let[@inline] equal (x:t) (y:t) : bool = x=y
  let[@inline] compare (x:t) (y:t) : int = compare x y
  let[@inline] not x = x lxor 1
  let[@inline] xor x b = x lxor (__int_of_bool b)
  let[@inline] sign x = (x land 1) <> 0
  let[@inline] var x = Var.__make_unsafe (x lsr 1)
  let[@inline] to_int x = x
  let undef = -2
  let error = -1
  let pad = undef
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
  (**/**)
  val pad : t
  (**/**)
end = struct
  type t = int

  let true_ = 0
  let false_ = 1
  let undef = 2
  let pad = undef

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

module Clause : sig
  module Alloc : sig
    type t
    val make : ?start:int -> unit -> t 
    val wasted : t -> int

    val move_to : t -> into:t -> unit
  end

  type cref = private int
  val undef : cref

  module Header : sig
    type t [@@ocaml.immediate]
    val make : int -> t (* from size *)
    val make_learnt : int -> t
    val mark : t -> int
    val learnt : t -> bool
    val has_extra : t -> bool
    val reloced : t -> bool
    val size : t -> int
  end

  type t = private {
    c_h: Header.t;
    c_alloc: Alloc.t;
    c_r: cref;
  }

  val get : Alloc.t -> cref -> t
end = struct
  type cref = int

  let undef : cref = max_int

  (* we imitate Minisat's allocator, but cannot fit both integers and floats
     in a single array.
     Therefore we have a separate array for the activity. *)
  type alloc = {
    mutable memory: int array; (* large array *)
    mutable sz: int;
    mutable act: float array;
    mutable sz_act: int;
    mutable wasted: int;
  }

  module Alloc = struct
    type t = alloc

    let make ?(start=1024 * 1024) () : t =
      { memory=Array.make start (-1);
        sz=0;
        act= [| |]; sz_act=0;
        wasted=0;
      }

    let[@inline] size self = self.sz
    let[@inline] wasted self = self.wasted

    let[@inline] cap_act a = Array.length a.act

    let ensure_act_cap a (min_cap:int) =
      let cap = cap_act a in
      if cap < min_cap then (
        let new_cap = min Sys.max_array_length (max min_cap (cap + 2 + cap lsr 1)) in
        if new_cap < min_cap then raise_notrace Out_of_memory; (* too big *)
        let new_act = Array.create_float new_cap in
        Array.blit new_act 0 a.act 0 a.sz_act;
        a.act <- new_act;
      )

    let ensure self (min_cap:int) : unit =
      assert false (* TODO *)

    (* TODO: ensure_act *)

    let alloc self size : cref = assert false (* TODO *)

    let[@inline] free self size : unit = self.wasted <- size + self.wasted

    let move_to self ~into : unit =
      into.memory <- self.memory;
      into.sz <- self.sz;
      into.wasted <- self.wasted;
      self.memory <- [| |];
      self.wasted <- 0;
      self.sz <- 0;
      ()
  end

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
    let[@inline] mark (h:t)  = h lsl 30
    let[@inline] set_mark h b = assert (b = b land 3); (h land (lnot m_mark)) lor (b lsr 30)
    let[@inline] learnt (h:t) : bool = ((h lsr 29) land 1) <> 0
    let[@inline] has_extra h = ((h lsr 28) land 1) <> 0
    let[@inline] set_has_extra h = h lor (1 lsl 28)
    let[@inline] reloced h = ((h lsr 27) land 1) <> 0
    let[@inline] set_reloced h = h lor (1 lsl 27)
    let[@inline] make_learnt size = assert (size<m_size); size lor (1 lsl 29)
    let[@inline] size (h:t) : int = h land m_size
    let[@inline] set_size h sz : t = (h land (lnot m_size)) lor sz
  end

  (** A clause.

      The clause is actually an offset in the allocator, pointing to a slice of
      integers with the following layout:

      cref
       v
      [header; lit0; lit1; …; lit_{size-1}; (abstraction | activity_index)?]

      where the last slot is there only if [header.has_extra] and is
      either an offset in alloc.act if [header.learnt], or
      an abtraction of the clause literals otherwise
  *)
  type t = {
    c_h: Header.t;
    c_alloc: alloc;
    c_r: cref; (* just a temporary alias anyway *)
  }

  let[@inline] header (a:alloc) (r:cref) : Header.t = a.memory.(r)
  let[@inline] set_header (a:alloc) (r:cref) (h:Header.t) : unit = a.memory.(r) <- h

  let[@inline] get (c_alloc:alloc) (c_r:cref) : t =
    {c_alloc; c_r; c_h=header c_alloc c_r}

  let[@inline] get_data_ alloc (c_r:cref) (i:int) : int =
    Array.get alloc.memory (c_r+1+i)

  let[@inline] set_data_ alloc (c_r:cref) (i:int) (x:int) : unit =
    Array.set alloc.memory (c_r+1+i) x

  let[@inline] get_lit_ c (i:int) : Lit.t = Lit.__make_unsafe (get_data_ c.c_alloc c.c_r i)

  (* abstraction of the literals *)
  let compute_abstraction (c:t) : int =
    let abs = ref 0 in
    for i = 0 to Header.size c.c_h do
      abs := !abs lor (1 lsl ((Lit.var (get_lit_ c i):Var.t :>int) land 31));
    done;
    !abs

  (* allocate a slot for the activity of a clause, and returns the index in [a.acts] *)
  let alloc_act (a:alloc) : int =
    if a.sz_act = Array.length a.act then Alloc.ensure_act_cap a (a.sz_act+1);
    assert (Alloc.cap_act a > a.sz_act);
    let i = a.sz_act in
    a.act.(i) <- 0.;
    a.sz_act <- 1 + i;
    i

  let[@inline] reloced c = Header.reloced c.c_h
  let relocation c : cref = get_data_ c.c_alloc c.c_r 0
  let relocate {c_h; c_r; c_alloc} (into:cref) : unit =
    set_header c_alloc c_r (Header.set_reloced c_h);
    set_data_ c_alloc c_r 0 into (* first lit --> into *)
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
