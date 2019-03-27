
type t

val make : cmp:(int -> int -> int) -> t

val empty : t -> bool
val in_heap : t -> int -> bool
val get : t -> int -> int

val increase : t -> int -> unit
val decrease : t -> int -> unit
val insert : t -> int -> unit

val update : t -> int -> unit (** safe variant of insert/decrease/increase *)

val remove_min : t -> int

(** rebuild from scratch from the given vector *)
val build : t -> int Minisat_vec.t -> unit

val clear : t -> unit
val clear_dealloc : t -> unit
