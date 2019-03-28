
type 'a t

val make : unit -> 'a t
val make_with : int -> 'a -> 'a t

val empty : _ t -> bool
val size : _ t -> int
val capacity : _ t -> int
val ensure : 'a t -> int -> 'a -> unit
val grow_to : 'a t -> int -> 'a -> unit
val grow_to_with : 'a t -> int -> (int -> 'a) -> unit

val shrink : _ t -> int -> unit
val push : 'a t -> 'a -> unit
val pop : _ t -> unit
val last : 'a t -> 'a

val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit

val clear : 'a t -> unit
val clear_dealloc : 'a t -> unit

val copy_to : 'a t -> into:'a t -> unit
val move_to : 'a t -> into:'a t -> unit

val blit : 'a t -> int -> 'a t -> int -> int -> unit

val iteri : (int -> 'a -> unit) -> 'a t -> unit

module Internal : sig
  val data : 'a t -> 'a array
end
