
module Dimacs = Dimacs
module System = System

let setup_gc() =
  let g = Gc.get () in
  g.Gc.space_overhead <- 3_000; (* major gc *)
  g.Gc.max_overhead <- 10_000; (* compaction *)
  g.Gc.minor_heap_size <- 500_000; (* ×8 to obtain bytes on 64 bits -->  *)
  Gc.set g

