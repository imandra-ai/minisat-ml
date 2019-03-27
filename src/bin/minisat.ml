
open Minisat_ml.Solver_types

module Solver = Minisat_ml.Solver
module System = Minisat_ml_util.System
module D = Minisat_ml_util.Dimacs

let print_stats (_solver:Solver.t) : unit =
  let cpu_time = System.cpu_time () in
  let mem_used = System.mem_used_peak () in
  (* TODO
  Printf.printf("restarts              : %"PRIu64"\n" (Solver.n_starts solver);
  Printf.printf("conflicts             : %-12"PRIu64"   (%.0f /sec)\n", solver.conflicts   , solver.conflicts   /cpu_time);
  Printf.printf("decisions             : %-12"PRIu64"   (%4.2f %% random) (%.0f /sec)\n", solver.decisions, (float)solver.rnd_decisions*100 / (float)solver.decisions, solver.decisions   /cpu_time);
  Printf.printf("propagations          : %-12"PRIu64"   (%.0f /sec)\n", solver.propagations, solver.propagations/cpu_time);
  Printf.printf("conflict literals     : %-12"PRIu64"   (%4.2f %% deleted)\n", solver.tot_literals, (solver.max_literals - solver.tot_literals)*100 / (double)solver.max_literals);
     *)
  if mem_used <> 0. then Printf.printf "Memory used           : %.2f MB\n" mem_used;
  Printf.printf "CPU time              : %g s\n" cpu_time;
  ()

let process_ic_ ~verb ic =
  let dparse = D.make ic in
  let solver = Solver.create() in
  Solver.set_verbosity solver verb;
  D.parse_dimacs dparse solver;
  let res = Solver.solve solver ~assumps:(Vec.make()) in
  print_endline (if res then "SAT" else "UNSAT");
  ()

let process_stdin ~verb () : unit =
  process_ic_ ~verb stdin

let process_file ~verb (file:string) : unit =
  let ic = open_in file in
  try process_ic_ ~verb ic; close_in ic
  with e -> close_in_noerr ic; raise e

let () =
  let verb = ref 1 in
  let cpu_lim = ref ~-1 in
  let mem_lim = ref ~-1 in
  let files = ref [] in
  let opts = [
    "-verb", Arg.Set_int verb, " verbosity (0,1,2)";
    "-cpu-lim", Arg.Set_int cpu_lim, " cpu limit (in seconds)";
    "-mem-lim", Arg.Set_int mem_lim, " memory limit (in MB)";
  ] |> Arg.align
  in
  Arg.parse opts (fun f -> files := f :: !files) "usage: minisat [opt]* <file>";
  Minisat_ml_util.setup_gc();
  (* TODO: setup mem/cpu limit using the GC *)
  if !files=[] then process_stdin ~verb:!verb ()
  else List.iter (process_file ~verb:!verb) @@ List.rev !files;
  ()
