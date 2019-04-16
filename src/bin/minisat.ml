(* Minisat-ml, adapted from Minisat by Simon Cruanes <simon@imandra.ai>
   Copyright (c) 2019-2019, Aesthetic Integration (https://imandra.ai)
*)

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

open Minisat_ml.Solver_types
module Solver = Minisat_ml.Solver
module System = Minisat_ml_util.System
module D = Minisat_ml_util.Dimacs

let print_stats (solver : Solver.t) : unit =
  let cpu_time = System.cpu_time () in
  let mem_used = System.mem_used_peak () in
  Printf.printf "restarts              : %d\n" (Solver.n_starts solver);
  Printf.printf "conflicts             : %-12d   (%.0f /sec)\n"
    (Solver.n_conflicts solver)
    (float (Solver.n_conflicts solver) /. cpu_time);
  Printf.printf
    "decisions             : %-12d   (%4.2f %% random) (%.0f /sec)\n"
    (Solver.n_decisions solver)
    ( float (Solver.n_rnd_decisions solver)
    *. 100.
    /. float (Solver.n_decisions solver) )
    (float (Solver.n_decisions solver) /. cpu_time);
  Printf.printf "propagations          : %-12d   (%.0f /sec)\n"
    (Solver.n_propagations solver)
    (float (Solver.n_propagations solver) /. cpu_time);
  Printf.printf "conflict literals     : %-12d   (%4.2f %% deleted)\n"
    (Solver.n_tot_literals solver)
    ( float (Solver.n_max_literals solver - Solver.n_tot_literals solver)
    *. 100.
    /. float (Solver.n_max_literals solver) );
  if mem_used <> 0. then
    Printf.printf "Memory used           : %.2f MB\n" mem_used;
  Printf.printf "CPU time              : %g s\n" cpu_time;
  ()

let process_ ~verb ~ccmin (dparse : D.t) =
  let initial_time = System.cpu_time () in
  let solver = Solver.create () in
  Solver.set_verbosity solver verb;
  Solver.set_ccmin_mode solver ccmin;
  D.parse_dimacs dparse solver;
  if verb > 0 then (
    Printf.printf
      "============================[ Problem Statistics \
       ]=============================\n";
    Printf.printf
      "|                                                                             \
       |\n";
    Printf.printf
      "|  Number of variables:  %12d                                         |\n"
      (Solver.n_vars solver);
    Printf.printf
      "|  Number of clauses:    %12d                                         |\n"
      (Solver.n_clauses solver);
    let parsed_time = System.cpu_time () in
    Printf.printf
      "|  Parse time:           %12.2f \
       s                                       |\n"
      (parsed_time -. initial_time);
    Printf.printf
      "|                                                                             \
       |\n";
    Printf.printf
      "===============================================================================\n\
       %!" );
  let res = Solver.solve solver ~assumps:(Vec.make ()) in
  print_stats solver;
  print_endline (if res then "SAT" else "UNSAT");
  ()

let process_ic_ ~verb ~ccmin ic = process_ ~verb ~ccmin (D.make_chan ic)

let process_stdin ~verb ~ccmin () : unit = process_ic_ ~verb ~ccmin stdin

let process_gzip_ic_ ~verb ~ccmin ic =
  let gic = Gzip.open_in_chan ic in
  let p = D.make ~refill:(Gzip.input gic) in
  process_ ~verb ~ccmin p

let process_file ~verb ~ccmin (file : string) : unit =
  let ic = open_in file in
  try
    let r =
      if
        String.length file >= 3
        && String.sub file (String.length file - 3) 3 = ".gz"
      then process_gzip_ic_ ~verb ~ccmin ic
      else process_ic_ ~verb ~ccmin ic
    in
    close_in ic;
    r
  with e ->
    close_in_noerr ic;
    raise e

let () =
  let verb = ref 1 in
  let cpu_lim = ref ~-1 in
  let mem_lim = ref ~-1 in
  let ccmin = ref 2 in
  let files = ref [] in
  let opts =
    [ "-verb", Arg.Set_int verb, " verbosity (0,1,2)";
      "-cpu-lim", Arg.Set_int cpu_lim, " cpu limit (in seconds)";
      "-mem-lim", Arg.Set_int mem_lim, " memory limit (in MB)";
      "-ccmin-mode", Arg.Set_int ccmin, " clause minimization mode (0,1,2)"
    ]
    |> Arg.align
  in
  Arg.parse opts (fun f -> files := f :: !files) "usage: minisat [opt]* <file>";
  (*   Minisat_ml_util.setup_gc(); *)
  (* TODO: setup mem/cpu limit using the GC *)
  if !files = [] then process_stdin ~verb:!verb ~ccmin:!ccmin ()
  else List.iter (process_file ~verb:!verb ~ccmin:!ccmin) @@ List.rev !files;
  ()
