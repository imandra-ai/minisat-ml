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
*)

let[@inline] cpu_time () : float = Sys.time ()

let file_read_stat = Printf.sprintf "/proc/%d/statm" (Unix.getpid ())

let mem_read_stat (field : int) : int =
  let field = ref field in
  let res = ref 0 in
  try
    let ic = open_in_bin file_read_stat in
    let b = Scanf.Scanning.from_channel ic in
    while !field >= 0 do
      Scanf.bscanf b "%d" (fun v -> res := v);
      decr field
    done;
    close_in ic;
    !res
  with e ->
    print_endline ("unable to read memory stats: " ^ Printexc.to_string e);
    0

let file_read_peak = Printf.sprintf "/proc/%d/status" (Unix.getpid ())

let mem_read_peak () : int = 0

(* FIXME
  let res = ref 0 in
  try
    let ic = open_in file_read_peak in
    while true do
      try (Scanf.fscanf[@warning "-3"]) ic "VmPeak: %d kB\n"
            (fun x -> res := x; raise_notrace Exit);
      with Scanf.Scan_failure s ->
        print_endline ("bad line: " ^ s);
        print_endline ("skip line " ^ input_line ic);
    done;
    close_in ic;
    !res
  with
  | Exit -> !res
  | e ->
    print_endline ("unable to read memory peak: " ^ Printexc.to_string e);
    0
   *)

(* TODO
external getpagesize : = 
   *)
let getpagesize () = 4_096

let mem_used () : float =
  float_of_int (mem_read_stat 0 * getpagesize ()) /. (1024. *. 1024.)

let mem_used_peak () : float =
  let peak = float_of_int (mem_read_peak ()) /. 1024. in
  if peak = 0. then mem_used () else peak
