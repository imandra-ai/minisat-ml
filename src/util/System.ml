

let[@inline] cpu_time () : float = Sys.time()

let file_read_stat = Printf.sprintf "/proc/%d/statm" (Unix.getpid())

let mem_read_stat (field:int) : int =
  let field = ref field in
  let res = ref 0 in
  try
    let ic = open_in_bin file_read_stat in
    let b = Scanf.Scanning.from_channel ic in
    while !field >= 0 do
      Scanf.bscanf b "%d" (fun v->res := v);
      decr field;
    done;
    close_in ic;
    !res
  with e ->
    print_endline ("unable to read memory stats: " ^ Printexc.to_string e);
    0

let file_read_peak = Printf.sprintf "/proc/%d/status" (Unix.getpid())

let mem_read_peak () : int =
  0
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
  (float_of_int (mem_read_stat 0 * getpagesize())) /. (1024. *. 1024.)
    
let mem_used_peak () : float =
  let peak = float_of_int (mem_read_peak ()) /. 1024. in
  if peak = 0. then mem_used() else peak
