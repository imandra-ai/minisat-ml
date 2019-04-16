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

(** {1 Dimacs parser} *)

open Minisat_ml

type t = {
  buf : Bytes.t;
  mutable eof : bool;
  mutable pos : int;
  mutable size : int;
  refill : Bytes.t -> int -> int -> int
}

let buf_size = 1048576

let[@inline] eof self : bool = self.eof

let fill_if_empty (self : t) : unit =
  if self.pos >= self.size && not self.eof then (
    self.pos <- 0;
    self.size <- self.refill self.buf 0 (Bytes.length self.buf);
    if self.size = 0 then self.eof <- true )

let make ~refill : t =
  let r =
    { refill;
      buf = Bytes.make buf_size '\000';
      pos = 0;
      size = 0;
      eof = false
    }
  in
  fill_if_empty r;
  (* initial read *)
  r

let make_chan ic : t = make ~refill:(fun buf i len -> input ic buf i len)

let[@inline] junk self =
  self.pos <- 1 + self.pos;
  fill_if_empty self

let[@inline] get self : char =
  assert (self.pos < self.size);
  Bytes.get self.buf self.pos

let rec skip_whitespace (self : t) : unit =
  if not self.eof then (
    match get self with
    | '\n' | ' ' | '\t' | '\r' ->
        junk self;
        skip_whitespace self
    | _ -> () )

let rec skip_line self : unit =
  if not self.eof then (
    match get self with
    | '\n' ->
        junk self;
        () (* done *)
    | _ ->
        junk self;
        skip_line self )

let parse_int (self : t) : int =
  let rec aux neg v =
    if eof self then if neg then -v else v
    else (
      match get self with
      | '0' .. '9' as c ->
          junk self;
          let v = (v * 10) + (Char.code c - Char.code '0') in
          aux neg v
      | _ -> if neg then -v else v )
    (* done *)
  in
  let neg =
    match get self with
    | '-' ->
        junk self;
        true
    | '+' ->
        junk self;
        false
    | _ -> false
  in
  ( match get self with
  | '0' .. '9' -> ()
  | c -> failwith (Printf.sprintf "unexpected char %c" c) );
  aux neg 0

let rec skip_metadata self =
  if not self.eof then (
    match get self with
    | 'c' ->
        junk self;
        skip_line self;
        skip_metadata self
    | 'p' ->
        junk self;
        skip_line self;
        skip_metadata self (* do not parse metadata or validate *)
    | '\n' ->
        junk self;
        skip_metadata self (* empty line *)
    | _ -> () )

let read_int_list self : _ list =
  let rec aux acc =
    skip_metadata self;
    if self.eof then List.rev acc
    else ( match parse_int self with 0 -> List.rev acc | i -> aux (i :: acc) )
  in
  aux []

let read_int_list_list self : _ list list =
  let rec aux acc =
    skip_metadata self;
    if self.eof then List.rev acc
    else (
      let c = read_int_list self in
      aux (c :: acc) )
  in
  aux []

let read_clause (self : t) (solver : Solver.t) (lits : Lit.t Vec.t) : bool =
  Vec.clear lits;
  let rec aux () =
    skip_metadata self;
    skip_whitespace self;
    if self.eof then false
    else (
      match parse_int self with
      | 0 -> true (* return *)
      | i ->
          let v_idx = abs i - 1 in
          while v_idx >= Solver.n_vars solver do
            ignore (Solver.new_var solver : Var.t)
          done;
          Vec.push lits (Lit.make_sign (Var.make v_idx) (i < 0));
          aux () )
  in
  aux ()

let parse_dimacs self solver : unit =
  let v = Vec.make () in
  let rec loop () =
    if not self.eof then (
      match
        skip_metadata self;
        read_clause self solver v
      with
      | true ->
          let ok = Solver.add_clause solver v in
          if ok then loop ()
      | false -> ()
      | exception End_of_file -> () )
  in
  loop ()
