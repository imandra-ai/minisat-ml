(*
Copyright (c) 2003-2007, Niklas Een, Niklas Sorensson
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


let[@specialise] selection_sort ~less a offset ~len : unit =
  assert (offset+len <= Array.length a);
  let rec loop_ i j best_i =
    if j<offset+len then (
      let best_i = if less a.(j) a.(best_i) then j else best_i in
      loop_ i (j+1) best_i
    ) else best_i
  in
  for i=offset to offset+len-2 do
    let best_i = loop_ i (i+1) i in
    (* swap *)
    if i<>best_i then (
      let tmp = a.(i) in
      a.(i) <- a.(best_i);
      a.(best_i) <- tmp
    )
  done

let[@specialise] rec sort_rec_ less a offset ~len : unit =
  if len <= 1 then ()
  else if len <= 15 then (
    selection_sort ~less a offset ~len
  ) else (
    let pivot = a.(offset+len/2) in

    (* partition [a] into smaller and larger than pivot *)
    let rec find_i i =
      let i = i+1 in
      if less a.(offset+i) pivot then find_i i else i
    in
    let rec find_j j =
      let j = j-1 in
      if less pivot a.(offset+j) then find_j j else j
    in

    let rec partition i j =
      let i = find_i i in
      let j = find_j j in
      if i>=j then i
      else (
        let tmp = a.(offset+i) in
        a.(offset+i) <- a.(offset+j);
        a.(offset+j) <- tmp;
        partition i j
      )
    in
    let i = partition (-1) len in
    sort_rec_ less a offset ~len:i;
    sort_rec_ less a (offset+i) ~len:(len-i);
  )

(* main entry to sort an array *)
let[@inline] sort ~less a ~len : unit = sort_rec_ less a 0 ~len

let[@specialise] sort_vec ~less v =
  let module V = Minisat_vec in
  sort ~less (V.Internal.data v) ~len:(V.size v)

(*$R
  let a = [| 1; 4;2;7;10;29;20; 4;2; 10;22;562;9;0 |] in
  sort ~less:(fun x y-> x<y) a ~len:(Array.length a);
  let is_sorted a =
    let rec aux i = i+1=Array.length a || (a.(i) <= a.(i+1) && aux (i+1)) in
    aux 0
  in
  assert_bool "sorted" (is_sorted a)
*)

(*$R
  let a = [| 1; 4;2;7;10;29;20; 4;2; 10;22;562;9;0; 1; 4;2;7;10;29;20; 22; 91; 19; 29; 5 |] in
  sort ~less:(fun x y-> x<y) a ~len:(Array.length a);
  let is_sorted a =
    let rec aux i = i+1=Array.length a || (a.(i) <= a.(i+1) && aux (i+1)) in
    aux 0
  in
  assert_bool "sorted" (is_sorted a)
*)
