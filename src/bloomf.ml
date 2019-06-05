let log2 = log 2.

type 'a t = { m : int; k : int; b : Bitv.t; h : ('a -> int) array }

let v m k =
  { m;
    k;
    b = Bitv.create m false;
    h = Array.init k (fun i -> Hashtbl.seeded_hash i)
  }

let location t i data = abs (t.h.(i) data mod t.m)

let estimate_parameters n p =
  let nf = float_of_int n in
  let m = ceil (-.nf *. log p /. log (2. ** log2)) in
  let k = ceil (log2 *. m /. nf) in
  (m, k)

let create ?(error_rate = 0.01) n_items =
  let m, k = estimate_parameters n_items error_rate in
  v (int_of_float m) (int_of_float k)

let add t data =
  let rec loop i =
    if i = 0 then ()
    else
      let loc = location t i data in
      let () = Bitv.set t.b loc true in
      loop (i - 1)
  in
  loop (t.k - 1)

let mem t data =
  let rec loop res i =
    if i = 0 then res
    else if not res then res
    else
      let loc = location t i data in
      let res = Bitv.get t.b loc in
      loop res (i - 1)
  in
  loop true (t.k - 1)

let clear t = Bitv.fill t.b 0 t.m false
