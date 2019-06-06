type 'a t = { m : int; k : int; p_len : (int * int) list; b : Bitv.t }

let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let partition_lengths m k =
  let rec aux sum acc i =
    if List.length acc = k then (sum, acc)
    else
      let rec loop step =
        let k = i + step in
        let gcd_k = gcd k in
        if List.for_all (fun p -> gcd_k p = 1) acc then
          aux (sum + k) (k :: acc) (k + 1)
        else loop (step + 1)
      in
      loop 1
  in
  aux 0 [] (m / k)

let v m k =
  let m, lengths = partition_lengths m k in
  let p_len =
    let rec aux acc off = function
      | [] -> acc
      | h :: t -> aux ((off, h) :: acc) (off + h) t
    in
    aux [] 0 lengths
  in
  { m; k; p_len; b = Bitv.create m false }

let estimate_parameters n p =
  let log2 = log 2. in
  let nf = float_of_int n in
  let m = ceil (-.nf *. log p /. log (2. ** log2)) in
  let k = ceil (log2 *. m /. nf) in
  (m, k)

let create ?(error_rate = 0.01) n_items =
  let m, k = estimate_parameters n_items error_rate in
  v (int_of_float m) (int_of_float k)

let add t data =
  let h = Hashtbl.hash data in
  let rec loop = function
    | [] -> ()
    | (off, len) :: tl ->
        let loc = off + (h mod len) in
        let () = Bitv.set t.b loc true in
        loop tl
  in
  loop t.p_len

let mem t data =
  let h = Hashtbl.hash data in
  let rec loop = function
    | [] -> true
    | (off, len) :: tl ->
        let loc = off + (h mod len) in
        let res = Bitv.get t.b loc in
        if res then loop tl else false
  in
  loop t.p_len

let clear t = Bitv.fill t.b 0 t.m false
