type 'a t = { m : int; k : int; p_len : (int * int) list; b : Bitv.t }

let sieve n =
  let is_prime = Array.make n true in
  let limit = truncate (sqrt (float (n - 1))) in
  let rec loop i =
    if i = limit then ()
    else
      let () =
        if is_prime.(i) then
          let rec set_i_multiples j =
            if j >= n then ()
            else (
              is_prime.(j) <- false;
              set_i_multiples (j + i) )
          in
          set_i_multiples (i * i)
        else ()
      in
      loop (i + 1)
  in
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  loop 2;
  is_prime

let primes n =
  let primes, _ =
    let sieve = sieve n in
    Array.fold_right
      (fun is_prime (xs, i) ->
        if is_prime then (i :: xs, i - 1) else (xs, i - 1) )
      sieve
      ([], Array.length sieve - 1)
  in
  Array.of_list primes

let partition_lengths m k =
  let p = primes ((m / k) + 300) in
  let rec find_pdex dmin i =
    let d = abs (p.(i) - (m / k)) in
    if d > dmin then i - 1 else find_pdex d (i + 1)
  in
  let pdex = find_pdex max_int 0 in
  let rec initial_sum sum i =
    if i = pdex - k + 1 then sum else initial_sum (sum + p.(i)) (i - 1)
  in
  let sum = initial_sum 0 pdex in
  let min = sum - m in
  let rec best_window sum min j =
    let new_sum = sum + p.(j) - p.(j - k) in
    let diff = new_sum - m in
    if diff >= min then j else best_window new_sum diff (j + 1)
  in
  let j = best_window 0 min (pdex + 1) in
  let rec get_primes acc s i =
    if i = 0 then (s, acc)
    else
      let chosen_prime = p.(j - k + i) in
      get_primes (chosen_prime :: acc) (s + chosen_prime) (i - 1)
  in
  get_primes [] 0 k

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
