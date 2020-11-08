type priv = { m : int; k : int; p_len : (int * int) list; b : Bitv.t }

type 'a t = priv

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
  try
    let b = Bitv.create m false in
    { m; k; p_len; b }
  with Invalid_argument _ -> invalid_arg "Bloomf.create"

let estimate_parameters n p =
  let log2 = log 2. in
  let nf = float_of_int n in
  let m = ceil (-.nf *. log p /. log (2. ** log2)) in
  let k = ceil (log2 *. m /. nf) in
  (m, k)

let create ?(error_rate = 0.01) n_items =
  let m, k = estimate_parameters n_items error_rate in
  if error_rate <= 0. || error_rate >= 1. then invalid_arg "Bloomf.create";
  v (int_of_float m) (int_of_float k)

let add_priv t hashed_data =
  let rec loop = function
    | [] -> ()
    | (off, len) :: tl ->
        let loc = off + (hashed_data mod len) in
        let () = Bitv.unsafe_set t.b loc true in
        loop tl
  in
  loop t.p_len

let add bf data = add_priv bf (Hashtbl.hash data)

let mem_priv t hashed_data =
  let rec loop = function
    | [] -> true
    | (off, len) :: tl ->
        let loc = off + (hashed_data mod len) in
        let res = Bitv.unsafe_get t.b loc in
        if res then loop tl else false
  in
  loop t.p_len

let mem bf data = mem_priv bf (Hashtbl.hash data)

let clear t = Bitv.fill t.b 0 t.m false

(* Bitv.pop is really slow *)
let size_estimate t =
  let mf = float_of_int t.m in
  let kf = float_of_int t.k in
  let xf = float_of_int (Bitv.pop t.b) in
  int_of_float (-.mf /. kf *. log (1. -. (xf /. mf)))

let to_bytes t =
  let flat_p_len =
    t.p_len
    |> List.fold_left
         (fun acc (a, b) -> Int64.of_int a :: Int64.of_int b :: acc)
         []
  in
  let first_to_encode =
    Array.of_list (Int64.of_int t.m :: Int64.of_int t.k :: flat_p_len)
  in

  Bytes.concat Bytes.empty
    [
      Bytes.init
        (8 * Array.length first_to_encode)
        (fun i ->
          Char.chr
            (Int64.to_int
               (Int64.logand
                  (Int64.shift_right first_to_encode.(i / 8) (8 * i))
                  255L)));
      Bitv.to_bytes t.b;
    ]

let of_bytes b =
  let int64_of_bytes b =
    let rec build x i =
      if i < 0 then x
      else
        build
          (Int64.logor (Int64.shift_left x 8)
             (Int64.of_int (Char.code (Bytes.get b i))))
          (pred i)
    in
    build 0L 7
  in

  let rec parse_p_len start stop acc last =
    match Int64.compare start stop with
    | i when i >= 0 -> (Int64.to_int start, acc)
    | _ -> (
        let next_value = int64_of_bytes (Bytes.sub b (Int64.to_int start) 8) in
        match last with
        | Some value ->
            parse_p_len (Int64.add start 8L) stop
              ((value, Int64.to_int next_value) :: acc)
              None
        | None ->
            parse_p_len (Int64.add start 8L) stop acc
              (Some (Int64.to_int next_value)) )
  in

  match int64_of_bytes (Bytes.sub b 0 8) with
  | exception Invalid_argument _ -> Error "Failed to parse (m)"
  | m -> (
      match int64_of_bytes (Bytes.sub b 8 8) with
      | exception Invalid_argument _ -> Error "Failed to parse (k)"
      | k -> (
          (* 16 initial offset + 2 ints per entry in p_len (which has `k` elements) *)
          match parse_p_len 16L (Int64.add 16L (Int64.mul k 16L)) [] None with
          | exception Invalid_argument _ -> Error "Failed to parse (p_len)"
          | i, p_len -> (
              match Bitv.of_bytes (Bytes.sub b i (Bytes.length b - i)) with
              | exception Invalid_argument _ -> Error "Failed to parse (b)"
              | b -> Ok { m = Int64.to_int m; k = Int64.to_int k; p_len; b } ) )
      )

module type Hashable = sig
  type t

  val hash : t -> int
end

module Make (H : Hashable) = struct
  type t = priv

  let create = create

  let add bf data = add_priv bf (H.hash data)

  let mem bf data = mem_priv bf (H.hash data)

  let clear = clear

  let size_estimate = size_estimate
end
