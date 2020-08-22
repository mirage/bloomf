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

let to_buf t =
  let gs = String.make 1 (Char.chr 29) in
  let rs = String.make 1 (Char.chr 30) in
  let us = String.make 1 (Char.chr 31) in
  let buf =
    Buffer.create (5 + (4 * List.length t.p_len) + (2 * Bitv.length t.b))
  in
  let p_len_to_string p_len =
    let buf = Buffer.create (4 * List.length p_len) in
    p_len
    |> List.iter (fun p ->
           let fst, snd = p in
           string_of_int fst ^ us ^ string_of_int snd ^ rs
           |> Buffer.add_string buf);
    buf |> Buffer.contents
  in
  let bitv_to_string bitv =
    let buf = Buffer.create (2 * List.length bitv) in
    bitv |> List.iter (fun b -> string_of_int b ^ rs |> Buffer.add_string buf);
    buf |> Buffer.contents
  in
  let contents =
    [
      bitv_to_string (t.b |> Bitv.to_list);
      gs;
      string_of_int t.k;
      gs;
      p_len_to_string t.p_len;
      gs;
      string_of_int t.m;
    ]
  in
  contents |> List.iter (fun s -> Buffer.add_string buf s);
  buf

let to_bytes t = to_buf t |> Buffer.to_bytes

let to_string t = to_buf t |> Buffer.contents

let parse_plen p_len_str =
  let rs = Char.chr 30 in
  let us = Char.chr 31 in
  let parse_pair_ary pair =
    match pair with
    | [| x; y |] -> (int_of_string x, int_of_string y)
    | _ -> failwith "Invalid p_len pair"
  in
  p_len_str
  |> String.split_on_char rs
  |> List.map (fun p ->
         p |> String.split_on_char us |> Array.of_list |> parse_pair_ary)

let of_string s =
  let gs = Char.chr 29 in
  match String.split_on_char gs s with
  | [ bv_str; k_str; p_len_str; m_str ] ->
      let rs = Char.chr 30 in
      let b =
        bv_str
        |> String.split_on_char rs
        |> List.map int_of_string
        |> Bitv.of_list
      in
      let p_len = parse_plen p_len_str in
      { m = int_of_string m_str; k = int_of_string k_str; p_len; b }
  | _ -> failwith "Invalid input"

let of_bytes b = of_string (Bytes.to_string b)

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
