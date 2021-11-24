type priv = { m : int; k : int; p_len : (int * int) list; b : Bitv.t }
type 'a t = priv

let copy t = { m = t.m; k = t.k; p_len = t.p_len; b = Bitv.copy t.b }
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

let op f bf1 bf2 =
  if bf1.k <> bf2.k || bf1.m <> bf2.m then
    invalid_arg "incompatible bloom filters";
  { m = bf1.m; k = bf2.k; p_len = bf1.p_len; b = f bf1.b bf2.b }

let union bf1 bf2 = op Bitv.bw_or bf1 bf2
let inter bf1 bf2 = op Bitv.bw_and bf1 bf2

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

(* Serialisers *)

external set_64 : bytes -> int -> int64 -> unit = "%caml_string_set64u"
external swap64 : int64 -> int64 = "%bswap_int64"

let set_uint64 buf off v =
  if not Sys.big_endian then set_64 buf off (swap64 v) else set_64 buf off v

(* type priv = { m : int; k : int; p_len : (int * int) list; b : Bitv.t } *)

let to_bytes t =
  let enc_b = Bitv.to_bytes t.b in
  let enc_b_len = Bytes.length enc_b in
  let enc_p_len_len = 16 * List.length t.p_len in
  let len = 8 + 8 + 8 + enc_p_len_len + enc_b_len in
  let buf = Bytes.create len in
  set_uint64 buf 0 (Int64.of_int t.m);
  set_uint64 buf 8 (Int64.of_int t.k);
  set_uint64 buf 16 (Int64.of_int (List.length t.p_len));
  List.iteri
    (fun i (i1, i2) ->
      set_uint64 buf (24 + (8 * (2 * i))) (Int64.of_int i1);
      set_uint64 buf (24 + (8 * ((2 * i) + 1))) (Int64.of_int i2))
    t.p_len;
  Bytes.blit enc_b 0 buf (24 + enc_p_len_len) enc_b_len;
  buf

external get_64 : bytes -> int -> int64 = "%caml_string_get64"

let get_uint64 buf off =
  if not Sys.big_endian then swap64 (get_64 buf off) else get_64 buf off

let of_bytes buf =
  try
    let m = get_uint64 buf 0 |> Int64.to_int in
    let k = get_uint64 buf 8 |> Int64.to_int in
    let p_len_len = get_uint64 buf 16 |> Int64.to_int in
    let p_len =
      List.init p_len_len (fun i ->
          let i1 = get_uint64 buf (24 + (8 * (2 * i))) |> Int64.to_int in
          let i2 = get_uint64 buf (24 + (8 * ((2 * i) + 1))) |> Int64.to_int in
          (i1, i2))
    in
    let read = 24 + (16 * p_len_len) in
    let b = Bytes.sub buf read (Bytes.length buf - read) |> Bitv.of_bytes in
    Ok { m; k; p_len; b }
  with _ -> Error (`Msg "invalid serialisation format")

module type Hashable = sig
  type t

  val hash : t -> int
end

module Make (H : Hashable) = struct
  type t = priv

  let create = create
  let copy = copy
  let add bf data = add_priv bf (H.hash data)
  let mem bf data = mem_priv bf (H.hash data)
  let clear = clear
  let size_estimate = size_estimate
  let to_bytes = to_bytes
  let of_bytes = of_bytes
end
