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
  let rs = String.make 1 (Char.chr 30) in
  let us = String.make 1 (Char.chr 31) in
  let buf =
    Buffer.create (5 + (4 * List.length t.p_len) + (2 * Bitv.length t.b))
  in
  let write_to_buf buf content =
    Buffer.add_string buf (string_of_int (Bytes.length content));
    Buffer.add_string buf "|";
    Buffer.add_bytes buf content
  in
  let p_len_to_string p_len =
    let buf = Buffer.create (4 * List.length p_len) in
    ( match p_len with
    | [] -> Buffer.add_string buf (" " ^ rs)
    | p_len ->
        p_len
        |> List.iter (fun p ->
               let fst, snd = p in
               Buffer.add_string buf
                 (string_of_int fst ^ us ^ string_of_int snd ^ rs)) );
    buf |> Buffer.contents
  in
  let contents =
    [
      Bytes.of_string (string_of_int t.k);
      Bytes.of_string (p_len_to_string t.p_len);
      Bytes.of_string (string_of_int t.m);
      Bitv.to_bytes t.b;
    ]
  in
  contents |> List.iter (write_to_buf buf);
  buf

let to_bytes t = to_buf t |> Buffer.to_bytes

let unwrap_input_list error_message l =
  let rec aux l acc =
    match l with
    | [] -> Ok (List.rev acc)
    | Some x :: tl -> aux tl (x :: acc)
    | None :: _ -> Error error_message
  in
  aux l []

let int_of_bytes_opt inpt =
  match int_of_string (Bytes.unsafe_to_string inpt) with
  | inpt_int -> Some inpt_int
  | exception Failure _ -> None

let parse_p_len p_len_str =
  let rs = Char.chr 30 in
  let us = Char.chr 31 in
  let parse_pair pair =
    match pair with
    | [ x; y ] -> (
        let x_opt = int_of_string_opt x in
        let y_opt = int_of_string_opt y in
        match (x_opt, y_opt) with
        | Some x_opt, Some y_opt -> Some (x_opt, y_opt)
        | _ -> None )
    | _ -> None
  in
  let p_len_lst =
    p_len_str |> String.split_on_char rs |> List.filter (fun p -> p <> "")
  in
  match p_len_lst with
  | [ " " ] -> Ok []
  | p_len_lst ->
      p_len_lst
      |> List.map (fun p -> p |> String.split_on_char us |> parse_pair)
      |> unwrap_input_list "Invalid p_len pair(s)"

let nth_opt buf n =
  match Buffer.nth buf n with
  | c -> ( Some c [@explicit_arity] )
  | exception Invalid_argument _ -> None

let read_n_bytes n buf p =
  let rec loop p n acc =
    match n with
    | n when n > 0 -> (
        match nth_opt buf p with
        | ((Some chr)[@explicit_arity]) ->
            loop (p + 1) (n - 1)
              (Bytes.concat Bytes.empty [ acc; Bytes.make 1 chr ])
        | None -> acc )
    | _ -> acc
  in
  loop (p + 1) n Bytes.empty

let read_until from til buf =
  let rec loop pos acc =
    match Buffer.nth buf pos with
    | cur when cur = til -> acc
    | cur -> loop (pos + 1) (Bytes.cat acc (Bytes.make 1 cur))
    | exception Invalid_argument _ -> acc
  in
  loop from Bytes.empty

let of_buffer buf =
  let separator = Char.chr 124 in
  let rec parse_loop buf contents offset =
    if offset >= Buffer.length buf then List.rev contents
    else
      let n_b = read_until offset separator buf in
      let n_len = Bytes.length n_b in
      let n = n_b |> Bytes.unsafe_to_string |> int_of_string in
      let block_contents = read_n_bytes n buf (offset + n_len) in
      let new_offset = offset + n_len + Bytes.length block_contents + 1 in
      parse_loop buf (block_contents :: contents) new_offset
  in
  match parse_loop buf [] 0 with
  | [ k_b; p_len_b; m_b; bv_b ] -> (
      let b = Bitv.of_bytes bv_b in
      match parse_p_len (Bytes.to_string p_len_b) with
      | Ok p_len -> (
          let m = int_of_bytes_opt m_b in
          let k = int_of_bytes_opt k_b in
          match (m, k) with
          | Some m, Some k -> Ok { m; k; p_len; b }
          | None, Some _ -> Error "Invalid int for 'm'"
          | Some _, None -> Error "Invalid int for 'k'"
          | None, None -> Error "Invalid int for 'm' and 'k'" )
      | Error msg -> Error msg )
  | _ -> Error "Invalid number of fields in input"

let of_bytes b =
  let buf = Buffer.create (Bytes.length b) in
  let () = Buffer.add_bytes buf b in
  of_buffer buf

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
