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
      Bitv.L.to_string t.b;
      gs;
      string_of_int t.k;
      gs;
      p_len_to_string t.p_len;
      gs;
      string_of_int t.m;
    ]
  in
  contents |> List.iter (Buffer.add_string buf);
  buf

let to_bytes t = to_buf t |> Buffer.to_bytes

let to_string t = to_buf t |> Buffer.contents

let unwrap_input_list error_message l =
  let rec aux l acc =
    match l with
    | [] -> ( Ok (List.rev acc) [@explicit_arity] )
    | ((Some x)[@explicit_arity]) :: tl -> aux tl (x :: acc)
    | None :: _ -> ( Error error_message [@explicit_arity] )
  in
  aux l []

let int_of_string_opt inpt =
  match int_of_string inpt with
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

let of_string s =
  let gs = Char.chr 29 in
  match String.split_on_char gs s with
  | [ bv_str; k_str; p_len_str; m_str ] -> (
      let b = Bitv.L.of_string bv_str in
      match parse_p_len p_len_str with
      | Ok p_len -> (
          let m = int_of_string_opt m_str in
          let k = int_of_string_opt k_str in
          match (m, k) with
          | Some m, Some k -> Ok { m; k; p_len; b }
          | None, Some _ -> Error "Invalid int for 'm'"
          | Some _, None -> Error "Invalid int for 'k'"
          | None, None -> Error "Invalid int for both 'm' and 'k'" )
      | Error msg -> Error msg )
  | _ -> Error "invalid number of fields in input"

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
