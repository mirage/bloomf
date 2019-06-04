module type Hashable = sig
  type t
end

module type S = sig
  type elt

  type t

  val create : ?error_rate:float -> int -> t

  val add : t -> elt -> unit

  val mem : t -> elt -> bool

  val clear : t -> unit
end

module Make (H : Hashable) = struct
  type elt = H.t

  let log2 = log 2.0

  let powlog2 = 2. ** log2

  type t = { m : int; k : int; b : Bitv.t }

  let v m k = { m; k; b = Bitv.create m false }

  let hash i data = Hashtbl.seeded_hash i data

  let location m i data = abs (hash i data mod m)

  let estimate_parameters n p =
    let nf = float_of_int n in
    let m = ceil (-.nf *. log p /. powlog2) in
    let k = ceil (log2 *. m /. nf) in
    (m, k)

  let create ?(error_rate = 0.01) n_items =
    let m, k = estimate_parameters n_items error_rate in
    v (int_of_float m) (int_of_float k)

  let add t data =
    let rec loop i =
      if i = 0 then ()
      else
        let loc = location t.m i data in
        let () = Bitv.set t.b loc true in
        loop (i - 1)
    in
    loop t.k

  let mem t data =
    let rec loop res i =
      if i = 0 then res
      else if not res then res
      else
        let loc = location t.m i data in
        let res = Bitv.get t.b loc in
        loop res (i - 1)
    in
    loop true t.k

  let clear t = Bitv.fill t.b 0 t.m false
end
