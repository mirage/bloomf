(* inspired from
   https://github.com/travisbrady/flajolet/blob/master/lib/bloom.ml
   and
   https://github.com/sergezloto/ocaml-bloom-filter/blob/master/bloomf.ml*)

module type Hashable = sig
  type t
end

(** A Bloom filter provides for creation, membership and addition only *)
module type S = sig
  (** The type of elements. *)
  type elt

  (** The type of the Bloom filter *)
  type t

  val create : ?error_rate:float -> int -> t
  (** Instantiates the filter *)

  val add : t -> elt -> unit
  (** [add t e] adds [e] to [t]. *)

  val mem : t -> elt -> bool
  (** [mem t e] is false if [e] is not part of the set (the reverse
     property doesn't hold). *)

  val clear : t -> unit
  (** Returns the set to an empty state *)
end

(** Bloom filter functor *)
module Make (H : Hashable) : S with type elt = H.t
