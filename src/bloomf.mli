(** Bloom filters

    bloomf is an implementation of Bloom filters in OCaml.

    Bloom filters are memory and time efficient data structures allowing
    probabilistic membership queries in a set.
    A query negative result ensures that the element is not present in the set,
    while a positive result might be a false positive, i.e. the element might
    not be present and the BF membership query can return true anyway.
    Internal parameters of the BF allow to control its false positive rate
    depending on the expected number of elements in it. *)

(** {1 Generic interface} *)

(** The type of the Bloom filter. *)
type 'a t

val create : ?error_rate:float -> ?bits:Bitv.t -> int -> 'a t
(** [create ~error_rate size] creates a fresh BF for which expected false
    positive rate when filled with [size] elements is [error_rate].
    @raise Invalid_argument if [error_rate] is not in \]0, 1\[, or [size] is
    negative. *)

val add : 'a t -> 'a -> unit
(** [add t e] adds [e] to [t]. *)

val mem : 'a t -> 'a -> bool
(** [mem t e] is [true] if [e] is in [t]. *)

val clear : 'a t -> unit
(** [clear t] clears the contents of [t]. *)

val size_estimate : 'a t -> int
(** [size_estimate t] is an approximation of the number of elements stored in
    the bloom filter.
    Please note that this operation is costly (see benchmarks). *)

val params : 'a t -> int * int
(** [params t] returns the parameters [(m, k)] of the bloom filter [t].

    @param m  length of the underlying bit vector
    @param k  number of hash functions *)

val bits : 'a t -> Bitv.t
(** [bits t] returns a copy of the underlying bit vector of [t] *)

(** {1 Functorial interface} *)

(** The functorial interface allows you to specify your own hash function. *)

(** The input interface for [Bloomf.Make]. *)
module type Hashable = sig
  (** The type of the values to be stored. *)
  type t

  val hash : t -> int
  (** The hash function. {e This function must return positive integers.}
      Behavior is undefined otherwise. Please note that false positive rate
      might be affected by unevenly distributed hash functions. *)
end

(** The output interface for [Bloomf.Make]. *)
module Make (H : Hashable) : sig
  type t

  val create : ?error_rate:float -> ?bits:Bitv.t -> int -> t

  val add : t -> H.t -> unit

  val mem : t -> H.t -> bool

  val clear : t -> unit

  val size_estimate : t -> int

  val params : t -> int * int

  val bits : t -> Bitv.t
end
