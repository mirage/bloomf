(** Bloom filters

    bloomf is an implementation of Bloom filters in OCaml.

    Bloom filters are memory and time efficient data structures allowing
    probabilistic membership queries in a set. A query negative result ensures
    that the element is not present in the set, while a positive result might be
    a false positive, i.e. the element might not be present and the BF
    membership query can return true anyway. Internal parameters of the BF allow
    to control its false positive rate depending on the expected number of
    elements in it. *)

(** {1 Generic interface} *)

type 'a t
(** The type of the Bloom filter. *)

val create : ?error_rate:float -> int -> 'a t
(** [create ~error_rate size] creates a fresh BF for which expected false
    positive rate when filled with [size] elements is [error_rate].

    @raise Invalid_argument
      if [error_rate] is not in \]0, 1\[, or [size] is negative. *)

val add : 'a t -> 'a -> unit
(** [add t e] adds [e] to [t]. *)

val mem : 'a t -> 'a -> bool
(** [mem t e] is [true] if [e] is in [t]. *)

val clear : 'a t -> unit
(** [clear t] clears the contents of [t]. *)

val union : 'a t -> 'a t -> 'a t
(** [union t1 t2] computes the union of the two inputs. This operation is
    lossless in the sense that the resulting Bloom filter is the same as the
    Bloom filter created from scratch using the union of the two sets.

    Raises [Invalid_argument] if the two bloom filters were created with
    different parameters *)

val inter : 'a t -> 'a t -> 'a t
(** [inter t1 t2] computes the intersection of the two inputs. The false
    positive probability in the resulting Bloom filter is at most the
    false-positive probability in one of the constituent Bloom filters, but may
    be larger than the false positive probability in the Bloom filter created
    from scratch using the intersection of the two sets.

    Raises [Invalid_argument] if the two bloom filters were created with
    different parameters *)

val size_estimate : 'a t -> int
(** [size_estimate t] is an approximation of the number of elements stored in
    the bloom filter. Please note that this operation is costly (see
    benchmarks). *)

(** {2 Serializers/Deserializers} *)

val to_bytes : 'a t -> bytes
val of_bytes : bytes -> ('a t, [ `Msg of string ]) result

(** {1 Functorial interface} *)

(** The functorial interface allows you to specify your own hash function. *)

(** The input interface for [Bloomf.Make]. *)
module type Hashable = sig
  type t
  (** The type of the values to be stored. *)

  val hash : t -> int
  (** The hash function. {e This function must return positive integers.}
      Behavior is undefined otherwise. Please note that false positive rate
      might be affected by unevenly distributed hash functions. *)
end

(** The output interface for [Bloomf.Make]. *)
module Make (H : Hashable) : sig
  type t

  val create : ?error_rate:float -> int -> t
  val add : t -> H.t -> unit
  val mem : t -> H.t -> bool
  val clear : t -> unit
  val size_estimate : t -> int
  val to_bytes : t -> bytes
  val of_bytes : bytes -> (t, [ `Msg of string ]) result
end
