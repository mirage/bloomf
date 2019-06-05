(** Bloom filters

    bloomf is an implementation of Bloom filters in OCaml.

    Bloom filters are memory and time efficient data structures allowing
    probabilistic membership queries in a set.
    A query negative result ensures that the element is not present in the set,
    while a positive result might be a false positive, i.e. the element might
    not be present and the BF membership query can return true anyway.
    Internal parameters of the BF allow to control its false positive rate
    depending on the expected number of elements in it. *)

(** The type of the Bloom filter *)
type 'a t

val create : ?error_rate:float -> int -> 'a t
(** [create ~error_rate size] creates a fresh BF for which expected false
    positive rate when filled with [size] elements is [error_rate]. *)

val add : 'a t -> 'a -> unit
(** [add t e] adds [e] to [t]. *)

val mem : 'a t -> 'a -> bool
(** [mem t e] is [true] if [e] is in [t]. *)

val clear : 'a t -> unit
(** [clear t] clears the contents of [t] *)
