  (** The type of the Bloom filter *)
  type 'a t

  val create : ?error_rate:float -> int -> 'a t
  (** Instantiates the filter *)

  val add : 'a t -> 'a -> unit
  (** [add t e] adds [e] to [t]. *)

  val mem : 'a t -> 'a -> bool
  (** [mem t e] is false if [e] is not part of the set (the reverse
     property doesn't hold). *)

  val clear : 'a t -> unit
  (** Returns the set to an empty state *)
