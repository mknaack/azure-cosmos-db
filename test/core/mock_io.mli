type +'a t = 'a

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
val sleep : float -> unit t

val with_timeout : float -> 'a t -> 'a option t
(** [with_timeout t cmd] runs [cmd] with timeout [t].

    Note: this mock can only trigger timeout expiry when [enable_timeouts] is
    [true] and [t <= 0.0]. For [t > 0.0], it always returns [Some cmd] and
    cannot simulate actual timeout expiry. *)

val parallel_map : ('a -> 'b t) -> 'a list -> 'b list t
val enable_timeouts : bool ref
val with_timeouts_enabled : (unit -> 'a) -> 'a
