module Ms_time : sig
  type t

  val weekday_of_tm_wday : int -> string
  val month_of_tm_mon : int -> string
  val create : float -> t
  val create_now : unit -> t
  val x_ms_date : t -> string
end

module Verb = Utilities__.Verb

val take_first : int -> 'a list -> 'a list * 'a list