(*
  "Mon"  / "Tue" /  "Wed"  / "Thu"
                 /  "Fri"  / "Sat" /  "Sun"*)
module Ms_time = struct
  type t = float

  let weekday_of_tm_wday = function
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | 6 -> "Sat"
    | d -> failwith @@ Printf.sprintf "Day number unknown: %i" d

  (* "Jan"  /  "Feb" /  "Mar"  /  "Apr"
                 /  "May"  /  "Jun" /  "Jul"  /  "Aug"
                 /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"*)
  let month_of_tm_mon = function
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | 11 -> "Dec"
    | d -> failwith @@ Printf.sprintf "Month number unknown: %i" d

  let create time = time
  let create_now = create Unix.time

  let x_ms_date time =
    let t = Unix.gmtime time in
    let weekday = weekday_of_tm_wday t.tm_wday in
    Printf.sprintf "%s, %02i %s %i %02i:%02i:%02i GMT" weekday t.tm_mday
      (month_of_tm_mon t.tm_mon) (t.tm_year + 1900) t.tm_hour t.tm_min t.tm_sec
end

module Verb = Verb
