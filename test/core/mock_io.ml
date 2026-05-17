type +'a t = 'a

let return x = x
let bind x f = f x
let catch f handler = try f () with exn -> handler exn
let sleep _secs = ()
let enable_timeouts = ref false
let with_timeout t cmd = if !enable_timeouts && t <= 0.0 then None else Some cmd
let parallel_map f xs = List.map f xs

let with_timeouts_enabled f =
  let old = !enable_timeouts in
  enable_timeouts := true;
  Fun.protect ~finally:(fun () -> enable_timeouts := old) f
