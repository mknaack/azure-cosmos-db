 val delete : ?v6:bool -> ?https:bool -> ?port:int -> ?headers:Http_headers.t -> host:string -> uri:string -> unit -> Ocsigen_http_frame.t Lwt.t
 val put_string : ?v6:bool -> ?https:bool -> ?port:int -> ?headers:Http_headers.t -> host:string -> uri:string -> content: string -> content_type: (string * string) -> unit -> Ocsigen_http_frame.t Lwt.t
