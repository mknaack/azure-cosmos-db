module Mock_config : Test_io_intf.Config = struct
  let prefix = "mock"
end

module Mock_test_io : Test_io_intf.IO with type 'a t = 'a = struct
  include Mock_io

  let run x = x
end

module Mock_http_impl :
  Cosmos.Databases_intf.Http_client with type 'a io := 'a Mock_io.t = struct
  type http_error = Mock_http.http_error =
    | Connection_refused
    | Other_error of exn

  let get = Mock_http.get
  let post = Mock_http.post
  let put = Mock_http.put
  let delete = Mock_http.delete
end

module Mock_db =
  Cosmos.Databases_core.Make (Mock_io) (Mock_http_impl) (Mock_auth.Auth)
