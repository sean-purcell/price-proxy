open! Core
open! Async

module Config = struct
  type t =
    { name : string
    ; url : string
    ; selector : string
    }
  [@@deriving compare, sexp]
end

let command =
  Command.async
    ~summary:"Fetch and cache prices"
    (let%map_open.Command configs =
       flag "cell" (listed sexp) ~doc:"SEXP config for a cell to server"
       >>| List.map ~f:[%of_sexp: Config.t]
     and port = flag "port" (required int) ~doc:"PORT where to listen" in
     fun () ->
       print_s [%message "Starting with config" (configs : Config.t list) (port : int)];
       return ())
;;
