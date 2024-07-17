open! Core
open! Async
open Cohttp
open Cohttp_async

let follow_redirects ?(max_redirects = 10) uri f =
  (* Copied from https://github.com/mirage/ocaml-cohttp#dealing-with-redirects *)
  let seen_uris = String.Hash_set.create () in
  let rec loop ~max_redirects uri =
    Hash_set.add seen_uris (Uri.to_string uri);
    let%bind ((response, response_body) as res) = f uri in
    let status_code = Response.status response |> Code.code_of_status in
    if Code.is_redirection status_code
    then (
      match Response.headers response |> Header.get_location with
      | Some new_uri when Uri.to_string new_uri |> Hash_set.mem seen_uris -> return res
      | Some new_uri ->
        if max_redirects > 0
        then
          (* Cohttp leaks connections if we don't drain the response body *)
          Body.drain response_body
          >>= fun () -> loop ~max_redirects:(max_redirects - 1) new_uri
        else (
          Log.Global.debug
            ~tags:[]
            "Ignoring %d redirect from %s to %s: redirect limit exceeded"
            status_code
            (Uri.to_string uri)
            (Uri.to_string new_uri);
          return res)
      | None ->
        Log.Global.debug
          ~tags:[]
          "Ignoring %d redirect from %s: there is no Location header"
          status_code
          (Uri.to_string uri);
        return res)
    else return res
  in
  loop ~max_redirects uri
;;

module Config = struct
  type t =
    { name : string
    ; url : string
    ; selector : string
    }
  [@@deriving compare, sexp]

  let parse_value t ~body =
    let open Soup in
    Or_error.try_with (fun () ->
        let parsed = parse body in
        let node = parsed $ t.selector in
        let text = leaf_text node |> Option.value_exn ~message:"No text found" in
        text)
  ;;

  let query_value t =
    let open Deferred.Or_error.Let_syntax in
    Log.Global.info_s [%message "Sending query" ~_:(t : t)];
    let%bind response, body =
      Deferred.Or_error.try_with (fun () ->
          follow_redirects (Uri.of_string t.url) Client.get)
    in
    let%bind.Deferred body = Body.to_string body in
    let code = Response.status response |> Code.code_of_status in
    Log.Global.info_s [%message "Request completed" ~_:(t : t) ~code:(code : int)];
    match Code.is_success code with
    | false ->
      Deferred.Or_error.error_s
        [%message "Failed to load value" ~url:t.url (code : int) body]
    | true ->
      let%bind value = parse_value t ~body |> Deferred.return in
      Log.Global.info_s [%message "Got value" ~_:(t : t) (value : string)];
      return value
  ;;
end

module State = struct
  type t = (Config.t * string) list

  let sexp_of_t (t : t) =
    List.map t ~f:(fun (config, value) -> config.name, value)
    |> [%sexp_of: (string * string) list]
  ;;

  let load configs =
    Log.Global.info_s [%message "Loading values" (configs : Config.t list)];
    Deferred.List.map ~how:`Sequential configs ~f:(fun config ->
        match%map Config.query_value config with
        | Ok value -> Some value
        | Error err ->
          Log.Global.error_s
            [%message "Failed to load value" (err : Error.t) (config : Config.t)];
          None)
  ;;

  let load_initial configs =
    load configs
    >>| List.map ~f:(function
            | Some v -> v
            | None -> "#ERROR")
    >>| List.zip_exn configs
  ;;

  let reload t =
    load (List.map t ~f:fst)
    >>| List.zip_exn t
    >>| List.map ~f:(fun ((config, prev), new_value) ->
            ( config
            , match new_value with
              | Some value -> value
              | None -> prev ))
  ;;

  let to_html (t : t) =
    let open Tyxml in
    let open Html in
    let rows =
      List.map t ~f:(fun (config, value) ->
          tr [ td [ txt config.name ]; td [ txt value ] ])
    in
    let page =
      [%html
        {|<html>
             <head>
              <title>Price proxy
      </title>
             </head>
             <body>
      <table>
      |}
          [ tbody rows ]
          {|
      </table>
      </body>
             </html>
         |}]
    in
    Format.asprintf "%a" (Tyxml.Html.pp ~indent:true ()) page
  ;;
end

let server_command =
  Command.async
    ~summary:"Fetch and cache prices"
    (let%map_open.Command configs =
       flag "cell" (listed sexp) ~doc:"SEXP config for a cell to server"
       >>| List.map ~f:[%of_sexp: Config.t]
     and port = flag "port" (required int) ~doc:"PORT where to listen"
     and query_interval =
       flag_optional_with_default_doc
         "refresh-every"
         Time_ns.Span.arg_type
         [%sexp_of: Time_ns.Span.t]
         ~default:(Time_ns.Span.of_min 5.)
         ~doc:"SPAN how often to refresh the values"
     in
     fun () ->
       print_s
         [%message
           "Starting with config"
             (configs : Config.t list)
             (port : int)
             (query_interval : Time_ns.Span.t)];
       let%bind state = State.load_initial configs in
       Log.Global.info_s [%message "Initial state" (state : State.t)];
       let state = ref state in
       Clock_ns.run_at_intervals' query_interval (fun () ->
           let%map new_state = State.reload !state in
           Log.Global.info_s [%message "Reloaded state" (new_state : State.t)];
           state := new_state);
       let%bind server =
         Server.create
           ~on_handler_error:`Raise
           (Tcp.Where_to_listen.of_port port)
           (fun ~body:_ address request ->
             Log.Global.info_s
               [%message
                 "Received request"
                   (address : Socket.Address.Inet.t)
                   (request : Request.t)];
             if [%equal: string] request.resource "/"
             then (
               let body = State.to_html !state |> Body.of_string in
               let response = Response.make ~status:`OK () in
               return (response, body))
             else (
               let body = Body.of_string "Not found" in
               let response = Response.make ~status:`Not_found () in
               return (response, body)))
       in
       let listening_on_port = Server.listening_on server in
       Log.Global.info_s [%message "HTTP server listening" (listening_on_port : int)];
       Deferred.never ())
;;

let query_command =
  Command.async_or_error
    ~summary:"Query a value from a config"
    (let%map_open.Command config = anon ("CONFIG" %: sexp) >>| [%of_sexp: Config.t] in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind value = Config.query_value config in
       print_endline value;
       return ())
;;

let command =
  Command.group
    ~summary:
      "Tools for querying and serving values from web pages as a table for google sheets"
    [ "server", server_command; "query", query_command ]
;;
