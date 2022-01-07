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
      Deferred.Or_error.try_with (fun () -> Client.get (Uri.of_string t.url))
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

let command =
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
       return ())
;;
