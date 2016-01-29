open Cohttp
open Cohttp_lwt_unix
open Lwt

let get_lines uri =
  let%lwt (resp, body) = Client.get uri in
    match Response.status resp with
      | `OK ->
          let%lwt body = Cohttp_lwt_body.to_string body in
          print_string body;
          Lwt.return 0
      | `Gone -> Lwt.return 1
      | _ -> Lwt.return 2

let _ =
  get_lines (Uri.make ~scheme:"http" ~host:"localhost" ~port:(int_of_string Sys.argv.(1)) ~path:"/" ())
    |> Lwt_main.run |> exit
