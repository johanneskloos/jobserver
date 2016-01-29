open Cohttp
open Cohttp_lwt_unix
open Lwt

let rec loop action uri =
  let%lwt (resp, body) = Client.get uri in
    match Response.status resp with
      | `OK ->
          let%lwt body = Cohttp_lwt_body.to_string body in
          action body;
          loop action uri
      | `Gone -> Lwt.return 0
      | _ -> Lwt.return 2

let job_loop port action =
  loop action (Uri.make ~scheme:"http" ~host:"localhost" ~port ~path:"/" ())
    |> Lwt_main.run |> exit
