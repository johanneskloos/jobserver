let lines = ref []

let rec splitn n l = match n, l with
  | 0, l -> ([], l)
  | _, [] -> ([], [])
  | _, x::l -> let (l1, l2) = splitn (n-1) l in (x::l1, l2)

let get_lines n =
  let (lines1, lines2) = splitn n !lines in
    lines := lines2;
    if n = 0 || lines1 <> [] then
      let body = List.fold_right (fun line body -> line ^ "\n" ^ body) lines1 "" in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:body ()
    else
      Cohttp_lwt_unix.Server.respond_error ~status:`Gone ~body:"No more data available" ()

let path_regex =
  let open Pcre in regexp ~flags:[`ANCHORED] "/(\\d*)"

let handler conn req body =
  let open Cohttp in
  let open Cohttp_lwt_unix in
    if Request.meth req <> `GET then
      Server.respond_error ~status:`Method_not_allowed ~body:"Only GET is supported" ()
    else
      let path = Request.uri req |> Uri.path in try
        let num = Pcre.get_substring (Pcre.exec ~rex:path_regex path) 1 in
        if num = "" then get_lines 1 else get_lines (int_of_string num)
      with Not_found ->
        if path = "/status" then
          Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(string_of_int (List.length !lines)) ()
        else
          Server.respond_error ~status:`Bad_request ~body:(path ^ " not understood") ()


let run_server port =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  Server.make ~callback:handler ()
    |> Server.create ~mode:(`TCP (`Port port))
    |> Lwt_main.run
    |> ignore

let _ =
  let rec read_lines fh lines =
    try read_lines fh (input_line fh :: lines)
    with End_of_file -> List.rev lines
  in let infh = open_in Sys.argv.(1) in
    lines := read_lines infh [];
    close_in infh;
    run_server (int_of_string (Sys.argv.(2)))
