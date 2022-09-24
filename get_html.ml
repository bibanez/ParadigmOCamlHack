open Lwt
open Cohttp
open Cohttp_lwt_unix
    (*
    Client.get (Uri.of_string s) >>= fun (resp, body) ->
    let code = Code.code_of_status (Response.status resp) in
    let code = resp |> Response.status |> Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    Printf.printf "Body of length: %d\n" (String.length body);
    body
    *)

let get_body s =
    Client.get (Uri.of_string s) >>= fun (resp, body) ->
    Cohttp_lwt.Body.to_string body >|= fun body ->
    body

let remove_whitespaces s =
    let to_space = function
        | ' ' -> '_'
        | c -> c
    in
    String.map s to_space 

let print_if_wiki s =
    if String.length s > 5 
        && String.equal "/wiki/" (String.common_prefix2 s "/wiki/")
    then
    print_endline s

let get_article s =
    Lwt_main.run (get_body ("https://en.wikipedia.org/wiki/" ^ (remove_whitespaces s)))

let print_links s =
    let soup = parse (get_article s) in
    soup $$ "a[href]" |> iter (fun a -> print_if_wiki (R.attribute "href" a))

(*
    Lwt_main.run (get_body "https://en.wikipedia.org/wiki/Special:Random")
let get_body url =
    Printf.sprintf "%s\n" (Lwt_main.run (get_body url))
 *)
