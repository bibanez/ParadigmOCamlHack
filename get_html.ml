open Lwt
open Cohttp
open Cohttp_lwt_unix
open Lambdasoup

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

let add_if_wiki s =
    if String.length s > 5 
        && not (String.equal "/wiki/Main_Page" s)
        && not (String.contains s ':')
        && String.equal "/wiki/" (String.common_prefix2 s "/wiki/")
    then
    Some (String.drop_prefix s 6)
    else None

let get_article s =
    Lwt_main.run (get_body ("https://en.wikipedia.org/wiki/" ^ (remove_whitespaces s)))

let rec remove_duplicates l =
  let rec contains l n =
    match l with
    | [] -> false
    | h :: t ->
      String.equal h n || contains t n
  in
  match l with
  | [] -> []
  | h :: t ->
    let acc = remove_duplicates t in
    if contains acc h then acc else h :: acc

let print_links s =
    let soup = parse (get_article s) in
    List.filter_map 
        (to_list (soup $$ "a[href]")) 
        (fun a -> add_if_wiki (R.attribute "href" a))
    |> remove_duplicates
(*
    soup $$ "a[href]" |> iter (fun a -> print_if_wiki (R.attribute "href" a));
    Lwt_main.run (get_body "https://en.wikipedia.org/wiki/Special:Random")
let get_body url =
    Printf.sprintf "%s\n" (Lwt_main.run (get_body url))
 *)
