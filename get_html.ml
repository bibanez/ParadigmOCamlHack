open Lwt
open Cohttp
open Cohttp_lwt_unix
open Soup

type 'a tree = 
  | Leaf 
  | Node of 'a node
and 'a node = { 
  value: 'a; 
  left:  'a tree; 
  right: 'a tree
}

let get_body s =
    Client.get (Uri.of_string s) >>= fun (resp, body) ->
    Cohttp_lwt.Body.to_string body >|= fun body ->
    body

let remove_whitespaces s =
    let to_space = function
        | ' ' -> '_'
        | c -> c
    in
    String.map ~f:to_space s  

let add_if_wiki s original =
    if String.length s > 5 
        && not (String.equal "/wiki/Main_Page" s)
        && not (String.contains s ':')
        && String.equal "/wiki/" (String.common_prefix2 s "/wiki/")
        && not (String.equal ("/wiki/" ^ original) s)
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

let list_links s =
  let soup = Soup.parse (get_article s) in
  List.filter_map 
      ~f:(fun a -> add_if_wiki (R.attribute "href" a) s)
      (to_list (soup $$ "a[href]")) 
  |> remove_duplicates

let rec links_tree i s =
  let links = list_links s in
  if i = 0 then
    let rec acc = function
      | [] -> Leaf
      | h :: t -> Node {
        value = h;
        left  = Leaf;
        right = acc t
      }
    in
    acc links
  else
    let rec acc = function
      | [] -> Leaf
      | h :: t -> Node {
        value = h;
        left  = links_tree (i - 1) h;
        right = acc t
      }
    in
    acc links

  

(*
let rec links_recursive i s =
  if i = 0 then list_links s
  else List.map ~f:(links_recursive (i-1)) (list_links s)
 *)


(*
    soup $$ "a[href]" |> iter (fun a -> print_if_wiki (R.attribute "href" a));
    Lwt_main.run (get_body "https://en.wikipedia.org/wiki/Special:Random")
let get_body url =
    Printf.sprintf "%s\n" (Lwt_main.run (get_body url))
 *)
