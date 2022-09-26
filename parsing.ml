open Base

open Lwt
(* open Cohttp *)
open Cohttp_lwt_unix
open Soup

(*  Binary tree *)
type 'a tree = 
  | Leaf 
  | Node of 'a node
and 'a node = { 
  value: 'a; 
  left:  'a tree; 
  right: 'a tree
}

(*  Helper functions *)

let wiki a = "https://en.wikipedia.org/wiki/" ^ a

let remove_whitespaces s =
    let to_space = function
        | ' ' -> '_'
        | c -> c
    in
    String.map ~f:to_space s  

(* Remove duplicates in a list of strings *)
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

(*  Get html body of a given url *)
let get_body s =
    Client.get (Uri.of_string s) >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body >|= fun body ->
    body

(*  Parse the correct urls for wikipedia *)
let add_if_wiki s original =
    if String.length s > 5 
        && not (String.equal "/wiki/Main_Page" s)
        && not (String.contains s ':')
        && String.equal "/wiki/" (String.common_prefix2 s "/wiki/")
        && not (String.equal ("/wiki/" ^ original) s)
    then
    Some (String.drop_prefix s 6)
    else None

(*  GET the html body for the wikedia article s *)
let get_article s =
    Lwt_main.run (get_body (remove_whitespaces s |> wiki))

(*  Parse a wikipedia article for references to other wikipedia pages and 
    generate a list *)
let list_links s =
  let soup = Soup.parse (get_article s) in
  List.filter_map 
      ~f:(fun a -> add_if_wiki (R.attribute "href" a) s)
      (to_list (soup $$ "a[href]")) 
  |> remove_duplicates

(*  Generate a multitree (implemented as binary tree with right: next sibling 
    and left: first son) with depth i for the article in s *)
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

(*  Generate a nested list structure with depth i and the links in article s *)
let rec links_tree_html i s =
  let links = list_links s in
  let e = create_element "ul" in
  let () =
    if i = 0 then
      List.iter links ~f:(fun a -> create_element "li" 
                                  ~inner_text:a 
                                  ~attributes:["href",wiki a] 
                                  |> append_child e)
    else
      List.iter links ~f:(fun a -> append_child e (links_tree_html (i-1) a));
    if List.length links > 0 then
      create_element "li" ~inner_text:s ~attributes:["href", wiki s]
        |> prepend_child e;
  in e

let save_article_tree i s out =
  let t = links_tree_html i s in
  write_file out (to_string t)
