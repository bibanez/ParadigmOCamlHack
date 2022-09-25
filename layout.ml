module Layout : sig

  type article = string

  type layout = {
    articles : (article * float * float) list;
    connections : (article * article) list;
  }

  val compute_layout :
    get_links_function:(article -> article list) ->
    root:article ->
    max_depth:int ->
    layout

  val iterations : int ref

end = struct

  type article = string

  type layout = {
    articles : (article * float * float) list;
    connections : (article * article) list;
  }

  (* articles are identified by numeric IDs, and we use a hash table
   * to keep track of the mapping *)
  type id = int
  type id_cache = {
    tbl : (article, id) Hashtbl.t;
    next_id : id ref;
  }

  (* get the ID for an article from the cache, or create a new one *)
  let make_id (cache : id_cache) article =
    match Hashtbl.find_opt cache.tbl article with
    | Some id -> id
    | None ->
      let id = !(cache.next_id) in
      cache.next_id := id + 1;
      Hashtbl.add cache.tbl article id;
      id

  (* the first step is to cache the links in each article to avoid
   * making repeated calls to `get_links_function`. *)
  type links_cache = (id, id list) Hashtbl.t

  let create_caches ~get_links_function ~root ~max_depth
      : id_cache * links_cache =

    (* start with empty caches *)
    let id_cache = { tbl = Hashtbl.create 1000; next_id = ref 0 } in
    let links_cache : links_cache = Hashtbl.create 1000 in

    (* recusively add a particular article and its children to the cache.
     * doesn't check that the article is already present. *)
    let rec add_article depth article id =
      let links : (article * id) list =
        get_links_function article
        |> List.map (fun article -> (article, make_id id_cache article))
      in
      Hashtbl.add links_cache id (List.map snd links);
      (* recursively add children *)
      if depth < max_depth then links |> List.iter (fun ((linked : article), id') ->
          if not (Hashtbl.mem links_cache id') then
            add_article (depth + 1) linked id')
    in
    add_article 0 root (make_id id_cache root); (id_cache, links_cache)

  let cache_size id_cache = !(id_cache.next_id)

  (* second step: build an NxN matrix storing the strength of connections
   * between articles given by their IDs *)
  type connections_matrix = int array
  let build_connections_matrix n id_cache links_cache =
    let matrix : connections_matrix = Array.make (n * n) 0 in

    let add_connection a1 a2 =
      (* ignore order *)
      let (x, y) = if a1 < a2 then (a1, a2) else (a2, a1) in
      let idx = y * n + x in
      matrix.(idx) <- 1 + matrix.(idx)
    in

    links_cache |> Hashtbl.iter (fun x links ->
        List.iter (fun y -> add_connection x y) links);
    matrix

  let connection_strength matrix n a1 a2 =
    (* ignore order *)
    let (x, y) = if a1 < a2 then (a1, a2) else (a2, a1) in
    let idx = y * n + x in
    matrix.(idx)

  (* third step: build an array of particles representing the articles
   * and move them around according to the strength of their connections *)
  type particles = { x : float array; vx : float array;
                     y : float array; vy : float array }

  let init_particles n : particles =
    { x = Array.init n (fun _ -> Random.float 1.0);
      y = Array.init n (fun _ -> Random.float 1.0);
      vx = Array.make n 0.0;
      vy = Array.make n 0.0;
    }

  let move_particles n connection_matrix ({ x; y; vx; vy } : particles) =

    let acceleration_towards par cx cy a =
      let dx = x.(par) -. cx in
      let dy = y.(par) -. cy in
      let norm = 1. /. hypot dx dy in
      vx.(par) <- (vx.(par) +. (a *. dx *. norm)) *. 0.9995;
      vy.(par) <- (vy.(par) +. (a *. dy *. norm)) *. 0.9995
    in

    (* individual forces on particles *)
    (let keep_near_center par =
       let r = hypot (x.(par) -. 0.5) (y.(par) -. 0.5) in
       acceleration_towards par 0.5 0.5 (r *. r *. r *. r *. 0.0005)
     in

     for par = 0 to n - 1 do begin
       (* keep parect near the center *)
       keep_near_center par;
       x.(par) <- x.(par) +. vx.(par);
       y.(par) <- y.(par) +. vy.(par);
     end done
    );

    (* forces between particles *)
    for p1 = 0 to n - 2 do
      for p2 = p1 + 1 to n - 1 do
        let conn = connection_strength connection_matrix n p1 p2 in
        let r = hypot (x.(p1) -. x.(p2)) (y.(p1) -. y.(p2)) in
        let f =
          (* attractive force between connected particles *)
          1e-4 *. Float.min 0.5 r *. (if conn > 0 then 1.0 else 0.0)
          (* repulsive force between particles *)
          -. Float.min 0.01 (0.1e-7 /. (r *. r))
        in
        (acceleration_towards p1 (x.(p2)) (y.(p2)) f;
         acceleration_towards p2 (x.(p1)) (y.(p1)) f)
      done
    done

  let iterations = ref 1000

  let compute_layout ~get_links_function ~root ~max_depth =
    let id_cache, links_cache = create_caches ~get_links_function ~root ~max_depth in
    let n = cache_size id_cache in
    let connections_matrix = build_connections_matrix n id_cache links_cache in
    let particles = init_particles n in

    for i = 0 to !iterations do
      move_particles n connections_matrix particles
    done;

    let names_cache = Array.make n "" in
    id_cache.tbl |> Hashtbl.iter (fun article id -> names_cache.(id) <- article);

    let connections_list =
      (let res = ref [] in
       for p1 = 0 to n - 2 do
         for p2 = p1 + 1 to n - 1 do
           if connection_strength connections_matrix n p1 p2 > 0 then
             let item = (names_cache.(p1), names_cache.(p2)) in
             res := item :: !res
         done
       done;
       !res) in

    { articles =
        names_cache
        |> Array.to_seqi
        |> Seq.map (fun (id, article) -> (article, particles.x.(id), particles.y.(id)))
        |> List.of_seq;
      connections = connections_list }
end
