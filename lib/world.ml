open Map
open Player
open Tilemaps

type game_map = map list

type world = {
  game_map : game_map;
  map_table : (string, map) Hashtbl.t;
  mutable current_location : string;
}

let instance : world option ref = ref None

let initialize () =
  let initial_maps =
    [
      Map.make () "Shore1" shore1map (20, 11) [ ""; "Route4"; "Shore2"; "" ];
      Map.make () "Shore2" shore2map (20, 11) [ "Shore1"; ""; ""; "" ];
      Map.make () "Route4" route4 (20, 11) [ "Route3"; ""; ""; "Shore1" ];
      Map.make () "Route3" route3 (20, 11) [ "SandCastle"; ""; "Route4"; "" ];
      Map.make () "SandCastle" sand_castle (20, 11) [ ""; ""; "Route3"; "" ];
    ]
  in
  let map_table = Hashtbl.create 10 in
  List.iter (fun m -> Hashtbl.add map_table (name m) m) initial_maps;
  instance :=
    Some { game_map = initial_maps; map_table; current_location = "Shore1" }

let get_instance () =
  match !instance with
  | Some world -> world
  | None -> failwith "World uninitialized"

let get_map name =
  let world = get_instance () in
  match Hashtbl.find_opt world.map_table name with
  | Some map -> map
  | None -> failwith "map name area not found"

let update_map next (x, y) =
  let world = get_instance () in
  match Hashtbl.find_opt world.map_table next with
  | Some map ->
      world.current_location <- name map;
      create_player map (Some x, Some y)
  | None -> failwith "map name area not found"

(* let shore1 = Map.make () "shore" shore1map (20, 15) [ ""; "route 4"; "shore
   2"; "" ]

   let master_map = [ shore1 ] let world = { game_map = master_map;
   current_location = "Shore" } *)
