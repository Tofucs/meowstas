open Tile
open Player

type map = {
  area_name : string;
  grid : tile array array;
  mutable player_pos : int option * int option;
  size : int * int;
  player : player;
  neighboring : string array;
}
(**RI: All tile arrays within grid are the same length. AF: A 2D-array of tiles
   of x length, and each array is y length, represents a xy-coordinate grid of
   size x*y. 0,0 is the top left grid tile, and x,y is the bottom right grid
   tile.*)

type move_map_callback = string -> int * int -> unit

exception PlayerUninstantiated

let make () name grid (width, height) neighboring =
  {
    area_name = name;
    grid;
    player_pos = (None, None);
    size = (width, height);
    player = Player.make ();
    neighboring = Array.of_list neighboring;
  }

let create_player map (start_pos : int option * int option) =
  map.player_pos <- start_pos

let get_player map = map.player
let get_grid map = map.grid

let extract_int opt =
  match opt with
  | Some i -> i
  | None -> 0

let size map = map.size
let name map = map.area_name
let get_width map = fst (size map)
let get_height map = snd (size map)

let query_tile map (x, y) =
  match (x, y) with
  | Some x, Some y ->
      Printf.printf "%d, %d" x y;
      map.grid.(y).(x)
  | _, _ -> raise PlayerUninstantiated

let get_player_pos map = map.player_pos

let within_bounds tile map =
  extract_int (fst tile) >= 0
  && extract_int (fst tile) < get_width map
  && extract_int (snd tile) >= 0
  && extract_int (snd tile) < get_height map

let update_location map attempt_move (callback : move_map_callback) =
  match map.player_pos with
  | None, _ -> raise PlayerUninstantiated
  | _, None -> raise PlayerUninstantiated
  | Some _, Some _ -> (
      let curr = get_player_pos map in
      let next =
        match attempt_move with
        | Idle -> curr
        | Up -> (fst curr, Some (extract_int (snd curr) - 1))
        | Down -> (fst curr, Some (extract_int (snd curr) + 1))
        | Left -> (Some (extract_int (fst curr) - 1), snd curr)
        | Right -> (Some (extract_int (fst curr) + 1), snd curr)
      in
      if within_bounds next map then
        match attempt_move with
        | Idle -> false
        | Up ->
            let move =
              match (query_tile map next).interact with
              | W ->
                  if map.player.state = North then (
                    map.player_pos <- next;
                    true)
                  else (
                    map.player.state <- North;
                    false)
              | NW ->
                  map.player.state <- North;
                  false (*don't change location as attempted move is blocked*)
              | IW _ ->
                  if map.player.state = North then (
                    map.player_pos <- next;
                    true)
                  else (
                    map.player.state <- North;
                    false)
              | INW _ ->
                  map.player.state <- North;
                  false
            in
            move
        | Down ->
            let move =
              match (query_tile map next).interact with
              | W ->
                  if map.player.state = South then (
                    map.player_pos <- next;
                    true)
                  else (
                    map.player.state <- South;
                    false)
              | NW ->
                  map.player.state <- South;
                  false
              | IW _ ->
                  if map.player.state = South then (
                    map.player_pos <- next;
                    true)
                  else (
                    map.player.state <- South;
                    false)
              | INW _ ->
                  map.player.state <- South;
                  false
            in
            move
        | Left ->
            let move =
              match (query_tile map next).interact with
              | W ->
                  if map.player.state = West then (
                    map.player_pos <- next;
                    true)
                  else (
                    map.player.state <- West;
                    false)
              | NW ->
                  map.player.state <- West;
                  false
              | IW _ ->
                  if map.player.state = West then (
                    map.player_pos <- next;
                    true)
                  else (
                    map.player.state <- West;
                    false)
              | INW _ ->
                  map.player.state <- West;
                  false
            in
            move
        | Right ->
            let move =
              match (query_tile map next).interact with
              | W ->
                  if map.player.state = East then (
                    map.player_pos <- next;
                    true)
                  else (
                    map.player.state <- East;
                    false)
              | NW ->
                  map.player.state <- East;
                  false
              | IW _ ->
                  if map.player.state = East then (
                    map.player_pos <- next;
                    true)
                  else (
                    map.player.state <- East;
                    false)
              | INW _ ->
                  map.player.state <- East;
                  false
            in
            move
      else
        match (query_tile map curr).interact with
        | IW LoadMap ->
            let y = extract_int (snd curr) and x = extract_int (fst curr) in
            let next_map =
              match attempt_move with
              | Up when y = 0 -> map.neighboring.(1)
              | Down when y = get_height map - 1 -> map.neighboring.(3)
              | Right when x = get_width map - 1 -> map.neighboring.(0)
              | Left when x = 0 -> map.neighboring.(2)
              | _ -> ""
            in
            let new_xy =
              match attempt_move with
              | Up -> (x, get_height map - 1)
              | Down -> (x, 0)
              | Right -> (0, y)
              | Left -> (get_width map - 1, y)
              | _ -> (0, 0)
            in
            if next_map <> "" then (
              Printf.printf "%d,%d" (fst new_xy) (snd new_xy);
              callback next_map new_xy;
              true)
            else false
        | _ -> false)
