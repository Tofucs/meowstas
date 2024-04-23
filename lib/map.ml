open Tile
open Player

type map = {
  area_name : string;
  grid : tile array array;
  mutable player_pos : int option * int option;
  size : int * int;
  player : player;
}
(**RI: All tile arrays within grid are the same length. AF: A 2D-array of tiles
   of x length, and each array is y length, represents a xy-coordinate grid of
   size x*y. 0,0 is the top left grid tile, and x,y is the bottom right grid
   tile.*)

exception PlayerUninstantiated

let make () name grid (width, height) =
  {
    area_name = name;
    grid;
    player_pos = (None, None);
    size = (32 * width, 32 * height);
    player = Player.make ();
  }

let create_player map (start_pos : int option * int option) =
  map.player_pos <- start_pos

let get_grid map = map.grid

let extract_int opt =
  match opt with
  | Some i -> i
  | None -> 0

let size map = map.size
let name map = map.area_name

let query_tile map (x, y) =
  match (x, y) with
  | Some x, Some y -> map.grid.(x).(y)
  | _, _ -> raise PlayerUninstantiated

let get_player_pos map = map.player_pos

let update_location map attempt_move =
  match map.player_pos with
  | None, _ -> raise PlayerUninstantiated
  | _, None -> raise PlayerUninstantiated
  | Some _, Some _ -> (
      match attempt_move with
      | Idle -> ()
      | Up ->
          let curr = get_player_pos map in
          let next = (fst curr, Some (extract_int (snd curr) + 1)) in
          let move =
            match query_tile map next with
            | W -> map.player_pos <- next
            | NW ->
                map.player.state <-
                  North (*don't change location as attempted move is blocked*)
            | IW -> map.player_pos <- next
            | INW -> map.player.state <- North
          in
          move
      | Down ->
          let curr = get_player_pos map in
          let next = (fst curr, Some (extract_int (snd curr) - 1)) in
          let move =
            match query_tile map next with
            | W -> map.player_pos <- next
            | NW -> map.player.state <- South
            | IW -> map.player_pos <- next
            | INW -> map.player.state <- South
          in
          move
      | Left ->
          let curr = get_player_pos map in
          let next = (Some (extract_int (fst curr) - 1), snd curr) in
          let move =
            match query_tile map next with
            | W -> map.player_pos <- next
            | NW -> map.player.state <- West
            | IW -> map.player_pos <- next
            | INW -> map.player.state <- West
          in
          move
      | Right ->
          let curr = get_player_pos map in
          let next = (Some (extract_int (fst curr) + 1), snd curr) in
          let move =
            match query_tile map next with
            | W -> map.player_pos <- next
            | NW -> map.player.state <- East
            | IW -> map.player_pos <- next
            | INW -> map.player.state <- East
          in
          move)
