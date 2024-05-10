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

let print_grid (grid : Tile.tile array array) =
  Array.iter
    (fun row ->
      Array.iter
        (fun tile ->
          print_string
            (match tile with
            | W -> "W " (* Walkable tile *)
            | NW -> "NW " (* Non-walkable tile *)
            | IW -> "IW " (* Interactive walkable tile *)
            | INW -> "INW " (* Interactive non-walkable tile *)))
        row;
      print_newline () (* Move to the next line after each row *))
    grid

let make () name grid (width, height) =
  print_grid grid;
  {
    area_name = name;
    grid;
    player_pos = (None, None);
    size = (32 * width, 32 * height);
    player = Player.make ();
  }

let create_player map (start_pos : int option * int option) =
  map.player_pos <- start_pos

let get_player map = map.player
let get_grid map = map.grid

let extract_int opt =
  match opt with
  | Some i -> i
  | None -> 0

let print_tile tile =
  match tile with
  | W -> Printf.printf "W"
  | NW -> Printf.printf "NW"
  | IW -> Printf.printf "IW"
  | INW -> Printf.printf "INW"

let size map = map.size
let name map = map.area_name

let query_tile map (x, y) =
  match (x, y) with
  | Some x, Some y ->
      Printf.printf "%d, %d" x y;
      map.grid.(y).(x)
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
          let next = (fst curr, Some (extract_int (snd curr) - 1)) in
          let move =
            match query_tile map next with
            | W ->
                map.player_pos <- next;
                map.player.state <- North
            | NW ->
                map.player.state <-
                  North (*don't change location as attempted move is blocked*)
            | IW ->
                map.player_pos <- next;
                map.player.state <- North
            | INW -> map.player.state <- North
          in
          move
      | Down ->
          let curr = get_player_pos map in
          let next = (fst curr, Some (extract_int (snd curr) + 1)) in
          let move =
            print_tile (query_tile map next);
            match query_tile map next with
            | W ->
                map.player_pos <- next;
                map.player.state <- South
            | NW -> map.player.state <- South
            | IW ->
                map.player_pos <- next;
                map.player.state <- South
            | INW -> map.player.state <- South
          in
          move
      | Left ->
          let curr = get_player_pos map in
          let next = (Some (extract_int (fst curr) - 1), snd curr) in
          let move =
            match query_tile map next with
            | W ->
                map.player_pos <- next;
                map.player.state <- West
            | NW -> map.player.state <- West
            | IW ->
                map.player_pos <- next;
                map.player.state <- West
            | INW -> map.player.state <- West
          in
          move
      | Right ->
          let curr = get_player_pos map in
          let next = (Some (extract_int (fst curr) + 1), snd curr) in
          let move =
            match query_tile map next with
            | W ->
                map.player_pos <- next;
                map.player.state <- East
            | NW -> map.player.state <- East
            | IW ->
                map.player_pos <- next;
                map.player.state <- East
            | INW -> map.player.state <- East
          in
          move)
