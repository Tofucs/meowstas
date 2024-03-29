open Tile
open Player

type map = tile array array
(**RI: All tile arrays within this array are the same length*)
let size map = (Array.length map, Array.length map.(0))
let query_tile (map : tile array array) x y = map.(x).(y)

let update_location player attempt_move = 
  match attempt_move with
  | Up -> let pos = position player in (fst pos, snd pos + 1);
  | Down -> let pos = position player in (fst pos, snd pos - 1);
  | Left -> let pos = position player in (fst pos - 1, snd pos);
  | Right -> let pos = position player in (fst pos + 1, snd pos);

