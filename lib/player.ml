type player = { position : int * int }

type moves =
  | Up
  | Down
  | Left
  | Right

let position player = player.position
