(*think of as current player velocity*)
type moves =
  | Idle
  | Up
  | Down
  | Left
  | Right

(*think of as current player state*)
type facing =
  | North
  | South
  | West
  | East

type player = {
  mutable velocity : moves;
  mutable state : facing;
}

let make () = { velocity = Idle; state = South }

(*state helps us understand what to do with player input - i.e. facing east and
  the block on the right is a sign, with specified interact key, will read the
  sign. Given knowledge about the entire game world, certain inputs mean nothing
  in certain states, and others should only matter in certain states.*)

(*velocity helps the graphics understand how to update player character.
  Changing the magnitude of the velocity isn't super crucial in a game like
  pokemon; instead, the direction of the vector tells us, for example, what kind
  of animation a sprite should have.*)
