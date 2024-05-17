open Tsdl

type t = {
  mutable rect : Sdl.rect;
  mutable text : string;
  mutable texture : Sdl.texture;
  renderer : Sdl.renderer;
  font_size : int;
  font_color : Sdl.color;
}

val create :
  ?font_size:int ->
  ?font_color:Sdl.color ->
  Sdl.renderer ->
  x:int ->
  y:int ->
  text:string ->
  t
(** [create] creates a label*)

val add_text : t -> string -> unit
(** [add_text] adds to the text of the label*)

val render : t -> unit
(** [render] renders the actual label for the user to see*)
