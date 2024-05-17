open Tsdl
open Tsdl_image
open Tsdl_ttf
open Items
open Meowsta

type t = {
  rect : Sdl.rect;
  text : string;
  texture : Sdl.texture;
  renderer : Sdl.renderer;
  item : items;
  meowsta : Meowsta.t;
}
(** [type t] is the button type*)

val create :
  ?font_size:int ->
  ?font_color:Sdl.color ->
  Sdl.renderer ->
  x:int ->
  y:int ->
  w:int ->
  h:int ->
  text:string ->
  item:items ->
  meowsta:Meowsta.t ->
  t
(** [create] creates a button*)

val render : t -> unit
(** [render] renders the actual button for the user to see*)
