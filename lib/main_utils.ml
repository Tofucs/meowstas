open Tsdl
open Tsdl_image

let load_texture_from_file renderer file =
  match Image.load file with
  | Error (`Msg e) ->
      Sdl.log "Failed to load %s: %s" file e;
      exit 1
  | Ok surface -> (
      match Sdl.create_texture_from_surface renderer surface with
      | Error (`Msg e) ->
          Sdl.log "Create texture from surface error: %s" e;
          exit 1
      | Ok texture ->
          Sdl.free_surface surface;
          texture)

type action_state =
  | Roaming
  | Battle
  | Interlude

type global_state = {
  mutable renderer : Sdl.renderer;
  mutable window : Sdl.window;
  mutable window_size : int * int;
  mutable action_state : action_state;
  mutable world : World.world;
}

module type GameState = sig
  type t = global_state option

  val update : t -> unit
  val render : t -> unit
  val handle_event : t -> unit
end
