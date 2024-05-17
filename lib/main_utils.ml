open Tsdl
open Tsdl_image
open Meowsta

let load_texture_from_png renderer file =
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

let load_texture_from_bmp renderer file =
  match Sdl.load_bmp file with
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

type roaming_state = {
  world : World.world;
  mutable in_animation : bool;
  mutable in_transition : bool;
  mutable animation_start : int;
  mutable animation_duration : int;
  mutable start_pos : int * int;
  mutable end_pos : int * int;
}

type battle_state = {
  mutable player : Meowsta.t;
  mutable enemy : Meowsta.t;
}

type action_state =
  | Roaming
  | Battle
  | Menu

type global_state = {
  mutable is_running : bool;
  mutable renderer : Sdl.renderer;
  mutable window : Sdl.window;
  mutable window_size : int * int;
  mutable action_state : action_state;
  mutable battle_state : battle_state option;
  mutable roaming_state : roaming_state option;
  texture_table : (string, Sdl.texture) Hashtbl.t;
  mutable previous_state : action_state;
}

(* the global state is initialized in main, each action state is initialized by
   the inits in each game mode *)
module type GameMode = sig
  type t = global_state

  val init : t -> unit
  (** initializes the state of that specific gamemode, only called the first
      time the state is entered *)

  val update : t -> unit
  val render : t -> unit
  val handle_events : t -> unit
end
