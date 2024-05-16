open Tsdl
open Tile
open Tsdl_image
open Tsdl_ttf
open Main_utils

module BattleMode : GameMode = struct
  type t = global_state

  (* positions start from top left, rectangle is defined by top left as well *)
  let pos1 = (425, 575)
  let pos2 = (1100, 325)
  let in_animation = ref false
  let animation_start = ref 0
  let animation_duration = ref 0
  let start_pos = ref (0, 0)
  let end_pos = ref (0, 0)
  let background_file = "textures/battle-background.png"
  let pokemon1_file = "textures/electro_base.png"
  let pokemon2_file = "textures/grass_base.png"

  let preload_textures renderer texture_table =
    let texture = load_texture_from_png renderer background_file in
    Hashtbl.add texture_table background_file texture;
    let texture = load_texture_from_png renderer pokemon1_file in
    Hashtbl.add texture_table pokemon1_file texture;
    let texture = load_texture_from_png renderer pokemon2_file in
    Hashtbl.add texture_table pokemon2_file texture

  let init state =
    if state.battle_state = None then (
      state.battle_state <- Some { place_holder = "battle started!" };
      preload_textures state.renderer state.texture_table)
    else ()

  let update state = ()

  let render_pokemon state sprite_name = function
    | x, y ->
        let dst = Sdl.Rect.create ~x ~y ~w:300 ~h:300 in
        let _ =
          Sdl.render_copy ~dst state.renderer
            (Hashtbl.find state.texture_table sprite_name)
        in
        ()

  let render state =
    let renderer = state.renderer in
    let _ = Sdl.render_clear renderer in
    let _ =
      Sdl.render_copy renderer
        (Hashtbl.find state.texture_table background_file)
    in
    render_pokemon state pokemon1_file pos1;
    render_pokemon state pokemon2_file pos2;
    Sdl.render_present renderer

  let handle_events state =
    let e = Sdl.Event.create () in
    while Sdl.poll_event (Some e) do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> state.is_running <- false
      | _ -> ()
    done
end
