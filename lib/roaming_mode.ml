open Tsdl
open Tile
open Tsdl_image
open Tsdl_ttf
open Main_utils

module RoamingMode : GameMode = struct
  type t = global_state

  let extract_int opt =
    match opt with
    | Some i -> i
    | None -> 0

  let init state =
    if state.roaming_state = None then
      state.roaming_state <-
        Some
          {
            world = World.get_instance ();
            in_animation = false;
            in_transition = false;
            animation_start = 0;
            animation_duration = 0;
            start_pos = (0, 0);
            end_pos = (0, 0);
          }
    else ()

  let update state = print_int (snd state.window_size)

  let render state =
    let renderer = state.renderer in
    let _ = Sdl.render_clear renderer in
    Sdl.render_present renderer

  let handle_events state =
    let e = Sdl.Event.create () in
    while Sdl.poll_event (Some e) do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> state.is_running <- false
      | _ -> ()
    done
end
