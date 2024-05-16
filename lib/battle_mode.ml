open Tsdl
open Tile
open Tsdl_image
open Tsdl_ttf
open Main_utils

module BattleMode : GameMode = struct
  type t = global_state

  let extract_int opt =
    match opt with
    | Some i -> i
    | None -> 0

  let init state =
    if state.battle_state = None then
      state.battle_state <- Some { place_holder = "battle started!" }
    else ()

  let update state = ()
  (* print_int (fst state.window_size) *)

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
