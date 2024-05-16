open Tsdl
open Tile
open Button
open Tsdl_image
open Tsdl_ttf
open Main_utils
open Trainer
open Meowsta
open Items

module MenuMode : GameMode = struct
  type t = global_state

  let buttons_visible = ref true
  let bag_buttons_visible = ref false
  let party_buttons_visible = ref false
  let other_meowstas_buttons_visible = ref false
  let party_button = ref None
  let other_meowstas_button = ref None
  let bag_button = ref None
  let exit_button = ref None
  (* type screen_state = | Game | Menu | Party | OtherMeowstas | Bag *)

  (* let exit_button = Button.create renderer ~x:x_val ~y:y_val ~w:250 ~h:60
     ~text:item_string ?texture:None ~action:f ~action_param:x *)

  (*Reference to store the button *)

  let button_action param =
    print_endline ("Button pressed with param: " ^ string_of_int param)

  let rec count x lst =
    match lst with
    | [] -> 0
    | h :: t -> if h = x then 1 + count x t else count x t

  let rec contains x lst =
    match lst with
    | [] -> false
    | h :: t -> h = x || contains x t

  let render_bag_item renderer index item_string f x =
    let x_val = 60 + (300 * (index mod 6)) in
    let y_val = 100 + (70 * (index / 6)) in

    let btn =
      Button.create renderer ~x:x_val ~y:y_val ~w:250 ~h:60 ~text:item_string
        ?texture:None ~action:f ~action_param:x
    in
    btn

  let party_buttons_list = ref []

  let render_party_screen state =
    Sdl.set_render_draw_color state.renderer 200 200 200 200 |> ignore;
    Sdl.render_clear state.renderer |> ignore;

    Array.iteri
      (fun i item ->
        if item != empty then
          party_buttons_list :=
            render_bag_item state.renderer i item.name print_endline "123"
            :: !party_buttons_list)
      Trainer.party_meowsta

  let other_meowstas_buttons_list = ref []

  let render_other_meowstas_screen state =
    Sdl.set_render_draw_color state.renderer 200 200 200 200 |> ignore;
    Sdl.render_clear state.renderer |> ignore;

    List.iteri
      (fun i item ->
        other_meowstas_buttons_list :=
          render_bag_item state.renderer i item.name print_endline "123"
          :: !other_meowstas_buttons_list)
      !Trainer.extra_meowsta

  let bag_buttons_list = ref []

  let render_bag_screen state =
    Sdl.set_render_draw_color state.renderer 200 200 200 200 |> ignore;
    Sdl.render_clear state.renderer |> ignore;

    let items_set = ref [] in

    List.iter
      (fun i ->
        if not (contains i !items_set) then items_set := i :: !items_set)
      !Trainer.item_bag;
    List.iteri
      (fun i item ->
        match item with
        | NO -> ()
        | HardStone ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("HardStone x"
                ^ string_of_int (count HardStone !Trainer.item_bag))
                print_endline "HardStone"
              :: !bag_buttons_list
        | BlackBelt ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("BlackBelt x"
                ^ string_of_int (count BlackBelt !Trainer.item_bag))
                print_endline "BlackBelt"
              :: !bag_buttons_list
        | BlackGlasses ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("BlackGlasses x"
                ^ string_of_int (count BlackGlasses !Trainer.item_bag))
                print_endline "BlackGlasses"
              :: !bag_buttons_list
        | Charcoal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Charcoal x" ^ string_of_int (count Charcoal !Trainer.item_bag))
                print_endline "Charcoal"
              :: !bag_buttons_list
        | DragonFang ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("DragonFang x"
                ^ string_of_int (count DragonFang !Trainer.item_bag))
                print_endline "DragonFang"
              :: !bag_buttons_list
        | FairyFeather ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("FairyFeather x"
                ^ string_of_int (count FairyFeather !Trainer.item_bag))
                print_endline "FairyFeather"
              :: !bag_buttons_list
        | Magnet ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Magnet x" ^ string_of_int (count Magnet !Trainer.item_bag))
                print_endline "Magnet"
              :: !bag_buttons_list
        | MetalCoat ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("MetalCoat x"
                ^ string_of_int (count MetalCoat !Trainer.item_bag))
                print_endline "MetalCoat"
              :: !bag_buttons_list
        | MiracleSeed ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("MiracleSeed\n           x"
                ^ string_of_int (count MiracleSeed !Trainer.item_bag))
                print_endline "MiracleSeed"
              :: !bag_buttons_list
        | MysticWater ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("MysticWater\n           x"
                ^ string_of_int (count MysticWater !Trainer.item_bag))
                print_endline "MysticWater"
              :: !bag_buttons_list
        | NeverMeltIce ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("NeverMeltIce\n           x"
                ^ string_of_int (count NeverMeltIce !Trainer.item_bag))
                print_endline "NeverMeltIce"
              :: !bag_buttons_list
        | PoisonBarb ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("PoisonBarb x"
                ^ string_of_int (count PoisonBarb !Trainer.item_bag))
                print_endline "PoisonBarb"
              :: !bag_buttons_list
        | SharpBeak ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SharpBeak x"
                ^ string_of_int (count SharpBeak !Trainer.item_bag))
                print_endline "SharpBeak"
              :: !bag_buttons_list
        | SilkScarf ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SilkScarf x"
                ^ string_of_int (count SilkScarf !Trainer.item_bag))
                print_endline "SilkScarf"
              :: !bag_buttons_list
        | SilverPowder ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SilverPowder x"
                ^ string_of_int (count SilverPowder !Trainer.item_bag))
                print_endline "SilverPowder"
              :: !bag_buttons_list
        | SoftSand ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SoftSand x" ^ string_of_int (count SoftSand !Trainer.item_bag))
                print_endline "SoftSand"
              :: !bag_buttons_list
        | SpellTag ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SpellTag x" ^ string_of_int (count SpellTag !Trainer.item_bag))
                print_endline "SpellTag"
              :: !bag_buttons_list
        | TwistedSpoon ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("TwistedSpoon\n           x"
                ^ string_of_int (count TwistedSpoon !Trainer.item_bag))
                print_endline "TwistedSpoon"
              :: !bag_buttons_list
        | Potion ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Potion x" ^ string_of_int (count Potion !Trainer.item_bag))
                print_endline "Potion"
              :: !bag_buttons_list
        | SuperPotion ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SuperPotion x"
                ^ string_of_int (count SuperPotion !Trainer.item_bag))
                print_endline "SuperPotion"
              :: !bag_buttons_list
        | HyperPotion ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("HyperPotion x"
                ^ string_of_int (count HyperPotion !Trainer.item_bag))
                print_endline "HyperPotion"
              :: !bag_buttons_list
        | Revive ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Revive x" ^ string_of_int (count Revive !Trainer.item_bag))
                print_endline "Revive"
              :: !bag_buttons_list
        | ParalyzeHeal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("ParalyzeHeal x"
                ^ string_of_int (count ParalyzeHeal !Trainer.item_bag))
                print_endline "ParalyzeHeal"
              :: !bag_buttons_list
        | BurnHeal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("BurnHeal x" ^ string_of_int (count BurnHeal !Trainer.item_bag))
                print_endline "BurnHeal"
              :: !bag_buttons_list
        | Antidote ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Antidote x" ^ string_of_int (count Antidote !Trainer.item_bag))
                print_endline "Antidote"
              :: !bag_buttons_list
        | FreezeHeal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("FreezeHeal x"
                ^ string_of_int (count FreezeHeal !Trainer.item_bag))
                print_endline "FreezeHeal"
              :: !bag_buttons_list
        | SleepHeal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SleepHeal x"
                ^ string_of_int (count SleepHeal !Trainer.item_bag))
                print_endline "SleepHeal"
              :: !bag_buttons_list
        | MeowstaBall ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("MeowstaBall x"
                ^ string_of_int (count MeowstaBall !Trainer.item_bag))
                print_endline "MeowstaBall"
              :: !bag_buttons_list
        | GreatMeowstaBall ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("GreatMeowstaBall x"
                ^ string_of_int (count GreatMeowstaBall !Trainer.item_bag))
                print_endline "GreatMeowstaBall"
              :: !bag_buttons_list
        | UltraMeowstaBall ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("UltraMeowstaBall x"
                ^ string_of_int (count UltraMeowstaBall !Trainer.item_bag))
                print_endline "UltraMeowstaBall"
              :: !bag_buttons_list)
      !items_set (* Add item to set *)

  (* let list_items_set = StringSet.to_list !items_set *)

  (* Draw exit button *)
  (* let exit_button_rect = Sdl.Rect.create ~x:530 ~y:15 ~w:90 ~h:50 in match
     Sdl.render_copy ~dst:exit_button_rect renderer textures.exit with | Ok ()
     -> () | Error (`Msg e) -> Sdl.log "Failed to\n render exit button: %s" e *)

  let init state =
    if state.menu_state = None then (
      let btn =
        Button.create state.renderer ~x:400 ~y:500 ~w:150 ~h:50 ~text:"PARTY"
          ?texture:None ~action:print_endline ~action_param:"123"
      in
      party_button := Some btn;
      let btn2 =
        Button.create state.renderer ~x:800 ~y:500 ~w:300 ~h:50
          ~text:"OTHER MEOWSTAS" ?texture:None ~action:print_endline
          ~action_param:"123"
      in
      other_meowstas_button := Some btn2;
      let btn3 =
        Button.create state.renderer ~x:1300 ~y:500 ~w:120 ~h:50 ~text:"BAG"
          ?texture:None ~action:print_endline ~action_param:"123"
      in
      bag_button := Some btn3;
      let exitbtn =
        Button.create state.renderer ~x:1750 ~y:40 ~w:100 ~h:40 ~text:"EXIT"
          ?texture:None ~action:print_endline ~action_param:""
      in
      exit_button := Some exitbtn)
    else ()

  let update state = ()
  (* print_int (fst state.window_size) *)

  let render state =
    let renderer = state.renderer in
    let _ = Sdl.render_clear renderer in

    if !buttons_visible then begin
      Option.iter (fun btn -> Button.render btn) !party_button;
      Option.iter (fun btn -> Button.render btn) !other_meowstas_button;
      Option.iter (fun btn -> Button.render btn) !bag_button
    end
    else if !bag_buttons_visible then (
      render_bag_screen state;
      List.iter (fun b -> Button.render b) !bag_buttons_list;
      Option.iter (fun btn -> Button.render btn) !exit_button)
    else if !party_buttons_visible then (
      render_party_screen state;
      List.iter (fun b -> Button.render b) !party_buttons_list;
      Option.iter (fun btn -> Button.render btn) !exit_button)
    else if !other_meowstas_buttons_visible then begin
      render_other_meowstas_screen state;
      List.iter (fun b -> Button.render b) !other_meowstas_buttons_list;
      Option.iter (fun btn -> Button.render btn) !exit_button
    end;
    Sdl.render_present renderer

  (* let handle_events state = let e = Sdl.Event.create () in while
     Sdl.poll_event (Some e) do match Sdl.Event.(enum (get e typ)) with | `Quit
     -> state.is_running <- false | _ -> () done *)

  let handle_events state =
    let e = Sdl.Event.create () in
    while Sdl.poll_event (Some e) do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> state.is_running <- false
      (* | `Key_down -> let key = Sdl.Event.(get e keyboard_keycode) in if key =
         Sdl.K.space then ( is_running := false; buttons_visible := not
         !buttons_visible) *)
      | `Mouse_button_down ->
          let x = Sdl.Event.(get e mouse_button_x) in
          let y = Sdl.Event.(get e mouse_button_y) in
          if x >= 1300 && x <= 1300 + 120 && y >= 500 && y <= 500 + 50 then (
            buttons_visible := false;
            bag_buttons_visible := true)
          else if x >= 800 && x <= 800 + 350 && y >= 500 && y <= 500 + 50 then (
            buttons_visible := false;
            other_meowstas_buttons_visible := true)
          else if x >= 400 && x <= 400 + 150 && y >= 500 && y <= 500 + 50 then (
            buttons_visible := false;
            party_buttons_visible := true)
          else if x >= 1750 && x <= 1750 + 100 && y >= 40 && y <= 40 + 40 then (
            (* Option.iter (fun btn -> Button.handle_event btn e) !party_button;
               Option.iter (fun btn -> Button.handle_event btn e)
               !other_meowstas_button; Option.iter (fun btn ->
               Button.handle_event btn e) !bag_button *)
            buttons_visible := true;
            party_buttons_visible := false;
            other_meowstas_buttons_visible := false;
            bag_buttons_visible := false)
      | _ -> ()
      (* if !buttons_visible then begin Option.iter (fun btn ->
         Button.handle_event btn e) !party_button; Option.iter (fun btn ->
         Button.handle_event btn e) !other_meowstas_button; Option.iter (fun btn
         -> Button.handle_event btn e) !bag_button end else if
         !bag_buttons_visible then ( render_bag_screen state; List.iter (fun b
         -> Button.handle_event b e) !bag_buttons_list) else if
         !party_buttons_visible then ( render_party_screen state; List.iter (fun
         b -> Button.handle_event b e) !party_buttons_list) else if
         !other_meowstas_buttons_visible then ( render_other_meowstas_screen
         state; List.iter (fun b -> Button.handle_event b e)
         !other_meowstas_buttons_list) *)
    done
end

(* let on_destroy () = Option.iter Button.destroy !button; Sdl.destroy_renderer
   (Option.get !renderer); Sdl.destroy_window (Option.get !window); Sdl.quit ();
   print_endline "Quit Application" *)
