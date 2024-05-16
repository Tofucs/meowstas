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


  let rec count x lst =
    match lst with
    | [] -> 0
    | h :: t -> if h = x then 1 + count x t else count x t

  let rec contains x lst =
    match lst with
    | [] -> false
    | h :: t -> h = x || contains x t

  let render_bag_item renderer index item_string item m =
    let x_val = 60 + (300 * (index mod 6)) in
    let y_val = 100 + (70 * (index / 6)) in

    let btn =
      Button.create renderer ~x:x_val ~y:y_val ~w:250 ~h:60 ~text:item_string
        ~item ~meowsta:m
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
            render_bag_item state.renderer i item.name NO item
            :: !party_buttons_list)
      Trainer.party_meowsta

  let other_meowstas_buttons_list = ref []

  let render_other_meowstas_screen state =
    Sdl.set_render_draw_color state.renderer 200 200 200 200 |> ignore;
    Sdl.render_clear state.renderer |> ignore;

    List.iteri
      (fun i item ->
        other_meowstas_buttons_list :=
          render_bag_item state.renderer i item.name NO item
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
                HardStone Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | BlackBelt ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("BlackBelt x"
                ^ string_of_int (count BlackBelt !Trainer.item_bag))
                BlackBelt Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | BlackGlasses ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("BlackGlasses x"
                ^ string_of_int (count BlackGlasses !Trainer.item_bag))
                BlackGlasses Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | Charcoal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Charcoal x" ^ string_of_int (count Charcoal !Trainer.item_bag))
                Charcoal Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | DragonFang ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("DragonFang x"
                ^ string_of_int (count DragonFang !Trainer.item_bag))
                DragonFang Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | FairyFeather ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("FairyFeather x"
                ^ string_of_int (count FairyFeather !Trainer.item_bag))
                FairyFeather Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | Magnet ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Magnet x" ^ string_of_int (count Magnet !Trainer.item_bag))
                Magnet Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | MetalCoat ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("MetalCoat x"
                ^ string_of_int (count MetalCoat !Trainer.item_bag))
                MetalCoat Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | MiracleSeed ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("MiracleSeed x"
                ^ string_of_int (count MiracleSeed !Trainer.item_bag))
                MiracleSeed Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | MysticWater ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("MysticWater x"
                ^ string_of_int (count MysticWater !Trainer.item_bag))
                MysticWater Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | NeverMeltIce ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("NeverMeltIce x"
                ^ string_of_int (count NeverMeltIce !Trainer.item_bag))
                NeverMeltIce Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | PoisonBarb ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("PoisonBarb x"
                ^ string_of_int (count PoisonBarb !Trainer.item_bag))
                PoisonBarb Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | SharpBeak ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SharpBeak x"
                ^ string_of_int (count SharpBeak !Trainer.item_bag))
                SharpBeak Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | SilkScarf ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SilkScarf x"
                ^ string_of_int (count SilkScarf !Trainer.item_bag))
                SilkScarf Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | SilverPowder ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SilverPowder x"
                ^ string_of_int (count SilverPowder !Trainer.item_bag))
                SilverPowder Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | SoftSand ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SoftSand x" ^ string_of_int (count SoftSand !Trainer.item_bag))
                SoftSand Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | SpellTag ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SpellTag x" ^ string_of_int (count SpellTag !Trainer.item_bag))
                SpellTag Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | TwistedSpoon ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("TwistedSpoon x"
                ^ string_of_int (count TwistedSpoon !Trainer.item_bag))
                TwistedSpoon Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | Potion ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Potion x" ^ string_of_int (count Potion !Trainer.item_bag))
                Potion Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | SuperPotion ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SuperPotion x"
                ^ string_of_int (count SuperPotion !Trainer.item_bag))
                SuperPotion Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | HyperPotion ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("HyperPotion x"
                ^ string_of_int (count HyperPotion !Trainer.item_bag))
                HyperPotion Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | Revive ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Revive x" ^ string_of_int (count Revive !Trainer.item_bag))
                Revive Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | ParalyzeHeal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("ParalyzeHeal x"
                ^ string_of_int (count ParalyzeHeal !Trainer.item_bag))
                ParalyzeHeal Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | BurnHeal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("BurnHeal x" ^ string_of_int (count BurnHeal !Trainer.item_bag))
                BurnHeal Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | Antidote ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("Antidote x" ^ string_of_int (count Antidote !Trainer.item_bag))
                Antidote Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | FreezeHeal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("FreezeHeal x"
                ^ string_of_int (count FreezeHeal !Trainer.item_bag))
                FreezeHeal Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | SleepHeal ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("SleepHeal x"
                ^ string_of_int (count SleepHeal !Trainer.item_bag))
                SleepHeal Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | MeowstaBall ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("MeowstaBall x"
                ^ string_of_int (count MeowstaBall !Trainer.item_bag))
                MeowstaBall Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | GreatMeowstaBall ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("GreatMeowstaBall x"
                ^ string_of_int (count GreatMeowstaBall !Trainer.item_bag))
                GreatMeowstaBall Meowsta_dictionary.clawson
              :: !bag_buttons_list
        | UltraMeowstaBall ->
            bag_buttons_list :=
              render_bag_item state.renderer i
                ("UltraMeowstaBall x"
                ^ string_of_int (count UltraMeowstaBall !Trainer.item_bag))
                UltraMeowstaBall Meowsta_dictionary.clawson
              :: !bag_buttons_list)
      !items_set (* Add item to set *)

  let init state =
    if state.menu_state = None then (
      let btn =
        Button.create state.renderer ~x:400 ~y:500 ~w:150 ~h:50 ~text:"PARTY"
          ~item:NO ~meowsta:Meowsta_dictionary.clawson
      in
      party_button := Some btn;
      let btn2 =
        Button.create state.renderer ~x:800 ~y:500 ~w:300 ~h:50
          ~text:"OTHER MEOWSTAS" ~item:NO ~meowsta:Meowsta_dictionary.clawson
      in
      other_meowstas_button := Some btn2;
      let btn3 =
        Button.create state.renderer ~x:1300 ~y:500 ~w:120 ~h:50 ~text:"BAG"
          ~item:NO ~meowsta:Meowsta_dictionary.clawson
      in
      bag_button := Some btn3;
      let exitbtn =
        Button.create state.renderer ~x:1750 ~y:40 ~w:100 ~h:40 ~text:"EXIT"
          ~item:NO ~meowsta:Meowsta_dictionary.clawson
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
      List.iter (fun b -> Button.render b) !bag_buttons_list;
      Option.iter (fun btn -> Button.render btn) !exit_button)
    else if !party_buttons_visible then (
      List.iter (fun b -> Button.render b) !party_buttons_list;
      Option.iter (fun btn -> Button.render btn) !exit_button)
    else if !other_meowstas_buttons_visible then begin
      List.iter (fun b -> Button.render b) !other_meowstas_buttons_list;
      Option.iter (fun btn -> Button.render btn) !exit_button
    end;
    Sdl.render_present renderer

  let is_pressed rect x y =
    let rx = Sdl.Rect.x rect in
    let ry = Sdl.Rect.y rect in
    let rw = Sdl.Rect.w rect in
    let rh = Sdl.Rect.h rect in
    x >= rx && x <= rx + rw && y >= ry && y <= ry + rh

  let rec if_item lst x y =
    match lst with
    | [] -> None
    | h :: t -> if is_pressed h.rect x y then Some h else if_item t x y

  let handle_events state =
    let e = Sdl.Event.create () in
    while Sdl.poll_event (Some e) do
      match Sdl.Event.(enum (get e typ)) with
      | `Quit -> state.is_running <- false
      | `Mouse_button_down ->
          let x = Sdl.Event.(get e mouse_button_x) in
          let y = Sdl.Event.(get e mouse_button_y) in

          if x >= 1750 && x <= 1750 + 100 && y >= 40 && y <= 40 + 40 then (
            buttons_visible := true;
            party_buttons_visible := false;
            other_meowstas_buttons_visible := false;
            bag_buttons_visible := false)
          else if !bag_buttons_visible then
            (** possible_button is an option button. From here, you can extract 
                  the text from doing the pattern matching and doing b.item to get 
                  the item (ex: HardStone) it clicked or b.text to get the text 
                  (ex: "HardStone x2"). *)
            let possible_button = if_item !bag_buttons_list x y in
            match possible_button with
            | None -> ()
            | Some b -> print_endline ("pressed " ^ b.text)
          (** And if you want the meowsta in the "party" or "other meowstas" 
              just do b.meowsta*)
          else if !party_buttons_visible then
            let possible_button = if_item !party_buttons_list x y in
            match possible_button with
            | None -> ()
            | Some b -> print_endline ("pressed " ^ b.text)
          else if !other_meowstas_buttons_visible then
            let possible_button = if_item !other_meowstas_buttons_list x y in
            match possible_button with
            | None -> ()
            | Some b -> print_endline ("pressed " ^ b.text)
          else if x >= 1300 && x <= 1300 + 120 && y >= 500 && y <= 500 + 50 then (
            render_bag_screen state;
            buttons_visible := false;
            bag_buttons_visible := true)
          else if x >= 800 && x <= 800 + 350 && y >= 500 && y <= 500 + 50 then (
            render_other_meowstas_screen state;
            buttons_visible := false;
            other_meowstas_buttons_visible := true)
          else if x >= 400 && x <= 400 + 150 && y >= 500 && y <= 500 + 50 then (
            render_party_screen state;
            buttons_visible := false;
            party_buttons_visible := true)
      | _ -> ()
    done
end
