open Tsdl
open Meowstas
open Tile
open Meowsta_dictionary
open Set

type textures = {
  w : Sdl.texture;
  nw : Sdl.texture;
  iw : Sdl.texture;
  inw : Sdl.texture;
  menu : Sdl.texture;
  exit : Sdl.texture;
  meowsta : Sdl.texture;
  bag : Sdl.texture;
  party : Sdl.texture;
  otherMeowstas : Sdl.texture;
  hardStone : Sdl.texture;
  blackBelt : Sdl.texture;
  blackGlasses : Sdl.texture;
  charcoal : Sdl.texture;
  dragonFang : Sdl.texture;
  fairyFeather : Sdl.texture;
  magnet : Sdl.texture;
  metalCoat : Sdl.texture;
  miracleSeed : Sdl.texture;
  mysticWater : Sdl.texture;
  neverMeltIce : Sdl.texture;
  poisonBarb : Sdl.texture;
  sharpBeak : Sdl.texture;
  silkScarf : Sdl.texture;
  silverPowder : Sdl.texture;
  softSand : Sdl.texture;
  spellTag : Sdl.texture;
  twistedSpoon : Sdl.texture;
  potion : Sdl.texture;
  superPotion : Sdl.texture;
  hyperPotion : Sdl.texture;
  revive : Sdl.texture;
  paralyzeHeal : Sdl.texture;
  burnHeal : Sdl.texture;
  antidote : Sdl.texture;
  freezeHeal : Sdl.texture;
  sleepHeal : Sdl.texture;
}

let all_meowstas = ref [ Meowsta_dictionary.meowberger ]

let beginmap =
  let tiles =
    [
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
      [
        NW; W; W; W; W; W; W; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW; NW;
      ];
    ]
  in
  let convert_row row = Array.of_list row in
  Array.of_list (List.map convert_row tiles)

let game_map = Map.make () "begin" beginmap (20, 15)

let load_texture_from_file renderer file =
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

let render_tile renderer textures x y tile =
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w:32 ~h:32 in
  let dst = Sdl.Rect.create ~x:(x * 32) ~y:(y * 32) ~w:32 ~h:32 in
  let texture =
    match tile with
    | W -> textures.w
    | NW -> textures.nw
    | IW -> textures.iw
    | INW -> textures.inw
  in
  match Sdl.render_copy ~src ~dst renderer texture with
  | Ok () -> () (* Successfully rendered, do nothing *)
  | Error (`Msg e) -> Sdl.log "Failed to render tile: %s" e

let render_button renderer textures =
  let dst = Sdl.Rect.create ~x:530 ~y:15 ~w:90 ~h:50 in

  (* Sdl.set_render_draw_color renderer 255 0 0 150 |> ignore;
     Sdl.render_fill_rect renderer (Some rect) |> ignore; *)
  match Sdl.render_copy ~dst renderer textures.menu with
  | Ok () -> () (* Successfully rendered, do nothing *)
  | Error (`Msg e) -> Sdl.log "Failed to render tile: %s" e

let render_menu_screen renderer textures =
  Sdl.set_render_draw_color renderer 200 200 200 200 |> ignore;
  Sdl.render_clear renderer |> ignore;

  (* Draw exit button *)
  let exit_button_rect = Sdl.Rect.create ~x:530 ~y:15 ~w:90 ~h:50 in
  match Sdl.render_copy ~dst:exit_button_rect renderer textures.exit with
  | Ok () -> (
      let party_button = Sdl.Rect.create ~x:100 ~y:200 ~w:90 ~h:50 in
      match Sdl.render_copy ~dst:party_button renderer textures.party with
      | Ok () -> (
          let bag_button = Sdl.Rect.create ~x:450 ~y:200 ~w:90 ~h:50 in
          match Sdl.render_copy ~dst:bag_button renderer textures.bag with
          | Ok () -> (
              let other_meowstas_button =
                Sdl.Rect.create ~x:230 ~y:200 ~w:180 ~h:50
              in
              match
                Sdl.render_copy ~dst:other_meowstas_button renderer
                  textures.otherMeowstas
              with
              | Ok () -> ()
              | Error (`Msg e) ->
                  Sdl.log "Failed to render other meowstas button: %s" e)
          | Error (`Msg e) -> Sdl.log "Failed to render bag button: %s" e)
      | Error (`Msg e) -> Sdl.log "Failed to render meowsta button: %s" e)
  | Error (`Msg e) -> Sdl.log "Failed to render exit button: %s" e

let render_party_screen renderer textures =
  Sdl.set_render_draw_color renderer 200 200 200 200 |> ignore;
  Sdl.render_clear renderer |> ignore;

  (* Draw exit button *)
  let exit_button_rect = Sdl.Rect.create ~x:530 ~y:15 ~w:90 ~h:50 in
  match Sdl.render_copy ~dst:exit_button_rect renderer textures.exit with
  | Ok () -> ()
  | Error (`Msg e) -> Sdl.log "Failed to render exit button: %s" e

let render_other_meowstas_screen renderer textures =
  Sdl.set_render_draw_color renderer 200 200 200 200 |> ignore;
  Sdl.render_clear renderer |> ignore;

  (* Draw exit button *)
  let exit_button_rect = Sdl.Rect.create ~x:530 ~y:15 ~w:90 ~h:50 in
  match Sdl.render_copy ~dst:exit_button_rect renderer textures.exit with
  | Ok () -> ()
  | Error (`Msg e) -> Sdl.log "Failed to render exit button: %s" e

module StringSet = Set.Make (String)

let render_bag_item renderer textures index item_string =
  let x_val = 20 + (100 * (index mod 6)) in
  let y_val = 90 + (60 * (index / 6)) in
  let dst = Sdl.Rect.create ~x:x_val ~y:y_val ~w:90 ~h:50 in

  let texture =
    match item_string with
    | "HardStone" -> textures.hardStone
    | "BlackBelt" -> textures.blackBelt
    | "BlackGlasses" -> textures.blackGlasses
    | "Charcoal" -> textures.charcoal
    | "DragonFang" -> textures.dragonFang
    | "FairyFeather" -> textures.fairyFeather
    | "Magnet" -> textures.magnet
    | "MetalCoat" -> textures.metalCoat
    | "MiracleSeed" -> textures.miracleSeed
    | "MysticWater" -> textures.mysticWater
    | "NeverMeltIce" -> textures.neverMeltIce
    | "PoisonBarb" -> textures.poisonBarb
    | "SharpBeak" -> textures.sharpBeak
    | "SilkScarf" -> textures.silkScarf
    | "SilverPowder" -> textures.silverPowder
    | "SoftSand" -> textures.softSand
    | "SpellTag" -> textures.spellTag
    | "TwistedSpoon" -> textures.twistedSpoon
    | "Potion" -> textures.potion
    | "SuperPotion" -> textures.superPotion
    | "HyperPotion" -> textures.hyperPotion
    | "Revive" -> textures.revive
    | "ParalyzeHeal" -> textures.paralyzeHeal
    | "BurnHeal" -> textures.burnHeal
    | "Antidote" -> textures.antidote
    | "FreezeHeal" -> textures.freezeHeal
    | "SleepHeal" -> textures.sleepHeal
    | _ -> textures.hardStone
  in
  match Sdl.render_copy ~dst renderer texture with
  | Ok () -> () (* Successfully rendered, do nothing *)
  | Error (`Msg e) -> Sdl.log "Failed to render tile: %s" e

let render_bag_screen renderer textures =
  Sdl.set_render_draw_color renderer 200 200 200 200 |> ignore;
  Sdl.render_clear renderer |> ignore;

  let items_set = ref StringSet.empty in
  List.iter
    (fun m ->
      match Meowsta.get_item m with
      | NO -> ()
      | item ->
          items_set :=
            StringSet.add
              (match item with
              | HardStone -> "HardStone"
              | BlackBelt -> "BlackBelt"
              | BlackGlasses -> "BlackGlasses"
              | Charcoal -> "Charcoal"
              | DragonFang -> "DragonFang"
              | FairyFeather -> "FairyFeather"
              | Magnet -> "Magnet"
              | MetalCoat -> "MetalCoat"
              | MiracleSeed -> "MiracleSeed"
              | MysticWater -> "MysticWater"
              | NeverMeltIce -> "NeverMeltIce"
              | PoisonBarb -> "PoisonBarb"
              | SharpBeak -> "SharpBeak"
              | SilkScarf -> "SilkScarf"
              | SilverPowder -> "SilverPowder"
              | SoftSand -> "SoftSand"
              | SpellTag -> "SpellTag"
              | TwistedSpoon -> "TwistedSpoon"
              | Potion -> "Potion"
              | SuperPotion -> "SuperPotion"
              | HyperPotion -> "HyperPotion"
              | Revive -> "Revive"
              | ParalyzeHeal -> "ParalyzeHeal"
              | BurnHeal -> "BurnHeal"
              | Antidote -> "Antidote"
              | FreezeHeal -> "FreezeHeal"
              | SleepHeal -> "SleepHeal"
              | _ -> "")
              !items_set) (* Add item to set *)
    !all_meowstas;

  (* let list_items_set = StringSet.to_list !items_set *)
  let list_items_set =
    [
      "HardStone";
      "MetalCoat";
      "MiracleSeed";
      "MysticWater";
      "NeverMeltIce";
      "PoisonBarb";
      "SharpBeak";
      "SilkScarf";
      "SilverPowder";
      "SoftSand";
      "BlackBelt";
      "SpellTag";
      "TwistedSpoon";
      "Potion";
      "SuperPotion";
      "HyperPotion";
      "Revive";
      "ParalyzeHeal";
      "BurnHeal";
      "BlackGlasses";
      "Antidote";
      "FreezeHeal";
      "SleepHeal";
      "Charcoal";
      "DragonFang";
      "FairyFeather";
      "Magnet";
    ]
  in

  (* print_int (List.length list_items_set); *)
  List.iteri
    (fun i name -> render_bag_item renderer textures i name)
    list_items_set;

  (* Draw exit button *)
  let exit_button_rect = Sdl.Rect.create ~x:530 ~y:15 ~w:90 ~h:50 in
  match Sdl.render_copy ~dst:exit_button_rect renderer textures.exit with
  | Ok () -> ()
  | Error (`Msg e) -> Sdl.log "Failed to render exit button: %s" e

type screen_state =
  | Game
  | Menu
  | Party
  | OtherMeowstas
  | Bag

let current_screen = ref Game

let handle_events map =
  let e = Sdl.Event.create () in
  let continue = ref true in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Quit -> continue := false
    | `Key_down -> (
        if !current_screen = Game then
          let key = Sdl.Event.(get e keyboard_keycode) in
          match key with
          | 0x77 -> Map.update_location map Up
          | 0x61 -> Map.update_location map Down
          | 0x73 -> Map.update_location map Left
          | 0x64 -> Map.update_location map Right
          | _ -> ())
    | `Mouse_button_down -> (
        let x = Sdl.Event.(get e mouse_button_x) in
        let y = Sdl.Event.(get e mouse_button_y) in

        if x >= 530 && x <= 530 + 90 && y >= 15 && y <= 15 + 50 then
          match !current_screen with
          | Game -> current_screen := Menu
          | Menu -> current_screen := Game
          | Party -> current_screen := Menu
          | OtherMeowstas -> current_screen := Menu
          | Bag -> current_screen := Menu
        else if x >= 100 && x <= 100 + 90 && y >= 200 && y <= 200 + 50 then
          match !current_screen with
          | Game -> current_screen := Game
          | Menu -> current_screen := Party
          | Party -> current_screen := Party
          | OtherMeowstas -> current_screen := OtherMeowstas
          | Bag -> current_screen := Bag
        else if x >= 450 && x <= 450 + 90 && y >= 200 && y <= 200 + 50 then
          match !current_screen with
          | Game -> current_screen := Game
          | Menu -> current_screen := Bag
          | Party -> current_screen := Party
          | OtherMeowstas -> current_screen := OtherMeowstas
          | Bag -> current_screen := Bag
        else if x >= 230 && x <= 230 + 180 && y >= 200 && y <= 200 + 50 then
          match !current_screen with
          | Game -> current_screen := Game
          | Menu -> current_screen := OtherMeowstas
          | Party -> current_screen := Party
          | OtherMeowstas -> current_screen := OtherMeowstas
          | Bag -> current_screen := Bag)
    | _ -> ()
  done;
  !continue

let render_map renderer textures (map : Map.map) =
  Array.iteri
    (fun i row ->
      Array.iteri (fun j tile -> render_tile renderer textures j i tile) row)
    (Map.get_grid map)

let main () =
  match Sdl.init Sdl.Init.(video + events) with
  | Error (`Msg e) ->
      Sdl.log "Init error: %s" e;
      exit 1
  | Ok () -> (
      match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
      | Error (`Msg e) ->
          Sdl.log "Create window error: %s" e;
          exit 1
      | Ok w -> (
          match
            Sdl.create_renderer w ~index:(-1) ~flags:Sdl.Renderer.accelerated
          with
          | Error (`Msg e) ->
              Sdl.log "Create renderer error: %s" e;
              exit 1
          | Ok renderer ->
              let textures =
                {
                  w = load_texture_from_file renderer "textures/sand1.bmp";
                  nw = load_texture_from_file renderer "textures/ocean1.bmp";
                  iw = load_texture_from_file renderer "textures/sand1.bmp";
                  inw = load_texture_from_file renderer "textures/ocean1.bmp";
                  menu = load_texture_from_file renderer "textures/menu.bmp";
                  exit = load_texture_from_file renderer "textures/exit.bmp";
                  meowsta =
                    load_texture_from_file renderer "textures/meowsta.bmp";
                  bag = load_texture_from_file renderer "textures/bag.bmp";
                  hardStone =
                    load_texture_from_file renderer "textures/hardStone.bmp";
                  blackBelt =
                    load_texture_from_file renderer "textures/blackBelt.bmp";
                  blackGlasses =
                    load_texture_from_file renderer "textures/blackGlasses.bmp";
                  charcoal =
                    load_texture_from_file renderer "textures/charcoal.bmp";
                  dragonFang =
                    load_texture_from_file renderer "textures/dragonFang.bmp";
                  fairyFeather =
                    load_texture_from_file renderer "textures/fairyFeather.bmp";
                  magnet = load_texture_from_file renderer "textures/magnet.bmp";
                  metalCoat =
                    load_texture_from_file renderer "textures/metalCoat.bmp";
                  miracleSeed =
                    load_texture_from_file renderer "textures/miracleSeed.bmp";
                  mysticWater =
                    load_texture_from_file renderer "textures/mysticWater.bmp";
                  neverMeltIce =
                    load_texture_from_file renderer "textures/neverMeltIce.bmp";
                  poisonBarb =
                    load_texture_from_file renderer "textures/poisonBarb.bmp";
                  sharpBeak =
                    load_texture_from_file renderer "textures/sharpBeak.bmp";
                  silkScarf =
                    load_texture_from_file renderer "textures/silkScarf.bmp";
                  silverPowder =
                    load_texture_from_file renderer "textures/silverPowder.bmp";
                  softSand =
                    load_texture_from_file renderer "textures/softSand.bmp";
                  spellTag =
                    load_texture_from_file renderer "textures/spellTag.bmp";
                  twistedSpoon =
                    load_texture_from_file renderer "textures/twistedSpoon.bmp";
                  potion = load_texture_from_file renderer "textures/potion.bmp";
                  superPotion =
                    load_texture_from_file renderer "textures/superPotion.bmp";
                  hyperPotion =
                    load_texture_from_file renderer "textures/hyperPotion.bmp";
                  revive = load_texture_from_file renderer "textures/revive.bmp";
                  paralyzeHeal =
                    load_texture_from_file renderer "textures/paralyzeHeal.bmp";
                  burnHeal =
                    load_texture_from_file renderer "textures/burnHeal.bmp";
                  antidote =
                    load_texture_from_file renderer "textures/antidote.bmp";
                  freezeHeal =
                    load_texture_from_file renderer "textures/freezeHeal.bmp";
                  sleepHeal =
                    load_texture_from_file renderer "textures/sleepHeal.bmp";
                  party = load_texture_from_file renderer "textures/party.bmp";
                  otherMeowstas =
                    load_texture_from_file renderer "textures/otherMeowstas.bmp";
                }
              in
              let rec loop () =
                let start_ticks = Sdl.get_ticks () in

                begin
                  match !current_screen with
                  | Game ->
                      render_map renderer textures game_map;
                      render_button renderer textures
                  | Menu -> render_menu_screen renderer textures
                  | Party -> render_party_screen renderer textures
                  | OtherMeowstas ->
                      render_other_meowstas_screen renderer textures
                  | Bag -> render_bag_screen renderer textures
                end;
                Sdl.render_present renderer;

                let get_ticks = Sdl.get_ticks () in
                let elapsed_ticks = Int32.sub get_ticks start_ticks in
                let frame_delay = Int32.sub 33l elapsed_ticks in
                (* Approximately 30 FPS *)
                if frame_delay > 0l then Sdl.delay frame_delay;
                if handle_events game_map then loop () else ()
              in

              loop ();
              Sdl.destroy_window w;
              Sdl.quit ()))

let () = main ()
