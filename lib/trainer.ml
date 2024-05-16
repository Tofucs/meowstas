open Items
open Meowsta

let party_meowsta =
  (* [| Meowsta_dictionary.meowberger; Meowsta_dictionary.clawson |] *)
  Array.make 6 empty

let (extra_meowsta : t list ref) =
  (* ref [ Meowsta_dictionary.meowberger; Meowsta_dictionary.clawson ] *)
  ref []

let (item_bag : items list ref) =
  (* ref [] *)
  ref
    [
      HardStone;
      BlackBelt;
      HardStone;
      BlackBelt;
      Charcoal;
      BurnHeal;
      MysticWater;
      NeverMeltIce;
      TwistedSpoon;
      GreatMeowstaBall;
      GreatMeowstaBall;
      UltraMeowstaBall;
    ]

let apply_medicine_item (item : items) (pokemon : t) =
  let match_medicine item pokemon =
    match item with
    | Potion -> pokemon.hp <- pokemon.hp + 20
    | SuperPotion -> pokemon.hp <- pokemon.hp + 50
    | HyperPotion -> pokemon.hp <- pokemon.hp + 100
    | Revive when pokemon.hp <= 0 -> pokemon.hp <- 1 / 3 * pokemon.max_hp
    | ParalyzeHeal when pokemon.status = Paralysis ->
        pokemon.status <- NO;
        pokemon.speed <- pokemon.max_speed
    | BurnHeal when pokemon.status = Burn ->
        pokemon.status <- NO;
        pokemon.attack <- pokemon.max_attack
    | Antidote when pokemon.status = Poison -> pokemon.status <- NO
    | FreezeHeal when pokemon.status = Frozen -> pokemon.status <- NO
    | SleepHeal when pokemon.status = Sleep -> pokemon.status <- NO
    | _ -> Printf.printf "Invalid Item!"
  in
  begin
    if List.mem item !item_bag then begin
      match_medicine item pokemon;
      if pokemon.hp > pokemon.max_hp then pokemon.hp <- pokemon.max_hp else ()
    end
    else Printf.printf "You don't have this item!"
  end

let apply_catching_item (item : items) =
  match item with
  | MeowstaBall -> 1.
  | GreatMeowstaBall -> 1.5
  | UltraMeowstaBall -> 2.
  | _ -> failwith "Invalid Item"

let try_catch (item : items) (m : t) =
  if List.mem item !item_bag then begin
    if item = MeowstaBall || item = GreatMeowstaBall || item = UltraMeowstaBall
    then begin
      let base_chance = 1.0 in

      let status_modifier =
        match m.status with
        | Sleep | Paralysis -> 2.0
        | Poison | Burn | Frozen -> 1.5
        | _ -> 1.0
      in
      let hp_factor = float_of_int m.hp /. 100.0 in
      let chance =
        base_chance *. status_modifier *. apply_catching_item item /. hp_factor
      in
      Random.float 1.0 < chance
    end
    else failwith "You need to choose a catching item!"
  end
  else failwith "You don't have this item!"

let add_item (item : items) = item_bag := item :: !item_bag

let add_meowsta (pokemon : t) =
  let replaced = ref false in
  for i = 0 to Array.length party_meowsta - 1 do
    if party_meowsta.(i) = empty then begin
      party_meowsta.(i) <- pokemon;
      replaced := true;
      raise Exit
    end
    else ()
  done;
  if !replaced = false then extra_meowsta := pokemon :: !extra_meowsta else ()

let switch_meowsta (index : int) (pokemon : t) =
  if List.mem pokemon !extra_meowsta then begin
    extra_meowsta := party_meowsta.(index) :: !extra_meowsta;
    party_meowsta.(index) <- pokemon
  end
  else Printf.printf "You don't have that pokemon in your PC!"

let hold_item (item : items) (index : int) =
  if party_meowsta.(index) <> empty then party_meowsta.(index).item <- item
