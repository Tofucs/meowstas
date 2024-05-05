open Items
open Meowsta

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
    match_medicine item pokemon;
    if pokemon.hp > pokemon.max_hp then pokemon.hp <- pokemon.max_hp else ()
  end
