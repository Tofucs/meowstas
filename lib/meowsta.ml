open MType
open Abilities
open Moves
open Items
open Status

type t = {
  name : string;
  poke_type : mtype * mtype;
  ability : abilities;
  mutable exp : int;
  mutable level_threshold : int;
  mutable level : int;
  mutable item : items;
  mutable max_hp : int;
  mutable hp : int;
  mutable max_attack : int;
  mutable attack : int;
  mutable max_defense : int;
  mutable defense : int;
  mutable max_speed : int;
  mutable speed : int;
  mutable status : status;
  moveset : moves array;
  mutable learnset : (int * moves) list;
}

let empty =
  {
    name = "";
    poke_type = (NO, NO);
    ability = NO;
    exp = 0;
    level_threshold = 1000;
    level = 0;
    item = NO;
    max_hp = 0;
    hp = 0;
    max_attack = 0;
    attack = 0;
    max_defense = 0;
    defense = 0;
    max_speed = 0;
    speed = 0;
    status = NO;
    moveset = Array.make 4 no;
    learnset = [];
  }

let apply_booster_item (item : items) (move : moves) =
  match item with
  | NO -> float_of_int move.damage
  | HardStone when move.attack_type = Rock -> 0.2 *. float_of_int move.damage
  | BlackBelt when move.attack_type = Fighting ->
      0.2 *. float_of_int move.damage
  | BlackGlasses when move.attack_type = Dark -> 0.2 *. float_of_int move.damage
  | Charcoal when move.attack_type = Fire -> 0.2 *. float_of_int move.damage
  | DragonFang when move.attack_type = Dragon -> 0.2 *. float_of_int move.damage
  | FairyFeather when move.attack_type = Fairy ->
      0.2 *. float_of_int move.damage
  | Magnet when move.attack_type = Electric -> 0.2 *. float_of_int move.damage
  | MetalCoat when move.attack_type = Steel -> 0.2 *. float_of_int move.damage
  | MiracleSeed when move.attack_type = Grass -> 0.2 *. float_of_int move.damage
  | MysticWater when move.attack_type = Water -> 0.2 *. float_of_int move.damage
  | NeverMeltIce when move.attack_type = Ice -> 0.2 *. float_of_int move.damage
  | PoisonBarb when move.attack_type = Poison -> 0.2 *. float_of_int move.damage
  | SharpBeak when move.attack_type = Flying -> 0.2 *. float_of_int move.damage
  | SilkScarf when move.attack_type = Normal -> 0.2 *. float_of_int move.damage
  | SilverPowder when move.attack_type = Bug -> 0.2 *. float_of_int move.damage
  | SoftSand when move.attack_type = Ground -> 0.2 *. float_of_int move.damage
  | SpellTag when move.attack_type = Ghost -> 0.2 *. float_of_int move.damage
  | TwistedSpoon when move.attack_type = Psychic ->
      0.2 *. float_of_int move.damage
  | _ -> float_of_int move.damage

let attack attacker defender move =
  let base_damage =
    float_of_int attacker.attack
    /. float_of_int defender.defense
    *. apply_booster_item attacker.item move
  in
  defender.hp <-
    float_of_int defender.hp
    -. base_damage
       *. type_multiplier move.attack_type (fst defender.poke_type)
       *. type_multiplier move.attack_type (snd defender.poke_type)
    |> int_of_float

let is_dead pokemon =
  if pokemon.hp <= 0 then begin
    pokemon.hp <- 0;
    true
  end
  else false

(** TODO: Need to CHANGE *)

let add_move (pokemon : t) (move : moves) =
  let replaced = ref false in
  for i = 0 to Array.length pokemon.moveset do
    if pokemon.moveset.(i) = no then begin
      pokemon.moveset.(i) <- move;
      replaced := true;
      raise Exit
    end
  done;
  if !replaced = false then begin
    Printf.printf
      "%s wants to learn a new move but already knows four moves! Replace an \
       existing move with %s? (Y/N)\n"
      pokemon.name move.name;
    let choice = read_line () in
    if choice = "Y" then begin
      Array.iteri
        (fun i (move1 : moves) -> Printf.printf "%d: %s\n" (i + 1) move1.name)
        pokemon.moveset;
      Printf.printf "What move should be replaced? (type number)\n";
      let choice2 = read_int () in
      let index = choice2 - 1 in
      pokemon.moveset.(index) <- move
    end
    else Printf.printf "%s did not learn %s." pokemon.name move.name
  end

let check_levelup m =
  if m.exp > m.level_threshold then begin
    m.exp <- m.exp / m.level_threshold;
    m.level <- m.level + 1;
    m.level_threshold <- m.level_threshold + 200;
    m.max_hp <- int_of_float (1.2 *. float_of_int m.max_hp);
    m.hp <- int_of_float (1.2 *. float_of_int m.hp);
    m.max_attack <- int_of_float (1.2 *. float_of_int m.max_attack);
    m.attack <- int_of_float (1.2 *. float_of_int m.attack);
    m.max_defense <- int_of_float (1.2 *. float_of_int m.max_defense);
    m.defense <- int_of_float (1.2 *. float_of_int m.defense);
    m.max_speed <- int_of_float (1.2 *. float_of_int m.max_speed);
    m.speed <- int_of_float (1.2 *. float_of_int m.speed);
    print_string (m.name ^ " has level\n  upped!");
    let rec learn_move m =
      match m.learnset with
      | [] -> ()
      | (level, move) :: _ when m.level >= level -> begin
          add_move m move;
          m.learnset <-
            List.filter (fun (lvl, m) -> lvl <> level || m <> move) m.learnset;
          learn_move m
        end
      | _ -> ()
    in
    learn_move m
  end
