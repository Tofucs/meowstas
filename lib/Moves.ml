open MType
open Status

type moves = {
  name: string;
  damage: int;
  attack_type: mtype;
  status_effect: status;

}

(*SORT ALPHABETICALLY (after no)*)

let no = {name = ""; damage = 0; attack_type = NO; status_effect = NO}
let ember = {name = "ember"; damage = 10; attack_type = Fire; status_effect = Burn}
let tackle = {name = "tackle"; damage = 10; attack_type = Normal; status_effect = NO}

let water_gun = {name = "water_gun"; damage = 10; attack_type = Water; status_effect = NO}