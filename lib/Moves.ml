open MType
open Status

type moves = {
  name : string;
  damage : int;
  attack_type : mtype;
  status_effect : status;
}

(*SORT ALPHABETICALLY (after no)*)
(*Moves with recoil, healing*)

let no = { name = ""; damage = 0; attack_type = NO; status_effect = NO }

let absorb =
  { name = "absorb"; damage = 10; attack_type = Grass; status_effect = Heal }

let ember =
  { name = "ember"; damage = 10; attack_type = Fire; status_effect = Burn }

let tackle =
  { name = "tackle"; damage = 10; attack_type = Normal; status_effect = NO }

let take_down =
  {
    name = "take down";
    damage = 20;
    attack_type = Normal;
    status_effect = Recoil;
  }

let water_gun =
  { name = "water_gun"; damage = 10; attack_type = Water; status_effect = NO }
