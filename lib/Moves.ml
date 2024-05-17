open MType
open Status

type moves = {
  name : string;
  damage : int;
  attack_type : mtype;
  status_effect : status;
}

let no = { name = ""; damage = 0; attack_type = NO; status_effect = NO }

let absorb =
  { name = "absorb"; damage = 10; attack_type = Grass; status_effect = Heal }

let ember =
  { name = "ember"; damage = 10; attack_type = Fire; status_effect = Burn }

let ice_ball =
  { name = "ice ball"; damage = 10; attack_type = Ice; status_effect = Frozen }

let shock =
  {
    name = "shock";
    damage = 10;
    attack_type = Electric;
    status_effect = Paralysis;
  }

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

let rock_toss =
  { name = "rock_toss"; damage = 10; attack_type = Rock; status_effect = NO }

let psychic =
  { name = "psychic"; damage = 10; attack_type = Psychic; status_effect = NO }

let poison_jab =
  {
    name = "poison_jab";
    damage = 12;
    attack_type = Poison;
    status_effect = Poison;
  }
