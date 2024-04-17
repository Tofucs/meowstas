type status =
  | NO
  | Burn
  | Poison
  | Sleep
  | Paralysis
  | Recoil
  | Heal

let string_wrap (s : status) =
  match s with
  | Burn -> "Burned"
  | Poison -> "Poisoned"
  | Sleep -> "Sleept"
  | Paralysis -> "Paralyzed"
  | _ -> ""
