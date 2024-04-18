type status =
  | NO
  | Burn
  | Poison
  | Sleep
  | Paralysis
  | Frozen
  | Recoil
  | Heal

let string_wrap (s : status) =
  match s with
  | Burn -> "Burned"
  | Poison -> "Poisoned"
  | Sleep -> "Slept"
  | Frozen -> "Frozen"
  | Paralysis -> "Paralyzed"
  | _ -> ""
