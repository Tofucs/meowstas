(**type [status] represents the catalog of all possible status conditions*)
type status =
  | NO
  | Burn
  | Poison
  | Sleep
  | Paralysis
  | Frozen
  | Recoil
  | Heal

(**[string_wrap status] simply converts [status] to a string representation*)
let string_wrap (s : status) =
  match s with
  | Burn -> "Burned"
  | Poison -> "Poisoned"
  | Sleep -> "Slept"
  | Frozen -> "Frozen"
  | Paralysis -> "Paralyzed"
  | _ -> ""
