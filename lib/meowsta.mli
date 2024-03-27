open MType
open Abilities
open Moves
open Items



module type PokemonType = sig
  val name : string
  val poke_type : mtype * mtype
  val ability : abilities
  val level : int ref
  val item : items ref

  val base_hp : int ref
  val base_attack : int ref
  val base_defence : int ref
  val base_speed : int ref

  val moveset : moves array
end

module type PokemonSig = sig
  include PokemonType


  val attack : moves -> int
end

module Make (P : PokemonType) : PokemonSig 


