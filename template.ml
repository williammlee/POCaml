open Yojson.Basic.Util


(*********)
(* TYPES *)
(*********)

type pokemon_name = string
type type_name = string
type move_name = string
type healing_name = string
type pokeball_name = string


(**************)
(* EXCEPTIONS *)
(**************)


exception UnknownPokemon of pokemon_name
exception UnknownType of type_name
exception UnknownMove of move_name
exception UnknownHealing of healing_name
exception UnknownPokeball of pokeball_name


(******************)
(* CUSTOM OBJECTS *)
(******************)


type stats = {hp: int; atk: int; def: int}
type pokemon = {pname: pokemon_name; ptype: type_name; moves: move_name list; 
                stats: stats}
type move = {mname: move_name; mtype: type_name; pp: int; power: int; 
             acc: float}
type healing = {hname: healing_name; heal_amount: int; revive: bool; 
                drop_chance: float}
type pokeball = {pbname: pokeball_name; catch_rate: float; drop_chance: float }
type inventory = {healing: healing list; pokeball: pokeball list}
type i_healing = {i_hname: healing_name; hquantity: int}
type i_pokeball = {i_pbname: pokeball_name; pbquantity: int}
type i_inventory = {init_healing: i_healing list; 
                    init_pokeball: i_pokeball list}
type template = {pokemon: pokemon list; moves: move list; items: inventory;
                 starter_pokemon: pokemon_name list; init_inventory: 
                   i_inventory}
type t = template


(***********)
(* PARSERS *)
(***********)


(** [stats_of_json json] are parsed stats. *)
let stats_of_json json = {
  hp = json |> member "hp" |> to_int;
  atk = json |> member "atk" |> to_int;
  def = json |> member "def" |> to_int;
}

(** [pokemon_of_json json] is a parsed pokemon. *)
let pokemon_of_json json = {
  pname = json |> member "pname" |> to_string;
  ptype = json |> member "ptype" |> to_string;
  moves = json |> member "moves" |> to_list |> List.map to_string;
  stats = json |> member "stats" |> stats_of_json;
}

(** [move_of_json json] is a parsed move. *)
let move_of_json json = {
  mname = json |> member "mname" |> to_string;
  mtype = json |> member "mtype" |> to_string;
  pp = json |> member "pp" |> to_int;
  power = json |> member "power" |> to_int;
  acc = json |> member "acc" |> to_float;
}

(** [healing_of_json json] is a parsed healing item. *)
let healing_of_json json = {
  hname = json |> member "hname" |> to_string;
  heal_amount = json |> member "heal_amount" |> to_int;
  revive = json |> member "revive" |> to_bool;
  drop_chance = json |> member "drop_chance" |> to_float;
}

(** [pokeball_of_json json] is a parsed pokeball. *)
let pokeball_of_json json = {
  pbname = json |> member "pbname" |> to_string;
  catch_rate = json |> member "catch_rate" |> to_float;
  drop_chance = json |> member "drop_chance" |> to_float;
}

(** [inventory_of_json] is a parsed inventory. *)
let inventory_of_json json = {
  healing = json |> member "healing" |> to_list |> List.map healing_of_json;
  pokeball = json |> member "pokeball" |> to_list |> List.map pokeball_of_json;
}

(** [i_healing_of_json] is a parsed initial healing item. *)
let i_healing_of_json json = {
  i_hname = json |> member "i_hname" |> to_string;
  hquantity = json |> member "hquantity" |> to_int;
}

(** [i_pokeball_of_json json] is a parsed initial pokeball. *)
let i_pokeball_of_json json = {
  i_pbname = json |> member "i_pbname" |> to_string;
  pbquantity = json |> member "pbquantity" |> to_int;
}

(** [i_inventory_of_json] is a parsed initial inventory. *)
let i_inventory_of_json json = {
  init_healing = json |> member "init_healing" |> to_list |> 
                 List.map i_healing_of_json;
  init_pokeball = json |> member "init_pokeball" |> to_list |>
                  List.map i_pokeball_of_json;
}

(** [template_of_json] is our parsed template JSON. *)
let template_of_json json = {
  pokemon = json |> member "pokemon" |> to_list |> List.map pokemon_of_json;
  moves = json |> member "moves" |> to_list |> List.map move_of_json;
  items = json |> member "items" |> inventory_of_json;
  starter_pokemon = json |> member "starter_pokemon" |> to_list |> 
                    List.map to_string;
  init_inventory = json |> member "init_inventory" |> i_inventory_of_json;
}

let from_json (j : Yojson.Basic.t) : t =
  template_of_json j


(***********)
(* POKEMON *)
(***********)


let pokemon_list (tem : t) : pokemon_name list =
  let rec pokemon_list_helper pokemon acc = 
    match pokemon with
    |[] -> acc
    |h :: t -> pokemon_list_helper t (h.pname::acc) in 
  List.rev (pokemon_list_helper tem.pokemon [])

let pokemon_type (tem : t) (pn : pokemon_name) : type_name =
  let rec pokemon_type_helper poke_list tem pn = 
    match poke_list with
    |[] -> raise (UnknownPokemon pn)
    |h::t -> if h.pname = pn then h.ptype 
      else pokemon_type_helper t tem pn in
  pokemon_type_helper tem.pokemon tem pn

let pokemon_moves (tem : t) (pn : pokemon_name) : move_name list =
  let rec pokemon_moves_helper tem pn poke_list =
    match poke_list with
    |[] -> raise (UnknownPokemon pn)
    |h::t -> if h.pname = pn then h.moves 
      else pokemon_moves_helper tem pn t in
  pokemon_moves_helper tem pn tem.pokemon

let pokemon_stats (tem : t) (pn : pokemon_name) : (int*int*int) =
  let rec pokemon_stats_helper tem pn poke_list = 
    match poke_list with
    |[] -> raise (UnknownPokemon pn)
    |h::t -> if h.pname = pn then (h.stats.hp, h.stats.atk, h.stats.def)
      else pokemon_stats_helper tem pn t in
  pokemon_stats_helper tem pn tem.pokemon


(********)
(* MOVE *)
(********)


let move_list (tem : t) : move_name list =
  let rec move_list_helper moves acc = 
    match moves with
    |[] -> acc
    |h :: t -> move_list_helper t (h.mname::acc) in 
  List.rev (move_list_helper tem.moves [])

let move_type (tem : t) (m : move_name) : type_name =
  let rec move_type_helper tem m move_list =
    match move_list with
    |[] -> raise (UnknownMove m)
    |h::t -> if h.mname = m then h.mtype 
      else move_type_helper tem m t in 
  move_type_helper tem m tem.moves

let move_stats (tem : t) (m : move_name) : (int*int*float) =
  let rec move_stats_helper tem m move_list =
    match move_list with
    |[] -> raise (UnknownMove m)
    |h::t -> if h.mname = m then (h.pp, h.power, h.acc) 
      else move_stats_helper tem m t in 
  move_stats_helper tem m tem.moves


(***********)
(* HEALING *)
(***********)


let healing_list (tem : t) : healing_name list =
  let rec healing_list_helper healing_list acc = 
    match healing_list with
    |[] -> acc
    |h :: t -> healing_list_helper t (h.hname::acc) in 
  List.rev (healing_list_helper tem.items.healing [])

let healing_amount (tem : t) (hn : healing_name) : int =
  let rec healing_amount_helper tem hn heal_list =
    match heal_list with
    |[] -> raise (UnknownHealing hn)
    |h::t -> if h.hname = hn then h.heal_amount 
      else healing_amount_helper tem hn t in 
  healing_amount_helper tem hn tem.items.healing

let is_revive (tem : t) (hn : healing_name) : bool =
  let rec is_revive_helper tem hn heal_list = 
    match heal_list with
    |[] -> raise (UnknownHealing hn)
    |h::t -> if h.hname = hn then h.revive 
      else is_revive_helper tem hn t in  
  is_revive_helper tem hn tem.items.healing

let healing_drop (tem : t) (hn : healing_name) : float =
  let rec healing_drop_helper tem hn heal_list = 
    match heal_list with
    |[] -> raise (UnknownHealing hn)
    |h::t -> if h.hname = hn then h.drop_chance 
      else healing_drop_helper tem hn t in
  healing_drop_helper tem hn tem.items.healing


(************)
(* POKEBALL *)
(************)


let pokeball_list (tem : t) : pokeball_name list =
  let rec pokeball_list_helper ball_list acc = 
    match ball_list with
    |[] -> acc
    |h :: t -> pokeball_list_helper t (h.pbname::acc) in 
  List.rev (pokeball_list_helper tem.items.pokeball [])

let pokeball_catch (tem : t) (pb : pokeball_name) : float =
  let rec pokeball_catch_helper tem pb ball_list =
    match ball_list with
    |[] -> raise (UnknownPokeball pb)
    |h::t -> if h.pbname = pb then h.catch_rate 
      else pokeball_catch_helper tem pb t in
  pokeball_catch_helper tem pb tem.items.pokeball

let pokeball_drop (tem : t) (pb : pokeball_name) : float =
  let rec pokeball_drop_helper tem pb ball_list =
    match ball_list with
    |[] -> raise (UnknownPokeball pb)
    |h::t -> if h.pbname = pb then h.drop_chance
      else pokeball_drop_helper tem pb t in
  pokeball_drop_helper tem pb tem.items.pokeball


(************)
(* STARTING *)
(************)


let starter_pokemon (tem : t) : pokemon_name list =
  tem.starter_pokemon

let starter_healings (tem : t) : healing_name list =
  let rec starter_healings_helper tem heal_list acc =
    match heal_list with
    |[] -> acc
    |h :: t -> starter_healings_helper tem t (h.i_hname::acc) in 
  List.rev (starter_healings_helper tem tem.init_inventory.init_healing [])

let starter_healing_quantity (tem : t) (hn : healing_name) : int =
  let rec starter_healing_quantity_helper tem hn heal_list = 
    match heal_list with
    |[] -> raise (UnknownHealing hn)
    |h :: t -> if h.i_hname = hn then h.hquantity 
      else starter_healing_quantity_helper tem hn t in
  starter_healing_quantity_helper tem hn tem.init_inventory.init_healing

let starter_pokeballs (tem : t) : pokeball_name list =
  let rec starter_pokeballs_helper tem ball_list acc =
    match ball_list with
    |[] -> acc
    |h :: t -> starter_pokeballs_helper tem t (h.i_pbname::acc) in 
  List.rev (starter_pokeballs_helper tem tem.init_inventory.init_pokeball [])

let starter_pokeball_quantity (tem : t) (pb : pokeball_name) : int =
  let rec starter_pokeball_quantity_helper tem pb ball_list =
    match ball_list with
    |[] -> raise (UnknownPokeball pb)
    |h::t -> if h.i_pbname = pb then h.pbquantity 
      else starter_pokeball_quantity_helper tem pb t in 
  starter_pokeball_quantity_helper tem pb tem.init_inventory.init_pokeball
