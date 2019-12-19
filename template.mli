(** Parses our template JSON into a custom OCaml record and provides multiple 
    functions to use the record. *)


(**************)
(** {0 TYPES} *)
(**************)


(** The abstract type of values representing templates. *)
type t

(** The type of Pokemon names. *)
type pokemon_name = string

(** The type of type names. *)
type type_name = string

(** The type of move names. *)
type move_name = string

(** The type of healing item names. *)
type healing_name = string

(** The type of pokeball names. *)
type pokeball_name = string


(*******************)
(** {0 EXCEPTIONS} *)
(*******************)


(** Raised when an unknown Pokemon is encountered. *)
exception UnknownPokemon of pokemon_name

(** Raised when an unknown type is encountered. *)
exception UnknownType of type_name

(** Raised when an unknown move is encountered. *)
exception UnknownMove of move_name

(** Raised when an unknown healing item is encountered. *)
exception UnknownHealing of healing_name

(** Raised when an unknown pokeball is encountered. *)
exception UnknownPokeball of pokeball_name


(***********************)
(** {0 CUSTOM OBJECTS} *)
(***********************)


(** The type for a pokemon's stats. *)
type stats = {hp: int; atk: int; def: int}

(** The type for a pokemon. *)
type pokemon = {pname: pokemon_name; ptype: type_name; moves: move_name list; 
                stats: stats}

(** The type for a pokemon move. *)          
type move = {mname: move_name; mtype: type_name; pp: int; power: int; 
             acc: float}

(** The type for a healing item. *)   
type healing = {hname: healing_name; heal_amount: int; revive: bool; 
                drop_chance: float}

(** The type for a pokeball. *)  
type pokeball = {pbname: pokeball_name; catch_rate: float; drop_chance: float }

(** The type for the player's inventory. *)  
type inventory = {healing: healing list; pokeball: pokeball list}

(** The type for an initial healing item a player starts with. *)  
type i_healing = {i_hname: healing_name; hquantity: int}

(** The type for an initial pokeball a player starts with. *)  
type i_pokeball = {i_pbname: pokeball_name; pbquantity: int}

(** The type for the player's initial inventory. *)  
type i_inventory = {init_healing: i_healing list; 
                    init_pokeball: i_pokeball list}

(** The type for the template that is used in our JSON. *)            
type template = {pokemon: pokemon list; moves: move list; items: inventory;
                 starter_pokemon: pokemon_name list; 
                 init_inventory: i_inventory}


(****************)
(** {0 PARSERS} *)
(****************)


(** [from_json j] are the templates that [j] represents.
    Requires: [j] is a valid JSON template representation. *)
val from_json : Yojson.Basic.t -> t


(****************)
(** {0 POKEMON} *)
(****************)


(** [pokemon_list tem] is a list of Pokemon names in template [tem]. *)
val pokemon_list : t -> pokemon_name list

(** [pokemon_type tem pn] is the type of Pokemon [pn] in template [tem].
     Raises [UnknownPokemon pn] if [pn] isn't a Pokemon in [tem]. *)
val pokemon_type : t -> pokemon_name -> type_name

(** [pokemon_moves tem pn] is a list of moves that Pokemon [pn] can have in 
    template [tem]. 
    Raises [UnknownPokemon pn] if [pn] isn't a Pokemon in [tem]. *)
val pokemon_moves : t -> pokemon_name -> move_name list

(** [pokemon_stats tem pn] is a tuple containing the base health, attack, and 
    defense for Pokemon [pn] in template [tem].
    Raises [UnknownPokemon pn] if [pn] isn't a Pokemon in [tem]. *)
val pokemon_stats : t -> pokemon_name -> (int*int*int)


(*************)
(** {0 MOVE} *)
(*************)


(** [move_list tem] is a list of Pokemon moves in template [tem]. *)
val move_list : t -> move_name list

(** [move_type tem m] is the type of move [m] in template [tem].
     Raises [UnknownMove m] if [m] isn't a move in [tem]. *)
val move_type : t -> move_name -> type_name

(** [move_stats tem m] is the tuple containing the power points, base power, and
    accuracy of move [m] in template [tem].
    Raises [UnknownMove m] if [m] isn't a move in [tem]. *)
val move_stats : t -> move_name -> (int*int*float)


(****************)
(** {0 HEALING} *)
(****************)


(** [healing_list tem] is a list of healing items in template [tem]. *)
val healing_list : t -> healing_name list

(** [healing_amount tem h] is the amount of healing done by [h] in template 
    [tem].
    Raises [UnknownHealing h] if [h] isn't a healing item in [tem]. *)
val healing_amount : t -> healing_name -> int

(** [is_revive tem h] is true if [h] is a revive and false if [h] is a potion in 
    template [tem].
    Raises [UnknownHealing h] if [h] isn't a healing item in [tem]. *)
val is_revive : t -> healing_name -> bool

(** [healing_drop tem h] is the drop chance of [h] in template [tem].
    Raises [UnknownHealing h] if [h] isn't a healing item in [tem]. *)
val healing_drop : t -> healing_name -> float


(*****************)
(** {0 POKEBALL} *)
(*****************)


(** [pokeball_list tem] is a list of pokeballs in template [tem]. *)
val pokeball_list : t -> pokeball_name list

(** [pokeball_catch tem pb] is the catch rate of [pb] in template [tem].
    Raises [UnknownPokeball pb] if [pb] isn't a pokeball in [tem]. *)
val pokeball_catch : t -> pokeball_name -> float

(** [pokeball_catch tem pb] is the drop chance of [pb] in template [tem].
    Raises [UnknownPokeball pb] if [pb] isn't a pokeball in [tem]. *)
val pokeball_drop : t -> pokeball_name -> float


(*****************)
(** {0 STARTING} *)
(*****************)


(** [starter_pokemon tem] is the possible starter Pokemon in template [tem]. *)
val starter_pokemon : t -> pokemon_name list

(** [starter_healings tem] is a list of healing items you start with in template
    [tem]. *)
val starter_healings : t -> healing_name list

(** [starter_healing_quantity tem h] is the quantity of [h] you start with in
    template [tem].
    Raises [UnknownHealing h] if [h] isn't a healing item in [tem]. *)
val starter_healing_quantity : t -> healing_name -> int

(** [starter_healings tem] is a list of pokeballs you start with in template
    [tem]. *)
val starter_pokeballs : t -> pokeball_name list

(** [starter_pokeball_quantity tem pb] is the quantity of [pb] you start with in
    template [tem].
    Raises [UnknownPokeball pb] if [pb] isn't a pokeball in [tem]. *)
val starter_pokeball_quantity : t -> pokeball_name -> int