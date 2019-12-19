open Template

(** The abstract type of values representing the game state for players. *)
type p

(** The abstract type of values representing the game state for enemies. *)
type e

(** The type representing the result of an attempted action. *)
type result = Legal of p * e | Illegal

(** The type of elemental types. *)
type elemtype = 
  | Normal 
  | Fire
  | Water
  | Grass

(** The type of elemental type effectiveness. *)
type effective =
  | Super
  | Regular
  | Not

(** [init_player_state tem] is the initial player state in template [tem]. *)
val init_player_state : Template.t -> p

(** [player_pokemon ps] is a list of Pokemon that the player currently has in
    state [ps]. *)
val player_pokemon : p -> pokemon_name list

(** [player_pokemon_level ps pn] is the current level of Pokemon [pn] of the
    player in state [ps].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps]. *)
val player_pokemon_level : p -> pokemon_name -> int

(** [player_pokemon_hp ps pn] is the current health of Pokemon [pn] of the
    player in state [ps].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps]. *)
val player_pokemon_hp : p -> pokemon_name -> int

(** [player_pokemon_exp ps pn] is the current experience of Pokemon [pn] of the
    player in state [ps].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps]. *)
val player_pokemon_exp : p -> pokemon_name -> int

(** [player_pokemon_fighting ps pn] is true if Pokemon [pn] of the player is 
    currently fighting in state [ps].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps]. *)
val player_pokemon_fighting : p -> pokemon_name -> bool

(** [player_pokemon_fighting_name ps] is the name of the player pokemon that is
    currently fighting. *)
val player_pokemon_fighting_name : p -> pokemon_name

(** [player_pokemon_moveset ps pn] is a list of the moveset of Pokemon [pn] of 
    the player in state [ps].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps]. *)
val player_pokemon_moveset : p -> pokemon_name -> move_name list

(** [player_pokemon_move_pp ps pn m] is the current power points for move [m] of 
    Pokemon [pn] of the player in state [ps].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps] or [UnknownMove m] if Pokemon [pn] does not have that move in 
    state [ps]. *)
val player_pokemon_move_pp : p -> pokemon_name -> move_name -> int

(** [player_healing ps] is a list of the healing items in the player's
    inventory in state [ps]. *)
val player_healing : p -> healing_name list

(** [player_healing_quantity ps h] is the quantity of healing item [h] in the 
    player's inventory in state [ps].
    Raises [UnknownHealing h] if the player does not have healing item [h] in 
    state [ps]. *)
val player_healing_quantity : p -> healing_name -> int

(** [player_pokeball ps] is a list of the pokeballs in the player's inventory in 
    state [ps]. *)
val player_pokeball : p -> pokeball_name list

(** [player_pokeball_quantity ps pb] is the quantity of pokeballs [pb] in the 
    player's inventory in state [ps].
    Raises [UnknownPokeball pb] if the player does not have pokeball [pb] in 
    state [ps]. *)
val player_pokeball_quantity : p -> pokeball_name -> int

(** [player_battling ps] is true if the player is currently in a battle in state
    [ps]. *)
val player_battling : p -> bool

(** [switch_battling p] is a new player state after switching the players 
    battling record to false if originally true and true if originally false*)
val switch_battling : p -> p

(** [init_enemy_state tem wild] is an initial enemy state in template [tem] that
    is a Wild Pokemon if [wild] is true or a trainer otherwise. *)
val init_enemy_state : Template.t -> bool -> bool -> e

(** [init_enemy_state_2 tem wild] is an initial enemy state for a trainer 
    in template [tem] that is a Wild Pokemon if [wild] is true or a 
    trainer otherwise. *)
val init_enemy_state_2 : Template.t -> bool -> bool -> e

(** [enemy_pokemon es] is a list of Pokemon that the enemy currently has in
    state [es]. *)
val enemy_pokemon : e -> pokemon_name list

(** [enemy_pokemon_level es pn] is the current level of Pokemon [pn] of the
    enemy in state [es].
    Raises [UnknownPokemon pn] if the enemy does not have Pokemon [pn] in 
    state [es]. *)
val enemy_pokemon_level : e -> pokemon_name -> int

(** [enemy_pokemon_hp es pn] is the current health of Pokemon [pn] of the
    enemy in state [es].
    Raises [UnknownPokemon pn] if the enemy does not have Pokemon [pn] in 
    state [es]. *)
val enemy_pokemon_hp : e -> pokemon_name -> int

(** [enemy_pokemon_fighting es pn] is true if Pokemon [pn] of the enemy is 
    currently fighting in state [es].
    Raises [UnknownPokemon pn] if the enemy does not have Pokemon [pn] in 
    state [es]. *)
val enemy_pokemon_fighting : e -> pokemon_name -> bool

(** [enemy_pokemon_fighting_name es] is the name of the enemy pokemon that is
    currently fighting. *)
val enemy_pokemon_fighting_name : e -> pokemon_name

(** [enemy_pokemon_moveset es pn] is a list of the moveset of Pokemon [pn] of 
    the enemy in state [es].
    Raises [UnknownPokemon pn] if the enemy does not have Pokemon [pn] in 
    state [es]. *)
val enemy_pokemon_moveset : e -> pokemon_name -> move_name list

(** [enemy_wild es] is true if the enemy is a Wild Pokemon and false if the
    enemy is a trainer *)
val enemy_wild : e -> bool

(** [enemy_rival es] is true if the enemy is the rival and false if it is not*)
val enemy_rival : e -> bool 

(** [enemy_healing_quantity es h] is the quantity of healing item [h] in the 
    player's inventory in state [es].
    Raises [UnknownHealing h] if the player does not have healing item [h] in 
    state [es]. *)
val enemy_healing_quantity : e -> Template.healing_name -> int

(** [enemy_pokemon_level es pn] is the level of pokemon [pn]*)
val enemy_pokemon_level : e -> Template.pokemon_name -> int

(** [type_converter t] is the elemtype of [tn].
    Raises [UnknownType t] if [t] is not a recognized type. *)
val type_converter : type_name -> elemtype

(** [type_effectiveness et1 et2] is the effectiveness of [et1] on [et2]. *)
val type_effectiveness : elemtype -> elemtype -> effective

(** [effective_multipler eff] is the multiplier associated with effectiveness
    [eff]. *)
val effective_multiplier : effective -> int

(** [multiplied_stats tem pn lvl] are the multiplied stats of Pokemon [pn] 
    determined by its current level [lvl] and its base stats in template 
    [tem]. *)
val multiplied_stats : Template.t -> pokemon_name -> int -> (int*int*int)

(** [damage_calculation tem ps es m is_player] is the damage done by the 
    fighting Pokemon of the player in [ps] on the fighting Pokemon of the enemy 
    in [es] by move [m] in template [tem] if [is_player] is true. Otherwise, 
    the damage calculation is by the enemy Pokemon onto the player Pokemon.
    Raises [UnknownMove m] if [m] isn't a move in [tem]. *)
val damage_calculation : Template.t -> p -> e -> move_name 
  -> bool -> int

(** [use_move tem pn m ps es is_player] is [r] if attempting to use move [m] of 
    Pokemon [pn] on an enemy in state [es] by a player in state [ps], or vice 
    versa, results in [r], given template [tem]. If move [m] still has power 
    points left, then [r] is [Legal st'], where in [st'] the move has one less 
    power point and the enemy Pokemon is attacked if [is_player] is true or, 
    otherwise, the player Pokemon is attacked. Otherwise, the result is 
    [Illegal].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps] or [UnknownMove m] if Pokemon [pn] does not have that move in 
    state [ps]. *)
val use_move : Template.t -> move_name -> p -> e -> bool -> result

(** [enemy_heal es] is true if the enemy heals its current fighting Pokemon;
    otherwise the enemy does not heal their fighting Pokemon. *)
val enemy_heal : e -> Template.t -> int -> bool

(** [heal_calculation tem ps es h is_player] is the healing done by the healing
    item [h] in template [tem] on the fighting Pokemon of the player in [ps] if 
    [is_player] is true; otherwise, the heal calculation is of the fighting 
    Pokemon of the enemy in [es].
    Raises [UnknownHealing h] if the healing item [h] is not in [tem]. *)
val heal_calculation : Template.t -> p -> e -> healing_name 
  -> bool -> pokemon_name -> int

(** [use_healing tem pn h ps es is_player] is [r] if attempting to use healing 
    item [h] on a Pokemon [pn] belonging to either a player in state [ps] or an 
    enemy in state [es] results in [r], given template [tem].  If Pokemon [pn] 
    has health and [h] is a potion or if Pokemon [pn] has no health and [h] is 
    a revive, then [r] is [Legal st'], where in [st'] healing item [h] is 
    removed from the player inventory and Pokemon [pn] is healed if [is_player]
    is true, otherwise the healing item removal and the Pokemon healing is done
    on the enemy. Otherwise, the result is [Illegal].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps] or [UnknownHealing h] if the player does not have healing item 
    [h] in state [ps]. *)
val use_healing : Template.t -> pokemon_name -> healing_name -> p -> e -> 
  bool -> result

(** [did_catch tem e pb] is true if the fighting Pokemon of the enemy in [e] is
    successfuly caught by the pokeball [pb] in [tem].
    Raises [UnknownPokeball pb] if the pokeball [pb] is not in [tem]. *)
val did_catch : Template.t -> e -> pokeball_name -> bool

(** [use_pokeball tem ps pb es] is [r] if attempting to use pokeball [pb] on an
    enemy in [es] by a player in [ps] results in [r]. If the enemy in state [es]
    is a Wild Pokemon, then [r] is [Legal st'], where in [st'] the player may
    receive a new Pokemon. Otherwise, the result is [Illegal]. *)
val use_pokeball : Template.t -> p -> pokeball_name -> e -> (result*bool)

(** [can_switch ps pn] is true if the player in [ps] can switch to Pokemon [pn] 
    Raises [UnknownPokemon pn] if the player in [ps] does not have the Pokemon 
    [pn]. *)
val can_switch : p -> pokemon_name -> bool

(** [can_switch_enemy es pn] is true if the enemy in [es] can switch to Pokemon 
    [pn] 
    Raises [UnknownPokemon pn] if the enemy in [es] does not have the Pokemon 
    [pn]. *)
val can_switch_enemy : e -> pokemon_name -> bool

(** [use_pokemon ps pn es] is [r] if attempting to switch to Pokemon [pn] during
    battle in state [ps] results in [r].  If Pokemon [pn] is not currently 
    fighting and isn't fainted, then [r] is [Legal st'], where in [st'] Pokemon 
    [pn] is fighting and the previous fighting Pokemon isn't all while the enemy
    state [es] remains the same. Otherwise, the result is [Illegal].
    Raises [UnknownPokemon pn] if the player does not have Pokemon [pn] in 
    state [ps]. *)
val use_pokemon : p -> Template.pokemon_name -> e -> result

(** [enemy_switch ps pn es] is [r] if attempting to switch to Pokemon [pn] 
    during battle in state [es] results in [r].  If Pokemon [pn] is not 
    currently fighting and isn't fainted, then [r] is [Legal st'], where in 
    [st'] Pokemon 
    [pn] is fighting and the previous fighting Pokemon isn't all while the 
    player state [ps] remains the same . Otherwise, the result is [Illegal].
    Raises [UnknownPokemon pn] if the enemy does not have Pokemon [pn] in 
    state [ps]. *)
val enemy_switch : p -> Template.pokemon_name -> e -> result

(** [did_run e] is true if the player was able to run away successfully from the
    enemy in [e]; otherwise the player does not run away. *)
val did_run : e -> bool

(** [run ps es] is [r] if attempting to run from a Pokemon battle from an enemy
    in state [es] results in [r].  If the enemy in state [es] is a Wild Pokemon, 
    then [r] is [Legal st'], where in [st'] the player is no longer battling and 
    neither of their Pokemon are fighting. Otherwise, the result is [Illegal].*)
val run : p -> e -> result

(** [pokeCenter tem ps es] is [r] if attempting to heal or recover Pokemon back
    to its original state in template [tem] in the player's [ps] inventory 
    while the enemy's state [es] remains the same *)
val pokeCenter: Template.t -> p -> e -> result