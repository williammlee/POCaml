(** Initializes the battle screen in which a player may hover between options
    using WASD, resize the screen using -+, select an option using E, return 
    to the previous menu using R, and quit the game using Q. *)

(** The type of battle phases in which a player can currently be in. *)
type battle_phase =
  | START
  | MENU_SELECT
  | FIGHT_MENU
  | INVENTORY_MENU
  | HEALING_MENU
  | ATTEMPT_CATCH
  | POKEMON_MENU
  | ATTEMPT_RUN
  | PLAYER_TURN
  | ENEMY_TURN
  | END

(** [phase] is the current battle phase that the player is currently in. *)
val phase : battle_phase ref

(** [battle_sprites_dir] is the directory containing all battle sprites. *)
val battle_sprites_dir : string

(** [trainer_dir] is the subdirectory containing all trainer sprites. *)
val trainer_dir : string

(** [pokemon_dir] is the subdirectory containing all pokemon sprites. *)
val pokemon_dir : string

(** [inventory_dir] is the subdirectory containing all inventory sprites. *)
val inventory_dir : string

(** [pokemon_cries_dir] is the subdirectory containing all pokemon cry sounds.*)
val pokemon_cries_dir : string

(** [battle_music_dir] is the subdirectory containing all battle music.*)
val battle_music_dir : string

(** [sfx_dir] is the subdirectory containing all pokemon cry sounds. *)
val sfx_dir : string

(** [screen_dimensions] is the (width,height) of the screen in terms of cell 
    size. *)
val screen_dimensions : int * int

(** [enter_battle_phase event fdn] will enter the phase we are currently in
    upon detecting a keypress in [event] if we are in a controllable phase.
    Effects: Draws the state we are in using [fdn] as the background. *)
val enter_battle_phase : Graphics.status -> Images.t -> unit

val initial_battle_setup : unit -> Images.t