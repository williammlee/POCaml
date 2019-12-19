(** Initializes the map in which a player may move around using WASD, resize the
    screen using -+, and quit the game using Q. The map contains trainers and
    grass which can transition to the battlegui. *)

(** The type for directions that a trainer, including the player, may face. *)
type orientation =
  | Up
  | Down
  | Left
  | Right

(** The type for cells within the grid. *)
type cell =
  | Ground
  | Grass
  | Wall
  | Player of orientation
  | Trainer of orientation

(** The type for the list containing rows of cells that represent the world 
    map *)
type game_map = cell list list

(** The type for trainer locations, including the player, including their (x,y)
    location relative to the bottom left corner of the grid, along with the 
    cell identifying the trainer and their orientation. *)
type trainer_location = (int * int * cell)

(** The type for the mutable list containing the trainer locations, including
    their orientation. *)
type trainer_locations = trainer_location list ref

(** [screen_dimensions] is the (width,height) of the screen in terms of cell 
    size. *)
val screen_dimensions : (int * int)

(** [map_sprites_dir] is the directory containing all map sprites. *)
val map_sprites_dir : string

(** [worldgui_repl mp t_loc event] is the REPL that either prepares the player
    to enter battle or uses the map [mp] and locations of trainers [t_loc] to 
    reset itself when transitioning back to the GUI from battle and to handle 
    the plausible press in [event]. *)
val worldgui_repl : game_map -> trainer_locations -> Graphics.status -> unit

(** [initial_world_setup s_mp] is a decoded string map, a pre-generated ground 
    foundation and the locations of all trainers, including the player for the 
    map [s_mp]. 
    Effects: Opens the graphics window and initializes the game graphics as 
    given in [s_mp]. *)
val initial_world_setup : string -> game_map * trainer_locations