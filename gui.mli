(** Factors out many functions in common with all GUIs to prevent code 
    duplication. Also maintains multiple setup references to prevent circular
    dependecies between GUIs. *)

(** [px_size] is a pre-defined value, n, that defines the nxn size of a grid
    space. *)
val px_size : int

(** [scaling] is the current scale size of the game window. *)
val scaling : int ref

(** [on_widgets_dir] is the directory containing fonts that typically goes on 
    widgets. *)
val on_widgets_dir : string

(** [on_widgets_dir] is the directory containing fonts that typically goes on 
    the message bar. *)
val on_message_dir : string

(** [on_widgets_dir] is the directory containing fonts that typically goes on
    the fight menu. *)
val on_fight_dir : string

(** [worldgui_did_setup] is true if the worldgui has been setup. *)
val worldgui_did_setup : bool ref

(** [reset_worldgui] is true whenever the battlegui is returning to worldgui. *)
val reset_worldgui : bool ref

(** [template] is the custom OCaml object holding our template JSON information.
*)
val template : Template.t option ref

(** [player_status] is possibly the current status of the player's battle state,
    their pokemon, and their items. *)
val player_status : Status.p option ref

(** [enemy_status] is possibly the current status of their pokemon, their items,
    and whether they're a trainer or wild pokemon.  *)
val enemy_status : Status.e option ref

(** [battlegui_did_setup] is true if the battlegui has been setup. *)
val battlegui_did_setup : bool ref

(** [is_battling] is true when entering battlegui, during the battle, and when
    exiting battlegui. *)
val is_battling : bool ref

(** [unwrap_option opt] is the value [s] inside [opt] or fails otherwise. *)
val unwrap_option : 'a option -> 'a

(** [start_music music] starts playing the music (MP3 file) with the filename 
    [music]. *)
val start_music : string -> unit

(** [start_sound sound] starts playing the sound (WAV file) with the filename 
    [sound]. *)
val start_sound : string -> unit

(** [draw_img img x y scale is_grid] draws [img] onto the graphics window with
    its bottom left corner at [(x,y)] after it undergoes transparency
    application and scaling by [scale] which remains the same if [is_grid] or
    otherwise is divided by [px_size]. *)
val draw_img : Images.t -> int -> int -> int -> bool -> unit

(** [prepare_draw dir fn x y] prepares the file [fn] in directory [dir] to draw
    its lower left corner at [(x,y)]. *)
val prepare_draw : string -> string -> int -> int -> unit

(** [explode s] is a list of all the characters in [s]. *)
val explode : string -> char list

(** [prepare_draw_string dir s pos spacing] prepares the string [s] to be drawn
    with the font in directory [dir] at position [pos] with the spacing between
    letters being [spacing]. *)
val prepare_draw_string : string -> string -> int * int -> int * int -> unit

(** [scale_window width height scaler scaling] resizes the size of the window to
    be [width] by [height] with the dimensions scaled based on [scaler scaling],
    restricted between 2 and 4 inclusive.
    NOTE: Scaling above 2 will slow down the game, especially during selecting
    a choice during battle. *)
val scale_window : int -> int -> (int -> int) -> int ref -> unit
