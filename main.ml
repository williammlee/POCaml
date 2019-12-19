open Graphics
open Images
open Png
open Gui
open Sdl
open Sdlmixer
open Template
open Worldgui

(* TODO - retrieve maps from a text file inside our game directory *)
let first_map =
  "WWWWWWWWW
WR  GG  W
W   GRGGW
W R P RGW
WGGGR   W
WGGG  L W
WWWWWWWWW"

let big_map =
  "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
W         GGGGGGGG                  GGGGGGGG         W
W       GGGGGGGGGGGG              GGGGGGGGGGGG       W
W     GGGGGGG  GGGGGGG          GGGGGGG  GGGGGGG     W
W    LGGGGGG    GGGGGGR        LGGGGGG    GGGGGGR    W
W     GGGGGGG  GGGGGGG          GGGGGGG  GGGGGGG     W
W       GGGGGGGGGGGG              GGGGGGGGGGGG       W
W         GGGGGGGG                  GGGGGGGG         W
W                                                    W
W                      GGGGGGGG                      W
W                    GGGGGGGGGGGG                    W
W                  GGGGGGG  GGGGGGG                  W
W           P     LGGGGGG    GGGGGGR                 W
W                  GGGGGGG  GGGGGGG                  W
W                    GGGGGGGGGGGG                    W
W                      GGGGGGGG                      W
W         GGGGGGGG                  GGGGGGGG         W
W       GGGGGGGGGGGG              GGGGGGGGGGGG       W
W     GGGGGGG  GGGGGGG          GGGGGGG  GGGGGGG     W
W    DGGGGGG    GGGGGGD        DGGGGGG    GGGGGGD    W
W     GGGGGGG  GGGGGGG          GGGGGGG  GGGGGGG     W
W       GGGGGGGGGGGG              GGGGGGGGGGGG       W
W         GGGGGGGG                  GGGGGGGG         W
WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
"

let dense_map =
  "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
W D  GGGGGGGGGGGGGGGGGGGGGGGGGGGG                    W
W   GGGGGGGGG                  L                     W
W      G                            GGGGGGG          W
WGGG                                  GGGGGGG        W
WGGG  R                  GP              GGGGGGG     W
W GG                   GGGGG                         W
W  GG                  GGGGG         D               W
W  GG  R          L    GGGGG                         W
W                         GGGDGGGDGGG        GG G    W
W    D   D         GG      LGGGRGGGR    L     GGGG   W
W      U           GGG    GGGUGGGUGGG          G G   W
WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
"

let test_trainer_range =
  "WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
W                                                     W
W                                                     W
W                      U                              W
W                     L  PR                           W
W                       D                             W
W                                                     W
W                                                     W
W                                                     W
W                                                     W
WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
"

(** [gamemap_ref] is possibly a map that is maintained in main to pass into
    worldgui *)
let gamemap_ref : game_map option ref = ref None

(** [trainer_loc_ref] is possibly the locations of all trainers and the player
    that is maintained in main to pass into worldgui.  *)
let trainer_loc_ref : trainer_locations option ref = ref None

(** [setup_template ()] is true if the template file is correctly formatted or
    otherwise prints a message saying it's deformed.*)
let setup_template () =
  try template := Some ("template.json" |> Yojson.Basic.from_file |> from_json);
    true
  with | _  -> print_endline "The template JSON has been deformed."; 
    false

(** [setup_POCaml ()] will open the graphics window and draw the startup screen
    and will also open the audio channel and play the opening music. *)
let setup_POCaml () = 
  open_graph " 480x320";
  open_audio ~freq:44100 ();
  let opening_music = load_music "music/opening.mp3" in
  play_music opening_music;
  draw_img (Png.load "logo/pokemon_startup_screen.png" []) 0 0 1 false

(** [begin_worldgui_transition event did_setup] will setup the worldgui if
    [did_setup] is false and otherwise will enter the worldgui, sending in 
    [event] with it. *)
let begin_worldgui_transition event = function
  | true -> let gamemap = unwrap_option !gamemap_ref in
    let trainer_loc = unwrap_option !trainer_loc_ref in
    worldgui_repl gamemap trainer_loc event
  | false -> let gamemap, trainer_loc = initial_world_setup big_map in
    gamemap_ref := Some gamemap; trainer_loc_ref := Some trainer_loc;
    worldgui_did_setup := true

let run_POCaml () =
  setup_POCaml ();
  if setup_template () then (
    player_status := Some (Status.init_player_state (unwrap_option !template));
    loop_at_exit [Key_pressed]
      (fun event ->
         if event.key = 'q' then raise Exit;
         if event.keypressed then begin_worldgui_transition event 
             !worldgui_did_setup
      ))

let () = run_POCaml ()
