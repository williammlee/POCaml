open Graphics
open Images
open Png
open Gui
open Sdl
open Sdlmixer
open Battlegui

(* INSPIRED BY JS_OF_OCAML BOULDERDASH *)

type orientation =
  | Up
  | Down
  | Left
  | Right

type cell =
  | Ground
  | Grass
  | Wall
  | Player of orientation
  | Trainer of orientation

type game_map = cell list list

type trainer_location = (int * int * cell)
type trainer_locations = trainer_location list ref

let screen_dimensions = (13 * px_size , 9 * px_size)

let map_sprites_dir = "map_sprites/"

(** [foundation_ref] is possibly a foundation that is maintained in worldgui to
    pass into battlegui. *)
let foundation_ref: Images.t option ref = ref None

(** [rnd_img_no prefix max] is a randomly chosen PNG file with filename 
    [prefix], followed by some random number from 1...[max]. *)
let rnd_img_no (prefix : string) (max : int) : string =
  Random.self_init ();
  let img_no = string_of_int (Random.int max + 1) in
  prefix ^ img_no ^ ".png"

(** [cell_of_char char] is the cell representation of [char]. *)
let cell_of_char = function
  | ' ' -> Ground
  | 'G' -> Grass
  | 'W' -> Wall
  | 'U' -> Trainer Up
  | 'D' -> Trainer Down
  | 'L' -> Trainer Left
  | 'R' -> Trainer Right
  | 'P' -> Player Down
  | _ -> failwith "malformed map"

(** [oriented_img prefix orient] is an oriented PNG file with filename [prefix]
    followed by an underscore and the lowercase orientation [orient]. *)
let oriented_img (prefix : string) = function
  | Up -> prefix ^ "_up.png"
  | Down -> prefix ^ "_down.png"
  | Left -> prefix ^ "_left.png"
  | Right -> prefix ^ "_right.png"

(** [png_of_cell cell] is the location of the PNG file for [cell].  *)
let png_of_cell = function
  | Ground -> rnd_img_no (map_sprites_dir ^ "ground") 4
  | Grass -> map_sprites_dir ^ "grass.png"
  | Wall -> map_sprites_dir ^ "wall.png"
  | Player o -> oriented_img (map_sprites_dir ^ "player") o
  | Trainer o -> oriented_img (map_sprites_dir ^ "trainer") o

(** [img_of_cell cell] is the PNG image for [cell]. *)
let img_of_cell (cell : cell) : Images.t =
  let fn = png_of_cell cell in
  Png.load fn []

(** [decode_map mp] is the cell equivalent version of the string map [s_mp]. *)
let decode_map (s_mp : string) : game_map =
  let grid_line = String.split_on_char '\n' s_mp in
  List.map (fun x -> let char_list = explode x in
             List.fold_right (fun x' acc -> cell_of_char x' :: acc) char_list []
           ) grid_line

(** [encode_map mp] is the string equivalent version of the cell map [mp]. *)
let encode_map (mp : game_map) : string =
  List.fold_left (fun acc x ->
      acc ^ (List.fold_right (fun x' acc' ->
          png_of_cell x' ^ acc' 
        ) x "\n")
    ) "" mp

(** [get_cell mp cx cy] is the cell in map [mp] at location [(cx,cy)]. *)
let get_cell (mp : game_map) (cx : int) (cy : int) : cell = 
  let specified_row = List.nth mp cy in
  List.nth specified_row cx

(** [map_section_cell_drawing x y cell] draws [cell] appropriately at [(x,y)].*)
let map_section_cell_drawing x y cell =
  match cell with
  (* player is used for initial location only*)
  | Player _ -> draw_img (img_of_cell Ground) x y !scaling true
  (* trainers need a ground cell under them *)
  | Trainer _ -> draw_img (img_of_cell Ground) x y !scaling true;
    draw_img (img_of_cell cell) x y !scaling true
  | _ -> draw_img (img_of_cell cell) x y !scaling true

(** [draw_player player x y] draws the player if [(x,y)] is the center of the
    defined screen dimensions. *)
let draw_player player x y =
  let width, height = screen_dimensions in
  if x = (width / (2 * px_size)) && y = (height / (2 * px_size)) then
    draw_img (img_of_cell player) x y !scaling true

(** [get_map_section mp ul ur ll lr] draws a rectangular subsection of [mp] with
    the map constrained between rows [y_low] to [y_high] and columns [x_low] to
    [x_high]. The player is always centered in this section. *)
let draw_map_section (mp : game_map) (player : cell) (x_low : int) 
    (x_high : int) (y_low : int) (y_high : int) : unit =
  let height = List.length mp in
  ignore (List.fold_left (fun r_no row -> 
      if y_low <= r_no && r_no <= y_high then (
        ignore (List.fold_left (fun elem_no cell -> 
            if x_low <= elem_no && elem_no <= x_high then (
              let offset_elem, offset_r = (elem_no - x_low), (r_no - y_low) in
              (* Place walls when off boundaries of [mp] *)
              map_section_cell_drawing offset_elem offset_r cell;
              draw_player player offset_elem offset_r;
              elem_no + 1)
            else elem_no + 1 ) 0 row);
        r_no - 1)
      else r_no - 1
    ) (height-1) mp)

(** [get_player t_loc] is a tuple with the location and orientation of the
    player in [t_loc].*)
let get_player (t_loc : trainer_locations) : trainer_location = 
  List.find (fun (_,_,cell) -> match cell with 
      | Player _ -> true 
      | _ -> false) !t_loc

(** [is_before px py x y] is true if [(px,py)] comes before [(x,y)] from bottom
    to top, right to left. *)
let is_before (px : int) (py : int) (x : int) (y : int) : bool = 
  (px > x && py = y) || (* on the same row, but more to the right*)
  py < y (* on a higher row *)

(** [put_player t_loc pl] is a tuple list containing all the original locations
    and orientations of the trainers in [t_loc] along with an updated location 
    and orientation for the player with [pl]. *)
let put_player (t_loc : trainer_locations) (pl : trainer_location) :
  trainer_location list = 
  let (px,py,_) = pl in
  let placed = ref false in
  let temp_loc = !t_loc |> List.rev |> List.fold_left (fun acc (x,y,cell) -> 
      match cell with
      | Player _ -> acc (* ignore old player *)
      | _ -> if not !placed && is_before px py x y then 
          (placed := true; (x,y,cell) :: pl :: acc)
        else (x,y,cell) :: acc) [] in
  if !placed then temp_loc else pl :: temp_loc

(** [draw_battle_message ()] will draw extra information to notify the player
    that they're about to transition to the battlegui. *)
let draw_battle_message () =
  let width, height = screen_dimensions in
  let x, y = (width-174)/2, (height-46)/2 in (* message box is 174x46 *)
  prepare_draw map_sprites_dir "message_box" x y;
  prepare_draw_string on_fight_dir 
    "   PREPARE FOR BATTLE_PRESS ANY KEY TO CONTINUE" (x+25,y+28) (5,0)

(** [draw_background] draws Wall cells to cover the entire screen. *)
let draw_background () : unit =
  let width, height = screen_dimensions in
  let rec draw i j = if i < height/px_size then
      (if j < width/px_size then (draw_img (img_of_cell Wall) j i !scaling true; 
                                  draw i (j+1))
       else draw (i+1) 0)
    else () in
  draw 0 0

(** [fill_whitespace ()] helps maintain the graphical side effect that walls are 
    redrawn where walls aren't defined in our map by filling in any white space 
    for edge cases such as resizing the screen or ending a battle. *)
let fill_whitespace () = 
  Graphics.auto_synchronize false;
  draw_background ()

(** [draw_grid mp t_loc] draws the map [mp] and the trainers and players in
    [t_loc]. *)
let draw_grid (mp : game_map) (t_loc : trainer_locations) =
  Graphics.auto_synchronize false;
  let (px,py,player) = get_player t_loc in
  let width, height = screen_dimensions in
  let x_low, x_high = px - width / (2*px_size), px + width / (2*px_size) in
  let y_low, y_high = py - height / (2*px_size), py + height / (2*px_size) in
  draw_map_section mp player x_low x_high y_low y_high;
  if !is_battling then draw_battle_message ();
  Graphics.auto_synchronize true

(* [initialize_trainer_locations mp] is the location and orientation of all
   trainers, including the player. *)
let initialize_trainer_locations (mp : game_map) : trainer_locations =
  let height = List.length mp in
  let trainer_loc = ref [] in
  ignore (List.fold_left (fun r_no row -> 
      ignore (List.fold_left (fun elem_no cell -> 
          (match cell with
           | Trainer o -> 
             trainer_loc := !trainer_loc @ [(elem_no, r_no, Trainer o)]
           | Player _ -> 
             trainer_loc := !trainer_loc @ [(elem_no, r_no, Player Down)]
           | _ -> ());
          elem_no + 1 ) 0 row);
      r_no - 1
    ) (height-1) mp);
  trainer_loc

(** [is_in_trainer_range mp t_loc pos range] is true if a player's current 
    [pos] is within a trainer's [range] in map [mp] and given trainer locations 
    [t_loc]. *)
let is_in_trainer_range (mp : game_map) (t_loc : trainer_locations) 
    (pos : int*int) (range : int) : bool =
  let height = List.length mp in
  let (px,py) = pos in
  let py = (height-1) - py in
  List.exists (fun (tx, ty, trainer) -> 
      match trainer with
      | Trainer orient -> begin match orient with
          | Up -> px = tx && ty < py && py < ty + range + 1
          | Down -> px = tx && ty - range - 1 < py && py < ty
          | Left -> py = ty && tx - range - 1 < px && px < tx
          | Right -> py = ty && tx < px && px < tx + range + 1 end
      | _ -> false
    ) !t_loc

(** [initialize_enemy is_wild is_rival] will store a new enemy status that
    could be
    - a trainer if [is_wild] and [is_rival] are false
    - a Wild pokemon if [is_wild]
    - our rival if [is_rival] is true
*)
let initialize_enemy (is_wild: bool) (is_rival: bool) =
  let tem = unwrap_option !template in
  let e_status = Status.init_enemy_state tem is_wild is_rival in
  enemy_status := Some e_status

(** [trigger_battle pos mp t_loc] is true if a player walks into a battle
    trigger event such as
    - walking in grass and randomly encountering a Pokemon
    - walking in front of a trainer's predefined range
      given a player's position [pos], a map [mp], and trainer_locations 
      [t_loc]. *)
let trigger_battle (pos : int * int) (mp: game_map) 
    (t_loc : trainer_locations) = function
  | Grass -> 
    Random.self_init ();
    if Random.int 100 < 10 then (phase := MENU_SELECT;
                                 initialize_enemy true false; true)
    else false
  | Trainer _ -> initialize_enemy false false; true
  | _ -> let in_range = is_in_trainer_range mp t_loc pos 4 in
    if in_range then (initialize_enemy false false; true)
    else false

(** [walkable cell] is true if the player should be able to walk through 
    [cell], including itself.*)
let walkable = function
  | Ground | Grass | Player _ -> true
  | _ -> false

(** [prepare_battle ()] sets up the transition before entering battlegui. *)
let prepare_battle () = 
  is_battling := true;
  start_music "music/map/entering_battle.mp3"

(** [check_movement mp offset t_loc dir] is true if the player can move. *)
let check_movement (mp : game_map) (offset : int*int) 
    (t_loc : trainer_locations) (dir : orientation) : bool =
  let height = List.length mp in
  (* When checking the mp, we must transform bottom left offset to upper left *)
  let (cx, cy) = (fst offset, (height-1) - snd offset) in
  (* player enters battle *)
  if get_cell mp cx cy |> trigger_battle (cx, cy) mp t_loc then 
    prepare_battle ();
  get_cell mp cx cy |> walkable 

(** [change_location mp offset t_loc dir] will first orient the player in 
    direction [dir] in [t_loc] and, if the player could move to position 
    [offset], relative to the bottom left corner of [mp], then it will also move 
    in direction [dir] and have its position updated in [t_loc]. *)
let change_location (mp : game_map) (offset : int*int) 
    (t_loc : trainer_locations) (dir : orientation) : unit =
  let (x,y,_) = get_player t_loc in
  t_loc := put_player t_loc (x,y,Player dir);
  if check_movement mp offset t_loc dir then
    let (cx, cy) = offset in
    t_loc := put_player t_loc (cx,cy,Player dir)

(** [handle_movement mp t_loc key] handles player movement in map [mp] based on 
    button press [key], changing it's location and orientation in [t_loc] *)
let handle_movement (mp : game_map) (t_loc : trainer_locations) (key : char) :
  unit  =
  let (x,y,player) = get_player t_loc in
  match key with
  | 'w' -> change_location mp (x, y+1) t_loc Up
  | 's' -> change_location mp (x, y-1) t_loc Down
  | 'a' -> change_location mp (x-1, y) t_loc Left
  | 'd' -> change_location mp (x+1, y) t_loc Right
  | _ -> ()

(** [handle_scaling key] handles the button press of [key] that will determine
    whether the screen size increases or decreases. *)
let handle_scaling (key : char) : unit =
  let width, height = screen_dimensions in
  match key with
  | '+' -> scale_window width height (fun scale -> scale + 1) scaling;
    fill_whitespace ();
  | '-' -> scale_window width height (fun scale -> scale - 1) scaling;
    fill_whitespace ();
  | _ -> () 

(** [setup_audio] opens the audio channel and plays background music. *)
let setup_audio () =
  open_audio ~freq:44100 ();
  start_music "music/map/route1.mp3"

(** [setup_resize x y] will resize the window to have dimensions [(x,y)] while
    also resizing all image assets appropriately. *)
let setup_resize x y =
  scale_window x y (fun scale -> scale) scaling

(** [setup_graph] opens the graph based on the defined screen dimensions. 
    Effects: Opens the graph window and initializes the grid.*)
let setup_graph mp trainer_loc =
  let x, y = screen_dimensions in
  setup_resize x y;
  draw_grid mp trainer_loc

(** [handle_press mp t_loc key] handles the button press of [key] and will
    appropriately exit the game, move the player given the positions of all
    trainers and the player in [t_loc] along map [mp], or scale the window, 
    then redraws the entire grid. *)
let handle_press (mp : game_map) (t_loc : trainer_locations) (key : char) =
  handle_movement mp t_loc key;
  handle_scaling key;
  draw_grid mp t_loc

(** [setup_world mp trainer_loc] initializes worldgui using the map [mp] and
    trainer locations [trainer_loc]. *)
let setup_world (mp : game_map) (trainer_loc : trainer_locations) =
  setup_graph mp trainer_loc;
  setup_audio ()

let initial_world_setup (s_mp : string) =
  let mp = decode_map s_mp in
  let trainer_loc = initialize_trainer_locations mp in
  setup_world mp trainer_loc;
  mp, trainer_loc

(** [reset_from_battle mp t_loc] will re-initialize worldgui using the map [mp]
    and trainer locations [t_loc] while maintaing the loop. *)
let reset_from_battle (mp : game_map) (t_loc : trainer_locations) =
  setup_world mp t_loc;
  fill_whitespace ();
  draw_grid mp t_loc;
  enemy_status := None;
  reset_worldgui := false

(** [begin_battlegui_transition event did_setup] will setup the battlegui if
    [did_setup] is false and otherwise will enter the battlegui, sending in 
    [event] with it. *)
let begin_battlegui_transition (event: Graphics.status) = function
  | true -> let foundation = unwrap_option !foundation_ref in
    enter_battle_phase event foundation
  | false -> let foundation = initial_battle_setup () in
    foundation_ref := Some foundation; 
    battlegui_did_setup := true

let worldgui_repl (mp : game_map) (t_loc : trainer_locations) 
    (event : Graphics.status) =
  if !is_battling then begin_battlegui_transition event !battlegui_did_setup
  else if !reset_worldgui then reset_from_battle mp t_loc
  else handle_press mp t_loc event.key