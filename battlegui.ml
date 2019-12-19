open Graphics
open Images
open Png
open Template
open Status
open Action
open Sdl
open Gui
open Sdlmixer
open Yojson.Basic

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

let battle_sprites_dir = "battle_sprites/"
let trainer_dir = "battle_trainer_sprites/"
let pokemon_dir = "battle_pokemon_sprites/"
let inventory_dir = battle_sprites_dir ^ "battle_inventory_sprites/"
let pokemon_cries_dir = "music/pokemon_cries/"
let battle_music_dir = "music/battle_music/"
let sfx_dir = "music/sfx/"

(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(*                             HARDCODING START                               *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)

(** [verify_file j] is the check for whether or not a user inputs a valid
    json file [j] that is in the directory or exits*)
let rec verify_file j = 
  try 
    j |> from_file |> from_json 
  with 
  | _  -> 
    print_endline "This is not a valid file. Please try again.\n";
    print_string  "> ";
    verify_file (read_line())

let template = ref (verify_file "template.json")
type p_healing = {p_hname: healing_name; p_pquantity: int}
type p_pokeball = {p_pbname: pokeball_name; p_pbquantity: int}
type p_inventory = {p_healing: p_healing list; p_pokeball: p_pokeball list}


type p_moveset = {p_mname: string; pp: int}

let move_list = [
  {p_mname = "Tackle"; pp = 22};
  {p_mname = "Headbutt"; pp = 5};
  {p_mname = "Dragon Claw"; pp = 2};
  {p_mname = "Flamethrower"; pp = 15}
]

let inventory : p_inventory = 
  { 
    p_healing = [
      {p_hname = "Potion"; p_pquantity = 3};
      {p_hname = "Super Potion"; p_pquantity = 2};
      {p_hname = "Ultra Potion"; p_pquantity = 1};
      {p_hname = "Revive"; p_pquantity = 2};
      {p_hname = "Max Revive"; p_pquantity = 1}
    ];
    p_pokeball= [
      {p_pbname = "Pokeball"; p_pbquantity = 10};
      {p_pbname = "Great Ball"; p_pbquantity = 1};
      {p_pbname = "Ultra Ball"; p_pbquantity = 1};
      {p_pbname = "Master Ball"; p_pbquantity = 1};
    ]
  }

type p_pokemon = {p_pname: pokemon_name; p_level: int; p_curr_hp: int; 
                  p_curr_exp: int; p_attacking: bool; p_moveset: p_moveset list}

let pokemon_ratatta : p_pokemon = {p_pname = "Rattata"; p_level = 5; 
                                   p_curr_hp = 14; 
                                   p_curr_exp = 24; p_attacking = true; 
                                   p_moveset = move_list
                                  }

let pokemon_charmander : p_pokemon = {p_pname = "Charmander"; p_level = 7; 
                                      p_curr_hp = 42; 
                                      p_curr_exp = 52; p_attacking = false; 
                                      p_moveset = move_list
                                     }

let pokemons_lists : p_pokemon list = [pokemon_ratatta;pokemon_charmander]


(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(*                               HARDCODING END                               *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)


(************)
(** STATES **)
(************)


let phase = ref START

(** [enemy_trainer] is the current trainer that we are facing. *)
let enemy_trainer = ref ""

(** [menu] is the menu option that we are hovering over in MENU_SELECT *)
let menu = ref Fight

(** [move] is the move number that we are hovering over in FIGHT_MENU where 
    moves are numbered 0-3, left to right, top to bottom order. *)
let move = ref 0

(** [item] is the item number that we are hovering over in INVENTORY_MENU where
    items are numbered 0-[n], where [n] is the number of items we have, top to 
    bottom order. *)
let item = ref 0

(** [pokemon] is the pokemon number that we are hovering over in POKEMON_MENU
    where pokemon are numbered 0-[n], where [n] is the number of pokemon we 
    have, 0 being our current pokemon and, if [n] > 0, 1 - [n] being pokemon we 
    currently have, top to bottom order *)
let pokemon = ref 0

(** [reset_to_defaults ()] will reset all states to their initial values. *)
let reset_to_defaults () =
  phase := START;
  enemy_trainer := "";
  menu := Fight;
  move := 0;
  item := 0;
  pokemon := 0


(*****************)
(** CONSTRAINTS **)
(*****************)


let screen_dimensions = (256,192)

(** [message_bar_height] is the height of the message bar. *)
let message_bar_height = 48

(** [message_text_x] is the bottom left x-coord of the message bar. *)
let message_text_x = 10

(** [message_text_y] is the bottom left y-coord of the message bar. *)
let message_text_y = 26

(** [inventory_text_x] is the bottom left x-coord of the first letter of the 
    inventory text. *)
let inventory_text_x = 130

(** [inventory_text_y] is the bottom left y-coord of the first letter of the 
    inventory text. *)
let inventory_text_y = 160

(** [inventory_text_spacing] is the common spacing between letters in the 
    inventory menu.  *)
let inventory_text_spacing = 12

(** [menu_bar_x] is the bottom left x-coord of the menu select bar. *)
let menu_bar_x = 136

(** [p_pname_x] is the bottom left x-coord of the first letter of the player
    pokemon's name on the player battle widget. *)
let p_pname_x = menu_bar_x + 22

(** [p_pname_y] is the bottom left y-coord of the first letter of the player
    pokemon's name on the player battle widget. *)
let p_pname_y = (message_bar_height+27)

(** [p_level_x_end] is the bottom left x-coord of the last number of the player
    pokemon's level on the player battle widget. *)
let p_level_x_end = p_pname_x + 74

(** [e_pname_x] is the bottom left x-coord of the first letter of the enemy
    pokemon's name on the enemy battle widget. *)
let e_pname_x = 20

(** [e_pname_y] is the bottom left y-coord of the first letter of the enemy
    pokemon's name on the enemy battle widget. *)
let e_pname_y = (message_bar_height+116)

(** [e_level_x_end] is the bottom left x-coord of the last number of the enemy
    pokemon's level on the enemy battle widget. *)
let e_level_x_end = e_pname_x + 74

(** [menu_selector_pos state] is the [(x,y)] position of the menu selector in
    [state]. *)
let menu_selector_pos = function
  | Fight -> menu_bar_x+9,26
  | Inventory -> menu_bar_x+65, 26
  | Pokemon -> menu_bar_x+9, 10
  | Run -> menu_bar_x+65, 10
  | _ -> failwith "impossible menu"

(** [move_selector_pos state] is the [(x,y)] position of the move selector in
    [state]. *)
let move_selector_pos = function
  | 0 -> message_text_x, message_text_y-2
  | 1 -> message_text_x+70, message_text_y-2
  | 2 -> message_text_x, message_text_y-18
  | 3 -> message_text_x+70, message_text_y-18
  | _ -> failwith "impossible move"

(** [item_selector_pos itemno] is the [(x,y)] position of the item selector for
    item [itemno]. *)
let item_selector_pos itemno = 
  inventory_text_x-9,inventory_text_y-1-itemno*inventory_text_spacing


(*************)
(** DRAWERS **)
(*************)


(** [prepare_draw_pokemon pname is_player] prepares pokemon [pname] to be drawn
    facing toward the enemy if [is_player] is true or otherwise facing away from 
    us is [is_player] is false. *)
let prepare_draw_pokemon pname = function
  | true -> prepare_draw (battle_sprites_dir ^ pokemon_dir) 
              (String.lowercase_ascii pname ^ "_back") 38 message_bar_height
  | false -> prepare_draw (battle_sprites_dir ^ pokemon_dir)
               (String.lowercase_ascii pname ^ "_front") 155 85

(** [prepare_draw_fdn img x y] prepares the base assets (foundation) of the
    battle screen to be drawn. *)
let prepare_draw_fdn img x y = draw_img img x y !scaling false

(** [add_spaces n] adds two spaces to [n] if one digit, one space if two digits,
    and no spaces otherwise. *)
let add_spaces n =
  if n / 10 = 0 then "  " ^ string_of_int n
  else if n / 100 = 0 then " " ^ string_of_int n
  else string_of_int n

(** [upper] is a shorter synonym for String.uppercase_ascii *)
let upper = String.uppercase_ascii

(** [draw_player_widget ps] draws the player's battle widget, using the player's
    current state [ps]. *)
let draw_player_widget ps =
  let p_fighter = player_pokemon_fighting_name ps in
  let p_fighter_lvl = player_pokemon_level ps p_fighter in
  let p_fighter_hp = player_pokemon_hp ps p_fighter in

  let (p_fighter_max_hp, _, _) = multiplied_stats !template p_fighter 
      p_fighter_lvl in
  prepare_draw battle_sprites_dir "player_widget" (menu_bar_x+6) 
    (message_bar_height+3);
  prepare_draw_string on_widgets_dir (upper p_fighter) (p_pname_x, p_pname_y)
    (5, 0);
  prepare_draw_string on_widgets_dir ("#" ^ (p_fighter_lvl |> string_of_int))  
    (p_level_x_end, p_pname_y)
    (-5, 0);
  let spaced_hp = add_spaces p_fighter_hp in
  let spaced_max_hp = add_spaces p_fighter_max_hp in
  prepare_draw_string on_widgets_dir ( spaced_hp ^ "/" ^ spaced_max_hp ) 
    (p_level_x_end, p_pname_y-18) (-5, 0)

(** [draw_enemy_widget es] draws the enemy's battle widget, using the enemy's
    current state [es]. *)
let draw_enemy_widget es = 
  let e_fighter = enemy_pokemon_fighting_name es in
  let e_fighter_lvl = enemy_pokemon_level es e_fighter in
  prepare_draw battle_sprites_dir "enemy_widget" 13 (message_bar_height+100);
  prepare_draw_string on_widgets_dir (upper e_fighter) (e_pname_x, e_pname_y) 
    (5,0);
  prepare_draw_string on_widgets_dir ("#" ^ string_of_int e_fighter_lvl) 
    (e_level_x_end, e_pname_y) (-5, 0)

(** [draw_common_phase p_pname e_pname fdn] draws all the assets that are
    usually on the screen such as the player's pokemon [p_pname] and the enemy's
    pokemon [e_pname] with their battle widgets and the base assets [fdn]. *)
let draw_common_phase ps es fdn =
  let p_fighter = player_pokemon_fighting_name ps in
  let e_fighter = enemy_pokemon_fighting_name es in
  prepare_draw_fdn fdn 0 0;
  draw_enemy_widget es;
  prepare_draw_pokemon e_fighter false;
  draw_player_widget ps;
  prepare_draw_pokemon p_fighter true

(** [back_to_worldgui ()] prepares the return to worldgui by resetting battlegui
    and setting up worldgui. *)
let back_to_worldgui () = 
  Gui.battlegui_did_setup := false;
  Gui.is_battling := false;
  Gui.reset_worldgui := true;
  reset_to_defaults ()

(** [draw_run p_pname e_pname fdn] draws the phase associated with ATTEMPT_RUN*)
let draw_run ps es fdn =
  Graphics.auto_synchronize false;
  draw_common_phase ps es fdn;
  prepare_draw_string on_message_dir 
    ("Got away safely!_Press any key to continue.")
    (message_text_x, message_text_y) (6,0);
  Graphics.auto_synchronize true;
  start_sound (sfx_dir ^ "run.wav");
  back_to_worldgui ()

(** [draw_pokemon_menu ps] draws the phase associated with POKEMON_MENU using
    the player's current [pokemons]. *)
let draw_pokemon_menu pokemons =
  Graphics.auto_synchronize false;

  (* Setup pokemon menu *)
  prepare_draw battle_sprites_dir "pokemon_menu" 0 0;

  Graphics.auto_synchronize true

(* https://stackoverflow.com/questions/11193783/ocaml-strings-and-substrings/11206238 *)
(** [str_contains_substr s1 s2] is true if [s1] contains the substring [s2]. *)
let str_contains_substr s1 s2 =
  try let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done; false
  with Exit -> true

(** [get_item inv itemno] is the item in [inv] located at index [itemno], such 
    that the inventory are healing items then pokeballs. *)
let get_item_name inv itemno =
  if 0 <= itemno && itemno < List.length inv.p_healing then
    let item = List.nth inv.p_healing itemno in item.p_hname
  else if itemno < List.length inv.p_healing + List.length inv.p_pokeball then
    let item = List.nth inv.p_pokeball (itemno - List.length inv.p_healing) in
    item.p_pbname
  else ""

(** [format_item item] is [item] but with no spaces and all lowercase. *)
let format_item item =
  let item_words = String.split_on_char ' ' item in
  List.fold_left (fun acc x -> acc ^ String.lowercase_ascii x) "" item_words

(** [draw_inventory_menu inventory] draws the phase associated with 
    INVENTORY_MENU using the player's current [inventory]. *)
let draw_inventory_menu inventory =
  Graphics.auto_synchronize false;

  (* Setup inventory menu *)
  prepare_draw battle_sprites_dir "inventory_menu" 0 0;
  let is_x, is_y = item_selector_pos !item in
  prepare_draw battle_sprites_dir "menu_select" is_x is_y;

  (* Draw menu items along with quantities *)
  let healing_acc = List.fold_left (fun acc h -> 
      prepare_draw_string on_widgets_dir h.p_hname 
        (inventory_text_x, inventory_text_y-acc) (5,0);
      prepare_draw_string on_widgets_dir ("x " ^ string_of_int h.p_pquantity)
        (inventory_text_x+110, inventory_text_y-acc) (-5,0);
      acc + inventory_text_spacing) 0 inventory.p_healing in
  let pokeball_acc = List.fold_left (fun acc pb ->
      prepare_draw_string on_widgets_dir pb.p_pbname 
        (inventory_text_x, inventory_text_y-acc) (5,0);
      prepare_draw_string on_widgets_dir ("x " ^ string_of_int pb.p_pbquantity)
        (inventory_text_x+110, inventory_text_y-acc) (-5,0);
      acc + inventory_text_spacing) healing_acc inventory.p_pokeball in
  prepare_draw_string on_widgets_dir "CLOSE BAG"
    (inventory_text_x, inventory_text_y-pokeball_acc) (5,0);

  (* Draw current item with image and description *)
  let item_name = !item |> get_item_name inventory |> format_item in
  if item_name <> "" then begin
    prepare_draw inventory_dir item_name 9 78;
    if str_contains_substr item_name "potion" then prepare_draw_string 
        on_fight_dir "HEALS A POKEMON WITH_HEALTH FOR 20" (10,55) (5,0)
    else if str_contains_substr item_name "revive" then prepare_draw_string 
        on_fight_dir "HEALS A FAINTED_POKEMON FOR 20" (10,55) (5,0)
    else if str_contains_substr item_name "ball" then prepare_draw_string 
        on_fight_dir "HAS A CHANCE TO_CATCH A WILD_POKEMON" (10,55) (5,0)
    else failwith "invalid item detected";
  end else ();

  Graphics.auto_synchronize true

(** [draw_inventory_menu inventory] draws the phase associated with FIGHT_MENU
    in the player's current state [ps] and the enemy's current state [es]. *)
let draw_fight_menu ps es fdn =

  let p_fighter = player_pokemon_fighting_name ps in
  let moves = player_pokemon_moveset ps p_fighter in

  let get = List.nth moves in
  let upper = String.uppercase_ascii in
  let get_move i = get i |> upper in
  let get_move_type i = get i |> move_type !template |> upper in
  let get_move_current_pp i = get i |> player_pokemon_move_pp ps p_fighter |>
                              string_of_int in
  let get_move_max_pp i = let (pp,_,_) = get i |> move_stats !template in 
    string_of_int pp in

  Graphics.auto_synchronize false;
  draw_common_phase ps es fdn;
  prepare_draw battle_sprites_dir "moves_bar" 0 0;

  (* Draw move names *)
  prepare_draw_string on_fight_dir (get_move 0) 
    (message_text_x+9, message_text_y) (5,0);
  prepare_draw_string on_fight_dir (get_move 1) 
    (message_text_x+79, message_text_y) (5,0);
  prepare_draw_string on_fight_dir ("_"^(get_move 2)) 
    (message_text_x+9, message_text_y) (5,0);
  prepare_draw_string on_fight_dir ("_"^(get_move 3))
    (message_text_x+79, message_text_y) (5,0);

  (* Draw currently selected move's PP and TYPE. *)
  prepare_draw_string on_fight_dir ("PP") (menu_bar_x+45, message_text_y) 
    (5,0);
  prepare_draw_string on_fight_dir 
    ((get_move_current_pp !move) ^ "/" ^ (get_move_max_pp !move)) 
    (menu_bar_x+90, message_text_y) (-5,0);
  prepare_draw_string on_fight_dir ("_TYPE/" ^ get_move_type !move) 
    (menu_bar_x+45, message_text_y) (5,0);

  (* Draw move selector appropriately to signify the currently selected move. *)
  let ms_x, ms_y = move_selector_pos !move in
  prepare_draw battle_sprites_dir "menu_select" ms_x ms_y;
  Graphics.auto_synchronize true

(** [draw_menu_select inventory] draws the phase associated with MENU_SELECT in
    the player's current state [ps] and the enemy's current state [es]. *)
let draw_menu_select ps es fdn =

  let p_fighter = player_pokemon_fighting_name ps in

  Graphics.auto_synchronize false;
  draw_common_phase ps es fdn;
  prepare_draw_string on_message_dir ("What will_" ^ p_fighter ^ " do?")
    (message_text_x, message_text_y) (6,0);
  prepare_draw battle_sprites_dir "menu_bar" menu_bar_x 0;
  let ms_x, ms_y = menu_selector_pos !menu in
  prepare_draw battle_sprites_dir "menu_select" ms_x ms_y;
  Graphics.auto_synchronize true


(** [play_pokemon_cry pname] will play a sound composed of a pokeball opening
    and pokemon [pname] emitting a cry. *)
let play_pokemon_cry pname =
  start_sound (sfx_dir ^ "pokeball_open" ^ ".wav");
  Unix.sleepf 0.5;
  start_sound (pokemon_cries_dir ^ String.lowercase_ascii pname ^ ".wav")

(** [rdm_img_no prefix max] is a randomly chosen PNG file with filename 
    [prefix], followed by some zero padded random number from 1...[max]. *)
let rdm_img_no prefix max =
  Random.self_init ();
  let img_no = Random.int max + 1 in
  if img_no / 10 = 0 then prefix ^ "00" ^ string_of_int img_no
  else if img_no / 100 = 0 then prefix ^ "0" ^ string_of_int img_no
  else prefix ^ string_of_int img_no

(** [draw_start ps es fdn] draws the phase associated with START in
    the player's current state [ps] and the enemy's current state [es]. *)
let draw_start ps es fdn =

  let p_fighter = player_pokemon_fighting_name ps in
  let e_fighter = enemy_pokemon_fighting_name es in
  let upper = String.uppercase_ascii in

  (* Trainer wants to battle *)
  Graphics.auto_synchronize false;
  prepare_draw_fdn fdn 0 0;
  prepare_draw_string on_message_dir "TRAINER_would like to battle!" 
    (message_text_x, message_text_y) (6,0);
  prepare_draw battle_sprites_dir "player_back" 48 message_bar_height;
  (* save the enemy_trainer for END *)
  enemy_trainer := rdm_img_no (trainer_dir ^ "trainer") 121;
  prepare_draw battle_sprites_dir !enemy_trainer 152 100;
  Graphics.auto_synchronize true;

  (* Message that trainer sends out their pokemon *)
  Unix.sleep 3;
  Graphics.auto_synchronize false;
  prepare_draw battle_sprites_dir "message_bar" 0 0;
  prepare_draw_string on_message_dir 
    ("TRAINER sent_out " ^ upper e_fighter ^ "!") 
    (message_text_x, message_text_y) (6,0);
  Graphics.auto_synchronize true;

  (* Trainer pokemon is sent out *)
  Unix.sleep 1;
  Graphics.auto_synchronize false;
  prepare_draw_fdn fdn 0 0; (* draw over old enemy trainer *)
  prepare_draw battle_sprites_dir "player_back" 48 message_bar_height;
  prepare_draw_string on_message_dir 
    ("TRAINER sent_out " ^ upper e_fighter ^ "!")
    (message_text_x, message_text_y) (6,0);
  prepare_draw_pokemon e_fighter false;
  draw_enemy_widget es;
  Graphics.auto_synchronize true;
  play_pokemon_cry e_fighter;

  (* Message that player sends out their pokemon *)
  Unix.sleep 3;
  Graphics.auto_synchronize false;
  prepare_draw battle_sprites_dir "message_bar" 0 0;
  prepare_draw_string on_message_dir ("Go! " ^ upper p_fighter ^ "!")
    (message_text_x, message_text_y) (6,0);
  Graphics.auto_synchronize true;

  (* Player pokemon is sent out *)
  Unix.sleep 1;
  Graphics.auto_synchronize false;
  prepare_draw_fdn fdn 0 0; (* draw over old player *)
  prepare_draw_pokemon p_fighter true;
  prepare_draw_string on_message_dir ("Go! " ^ upper p_fighter ^ "!")
    (message_text_x, message_text_y) (6,0);
  prepare_draw_pokemon e_fighter false;
  draw_player_widget ps;
  draw_enemy_widget es;
  Graphics.auto_synchronize true;
  play_pokemon_cry p_fighter;

  (* Prepare the menu for MENU_SELECT phase *)
  Unix.sleep 3;
  phase := MENU_SELECT;
  draw_menu_select ps es fdn

(** [draw_game fdn] will draw the phase that the player is currently in, using
    [fdn] as the background. *)
let draw_game fdn =
  let ps = unwrap_option !player_status in
  let es = unwrap_option !enemy_status in
  match !phase with
  | START -> draw_start ps es fdn;
  | MENU_SELECT -> draw_menu_select ps es fdn;
  | FIGHT_MENU -> draw_fight_menu ps es fdn;
  | INVENTORY_MENU -> draw_inventory_menu inventory;
  | HEALING_MENU -> ()
  | ATTEMPT_CATCH -> ()
  | POKEMON_MENU -> draw_pokemon_menu pokemons_lists;
  | ATTEMPT_RUN -> draw_run ps es fdn;
  | PLAYER_TURN -> ()
  | ENEMY_TURN -> ()
  | END -> ()


(**************)
(** UPDATERS **)
(**************)


(** [play_ding_and_update keys old_one new_one key] will play a menu noise and
    update [old_one] to become [new_one] if [key] is in [keys]. *)
let play_ding_and_update (keys : char list) (old_one : 'a ref) (new_one : 'a) 
    (key : char) =
  match List.mem key keys with
  | true ->
    start_sound (sfx_dir ^ "menunoise.wav");
    old_one := new_one
  | false -> ()

(** [pokemon_selector pokemon_no key pokemon_lst] will update the currently
    selected pokemon at position [pokemon_no] in [pokemon_lst] depending on the
    [key] pressed. *)
let pokemon_selector pokemon_no key pokemon_lst =
  let new_pokemon = match (pokemon_no, key) with
    | (n, 'w') -> if n = 0 then 0 else n-1
    | (n, 'a') -> 0
    | (n, 's') -> if n = 0 then List.length pokemon_lst else n+1
    | (n, 'd') -> if n = 0 then 1 else n
    | (n, 'e') -> 0
    | (_, 'r') -> phase := MENU_SELECT; 0
    | _ -> -1 in
  play_ding_and_update ['w';'s';'e';'r'] pokemon new_pokemon key

(** [item_selector item_no key inventory] will update the currently selected
    item at position [item_no] in [inventory] depending on the [key] pressed. *)
let item_selector item_no key inventory =
  let inv_length = List.length inventory.p_healing + 
                   List.length inventory.p_pokeball in
  let new_item = match (item_no, key) with
    | (n, 'w') -> if n = 0 then 0 else n-1
    | (n, 's') -> if n = inv_length then inv_length else n+1
    | (n, 'e') -> 
      if n < List.length inventory.p_healing then (phase := HEALING_MENU; n)
      else if n < inv_length then (phase := ATTEMPT_CATCH; n)
      else (phase := MENU_SELECT; 0)
    | (_, 'r') -> phase := MENU_SELECT; 0
    | _ -> !item in
  play_ding_and_update ['w';'s';'e';'r'] item new_item key

(** [move_selector mo key] will update the currently selected move at position 
    [m] depending on the [key] pressed. *)
let move_selector mo key =
  let new_move = match (mo, key) with
    | (0, 's') -> 2
    | (0, 'd') -> 1
    | (0, 'e') -> phase := PLAYER_TURN; 0
    | (1, 's') -> 3
    | (1, 'a') -> 0
    | (1, 'e') -> phase := PLAYER_TURN; 1
    | (2, 'w') -> 0
    | (2, 'd') -> 3
    | (2, 'e') -> phase := PLAYER_TURN; 2
    | (3, 'w') -> 1
    | (3, 'a') -> 2
    | (3, 'e') -> phase := PLAYER_TURN; 3
    | (_, 'r') -> phase := MENU_SELECT; 0
    | _ -> !move in
  play_ding_and_update ['w';'a';'s';'d';'e';'r'] move new_move key

(** [menu_selector me key] will update the currently selected menu at position 
    [me] and the pressed key [key].*)
let menu_selector me key =
  let new_menu = match (me, key) with
    | (Fight, 's') -> Pokemon
    | (Fight, 'd') -> Inventory
    | (Fight, 'e') -> phase := FIGHT_MENU; Fight
    | (Inventory, 's') -> Run
    | (Inventory, 'a') -> Fight
    | (Inventory, 'e') -> phase := INVENTORY_MENU; Inventory
    | (Pokemon, 'w') -> Fight
    | (Pokemon, 'd') -> Run
    | (Pokemon, 'e') -> phase := POKEMON_MENU; Pokemon
    | (Run, 'w') -> Inventory
    | (Run, 'a') -> Pokemon
    | (Run, 'e') -> phase := ATTEMPT_RUN; Run
    | _ -> !menu in
  play_ding_and_update ['w';'a';'s';'d';'e'] menu new_menu key


(************************)
(** GAME SETUP & LOGIC **)
(************************)


(** [handle_scaling key] handles the button press of [key] that will determine
    whether the screen size increases or decreases. *)
let handle_scaling key =
  let (x,y) = screen_dimensions in
  match key with
  | '+' -> scale_window x y (fun scale -> scale + 1) scaling
  | '-' -> scale_window x y (fun scale -> scale - 1) scaling
  | _ -> ()

(** [handle_press mp t_loc key] handles the button press of [key] and will
    appropriately change the menu we are on based on the current phase.
    Effects: Draws the state we are in using [fdn] as the background. *)
let handle_press fdn key =
  handle_scaling key;
  let () = match !phase with
    | MENU_SELECT -> menu_selector !menu key
    | FIGHT_MENU -> move_selector !move key
    | INVENTORY_MENU -> item_selector !item key inventory
    | POKEMON_MENU -> pokemon_selector !pokemon key pokemons_lists
    | _ -> () in
  draw_game fdn

let enter_battle_phase event fdn = 
  match !phase with
  | START -> () (* no button presses due to synchronous drawing *)
  | MENU_SELECT | FIGHT_MENU | INVENTORY_MENU | POKEMON_MENU -> 
    if event.keypressed then handle_press fdn event.key
  | HEALING_MENU -> ()
  | ATTEMPT_CATCH -> ()
  | ATTEMPT_RUN -> ()
  | PLAYER_TURN -> ()
  | ENEMY_TURN -> ()
  | END -> ()

(** [foundation_setup ()] sets up the background of the battle as a single image
    that will be used throughout a majority of the battle. *)
let foundation_setup () =
  let x, y = screen_dimensions in
  let old_scaling = !scaling in
  scaling := 1;
  Graphics.auto_synchronize false;
  prepare_draw battle_sprites_dir "grass_day_back" 0 message_bar_height;
  prepare_draw battle_sprites_dir "grass_day_field" 0 message_bar_height;
  prepare_draw battle_sprites_dir "message_bar" 0 0;
  let fdn = Rgba32 (Graphic_image.(get_image 0 0 x y |> Rgb24.to_rgba32)) in
  scaling := old_scaling;
  fdn

let choose_music () =
  Random.self_init ();
  let wild_prefix = if Random.int 1 = 0 then "wild_" else "" in
  Random.self_init ();
  wild_prefix ^ "battlemusic" ^ (Random.int 2 + 1 |> string_of_int) ^ ".mp3"

(** [setup_audio] opens the audio channel and plays background music. *)
let setup_audio () = 
  open_audio ~freq:22050 (); (** frequency retrieved by trial and error *)
  start_music (battle_music_dir ^ choose_music ());
  setvolume_music 0.5

(** [setup_graph] opens the graph based on the defined screen dimensions. 
    Effects: Opens the graph window and initializes the grid.*)
let setup_graph (foundation: Images.t) =
  let x, y = screen_dimensions in
  scale_window x y (fun scale -> scale) scaling;
  draw_game foundation

(** [setup_battle mp trainer_loc] initializes battlegui using [foundation] as
    its usual background. *)
let setup_battle (foundation: Images.t) =
  setup_audio ();
  setup_graph foundation

(** [initial_battle_setup ()] is a foundation that is used throughout the game
    to draw the battle background.
    Effects: Opens the graphics window and initializes the game graphics. *)
let initial_battle_setup () =
  let foundation = foundation_setup () in
  setup_battle foundation;
  foundation