open Action
open Yojson.Basic
open Status
open Template

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

(** [convert_to_string_display str_lst] converts a string list [str_lst] 
    to a string*) 
let convert_to_string_display (str_lst : string list) : string = 
  List.fold_left (fun a b -> if a <> "" then a^", "^b else a ^" " ^b) "" 
    str_lst |> String.trim

let convert_to_string (str_lst : string list) : string = 
  List.fold_left (fun a b -> a^" "^b ) "" str_lst |> String.trim
(** [find_active_pokemon player] is the name of the [player]'s pokemon 
    that is currently fighting*)
let rec find_active_pokemon (player : Status.p) : pokemon_name = 
  let p_pokemons = player_pokemon player in
  List.find (fun pokemon -> player_pokemon_fighting player pokemon ) p_pokemons

(** [find_active_pokemon_enemy enemy] is the name of the [enemy]'s pokemon 
    that is currently fighting*)
let rec find_active_pokemon_enemy (enemy : Status.e) = 
  let e_pokemons = enemy_pokemon enemy in
  List.find (fun pokemon -> enemy_pokemon_fighting enemy pokemon ) e_pokemons

(** [pick_move move_set moves] is the name of a randomly selected move in the
    enemy pokemon's moveset [move_name] using a 
    randomly generated number based on the number of moves 
    [moves] in the moveset *)
let pick_move (move_set : move_name list) (moves : int) : move_name =
  Random.self_init ();
  let chosen_move_num = Random.int moves in 
  List.nth move_set chosen_move_num

(** [do_enemy_turn tem player enemy] is a tuple of the state the game is in 
    after the enemy [enemy] attacks or heals and the name of the attack or 
    potion used*)
let do_enemy_turn (tem : Template.t) (player : Status.p) (enemy : Status.e) :
  (result * move_name) = 
  let active_pokemon = find_active_pokemon_enemy enemy in
  let enemy_level = enemy_pokemon_level enemy active_pokemon in 
  if (enemy_healing_quantity enemy "Super Potion") > 0 
  && enemy_heal enemy tem enemy_level && enemy_rival enemy then 
    (use_healing tem active_pokemon "Super Potion" player enemy 
       false,"Super Potion")
  else(
    let move_set = enemy_pokemon_moveset enemy active_pokemon in
    let num_moves = move_set |> List.length in 
    let chosen_move = pick_move move_set num_moves in
    (use_move tem chosen_move player enemy false, chosen_move))

(** [no_more_mons player] returns true if the player [player] has no more 
    available pokemon left to battle and false otherwise*)
let no_more_mons (player : Status.p): bool =
  let poke_list = player_pokemon player in
  let rec helper lst =
    match lst with
    |[] -> true
    |h::t -> if player_pokemon_hp player h <> 0 then false else helper t in 
  helper poke_list

(** [no_more_mons_enemy enemy] returns true if the enemy [enemy] has no more 
    available pokemon left to battle and false otherwise*)
let no_more_mons_enemy (enemy : Status.e): bool =
  let poke_list = enemy_pokemon enemy in 
  let rec helper lst = 
    match lst with
    |[] -> true 
    |h::t -> if enemy_pokemon_hp enemy h <> 0 then false else helper t in 
  helper poke_list

(** [next_mon enemy] is the next non fainted pokemon in the enemy's party*)
let next_mon (enemy : Status.e): string =
  let poke_list = enemy_pokemon enemy in
  let rec helper lst =
    match lst with
    |[] -> failwith "No more pokemon"
    |h::t -> if enemy_pokemon_hp enemy h <> 0 then h else helper t in 
  helper poke_list

(** [game_play tem player enemy fight_status pokemon_status 
    inventory_status enemy_turn] is the state the game is in by changing the 
    states of the [player] and [enemy] and using the commands from action.ml. 
    Specific actions can be used only if the player is in the fight menu 
    [fight_status], inventory menu [inventory_status], pokemon menu 
    [pokemon_status] and if it is the players or the enemy's turn [enemy_turn]*)
let rec game_play (tem : Template.t) (player : Status.p) (enemy : Status.e) 
    (fight_status : bool) (pokemon_status : bool) (inventory_status : bool)
    (enemy_turn : bool ) = 
  let init_e_state = init_enemy_state tem true true in
  let init_e_state_2 = init_enemy_state_2 tem false false in
  let p_pokemon = find_active_pokemon player in
  let phealth = player_pokemon_hp player p_pokemon in
  let in_fight = fight_status in 
  let in_pokemon = pokemon_status in
  let fainted = player_pokemon_hp player p_pokemon = 0 in 
  let in_inventory = inventory_status in
  let in_menu = fight_status || pokemon_status || inventory_status in
  let e_pokemon = find_active_pokemon_enemy enemy in
  let ehealth = enemy_pokemon_hp enemy e_pokemon in
  let e_wild = enemy |> enemy_wild in
  if not (player_battling player) then (
    print_endline"\nDo you want to fight a Wild pokemon or a Trainer?:";
    if read_line() = "Wild" then game_play tem (switch_battling player) 
        init_e_state false false false false
    else  game_play tem (switch_battling player) init_e_state_2 
        false false false false
  )
  else (
    if fainted && no_more_mons player then (
      (print_endline "\nYOU LOSE! TRY AGAIN"); switch_battling player
    )
    else (
      (* Enemy's Turn *)
      if enemy_turn && Status.enemy_pokemon_hp enemy e_pokemon > 0 then
        let get_move = fun (x,_) -> x in 
        let get_move_name = fun (_,y)  -> y in
        let enemy_move_string = get_move_name(do_enemy_turn tem player enemy) in
        (print_endline ("\nEnemy used " ^ enemy_move_string ^ "!"));
        let type_move = move_type tem enemy_move_string in 
        let get_pokemon_type = pokemon_type tem (find_active_pokemon player) |> 
                               type_converter in
        let get_move_type = type_converter type_move in 
        let how_effective = type_effectiveness get_move_type get_pokemon_type in 
        let effective_msg = function
          | Super -> "This move is Super Effective"
          | Regular -> ""
          | Not -> "This move is Not Very Effective" in
        let enemy_move = get_move (do_enemy_turn tem player enemy) in
        begin
          match enemy_move with 
          |Legal (p,e) -> if player_pokemon_hp player p_pokemon = 
                             player_pokemon_hp p p_pokemon then 
              (print_endline (e_pokemon ^ "'s attack missed!");
               (game_play tem p e in_fight in_pokemon in_inventory false)) else
              (print_endline (effective_msg how_effective);
               (game_play tem p e in_fight in_pokemon in_inventory false))
          |Illegal -> print_endline "\nCannot Move\n"; switch_battling player
        end
      else(
        if enemy_turn && not (no_more_mons_enemy enemy) then (
          let next_poke = next_mon enemy in
          let enemy_move = (enemy_switch player next_poke enemy) in 
          match enemy_move with
          |Legal (p,e) -> (print_endline ("\nEnemy sent out " ^ next_poke ^ 
                                          " !"));
            game_play tem p e in_fight in_pokemon in_inventory false
          |Illegal -> print_endline "\nCannot Move\n"; switch_battling player)
        else(
          (* Player's Turn *)
          if (Status.enemy_pokemon_hp enemy e_pokemon = 0 
              && no_more_mons_enemy enemy) then( 
            print_endline "\nYOU WIN!";
            game_play tem (switch_battling player) enemy false false false false)
          else try 
              let print_curr_p = print_endline 
                  ("\nCurrent Pokemon: " ^ p_pokemon) in
              let print_curr_p_health = print_endline 
                  ("     Current Pokemon Health: " ^ 
                   (phealth |> string_of_int)) in
              let print_curr_e = print_endline 
                  ("Current Enemy: " ^ e_pokemon) in
              let print_curr_e_health = print_endline 
                  ("     Current Enemy Health: " ^ 
                   (ehealth |> string_of_int)^"\n") in
              print_curr_p;
              print_curr_p_health;
              print_curr_e;
              print_curr_e_health;
              let get_pokemon = player_pokemon player |> 
                                convert_to_string_display  in 
              if fainted then 
                print_endline ("Your Pokemon has fainted! Choose your next 
                Pokemon!\n Commands: Switch ____" ^ "\n" ^ "Your Current Pokemon
                : " ^ get_pokemon);
              if not in_menu && not fainted then print_endline 
                  "Commands: Fight, Inventory, Pokemon, Run";
              if in_inventory then print_endline 
                  "Commands: Catch ____, Heal ____, Back";
              if in_fight then print_endline "Commands: Use ____, Back";
              if in_pokemon then print_endline "Commands: Switch ____, Back"; 
              match parse(read_line()) with 
              | Exit -> print_endline "\nYou are now exiting game.\n"; 
                switch_battling player
              | Fight -> if in_menu && not fainted
                then (print_endline "\nInvalid Action!\n"; 
                      game_play tem player enemy in_fight in_pokemon 
                        in_inventory false)
                else (
                  let get_moves = player_pokemon_moveset player p_pokemon 
                                  |> convert_to_string_display in 
                  print_endline("\n" ^ "Moves: " ^ get_moves);
                  game_play tem player enemy true in_pokemon in_inventory false)
              | Inventory -> if in_menu && not fainted
                then (print_endline "\nInvalid Action!\n"; 
                      game_play tem player enemy in_fight in_pokemon 
                        in_inventory false)
                else (
                  let get_healing = player_healing player 
                                    |> convert_to_string_display in 
                  print_endline("\n" ^ "Your Inventory: " ^ get_healing);
                  game_play tem player enemy in_fight in_pokemon true false)
              | Run -> if in_menu && not fainted then (
                  print_endline "\nInvalid Action!\n"; 
                  game_play tem player enemy in_fight in_pokemon in_inventory 
                    false) 
                else(
                  if e_wild 
                  then (print_endline "\nYou ran away!\n"; 
                        switch_battling player)
                  else (print_endline "\nYou can't run away from a 
                  trainer battle!\n";
                        game_play tem player enemy in_fight in_pokemon 
                          in_inventory false))
              | Use move -> if in_fight && not fainted then 
                  let name_move = move|>convert_to_string in
                  let type_move = move_type tem name_move in 
                  let get_pokemon_type = pokemon_type tem e_pokemon 
                                         |> type_converter in
                  let get_move_type = type_converter type_move in 
                  let how_effective = type_effectiveness get_move_type 
                      get_pokemon_type in 
                  let effective_msg = function
                    | Super -> "This move is Super Effective"
                    | Regular -> ""
                    | Not -> "This move is Not Very Effective" in
                  let used = use_move tem name_move player enemy true in
                  begin match used with
                    | Legal (p, e) -> 
                      if enemy_pokemon_hp enemy e_pokemon = enemy_pokemon_hp 
                           e e_pokemon then 
                        (print_endline (p_pokemon ^ "'s attack missed!") ;
                         (game_play tem p e false in_pokemon in_inventory true))
                      else
                        (print_endline (effective_msg how_effective);
                         (game_play tem p e false in_pokemon in_inventory true)) 
                    | Illegal -> print_endline "\nINVALID MOVE\n";
                      game_play tem player enemy in_fight in_pokemon 
                        in_inventory false end
                else (print_endline "\nInvalid Action!\n";game_play tem player 
                        enemy in_fight in_pokemon in_inventory false)
              | Pokemon -> if in_menu && not fainted
                then (print_endline "\nInvalid Action!\n"; game_play tem player
                        enemy in_fight in_pokemon in_inventory false) 
                else(
                  print_endline ("\n" ^ "Your Current Pokemon: " ^ get_pokemon);  
                  game_play tem player enemy in_fight true in_inventory false)
              | Catch b -> if in_inventory=true  && not fainted then (
                  if List.exists (fun x -> x = e_pokemon) 
                      (player_pokemon player) then( 
                    print_endline ("You can't catch this Pokemon! Your " ^ 
                                   e_pokemon ^ " will be jealous!");
                    game_play tem player enemy in_fight in_pokemon in_inventory 
                      false)
                  else (
                    let split_caught = fun (_,y) -> y in 
                    let split_result = fun (x,_) -> x in 
                    let catched = use_pokeball tem player (b 
                                                           |> convert_to_string) 
                        enemy in 
                    let caught = split_caught catched in 
                    let catch_result = split_result catched in 
                    begin match catch_result with 
                      | Legal (p, e) -> if (caught) then 
                          (print_endline "You caught the Pokemon!"; game_play 
                             tem p enemy false false false false )
                        else (game_play tem p e in_fight in_pokemon false true)
                      | Illegal -> print_endline "\nCANNOT CATCH\n";
                        game_play tem player enemy in_fight in_pokemon 
                          in_inventory false end))
                else (print_endline "\nInvalid Action!\n"; game_play tem player 
                        enemy in_fight in_pokemon in_inventory false)
              | Switch p -> if in_pokemon || fainted then (
                  if p|>convert_to_string = p_pokemon then 
                    (print_endline "\nCANNOT SWITCH\n";
                     game_play tem player enemy in_fight in_pokemon 
                       in_inventory false)
                  else (
                    let switched = use_pokemon player (p|>convert_to_string) 
                        enemy in 
                    begin match switched with 
                      | Legal (p, e) -> if not fainted then game_play tem p e 
                            in_fight false in_inventory true else game_play tem 
                            p e in_fight false in_inventory false
                      | Illegal -> print_endline "\nCANNOT SWITCH\n";
                        game_play tem player enemy in_fight in_pokemon 
                          in_inventory false end))
                else (print_endline "\nInvalid Action!\n"; game_play tem player 
                        enemy in_fight in_pokemon in_inventory false)
              | Back -> if not in_menu && not fainted then 
                  (print_endline "\nInvalid Action!\n"; 
                   game_play tem player enemy in_fight in_pokemon in_inventory 
                     false) 
                else(
                  if in_fight then game_play tem player enemy false in_pokemon 
                      in_inventory false
                  else if in_pokemon then game_play tem player enemy in_fight 
                      false in_inventory false
                  else game_play tem player enemy in_fight in_pokemon false 
                      false
                )
              |Heal h -> if in_inventory && not fainted then (
                  let mon = List.hd (List.rev h) in 
                  let healing = List.rev(List.tl(List.rev h)) |>
                                convert_to_string in
                  if player_healing_quantity player healing = 0 then (
                    print_endline("\nYou have no more " ^ healing^"s!");
                    game_play tem player enemy in_fight in_pokemon 
                      in_inventory false)

                  else(
                    if (is_revive tem healing && player_pokemon_hp player mon 
                                                 <> 0) 
                    || ((not (is_revive tem healing)) && player_pokemon_hp 
                          player mon = 0 )then( 
                      print_endline "\nCANNOT REVIVE";
                      game_play tem player enemy in_fight in_pokemon 
                        in_inventory false)
                    else(
                      let healed = use_healing tem mon healing player enemy 
                          true in 
                      begin match healed with
                        |Legal(p,e) -> game_play tem p e in_fight in_pokemon 
                                         false true
                        |Illegal -> print_endline "\nCANNOT HEAL\n";
                          game_play tem player enemy in_fight in_pokemon 
                            in_inventory false end)))
                else (print_endline "\nInvalid Action!\n"; game_play tem player
                        enemy in_fight in_pokemon in_inventory false)

            with 
            | exn -> let exn_string = Printexc.to_string exn in
              print_endline ("\nINVALID COMMAND - Exception: " ^ exn_string); 
              game_play tem player enemy in_fight in_pokemon in_inventory false 
        ))))

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let load_file = verify_file f in 
  let init_p_state = init_player_state load_file in
  let init_e_state = init_enemy_state load_file true true in
  game_play load_file init_p_state init_e_state false false false false 




(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name |> ignore

(* Execute the game engine. *)
let () = main ()