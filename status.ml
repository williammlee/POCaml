open Template


(**********)
(* PLAYER *)
(**********)


type p_moveset = {p_mname: move_name; pp: int}
type p_pokemon = {p_pname: pokemon_name; p_level: int; p_curr_hp: int; 
                  p_curr_exp: int; p_attacking: bool; p_moveset: p_moveset list}
type p_healing = {p_hname: healing_name; p_pquantity: int}
type p_pokeball = {p_pbname: pokeball_name; p_pbquantity: int}
type p_inventory = {p_healing: p_healing list; p_pokeball: p_pokeball list}
type player = {p_pokemon: p_pokemon list; p_inventory: p_inventory; 
               battling: bool}
type p = player

let init_player_state (tem : Template.t) : p =
  let p_name = List.hd (starter_pokemon tem) in
  let p2_name = List.hd (pokemon_list tem) in
  let get_first (a,_,_)= a in
  let rec find_pokemon (pn: Template.pokemon_name) (pn_lst: Template.pokemon_name list): Template.pokemon_name = 
    match pn_lst with 
    | [] -> failwith "Cannot Find Pokemon"
    | h::t -> if h = pn then h else find_pokemon pn t in

  let rec construct_move_set (pn: Template.pokemon_name) 
      (move_lst: Template.move_name list)=
    match move_lst with 
    | [] -> []
    | h::t1 -> {p_mname = h; pp = get_first (move_stats tem h)} :: construct_move_set pn t1

  in
  let rec construct_heal heal_lst acc =
    begin match heal_lst with
      | [] -> acc
      | h::t -> 
        construct_heal t ({p_hname = h; 
                           p_pquantity = starter_healing_quantity tem h} :: acc) 
    end in
  let rec construct_pokeball ball_lst acc =
    begin match ball_lst with
      | [] -> acc
      | h::t -> 
        construct_pokeball t ({p_pbname = h; 
                               p_pbquantity = starter_pokeball_quantity tem h} 
                              :: acc) 
    end in
  {
    p_pokemon = [{p_pname = p_name; p_level = 1; 
                  p_curr_hp = get_first (pokemon_stats tem p_name); 
                  p_curr_exp = 0; p_attacking = true; 
                  p_moveset = construct_move_set (find_pokemon p_name (pokemon_list tem)) (pokemon_moves tem p_name)};
                 {p_pname = p2_name; p_level = 1; 
                  p_curr_hp = get_first (pokemon_stats tem p2_name); 
                  p_curr_exp = 0; p_attacking = false; 
                  p_moveset = construct_move_set (find_pokemon p2_name 
                                                    (pokemon_list tem)) 
                      (pokemon_moves tem p2_name)}
                ];

    p_inventory = {p_healing = construct_heal (starter_healings tem) [] ; 
                   p_pokeball = construct_pokeball (starter_pokeballs tem) []};
    battling = false
  }

let player_pokemon (ps : p) : pokemon_name list =
  let rec helper pokemon acc =
    match pokemon with 
    | [] -> acc
    | h :: t -> helper t (h.p_pname::acc) in
  helper ps.p_pokemon []

let player_pokemon_level (ps : p) (pn : pokemon_name) : int =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.p_pname = pn then h.p_level else helper pn t in
  helper pn ps.p_pokemon

let player_pokemon_hp (ps : p) (pn : pokemon_name) : int =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.p_pname = pn then h.p_curr_hp else helper pn t in
  helper pn ps.p_pokemon

let player_pokemon_exp (ps : p) (pn : pokemon_name) : int =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.p_pname = pn then h.p_curr_exp else helper pn t in
  helper pn ps.p_pokemon

let player_pokemon_fighting (ps : p) (pn : pokemon_name) : bool =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.p_pname = pn then h.p_attacking else helper pn t in
  helper pn ps.p_pokemon

let player_pokemon_fighting_name (ps : p) : string =
  let rec helper ps pn_lst =
    match pn_lst with
    | [] -> failwith "none battling"
    | h :: t -> if h.p_attacking then h.p_pname else helper ps t in
  helper ps ps.p_pokemon

(* let pokemon_fighting (ps: p) : bool list = 
   let rec helper pn_lst acc = 
    match pn_list with 
    |[] -> acc
    |h :: t -> helper t ((player_pokemon_fighting h) :: acc) in 
   helper ps.p_pokemon []  *)

let player_pokemon_moveset (ps : p) (pn : pokemon_name) : move_name list =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.p_pname = pn then h.p_moveset else helper pn t in
  let move_set = helper pn ps.p_pokemon in
  let rec create_move_name ms acc=
    match ms with 
    | [] -> acc
    | h::t -> create_move_name t (h.p_mname::acc) in
  create_move_name move_set []

let player_pokemon_move_pp (ps : p) (pn : pokemon_name) (m : move_name) : int =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.p_pname = pn then h.p_moveset else helper pn t in
  let move_set = helper pn ps.p_pokemon in
  let rec get_pp move ms_lst = 
    match ms_lst with 
    | [] -> raise (UnknownMove move)
    | h::t -> if h.p_mname = move then h.pp else get_pp move t in
  get_pp m move_set

let player_healing (ps : p) : healing_name list =
  let rec helper h_lst acc = 
    match h_lst with 
    | [] -> acc
    | h::t -> helper t (h.p_hname::acc) in
  helper ps.p_inventory.p_healing []

let player_healing_quantity (ps : p) (h : healing_name) : int =
  let rec helper h_lst heal= 
    match h_lst with 
    | [] -> raise (UnknownHealing h)
    | h::t -> if h.p_hname = heal then h.p_pquantity else helper t heal in
  helper ps.p_inventory.p_healing h

let player_pokeball (ps : p) : pokeball_name list =
  let rec helper p_lst acc = 
    match p_lst with 
    | [] -> acc
    | h::t -> helper t (h.p_pbname::acc) in
  helper ps.p_inventory.p_pokeball []

let player_pokeball_quantity (ps : p) (pb : pokeball_name) : int =
  let rec helper p_lst ball= 
    match p_lst with 
    | [] -> raise (UnknownPokeball pb)
    | h::t -> if h.p_pbname = ball then h.p_pbquantity else helper t ball in
  helper ps.p_inventory.p_pokeball pb

let player_battling (ps : p) : bool =
  ps.battling

let switch_battling (ps : p): p =
  {
    p_pokemon = ps.p_pokemon;
    p_inventory = ps.p_inventory;
    battling = not ps.battling;
  }


(*********)
(* ENEMY *)
(*********)


type e_pokemon = {e_pname: pokemon_name; e_level: int; e_curr_hp: int; 
                  e_attacking: bool; e_moveset: move_name list}
type e_healing = {e_hname: healing_name; e_pquantity: int}
type enemy = {e_pokemon: e_pokemon list; inventory: e_healing list;
              wild: bool; is_rival:bool}
type e = enemy

let init_enemy_state (tem : Template.t) (wild : bool) (is_rival:bool): e =
  let p_name = List.hd (List.rev (pokemon_list tem)) in
  let get_hp (a,_,_)= a in

  let rec construct_heal heal_lst acc =
    begin match heal_lst with
      | [] -> acc
      | h::t -> 
        construct_heal t ({e_hname = h; 
                           e_pquantity = starter_healing_quantity tem h} :: acc) 
    end in
  {
    e_pokemon = [{e_pname = p_name; 
                  e_level = 3; 
                  e_curr_hp = get_hp (pokemon_stats tem p_name); 
                  e_attacking = true; 
                  e_moveset = pokemon_moves tem p_name}];
    inventory = construct_heal (starter_healings tem) [];
    wild = wild;
    is_rival = false
  }

let init_enemy_state_2 (tem : Template.t) (wild : bool) (is_rival:bool): e =
  let p_name = List.hd (pokemon_list tem) in
  let p_name_2 = List.hd (List.rev (pokemon_list tem)) in 
  let get_hp (a,_,_)= a in

  let rec construct_heal heal_lst acc =
    begin match heal_lst with
      | [] -> acc
      | h::t -> 
        construct_heal t ({e_hname = h; 
                           e_pquantity = starter_healing_quantity tem h} :: acc) 
    end in
  {
    e_pokemon = [{e_pname = p_name; 
                  e_level = 3; 
                  e_curr_hp = get_hp (pokemon_stats tem p_name); 
                  e_attacking = true; 
                  e_moveset = pokemon_moves tem p_name};
                 {e_pname = p_name_2;
                  e_level = 5;
                  e_curr_hp = get_hp (pokemon_stats tem p_name_2); 
                  e_attacking = false; 
                  e_moveset = pokemon_moves tem p_name_2};
                ];
    inventory = construct_heal (starter_healings tem) [];
    wild = wild;
    is_rival = false
  }

let enemy_pokemon (es : e) : pokemon_name list =
  let rec helper pokemon acc =
    match pokemon with 
    | [] -> acc
    | h :: t -> helper t (h.e_pname::acc) in
  helper es.e_pokemon []

let enemy_pokemon_level (es : e) (pn : pokemon_name) : int =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.e_pname = pn then h.e_level else helper pn t in
  helper pn es.e_pokemon

let enemy_pokemon_hp (es : e) (pn : pokemon_name) : int =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.e_pname = pn then h.e_curr_hp else helper pn t in
  helper pn es.e_pokemon

let enemy_pokemon_fighting (es : e) (pn : pokemon_name) : bool =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.e_pname = pn then h.e_attacking else helper pn t in
  helper pn es.e_pokemon

let enemy_pokemon_fighting_name (es : e) : string =
  let rec helper es pn_lst =
    match pn_lst with
    | [] -> failwith "none battling"
    | h :: t -> if h.e_attacking then h.e_pname else helper es t in
  helper es es.e_pokemon

let enemy_pokemon_moveset (es : e) (pn : pokemon_name) : move_name list =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.e_pname = pn then h.e_moveset else helper pn t in
  helper pn es.e_pokemon 

let enemy_wild (es : e) : bool =
  es.wild

let enemy_rival (es: e) : bool =
  es.is_rival

let enemy_healing_quantity (es : e) (h : healing_name) : int =
  let rec helper h_lst heal= 
    match h_lst with 
    | [] -> raise (UnknownHealing h)
    | h::t -> if h.e_hname = heal then h.e_pquantity else helper t heal in
  helper es.inventory h

let enemy_pokemon_level (es : e) (pn : pokemon_name) : int =
  let rec helper pn pn_lst =
    match pn_lst with
    | [] -> raise (UnknownPokemon pn)
    | h :: t -> if h.e_pname = pn then h.e_level else helper pn t in
  helper pn es.e_pokemon


(****************)
(* LEGAL CHECKS *)
(****************)


type result = Legal of p * e | Illegal


(***********)
(*  FIGHT  *)
(***********)


type elemtype = 
  | Normal 
  | Fire
  | Water
  | Grass

type effective =
  | Super
  | Regular
  | Not

let type_converter (t : type_name) : elemtype =
  match t with
  | "Normal" -> Normal
  | "Fire" -> Fire
  | "Water" -> Water
  | "Grass" -> Grass
  | _ -> raise (UnknownType t)

let type_effectiveness (et1 : elemtype) (et2 : elemtype) : effective =
  match et1 with
  | Normal -> Regular
  | Fire -> if et2 = Grass then Super
    else if List.mem et2 [Fire;Water] then Not 
    else Regular
  | Water -> if et2 = Fire then Super
    else if List.mem et2 [Water;Grass] then Not
    else Regular
  | Grass -> if et2 = Water then Super
    else if List.mem et2 [Grass;Fire] then Not
    else Regular

let effective_multiplier (eff: effective) : int =
  match eff with
  | Super -> 3
  | Regular -> 2
  | Not -> 1

let multiplied_stats (tem : Template.t) (pn : pokemon_name) (lvl : int) : 
  (int*int*int) =
  let (hp,atk,def) = Template.pokemon_stats tem pn in
  (* The final health should be double the base health + 110 *)
  let hp_increase = ((hp * lvl) / 100) + 
                    (110 * (lvl - 1) / 99) in
  (* The final attack/defense should be double that base stat + 5 *)
  let atk_increase = ((atk * lvl / 100)) + (5 * lvl / 100) in
  let def_increase = ((def * lvl / 100)) + (5 * lvl / 100) in
  (hp + hp_increase, atk + atk_increase, def + def_increase)

(** [chance prob] is true if a random float generated falls below probability
    [prob]. *)
let chance prob : bool =
  Random.self_init ();
  Random.float 1.0 < prob

let damage_calculation (tem : Template.t) (ps : p) (es : e) 
    (m : move_name) (is_player : bool) : int =
  let (pp,power,acc) = Template.move_stats tem m in
  if chance acc then
    let p_fighter = List.find (fun x -> player_pokemon_fighting ps x) 
        (player_pokemon ps) in 
    let e_fighter = List.find (fun x -> enemy_pokemon_fighting es x) 
        (enemy_pokemon es) in
    let move_type = move_type tem m |> type_converter in
    match is_player with
    | true -> (* player damaging enemy*)
      let (_,p_atk,_) = p_fighter |> player_pokemon_level ps |> 
                        multiplied_stats tem p_fighter in
      let (_,_,e_def) = e_fighter |> enemy_pokemon_level es |> 
                        multiplied_stats tem e_fighter in
      let e_ptype = (e_fighter |> pokemon_type tem |> type_converter) in
      let type_multiplier = type_effectiveness move_type e_ptype |> 
                            effective_multiplier in
      (power + p_atk/e_def) * type_multiplier 
    | false -> (* enemy damaging player*)
      let (_,e_atk,_) = e_fighter |> enemy_pokemon_level es |> 
                        multiplied_stats tem e_fighter in
      let (_,_,p_def) = p_fighter |> player_pokemon_level ps |> 
                        multiplied_stats tem p_fighter in
      let p_ptype = (p_fighter |> pokemon_type tem |> type_converter) in
      let type_multiplier = type_effectiveness move_type p_ptype |> 
                            effective_multiplier in
      (power + e_atk/p_def) * type_multiplier
  else 0 (* move missed *)

(** [find_and_replace ps pn lst] is the updated list of the player's Pokemon 
    [lst] in the player state[ps] after a component in 
    Pokemon [pn] has been updated *) 
let find_and_replace ps pn lst = 
  let rec helper state pn list acc =
    match list with
    | [] -> acc
    | h::t -> if h.p_pname = pn.p_pname then helper ps pn t (pn :: acc) 
      else helper ps pn t (h::acc) in
  helper ps pn lst []

(** [find_and_replace es pn lst] is the updated list of the enemy's Pokemon 
    [lst] in the enemy state [es] after a component in the Pokemon 
    [pn] has been updated *) 
let find_and_replace_enemy es pn lst = 
  let rec helper state pn list acc =
    match list with
    | [] -> acc
    | h::t -> if h.e_pname = pn.e_pname then helper es pn t (pn :: acc) 
      else helper es pn t (h::acc) in
  helper es pn lst []

(** [find_and_replace_pokeball ps pb lst] is the updated list of the player's 
    Pokeballs [lst] in the player state [ps] after a component in the Pokeball
    [pb] has been updated *) 
let find_and_replace_pokeball ps pb lst  = 
  let rec helper state ball list acc= 
    match list with 
    | [] -> acc
    | h::t -> if h.p_pbname = pb.p_pbname then helper ps pb t (pb :: acc) 
      else helper ps pb t (h::acc) in 
  helper ps pb lst []

(** [find_and_replace_item item lst] is the updated list of the player's 
    healing items [lst] in the player state [ps] after a component in the 
    Healing item [item] has been updated *) 
let find_and_replace_item item lst =
  let rec helper itm list acc =
    match list with 
    | [] -> acc
    | h::t -> if h.p_hname = itm.p_hname then helper itm t (itm :: acc) 
      else helper itm t (h::acc) in
  helper item lst []

(** [find_and_replace_item_enemy item lst] is the updated list of the player's 
    healing items [lst] in the enemy state [es] after a component in the Healing
     item [item] has been updated *) 
let find_and_replace_item_enemy item lst =
  let rec helper itm list acc =
    match list with 
    | [] -> acc
    | h::t -> if h.e_hname = itm.e_hname then helper itm t (itm :: acc) 
      else helper itm t (h::acc) in
  helper item lst []

let use_move (tem: Template.t) (m : move_name) (ps : p) (es : e) 
    (is_player: bool): result =
  let p_fighter = List.find (fun x -> player_pokemon_fighting ps x) 
      (player_pokemon ps) in 
  let e_fighter = List.find (fun x -> enemy_pokemon_fighting es x) 
      (enemy_pokemon es) in
  match is_player with 
  | true ->
    if player_pokemon_move_pp ps p_fighter m = 0 then Illegal else
      let rec construct_move_set state move move_lst acc=
        begin match move_lst with 
          | [] -> acc
          | h::t -> if h.p_mname <> move then construct_move_set state move t 
                ({p_mname = h.p_mname; 
                  pp = player_pokemon_move_pp state p_fighter m} :: acc)else 
              construct_move_set state move t 
                ({p_mname = h.p_mname; pp = 
                                         (player_pokemon_move_pp state 
                                            p_fighter m) - 1} :: acc) 
        end in
      let rec reconstruct_pokemon state pn lst tem= 
        match lst with 
        | [] -> raise (UnknownPokemon pn)
        | h::t -> if h.p_pname <> pn then reconstruct_pokemon state pn t tem 
          else
            {p_pname = pn; 
             p_level = h.p_level; 
             p_curr_hp = h.p_curr_hp; 
             p_curr_exp = h.p_curr_exp; 
             p_attacking = h.p_attacking; 
             p_moveset = construct_move_set state m (h.p_moveset) []} in

      let new_pokemon = reconstruct_pokemon ps p_fighter ps.p_pokemon tem in 

      let pokemon_lst = find_and_replace ps new_pokemon ps.p_pokemon in

      let health_logic hp =
        let calculate_enemy_hp = hp - damage_calculation tem ps es m true in
        if calculate_enemy_hp <= 0 then 0 else calculate_enemy_hp  in 

      let rec reconstruct_enemy_pokemon en lst tem : e_pokemon= 
        match lst with 
        | [] -> raise (UnknownPokemon en)
        | h::t -> if h.e_pname <> en then reconstruct_enemy_pokemon en t tem
          else
            {e_pname = en; 
             e_level = h.e_level; 
             e_curr_hp = health_logic h.e_curr_hp;  
             e_attacking = h.e_attacking; 
             e_moveset = h.e_moveset} in

      let new_enemy = reconstruct_enemy_pokemon e_fighter es.e_pokemon tem in

      let enemy_lst = find_and_replace_enemy es new_enemy es.e_pokemon in

      let player = ({p_pokemon = List.rev pokemon_lst; 
                     p_inventory= ps.p_inventory;
                     battling= ps.battling}) in 

      let enemy = ({e_pokemon = enemy_lst; 
                    inventory= es.inventory;
                    wild= es.wild;
                    is_rival = es.is_rival}) in 

      Legal (player, enemy)

  | false -> 
    let health_logic hp =
      let calculate_hp = hp - damage_calculation tem ps es m false in
      if calculate_hp <= 0 then 0 else calculate_hp in 

    let rec reconstruct_player_pokemon state en lst : p_pokemon= 
      match lst with 
      | [] -> raise (UnknownPokemon en)
      | h::t -> if h.p_pname <> en then reconstruct_player_pokemon state en t 
        else
          {p_pname = en; 
           p_level = h.p_level; 
           p_curr_hp = health_logic h.p_curr_hp;
           p_curr_exp = h.p_curr_exp;   
           p_attacking = h.p_attacking; 
           p_moveset = h.p_moveset} in

    let new_player = reconstruct_player_pokemon ps p_fighter ps.p_pokemon in


    let player_lst = find_and_replace ps new_player ps.p_pokemon in

    let player = ({p_pokemon = player_lst; 
                   p_inventory= ps.p_inventory;
                   battling= ps.battling}) in 

    let enemy = ({e_pokemon = es.e_pokemon; 
                  inventory= es.inventory;
                  wild= es.wild;
                  is_rival = es.is_rival}) in 

    Legal (player, enemy)


(*********)
(*  BAG  *)
(*********)


(** [split_hp (x,y,z)] returns the first element of a triple which is also the
    health of a Pokemon in this case *)
let split_hp (x,y,z) = x 

let enemy_heal (e : e) (tem : Template.t) (lvl : int): bool =
  let rec helper poke_list =
    match poke_list with 
    |[] -> false
    |h :: t -> if h.e_attacking 
               && split_hp (multiplied_stats tem h.e_pname h.e_level) /
                  h.e_curr_hp > 4  
      then (chance 0.5) 
      else helper t in 
  helper e.e_pokemon

let heal_calculation (tem : Template.t) (p : p) (e : e) 
    (h : healing_name) (is_player : bool) (pn:pokemon_name): int =
  let heal_amount = healing_amount tem h in 
  if is_player then 
    let rec p_helper poke_list =
      match poke_list with
      |[] -> raise (UnknownHealing h) 
      |h :: t -> if h.p_pname = pn then 
          if h.p_curr_hp + heal_amount > split_hp
               (multiplied_stats tem h.p_pname h.p_level) then 
            split_hp(multiplied_stats tem h.p_pname h.p_level)
          else
            h.p_curr_hp + heal_amount
        else
          p_helper t in 
    p_helper p.p_pokemon
  else
    let rec e_helper poke_list = 
      match poke_list with 
      |[] -> raise (UnknownHealing h)
      |h::t -> if h.e_attacking then 
          if h.e_curr_hp + heal_amount > 
             split_hp(multiplied_stats tem h.e_pname h.e_level) 
          then split_hp(multiplied_stats tem h.e_pname h.e_level)
          else
            h.e_curr_hp + heal_amount
        else
          e_helper t in
    e_helper e.e_pokemon

let use_healing (tem : Template.t) (pn : pokemon_name) (h : healing_name) 
    (ps : p) (es : e) (is_player : bool) : result =
  let heal_amount = heal_calculation tem ps es h is_player pn in 
  if is_player then 
    let rec reconstruct_pokemon_p state pn poke_list = 
      match poke_list with 
      |[] -> raise (UnknownPokemon pn)
      |h :: t -> if h.p_pname = pn then 
          {p_pname = pn;
           p_level = h.p_level;
           p_curr_hp = heal_amount;
           p_curr_exp = h.p_curr_exp;
           p_attacking = h.p_attacking;
           p_moveset = h.p_moveset;
          }
        else reconstruct_pokemon_p state pn t in 
    let new_pokemon_p = reconstruct_pokemon_p ps pn ps.p_pokemon in 
    let rec reconstruct_item_p state h1 inventory = 
      match inventory with 
      |[] -> raise (UnknownHealing h)
      |h :: t -> if h.p_hname = h1 then 
          {p_hname = h.p_hname;
           p_pquantity = h.p_pquantity - 1;
          }
        else reconstruct_item_p state h1 t in 
    let new_item_p = reconstruct_item_p ps h ps.p_inventory.p_healing in 
    let pokemon_lst = find_and_replace ps new_pokemon_p ps.p_pokemon in 
    let item_lst = find_and_replace_item new_item_p ps.p_inventory.p_healing in 
    Legal({
        p_pokemon = List.rev pokemon_lst;
        p_inventory = {p_healing = item_lst; 
                       p_pokeball = ps.p_inventory.p_pokeball};
        battling = ps.battling
      }, es)
  else 
    let rec reconstruct_pokemon_e state pn poke_list = 
      match poke_list with 
      |[] -> raise (UnknownPokemon pn)
      |h :: t -> if h.e_pname = pn then 
          {e_pname = pn;
           e_level = h.e_level;
           e_curr_hp = heal_amount;
           e_attacking = h.e_attacking;
           e_moveset = h.e_moveset;
          }
        else reconstruct_pokemon_e state pn t in 
    let new_pokemon_e = reconstruct_pokemon_e ps pn es.e_pokemon in 
    let rec reconstruct_item_e state hn inventory = 
      match inventory with 
      |[] -> raise (UnknownHealing hn)
      |h :: t -> if h.e_hname = hn then 
          {e_hname = h.e_hname;
           e_pquantity = h.e_pquantity - 1;
          }
        else reconstruct_item_e state hn t in 
    let new_item_e = reconstruct_item_e ps h es.inventory in 
    let pokemon_lst = find_and_replace_enemy es new_pokemon_e es.e_pokemon in  
    let item_lst = find_and_replace_item_enemy new_item_e es.inventory in 
    Legal(ps, {e_pokemon = List.rev pokemon_lst;
               inventory = item_lst; 
               wild = es.wild;
               is_rival = es.is_rival})

let did_catch (tem : Template.t) (e : e) (pb : pokeball_name) : bool =
  if e.wild = false then false
  else 
    let catch_rate = pokeball_catch tem pb in 
    chance catch_rate

let use_pokeball (tem : Template.t) (ps : p) (pb : pokeball_name) (es : e) : 
  (result * bool) =
  if es.wild = false || player_pokeball_quantity ps pb <= 0 
  then (Illegal, false)
  else 
    let rec construct_pokeball ball_lst pb =
      begin match ball_lst with
        | [] -> raise (UnknownPokeball pb)
        | h::t -> if h.p_pbname = pb then {p_pbname = h.p_pbname; 
                                           p_pbquantity = h.p_pbquantity - 1} 
          else
            construct_pokeball t pb
      end in
    let new_pokeball = construct_pokeball ps.p_inventory.p_pokeball pb in
    let new_pokeball_list = find_and_replace_pokeball ps new_pokeball 
        ps.p_inventory.p_pokeball in
    let caught = did_catch tem es pb in
    if caught then 
      let wild_pokemon = List.nth es.e_pokemon 0 in
      let get_hp (a,_,_)= a in
      let rec construct_move_set move_lst acc =
        begin match move_lst with 
          | [] -> acc
          | h::t -> 
            construct_move_set t ({p_mname = h; 
                                   pp = get_hp (move_stats tem h)} :: acc)
        end
      in 
      let new_pokemon = {
        p_pname = wild_pokemon.e_pname;
        p_level = wild_pokemon.e_level;
        p_curr_hp = wild_pokemon.e_curr_hp;
        p_curr_exp = 0;
        p_attacking = false;
        p_moveset = construct_move_set wild_pokemon.e_moveset [];
      } in 
      let enemy_poke = List.nth es.e_pokemon 0 in 
      let new_enemy_poke = {e_pname = enemy_poke.e_pname; 
                            e_level = enemy_poke.e_level;
                            e_curr_hp = enemy_poke.e_curr_hp; 
                            e_attacking = false; 
                            e_moveset = enemy_poke.e_moveset} in 
      let poke_list = find_and_replace_enemy es new_enemy_poke es.e_pokemon in 
      (Legal ({p_pokemon = List.rev (new_pokemon :: ps.p_pokemon);
               p_inventory = {p_healing = ps.p_inventory.p_healing;
                              p_pokeball = new_pokeball_list};
               battling = false;
              }
             ,{e_pokemon = poke_list;
               inventory = es.inventory;
               wild = es.wild;
               is_rival = es.is_rival}),caught)
    else
      (Legal({p_pokemon = List.rev ps.p_pokemon;
              p_inventory = {p_healing = ps.p_inventory.p_healing;
                             p_pokeball = new_pokeball_list};
              battling = true;
             },es),caught)

(***********)
(* POKEMON *)
(***********)


let can_switch (p : p) (pn : pokemon_name) : bool =
  let rec helper poke_list pn = 
    match poke_list with
    |[] -> raise (UnknownPokemon pn)
    |h :: t -> if h.p_pname = pn && h.p_curr_hp <> 0 
      then true else if (h.p_pname = pn || h.p_pname = "None") 
                     && h.p_curr_hp = 0 
      then false else helper t pn in 
  helper p.p_pokemon pn  

let can_switch_enemy (es : e) (pn : pokemon_name) : bool =
  let rec helper poke_list pn = 
    match poke_list with
    |[] -> raise (UnknownPokemon pn)
    |h :: t -> if h.e_pname = pn && h.e_curr_hp <> 0 
      then true else if (h.e_pname = pn || h.e_pname = "None") 
                     && h.e_curr_hp = 0 
      then false else helper t pn in 
  helper es.e_pokemon pn  

let use_pokemon (ps : p) (pn : pokemon_name) (es:e) : result =
  if can_switch ps pn then 
    let rec reconstruct_pokemon_in state pn poke_list = 
      begin
        match poke_list with 
        |[] -> raise (UnknownPokemon pn)
        |h :: t -> if h.p_pname = pn then
            {p_pname = pn;
             p_level = h.p_level;
             p_curr_hp = h.p_curr_hp;
             p_curr_exp = h.p_curr_exp;
             p_attacking = true;
             p_moveset = h.p_moveset;
            }
          else reconstruct_pokemon_in state pn t
      end
    in 
    let rec reconstruct_pokemon_out state poke_list =
      begin 
        match poke_list with 
        |[] -> raise (UnknownPokemon pn)
        |h :: t -> if h.p_attacking = true then 
            {p_pname = h.p_pname;
             p_level = h.p_level;
             p_curr_hp = h.p_curr_hp;
             p_curr_exp = h.p_curr_exp;
             p_attacking = false;
             p_moveset = h.p_moveset;
            }
          else reconstruct_pokemon_out state t
      end
    in 
    let new_pokemon_out = reconstruct_pokemon_out ps ps.p_pokemon in 
    let new_pokemon_in = reconstruct_pokemon_in ps pn ps.p_pokemon in 

    print_endline ("Good job " ^ new_pokemon_out.p_pname ^ "!");
    print_endline ("Go get'em " ^ new_pokemon_in.p_pname ^"!");

    let pokemon_lst = find_and_replace ps new_pokemon_out ps.p_pokemon in 
    let pokemon_lst_2 = find_and_replace ps new_pokemon_in pokemon_lst in 
    Legal ({p_pokemon = List.rev pokemon_lst_2;
            p_inventory = ps.p_inventory;
            battling = ps.battling;
           }, es) else Illegal

let enemy_switch (ps:p) (pn:pokemon_name) (es:e) : result =
  if can_switch_enemy es pn then
    let rec reconstruct_pokemon_in state pn poke_list = 
      begin
        match poke_list with 
        |[] -> raise (UnknownPokemon pn)
        |h :: t -> if h.e_pname = pn then
            {e_pname = pn;
             e_level = h.e_level;
             e_curr_hp = h.e_curr_hp;
             e_attacking = true;
             e_moveset = h.e_moveset;
            }
          else reconstruct_pokemon_in state pn t
      end
    in 
    let rec reconstruct_pokemon_out state poke_list =
      begin 
        match poke_list with 
        |[] -> raise (UnknownPokemon pn)
        |h :: t -> if h.e_attacking = true then 
            {e_pname = h.e_pname;
             e_level = h.e_level;
             e_curr_hp = h.e_curr_hp;
             e_attacking = false;
             e_moveset = h.e_moveset;
            }
          else reconstruct_pokemon_out state t
      end
    in 
    let new_pokemon_out = reconstruct_pokemon_out es es.e_pokemon in 
    let new_pokemon_in = reconstruct_pokemon_in es pn es.e_pokemon in 

    let pokemon_lst = find_and_replace_enemy es new_pokemon_out es.e_pokemon in 
    let pokemon_lst_2 = find_and_replace_enemy es new_pokemon_in pokemon_lst in 
    Legal (ps, {
        e_pokemon = pokemon_lst_2;
        inventory = es.inventory;
        wild = es.wild;
        is_rival = es.is_rival;
      }) else Illegal


(*********)
(*  RUN  *)
(*********)


let did_run (e : e) : bool =
  e.wild

let run (ps : p) (es : e) : result =
  if ps.battling && es.wild then 
    let enemy_poke = List.nth es.e_pokemon 0 in 
    let new_enemy_poke = {e_pname = enemy_poke.e_pname; 
                          e_level = enemy_poke.e_level;
                          e_curr_hp = enemy_poke.e_curr_hp; 
                          e_attacking = false; 
                          e_moveset = enemy_poke.e_moveset} in 
    let poke_list = find_and_replace_enemy es new_enemy_poke es.e_pokemon in 
    Legal({p_pokemon = ps.p_pokemon; 
           p_inventory = ps.p_inventory;
           battling = false}, 
          {e_pokemon = poke_list; 
           inventory = es.inventory; 
           wild = es.wild;
           is_rival = es.is_rival}) 
  else Illegal


(*****************)
(*  Poke Center  *)
(*****************)


let pokeCenter tem (ps:p) (es:e): result =
  let rec helper tem lst acc = 
    begin
      match lst with
      |[] -> acc
      |h::t -> {p_pname = h.p_pname;
                p_level = h.p_level;
                p_curr_hp = split_hp(multiplied_stats tem h.p_pname h.p_level);
                p_curr_exp = h.p_curr_exp;
                p_attacking = false;
                p_moveset = h.p_moveset;
               } :: helper tem t acc 
    end in 
  let team = helper tem ps.p_pokemon [] in 
  Legal({p_pokemon = team; 
         p_inventory = ps.p_inventory;
         battling = false}, 
        {e_pokemon = es.e_pokemon; 
         inventory = es.inventory; 
         wild = es.wild;
         is_rival = es.is_rival})