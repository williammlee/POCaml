open OUnit2
open Template
open Action
open Status

(*In template_tests, we basically tested all the functions that were in 
  template.ml in order to ensure that all the values were parsed correctly. 
  This is crucial because if our template.ml functions are not correct, then
  there will be significant errors with all the other functions in main.ml, 
  battletest.ml, and status.ml. We believe that our tests are sufficient because
  we also tested functions in action.ml and status.ml that did not require
  the change of states. For the functions that requires the change of states, 
  we manually tested them by playing the game ourselves in battletest.ml. These
  are the tests that are not explictly tested in this file because there was 
  no way we could have tested changes of states in a few lines of code.  *)
(*********************)
(** CUSTOM PRINTERS **)
(*********************)


(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_tuple_iif t] pretty-prints an int*int*float tuple [t]. *)
let pp_tuple_iif (x,y,z) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ "," ^ 
                           string_of_float z ^ ")"

(** [pp_tuple_iii t] pretty-prints an int*int*int tuple [t]. *)
let pp_tuple_iii (x,y,z) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ "," ^ 
                           string_of_int z ^ ")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h :: t ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h) ^ "; ") t
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"


(*********************)
(** TESTING HELPERS **)
(*********************)


(** [pokemon_list_test name tem expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [pokemon_list tem]. *)
let pokemon_list_test 
    (name : string) 
    (tem : Template.t) 
    (expected_output : pokemon_name list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pokemon_list tem) 
        ~printer:(pp_list pp_string)
    )

(** [pokemon_type_test name tem pn expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [pokemon_type tem pn]. *)
let pokemon_type_test 
    (name : string) 
    (tem : Template.t)
    (pn : pokemon_name)
    (expected_output : type_name) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pokemon_type tem pn) 
        ~printer: String.escaped
    )

(** [pokemon_type_exn_test name tem pn] constructs an OUnit test named [name] 
    that asserts that [UnknownPokemon pn] is raised by [pokemon_type tem pn]. *)
let pokemon_type_exn_test 
    (name : string) 
    (tem : Template.t)
    (pn : pokemon_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownPokemon pn) (fun () -> pokemon_type tem pn)
    )

(** [pokemon_moves_test name tem pn expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [pokemon_moves tem pn]. *)
let pokemon_moves_test 
    (name : string) 
    (tem : Template.t)
    (pn : pokemon_name)
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pokemon_moves tem pn) 
        ~printer: (pp_list pp_string))

(** [pokemon_moves_exn_test name tem pn expected_output] constructs an OUnit 
    test named [name] that asserts that [UnknownPokemon pn] is raised by
    [pokemon_moves tem pn]. *)
let pokemon_moves_exn_test 
    (name : string) 
    (tem : Template.t)
    (pn : pokemon_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownPokemon pn) (fun () -> pokemon_moves tem pn)
    )

(** [pokemon_stats_test name tem pn expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [pokemon_stats tem pn]. *)
let pokemon_stats_test
    (name : string) 
    (tem : Template.t)
    (pn : pokemon_name)
    (expected_output : (int * int * int)) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pokemon_stats tem pn) 
        ~printer: pp_tuple_iii) 

(** [pokemon_stats_exn_test name tem pn expected_output] constructs an OUnit 
    test named [name] that asserts that [UnknownPokemon pn] is raised by
    [pokemon_stats tem pn]. *)
let pokemon_stats_exn_test 
    (name : string) 
    (tem : Template.t)
    (pn : pokemon_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownPokemon pn) (fun () -> pokemon_stats tem pn)
    )

(** [move_list_test name tem expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [move_list tem]. *)
let move_list_test 
    (name : string) 
    (tem : Template.t) 
    (expected_output : move_name list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (move_list tem) 
        ~printer:(pp_list pp_string))

(** [move_type_test name tem mn expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [move_type tem mn]. *)
let move_type_test 
    (name : string) 
    (tem : Template.t)
    (mn : move_name)
    (expected_output : type_name) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (move_type tem mn) ~printer: String.escaped
    )

(** [move_type_exn_test name tem mn expected_output] constructs an OUnit test 
    named [name] that asserts that [UnknownMove mn] is raised by
    [move_type tem mn]. *)
let move_type_exn_test 
    (name : string) 
    (tem : Template.t)
    (mn : move_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownMove mn) (fun () -> move_type tem mn))

(** [move_stats_test name tem mn expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [move_stats tem mn]. *)
let move_stats_test
    (name : string) 
    (tem : Template.t)
    (mn : pokemon_name)
    (expected_output : (int * int * float)) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (move_stats tem mn) ~printer:(pp_tuple_iif))

(** [move_stats_exn_test name tem mn expected_output] constructs an OUnit test 
    named [name] that asserts that [UnknownMove mn] is raised by
    [move_stats tem mn]. *)
let move_stats_exn_test 
    (name : string) 
    (tem : Template.t)
    (mn : move_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownMove mn) (fun () -> move_stats tem mn)) 

(** [healing_list_test name tem expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [healing_list tem]. *)
let healing_list_test 
    (name : string) 
    (tem : Template.t) 
    (expected_output : healing_name list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (healing_list tem) 
        ~printer:(pp_list pp_string))

(** [healing_amount_test name tem h expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [healing_amount tem h]. *)
let healing_amount_test 
    (name : string) 
    (tem : Template.t) 
    (h : healing_name)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (healing_amount tem h) 
        ~printer:(string_of_int))

(** [healing_amount_exn_test name tem h expected_output] constructs an OUnit 
    test named [name] that asserts that [UnknownHealing h] is raised by
    [healing_amount tem h]. *)
let healing_amount_exn_test 
    (name : string) 
    (tem : Template.t)
    (h : healing_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownHealing h) (fun () -> healing_amount tem h)) 

(** [is_revive_test name tem h expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [is_revive tem h]. *)
let is_revive_test 
    (name : string) 
    (tem : Template.t) 
    (h : healing_name)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_revive tem h) ~printer:(string_of_bool))

(** [is_revive_exn_test name tem h expected_output] constructs an OUnit test 
    named [name] that asserts that [UnknownHealing h] is raised by
    [is_revive tem h]. *)
let is_revive_exn_test 
    (name : string) 
    (tem : Template.t)
    (h : healing_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownHealing h) (fun () -> is_revive tem h)) 

(** [healing_drop_test name tem h expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [healing_drop tem h]. *)
let healing_drop_test 
    (name : string) 
    (tem : Template.t) 
    (h : healing_name)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (healing_drop tem h) 
        ~printer:(string_of_float))

(** [healing_drop_exn_test name tem h expected_output] constructs an OUnit test 
    named [name] that asserts that [UnknownHealing h] is raised by
    [healing_drop tem h]. *)
let healing_drop_exn_test 
    (name : string) 
    (tem : Template.t)
    (h : healing_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownHealing h) (fun () -> healing_drop tem h)) 

(** [pokeball_list_test name tem expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [pokeball_list tem]. *)
let pokeball_list_test 
    (name : string) 
    (tem : Template.t) 
    (expected_output : pokeball_name list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pokeball_list tem) 
        ~printer:(pp_list pp_string))

(** [pokeball_catch_test name tem pb expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [pokeball_catch tem pb]. *)
let pokeball_catch_test 
    (name : string) 
    (tem : Template.t) 
    (pb : pokeball_name)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pokeball_catch tem pb) 
        ~printer:(string_of_float))

(** [pokeball_catch_exn_test name tem pb expected_output] constructs an OUnit 
    test named [name] that asserts that [UnknownPokeball pb] is raised by
    [pokeball_catch tem pb]. *)
let pokeball_catch_exn_test 
    (name : string) 
    (tem : Template.t)
    (pb : pokeball_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownPokeball pb) (fun () -> pokeball_catch tem pb)) 

(** [pokeball_drop_test name tem pb expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [pokeball_drop tem pb]. *)
let pokeball_drop_test 
    (name : string) 
    (tem : Template.t) 
    (pb : pokeball_name)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pokeball_drop tem pb) 
        ~printer:(string_of_float))

(** [pokeball_drop_exn_test name tem pb expected_output] constructs an OUnit 
    test named [name] that asserts that [UnknownPokeball pb] is raised by
    [pokeball_drop tem pb]. *)
let pokeball_drop_exn_test 
    (name : string) 
    (tem : Template.t)
    (pb : pokeball_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownPokeball pb) (fun () -> pokeball_drop tem pb)) 

(** [starter_pokemon_test name tem expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [starter_pokemon tem]. *)
let starter_pokemon_test 
    (name : string) 
    (tem : Template.t) 
    (expected_output : pokemon_name list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (starter_pokemon tem) 
        ~printer:(pp_list pp_string))

(** [starter_healings_test name tem expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [starter_healings tem ]. *)
let starter_healings_test 
    (name : string) 
    (tem : Template.t) 
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (starter_healings tem ) 
        ~printer:(pp_list pp_string))

(** [starter_healing_quantity_test name tem h expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [starter_healing_quantity tem h]. *)
let starter_healing_quantity_test 
    (name : string) 
    (tem : Template.t) 
    (h : healing_name)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (starter_healing_quantity tem h) 
        ~printer:(string_of_int))

(** [starter_healing_quantity_exn_test name tem h expected_output] constructs 
    an OUnit test named [name] that asserts that [UnknownHealing h] is raised 
    by [starter_healing_quantity tem h]. *)
let starter_healing_quantity_exn_test 
    (name : string) 
    (tem : Template.t)
    (h : healing_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownHealing h) (fun () -> 
          starter_healing_quantity tem h)) 

(** [starter_pokeballs_test name tem expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [starter_pokeballs tem ]. *)
let starter_pokeballs_test 
    (name : string) 
    (tem : Template.t) 
    (expected_output : string list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (starter_pokeballs tem ) 
        ~printer:(pp_list pp_string))

(** [starter_pokeball_quantity_test name tem pb expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [starter_pokeball_quantity tem pb]. *)
let starter_pokeball_quantity_test 
    (name : string) 
    (tem : Template.t) 
    (pb : pokeball_name)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (starter_pokeball_quantity tem pb) 
        ~printer:(string_of_int))

(** [starter_pokeball_quantity_exn_test name tem pb expected_output] constructs 
    an OUnit test named [name] that asserts that [UnknownPokeball pb] is raised 
    by [starter_pokeball_quantity tem pb]. *)
let starter_pokeball_quantity_exn_test 
    (name : string) 
    (tem : Template.t)
    (pb : pokeball_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownPokeball pb) (fun () -> pokeball_drop tem pb)) 

(** [parse_test name str expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with [parse str]. *)
let parse_test
    (name : string) 
    (str: string) 
    (expected_output : action) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str))

(** [parse_exn_test name kind str] constructs an OUnit test 
    named [name] that asserts that a [kind] exception is raised with 
    [parse str]. *)
let parse_exn_test
    (name : string)
    (kind : string)
    (str : string) : test = 
  name >:: (fun _ -> 
      if kind = "Empty" then
        assert_raises Empty (fun () -> parse str)
      else (* Malformed *)
        assert_raises Malformed (fun () -> parse str))

(* michael tests at end of parse and before my calcs*)

(** [type_converter_test name t expected_output] constructs an OUnit test named 
    [name] that asserts the quality of [expected_output] with 
    [type_converter t]. *)
let type_converter_test
    (name : string)
    (t : type_name)
    (expected_output : elemtype) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (type_converter t))

(** [type_converter_exn_test name t expected_output] constructs 
    an OUnit test named [name] that asserts that [UnknownType t] is raised 
    by [type_converter t]. *)
let type_converter_exn_test 
    (name : string)
    (t : type_name) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownType t) (fun () -> type_converter t))

(** [type_effectiveness_test name et1 expected_output] constructs an OUnit test 
    named [name] that asserts the quality of [expected_output] with 
    [type_effectiveness_test et1 et2]. *)
let type_effectiveness_test
    (name : string)
    (et1 : elemtype)
    (et2 : elemtype)
    (expected_output : effective) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (type_effectiveness et1 et2))

(** [multiplied_stats_test name tem pn lvl expected_output] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [multiplied_stats_test tem pn lvl]. *)
let multiplied_stats_test
    (name : string)
    (tem : Template.t)
    (pn : pokemon_name)
    (lvl : int)
    (expected_output : (int * int * int) ) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (multiplied_stats tem pn lvl)
        ~printer: pp_tuple_iii )


(**********)
(** JSON **)
(**********)


let tem = "template.json" |> Yojson.Basic.from_file |> from_json


(***************)
(** CONSTANTS **)
(***************)


let pokemon_LIST = ["Rattata"; "Charmander"; "Bulbasaur"]
let move_LIST = ["Tackle"; "Headbutt"; "Takedown"; 
                 "Scratch"; "Flamethrower"; "Ember"; 
                 "Razor Leaf"; "Vine Whip"]
let healing_LIST = ["Potion"; "Super Potion"; "Ultra Potion"; 
                    "Revive"; "Super Revive"; "Ultra Revive"]
let pokeball_LIST = ["Pokeball"; "Great Ball"; "Ultra Ball"; "Master Ball"]
let starter_pokemon_LIST = ["Charmander"]
let starter_healing_LIST = 
  ["Potion"; "Super Potion"; "Ultra Potion"; 
   "Revive"; "Super Revive"; "Ultra Revive"]
let starter_ball_LIST = ["Pokeball"; "Great Ball"; "Ultra Ball"; "Master Ball"]


(***********)
(** TESTS **)
(***********)


let template_tests = 
  [
    pokemon_list_test "pokemon_list test" tem pokemon_LIST;
    pokemon_type_test "pokemon type test" tem "Charmander" "Fire";
    pokemon_type_test "pokemon type test Rattata" tem "Rattata" "Normal";
    pokemon_moves_test "pokemon moves test" tem "Charmander" 
      ["Tackle"; "Scratch"; "Flamethrower"; "Ember"];
    pokemon_moves_test "pokemon moves test Rattata" tem "Rattata" 
      ["Tackle"; "Headbutt"; "Scratch"; "Takedown"];
    pokemon_stats_test "pokemon stats test" tem "Charmander" (85,20,30);
    pokemon_stats_test "pokemon stats test Rattata" tem "Rattata" (80,10,10);
    pokemon_type_exn_test "pokemon type exception test" tem "charmander";
    pokemon_type_exn_test "pokemon type exception test" tem "Caharmander";
    pokemon_type_exn_test "pokemon type exception test" tem "Pidgey";
    pokemon_moves_exn_test "pokemon type exception test" tem "charmander";
    pokemon_moves_exn_test "pokemon type exception test" tem "Caharmander";
    pokemon_moves_exn_test "pokemon type exception test" tem "Pidgey";
    pokemon_stats_exn_test "pokemon type exception test" tem "charmander";
    pokemon_stats_exn_test "pokemon type exception test" tem "Caharmander";
    pokemon_stats_exn_test "pokemon type exception test" tem "Pidgey";

    move_list_test "move list test" tem move_LIST;
    move_type_test "move type test fire" tem "Flamethrower" "Fire";
    move_stats_test "move stats test Flamethrower" tem "Flamethrower" 
      (15,20,0.7);
    move_stats_test "move stats test Tackle" tem "Tackle" (25,10,1.0);
    move_type_exn_test "move type exception test fire lowercase" tem 
      "flamethrower";
    move_type_exn_test "move type exception test not a type" tem "apple";
    move_stats_exn_test "move type exception test fire lowercase" tem 
      "flamethrower";
    move_stats_exn_test "move type exception test not a type" tem "apple";


    healing_list_test "healing list test" tem healing_LIST;
    healing_amount_test "healing amount test Potion" tem "Potion" 20;
    healing_amount_test "healing amount test Super Potion" tem "Super Potion" 
      50;
    is_revive_test "is revive test Super Potion" tem "Super Potion" false;
    is_revive_test "is revive test revive" tem "Revive" true;
    healing_drop_test "healing drop test Potion" tem "Potion" 0.3;
    healing_drop_test "healing drop test Ultra Potion" tem "Ultra Potion" 0.05;
    healing_amount_exn_test "exception on lower case" tem "ultra Potion";
    healing_amount_exn_test "exception on no space" tem "UltraPotion";
    healing_amount_exn_test "exception on not a healing item" tem "Pokeball";
    is_revive_exn_test "exception on lower case" tem "ultra Revive";
    is_revive_exn_test "exception on no space" tem "UltraRevive";
    is_revive_exn_test "exception on not a healing item" tem "Pokeball";
    healing_drop_exn_test "exception on lower case" tem "ultra Potion";
    healing_drop_exn_test "exception on no space" tem "UltraPotion";
    healing_drop_exn_test "exception on not a healing item" tem "Pokeball";


    pokeball_list_test "pokeball list test" tem pokeball_LIST;
    pokeball_catch_test "pokeball catch test pokeball" tem "Pokeball" 0.25;
    pokeball_catch_test "pokeball catch test Great Ball" tem "Great Ball" 0.5;
    pokeball_drop_test "pokeball drop test pokeball" tem "Pokeball" 0.5;
    pokeball_drop_test "pokeball drop test Master Ball" tem "Master Ball" 0.05;
    pokeball_catch_exn_test "pokeball catch exception test not ball" tem 
      "Potion";
    pokeball_catch_exn_test "pokeball catch exception test lowercase" tem 
      "pokeball";
    pokeball_catch_exn_test "pokeball catch exception test no space" tem 
      "Potion";
    pokeball_drop_exn_test "pokeball catch exception test not ball" tem 
      "Potion";
    pokeball_drop_exn_test "pokeball catch exception test lowercase" tem 
      "pokeball";
    pokeball_drop_exn_test "pokeball catch exception test no space" tem 
      "Potion";

    starter_pokemon_test "starter pokemon test" tem starter_pokemon_LIST;
    starter_healings_test "starter healing test" tem starter_healing_LIST;
    starter_healing_quantity_test "starter healing quantity test potion"
      tem "Potion" 3;
    starter_healing_quantity_test "starter healing quantity test Ultra Revive"
      tem "Ultra Revive" 2;
    starter_pokeballs_test "starter pokeballs test" tem starter_ball_LIST;
    starter_pokeball_quantity_test "starter pokeball quantity test pokeball"
      tem "Pokeball" 3;
    starter_pokeball_quantity_test "starter pokeball quantity test Master Ball"
      tem "Master Ball" 2;
    starter_healing_quantity_exn_test "exception for non healing" tem 
      "Pokeball";
    starter_healing_quantity_exn_test "exception for case" tem "potion";
    starter_healing_quantity_exn_test "exception for space" tem "UltraPotion";
    starter_pokeball_quantity_exn_test "exception for non ball" tem "Potion";
    starter_pokeball_quantity_exn_test "exception for case" tem "pokeball";
    starter_pokeball_quantity_exn_test "exception for space" tem "MasterBall";


  ]


let action_tests =
  [
    parse_test {|verb "Fight" returns Fight|} "Fight" Fight;
    parse_test {|verb "Inventory" returns Inventory|} "Inventory" Inventory;
    parse_test {|verb "Pokemon" returns Pokemon|} "Pokemon" Pokemon;
    parse_test {|verb "Run" returns Run|} "Run" Run;
    parse_test {|verb "Use" followed by "Flamethrower" returns Use 
    ["Flamethrower"]|} "Use Flamethrower" (Use ["Flamethrower"]);
    parse_test {|verb "Exit" returns Exit|} "Exit" Exit;
    parse_test {|verb "   Exit  " returns Exit|} "   Exit  " Exit;
    parse_test {|verb " Use  " followed by " Dragon  Claw     " returns Use 
    ["Dragon";"Claw"]|} " Use    Dragon  Claw     " (Use ["Dragon";"Claw"]);

    parse_exn_test "empty string raises Empty" "Empty" "";
    parse_exn_test "only spaces raises Empty" "Empty" "   ";
    parse_exn_test {|verb "win" doesn't exist and raises Malformed|} 
      "Malformed" "win";
    parse_exn_test {|verb "exit" followed by "game" raises 
    Malformed|} "Malformed" "exit game";
    parse_exn_test {|verb "use" followed by nothing raises 
    Malformed|} "Malformed" "use";
  ]

let status_tests =
  [
    type_converter_test "check Normal type" "Normal" Normal;
    type_converter_test "check Fire type" "Fire" Fire;
    type_converter_test "check Water type" "Water" Water;
    type_converter_test "check Grass type" "Grass" Grass;
    type_converter_exn_test "check bad type" "bad";

    type_effectiveness_test "Normal & Normal -> Regular" Normal Normal Regular;
    type_effectiveness_test "Normal & Fire -> Regular" Normal Fire Regular;
    type_effectiveness_test "Normal & Water -> Regular" Normal Water Regular;
    type_effectiveness_test "Normal & Grass -> Regular" Normal Grass Regular;
    type_effectiveness_test "Fire & Normal -> Regular" Fire Normal Regular;
    type_effectiveness_test "Fire & Fire -> Not" Fire Fire Not;
    type_effectiveness_test "Fire & Water -> Not" Fire Water Not;
    type_effectiveness_test "Fire & Grass -> Super" Fire Grass Super;
    type_effectiveness_test "Water & Normal -> Regular" Water Normal Regular;
    type_effectiveness_test "Water & Fire -> Super" Water Fire Super;
    type_effectiveness_test "Water & Water -> Not" Water Water Not;
    type_effectiveness_test "Water & Grass -> Not" Water Grass Not;
    type_effectiveness_test "Grass & Normal -> Regular" Grass Normal Regular;
    type_effectiveness_test "Grass & Fire -> Not" Grass Fire Not;
    type_effectiveness_test "Grass & Water -> Super" Grass Water Super;
    type_effectiveness_test "Grass & Grass -> Not" Grass Grass Not;

    multiplied_stats_test "Rattata at lvl 1 has base stats" tem "Rattata" 1
      (80, 10, 10);
    multiplied_stats_test "Rattata at lvl 100 has max stats" tem "Rattata" 100
      (270, 25, 25);
    multiplied_stats_test "Charmander at lvl 1 has base stats" tem "Charmander" 
      1 (85, 20, 30);
    multiplied_stats_test "Charmander at lvl 100 has base stats" tem 
      "Charmander" 100 (280, 45, 65);
  ]

let suite =
  "Pokemon Test Suite"  >::: List.flatten [
    template_tests;
    action_tests;
    status_tests;
  ]

let _ = run_test_tt_main suite