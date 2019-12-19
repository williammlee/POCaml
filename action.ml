type object_phrase = string list

type action = 
  | Fight
  | Inventory
  | Pokemon 
  | Catch of object_phrase
  | Run
  | Use of object_phrase
  | Switch of object_phrase 
  | Exit
  | Back
  | Heal of object_phrase

exception Empty

exception Malformed

(** [is_malformed verb obj_phrase] is true if [verb] and [obj_phrase] form a
    malformed action *)
let is_malformed verb obj_phrase =
  match verb with
  | "Fight" -> obj_phrase <> []
  | "Inventory" -> obj_phrase <> []
  | "Pokemon" -> obj_phrase <> []
  | "Catch" -> obj_phrase =  []
  | "Run" -> obj_phrase <> []
  | "Use" -> obj_phrase =  []
  | "Switch" -> obj_phrase = []
  | "Exit" -> obj_phrase <> []
  | "Back" -> obj_phrase <> []
  | "Heal" -> obj_phrase = []
  | _ -> true 

let parse (str: string) : action =
  let action_list = str |> String.split_on_char ' ' |> 
                    List.filter (fun s ->  s <> "") in
  match action_list with
  | [] -> raise Empty
  | verb :: obj_phrase -> 
    if is_malformed verb obj_phrase then raise Malformed
    else match verb with
      | "Fight" -> Fight
      | "Inventory" -> Inventory
      | "Pokemon" -> Pokemon
      | "Catch" -> Catch obj_phrase
      | "Run" -> Run
      | "Use" -> Use obj_phrase
      | "Switch" -> Switch obj_phrase
      | "Exit" -> Exit
      | "Back" -> Back
      | "Heal" -> Heal obj_phrase
      | _ -> failwith "impossible in parse"