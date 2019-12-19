(** Processes an object_phrase and associates it to an action. *)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player action.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player action.  For example:
    - If the player action is ["use Dragon Claw"], then the object phrase is 
      [["Dragon"; "Claw"]].
    - If the player action is ["use Dragon     Claw"], then the object phrase 
      is again [["Dragon"; "Claw"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [action] represents a player action that is decomposed
    into a verb and possibly an object phrase. *)
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

(** Raised when an empty action is parsed. *)
exception Empty

(** Raised when a malformed action is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [action], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    use   Dragon   Claw   "] is [Use ["Dragon"; "Claw"]]
    - [parse "exit"] is [Exit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the action is malformed. A action
    is {i malformed} if the verb isn't "fight", "inventory", "pokemon", "run",
    "use", or "exit", "use" without an object phrase following, or any other
    verb with an object phrase following. *)
val parse : string -> action