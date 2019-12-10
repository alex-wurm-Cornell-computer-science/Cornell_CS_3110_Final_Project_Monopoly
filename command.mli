(**
   Parsing of player commands.
*)

(* You are free to add more code here. *)

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["sell Reading Railroad"], then the object phrase is 
      [["sell"; "Reading"; "Railroad"]].
    - If the player command is ["   sell       Reading Railroad  "], then the 
      object phrase is again [["sell"; "Reading"; "Railroad"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Roll (* Used to initiate dice roll to move around the board on your turn. *)
  | Quit (* Used to end the game -> will prompt player to confirm. *)
  | Wallet (* Used to see current liquid assets of the player ($$$). *)
  | Inventory (* Used to see current properties that the player owns. *)
  | Buy (* Used to purchase a property that you are currently on. *) 
  | Sell of object_phrase (* Used to sell any properties in the current inventory. *)
  | Items (* Used to see what special cards the player holds. *)
  | Next
  | Build of object_phrase
  | Game 
  | Use

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    roll   "] is [Roll]
    - [parse "  sell  Reading Railroad   "] is [Sell["Reading Railroad"]]

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb isn't one of the following: "roll", "quit", 
    "wallet", "inventory", "buy", "sell", "items", "next", "build", "game", or
    "use"
    or if the verb is "sell" or "build" and there is a empty object phrase,
    or if the verb is "roll", "quit", "wallet", "inventory", "buy", "items",
    "next", "game", or "use"
    and there is a non-empty object phrase.*)
val parse : string -> command

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)