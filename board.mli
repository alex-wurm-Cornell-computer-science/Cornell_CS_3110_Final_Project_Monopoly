(** 
   Representation of static board data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(* You are free to add more code here. *)


(** The abstract type of values representing squares. *)
type square

(** The abstract type of values representing cards. *)
type card

(** The type of property names *)
type prop_name = string
type card_name = string

(** Raised when an unknown square is encountered. *)
exception UnknownSquare of prop_name

(** Raised when an unknown card is encountered *)
exception UnknownCard of card_name

(** The abstract type of values representing monopoly boards. *)
type board

(** The type representing square types*)
type squareType = 
  | Go
  | Jail 
  | Parking
  | GoToJail
  | Property
  | Chance
  | Chest
  | Tax 

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> board

(** [square_type b s_name] returns the type of property with name 
    [s_name] in [b]. Raises [UnknownSquare s_name] if s_name is not a square
      in [b]. *)
val square_type  : board -> prop_name -> squareType

(** [all_squares b] returns a list of the names of all squares in [b]*)
val all_squares : board -> string list

(** [cost b p] returns the cost of property [p] in [b]. Raises 
    [UnknownSquare p] if [p] is not a property in [b]*)
val cost : board -> string -> int 

(** [rent b p] returns the rent of property [p] in [b]. Raises 
    [UnknownSquare p] if [p] is not a property in [b]*)
val rent : board -> string -> int 

(** [chance_cards b] returns a list of the deck of chance cards in [b]*)
val chance_cards : board -> string list 

(** [chest_cards b] returns a list of the deck of chest cards in [b]*)
val chest_cards : board -> string list 

(** [chance_card_description b cd] returns the description of card [cd]
      in board [b]. Raises [Not_Found] if [cd] is not in [b]  *)
val chance_card_description : board -> string -> string 

(** [chest_card_description b cd] returns the description of card [cd]
      in board [b]. Raises [Not_Found] if [cd] is not in [b]  *)
val chest_card_description : board -> string -> string 

(** [num_squares b] returns the size of board [b], defined as the number of
      squares in the board*)
val size : board -> int 

(** [monopoly_group b col] returns the list of properties in the monopoly group
    with color [col] in [b]. A monopoly group is a set of properties
      that when owned together form a monopoly. Raises [UnknownSquare col] if 
      [col] is not in b. *)
val monopoly_group : board -> string -> string list

(** [nth_square bd n] returns the square at position [n] in [bd] *)
val nth_square : board -> int -> string

(** [square_pos b p] returns the position of property [p] in [b]. Raises 
    [UnknownProp p] if [p] is not in [b] *)
val square_pos : board -> prop_name -> int

(** [prop_color b p] returns the color of [p] in [b]. Raises 
    [UnknownProp p] if [p] is not in [b] *)
val square_color : board -> prop_name -> string option


(** [house_cost bd prop] returns [Some i] where [i] is the cost of 
      building a house on [prop]. Raises [UnknownCard prop] if [prop]
      is not in [bd]. *)
val house_cost : board -> prop_name -> int option


(** [hotel_cost bd prop] returns [Some i] where [i] is the cost of 
      building a hotel on [prop]. Raises [UnknownCard prop] if [prop]
      is not in [bd]. *)
val hotel_cost : board -> prop_name -> int option