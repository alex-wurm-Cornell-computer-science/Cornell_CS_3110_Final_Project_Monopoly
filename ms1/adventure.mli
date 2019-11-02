(** 
   Representation of static adventure data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(* You are free to add more code here. *)

(** The abstract type of values representing items. *)
type item 

(** The abstract type of values representing exits. *)
type exit 

(** The abstract type of values representing rooms. *)
type room

(** The type of item names. *)
type item_name = string

(** Raised when an unknown item is encountered. *)
exception UnknownItem of item_name

(**********************************************************************
 * DO NOT CHANGE THIS CODE
 * It is part of the interface the course staff will use to test your 
 * submission.
*)

(** The abstract type of values representing adventures. *)
type t

(** The type of room identifiers. *)
type room_id = string

(** The type of exit names. *)
type exit_name = string

(** Raised when an unknown room is encountered. *)
exception UnknownRoom of room_id

(** Raised when an unknown exit is encountered. *)
exception UnknownExit of exit_name

(** [from_json j] is the adventure that [j] represents.
    Requires: [j] is a valid JSON adventure representation. *)
val from_json : Yojson.Basic.t -> t

(** [start_room a] is the identifier of the starting room in adventure 
    [a]. *)
val start_room : t -> room_id

(** [room_ids a] is a set-like list of all of the room identifiers in 
    adventure [a]. *)
val room_ids : t -> room_id list

(** [description a r] is the description of room [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val description : t -> room_id -> string

(** [exits a r] is a set-like list of all exit names from room [r] in 
    adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)
val exits : t -> room_id -> exit_name list

(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a].  
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].
    Raises [UnknownExit e] if [e] is not an exit from room [r] in [a]. *)
val next_room : t -> room_id -> exit_name -> room_id

(** [next_rooms a r] is a set-like list of all rooms to which there is an exit 
    from [r] in adventure [a]. 
    Raises [UnknownRoom r] if [r] is not a room identifier in [a].*)
val next_rooms : t -> room_id -> room_id list

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)

(** [treasure_room adv] is the identifier of the starting room in adventure 
    [adv]. *)
val treasure_room : t -> room_id

(** [win_msg adv] is the string displayed when the game is won in adventure 
    [adv]. *)
val win_msg : t -> string

(** [items adv room] is the list representation of all items currently in 
    [room].
    Raise [UnknownRoom r] if [r] is not a room identifier in [a].*)
val items : t -> (item_name * room_id) list

(** [room_score adv room] is the score value of room [room] in adventure [adv].
      Raise [UnknownRoom room] if [room] is not a room identifier in [adv]. *)
val room_score : t -> room_id -> int 

(** [item_score adv item] is the score value of item [item] in adventure [adv].
      Raise [UnknownItem item] if [item] is not an item identifier in [adv]. *)
val item_score : t -> item_name -> int 