(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(* You are free to add more code here. *)

(** Raised when an unknown item is encountered. *)
exception UnknownProperty of Board.prop_name

type player

type property

(** The abstract type of values representing the game state. *)
type t 

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [init_state brd n] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Board.board -> int -> t

(** [current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. *)
val current_player : t -> player

val num_players : t -> int

val locations : t -> (player * int) list

val inventories : t -> (player * property list) list

val items : t -> (player * Board.prop_name list) list 

val wallets : t -> (player * int) list

val total_assets : t -> (player * int) list 

val roll : t -> result

val curr_player_inventory : t -> property list

val curr_player_wallet : t -> int 

val curr_player_items : t -> Board.card list 

val buy : t -> result 

val sell : t -> result 

val auction : t -> result 