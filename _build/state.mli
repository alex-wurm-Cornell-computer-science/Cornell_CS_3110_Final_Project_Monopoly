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

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(*
(** [go exit adv st] is [r] if attempting to go through exit [exit] in state 
    [st] and adventure [adv] results in [r].  If [exit] is an exit from the 
    adventurer's current room, then [r] is [Legal st'], where in [st'] the 
    adventurer is now located in the room to which [exit] leads.  Otherwise, 
    the result is [Illegal]. 
    Effects: none.  [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)

(** [score st] is the score in state [st]. *)
val score : t -> int

(** [get_current_room old_state result] is the current room of [st] if [result] 
    is [Legal st], otherwise if [result] is [Illegal] it's the current 
    room_id from [old_state]. *)
val get_current_room : t -> result -> string

(** [update_state old_state result] is [st] from [result] if [result] is 
    [Legal st], otherwise if [result] is [Illegal] it's [old_state]. *)
val update_state : t -> result -> t 

(** [print_inventory st room] is the string representation of all items in the 
    adventurer's inventory in state [st] that are in room [room]. *)
val print_inventory : t -> Adventure.room_id -> string

(*  [get_inventory st] is the adventurer's inventory in state [st]. *)
val get_inventory : t -> (Adventure.item_name * Adventure.room_id) list

(** [update_items st item room] is the inventory list of tuples from [st] 
    with the room attributed to [item] being changed to [room]. *)
val update_items : t -> Adventure.t -> Adventure.item_name ->  Adventure.room_id -> (Adventure.item_name * Adventure.room_id) list

(*  [updated_item_score st adv item] is the updated score for [item], which 
    updates only if the adventurer's current room in state [st] is the treasure 
    room of adventure [adv]. *)
val update_item_score : t -> Adventure.item_name -> Adventure.t -> int

(* [take item adv st] is [r] if attempting to add item [item] to inventory in 
   state [st] and adventure [adv] results in [r]. If [item] is an item from the 
   adventurer's current room, then [r] is [Legal st'], where in [st'] the 
   adventurer's inventory has been updated with [item] in room inventory. 
   Otherwise the result is [Illegal]. 
   Effects: none. [take] is not permitted to do any printing. *)
val take : string -> Adventure.t -> t -> result

(* [drop item adv st] is [r] if attempting to drop item [item] from inventory to 
   the adventurer's current room in state [st] and adventure [adv] results in [r]. 
   If [item] is an item in room inventory, then [r] is [Legal st'], where in 
   [st'] the adventurer's inventory has been updated with [item] in the 
   adventurer's current room in state [st] (or removed from inventory if 
   the adventurer's current room in state [st] is the treasure room of adventure 
   [adv]. Otherwise the result is [Illegal].
   Effects: none. [drop] is not permitted to do any printing. *)
val drop : string -> Adventure.t -> t -> result
*)