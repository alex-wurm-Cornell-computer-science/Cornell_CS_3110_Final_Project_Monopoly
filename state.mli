(** 
   Representation of dynamic adventure state.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

(* You are free to add more code here. *)
open Board

(* raised when attempting to build on non-property *)
exception Unbuildable of prop_name

(* type player *)
type property

(** The abstract type of values representing the game state. *)
type t 

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal | Win

(** [init_state brd n] is the initial state of the game when playing adventure [a]. 
    In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)
val init_state : Board.board -> int -> t

(** [current_room_id st] is the identifier of the room in which the adventurer
    currently is located in state [st]. *)
val current_player : t -> int

(** [num_players st] is the number of players in the current game [st] *)
val num_players : t -> int

(** [locations st] is a list of all players, their locations, and a boolean 
    specifying if they are eligible to move around the board in [st] *)
val locations : t -> (int * (int * bool)) list

(** [doubles_rolled st] is the number of doubles the current player has 
      rolled in the current [st] *)
val doubles_rolled : t -> int

(** [current_location st] is the integer square corresponding to the current
    player's location in [st] *)
val current_location : t -> int

(** [inventories st] is an association list where the keys are players 
    and the values are properties owned by the player in [st] *)
val inventories : t -> (int * Board.prop_name list) list

(** [items st] is an assoc list where the keys are players and the values 
    are the cards the players hold in [st] *)
val items : t -> (int * Board.card_name list) list 

(** [wallets st] is an assoc list where the keys are players and the 
    values are the amount of money that the players have in [st] *)
val wallets : t -> (int * int) list

(** [total_assets st] is an assoc list where the keys are players and 
    the values are the sum of the total amount of money and properties 
    that the players have *)
val total_assets : t -> (int * int) list 

(** [buildings st] is an assoc list where the keys are the names of the 
    properties and the values are a tuple of the number of houses and the number
    of hotels on that property *)
val buildings : t -> (prop_name * ( int * int)) list

(** [cards st] is a list of card names, where the head of the list is the 
    top of the deck *)
val cards : t -> card_name list 

(** [update_state st res] is [st] from [result] if [result] is 
    [Legal st], otherwise if [result] is [Illegal] it's [old_state].*)
val update_state : t -> result -> t 

(** [next_turn st] is Legal st', where st' has an incremented value for the 
    current player field, if the current player in [st] has rolled. 
    Otherwise is Illegal. *)
val next_turn : t -> result 

(** [roll brd st] is Legal st', where st' has a new value for the current 
    location of the current player based on the number of doubles "rolled", if 
    the current player hasn't rolled yet. Otherwise is Illegal.*)
val roll : Board.board -> t -> result

(** [curr_player_inventory st] is the list of property names associated with the 
    current player of [st] *)
val curr_player_inventory : t -> prop_name list

(** [curr_player_wallet st] is the integer value of the amount of cash
    associated with the current player of [st] *)
val curr_player_wallet : t -> int 

(** [curr_player_items st] is the list of card names associated with the 
    current player of [st] *)
val curr_player_items : t -> card_name list 

(** [buy bd prop st] adds [prop] to the current player's inventory and 
    pays the cost of the property. *)
val buy : Board.board -> prop_name -> t -> result 

(** [sell bd prop st] Sells the property [prop] from the current player's 
    inventory.  *)
val sell : Board.board -> prop_name -> t -> result 

val auction : prop_name -> t -> result 

val inventory_value : Board.board -> t -> int 

val wealthiest_player : Board.board -> t -> (int * int) list

(** [earn_cash t amt] Adds [amt] to the current player's wallet*)
val earn_cash : t -> int -> result 

(** [pay_rent bd prop st] Executes the transaction where the current player
      pays the owner of [prop] rent for landing on [prop]. *)
val pay_rent : board -> prop_name -> t -> result

(** [build_houses bd st prop n] is Legal st', builds [n] houses on [prop], 
    if the current player has enough currency to build all [n] houses. 
    Otherwise, or if the player cannot build on [prop] is [Illegal] *)
val build_houses : board -> t -> prop_name -> int -> result

(** [build_hotels bd st prop n] is Legal st', builds [n] hotels on [prop],
    refunds the player for houses on [prop],if the current player has enough 
    currency to build all [n] hotels. Otherwise, or if the player cannot 
    build on [prop] is [Illegal]*)
val build_hotels : board -> t -> prop_name -> int -> result

(** [card_action bd cd st] executed the action on the given [cd] *)
val card_action : board -> card_name -> t -> result

(** [move_cards brd crd st] moves [crd] to the bottom of the card pile *)
val move_cards : card_name -> t -> result
