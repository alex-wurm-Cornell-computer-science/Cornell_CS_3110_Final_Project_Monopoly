(** 
   Representation of dynamic game state.

   This module represents the state of a Monopoly game as it is being played,
   including the the current locations, properties owned, and cash held by 
   all players.
*)

open Board

(** raised when attempting to build on non-property *)
exception Unbuildable of prop_name

(** The abstract type of values representing a property on the board  *)
type property

(** The abstract type of values representing the game state. *)
type t = {
  curr_player : int;
  num_players : int;
  locations : (int * (int * bool)) list;
  doubles_rolled : int;
  inventories : (int * prop_name list) list;
  items : (int * prop_name list) list;
  wallets : (int * int) list;
  total_assets : (int * int) list;
  buildings : (prop_name * ( int * int)) list;
  cards : card_name list;
  player_status : (int * bool) list;
  in_jail : (int * bool) list;
}

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal | Win

(** [init_state brd n] is the initial state of the game when using board [brd] 
    with [n] players. Each player is initialized to start at the "GO" square on 
    the board, and own no properties and hold no cash *)
val init_state : Board.board -> int -> t

(** [current_player st] is the player whose turn it is in state [st]. *)
val current_player : t -> int

(** [num_players st] is the number of players in the current game, represented 
    in state [st] *)
val num_players : t -> int

(** [locations st] is a list of all players, their locations, and a boolean 
    specifying if they have already rolled (with true meaning they've rolled and
    false meaning they have yet to roll) in [st] *)
val locations : t -> (int * (int * bool)) list

(** [doubles_rolled st] is the number of doubles the current player has 
    rolled in the current [st] *)
val doubles_rolled : t -> int

(** [inventories st] is an association list where the keys are players 
    and the values are properties owned by the player in [st] *)
val inventories : t -> (int * prop_name list) list

(** [items st] is an assoc list where the keys are players and the values 
    are the cards the players hold in [st] *)
val items : t -> (int * card_name list) list 

(** [wallets st] is an assoc list where the keys are players and the 
    values are the amount of money that the players have in [st] *)
val wallets : t -> (int * int) list

(** [total_assets st] is an assoc list where the keys are players and 
    the values are the sum of the total amount of money and properties 
    that the players have *)
val total_assets : t -> (int * int) list 

(** [buildings st] is an assoc list where the keys are the names of the 
    properties and the values are a tuple of the number of houses and the number
    of hotels on that property. If there are no buildings, values are 0. *)
val buildings : t -> (prop_name * ( int * int)) list

(** [cards st] is a list of card names, where the head of the list is the 
    top of the deck *)
val cards : t -> card_name list 

(** [player_status st] is an association list where the keys are players and the 
    values are booleans representing their validity in the game (true means the 
    player is still a valid player, and false means the player is no longer a 
    valid player) *)
val player_status : t -> (int * bool) list 

(** [in_jail] is an association list where the keys are players and the values
    are booleans representing whether or not the player is in Jail (true means 
    the player is in Jail, false means otherwise) *)
val in_jail : t -> (int * bool) list

(** [is_in_jail st] is true if the current player is in jail, 
    false otherwise *)
val is_in_jail : t -> bool

(** [current_location st] is the integer square corresponding to the current
    player's location in [st] *)
val current_location : t -> int

(** [curr_player_inventory st] is the list of property names associated with the 
    current player of [st] *)
val curr_player_inventory : t -> prop_name list

(** [curr_player_wallet st] is the integer value of the amount of cash
    associated with the current player of [st] *)
val curr_player_wallet : t -> int 

(** [curr_player_items st] is the list of card names associated with the 
    current player of [st] *)
val curr_player_items : t -> card_name list 

(**  [current_status st] is the boolean corresponding to the current player's 
     status in [st] *)
val current_status : t -> bool

(** [update_state st res] is [st] from [result] if [result] is 
    [Legal st], otherwise if [result] is [Illegal] it's [st].*)
val update_state : t -> result -> t 

(** [next_turn st] is Legal st', where st' has an incremented value for the 
    current player field, if the current player in [st] has rolled and if the 
    incremented player is a valid player (has a current status of true). If the 
    latter condition isn't true, [next_turn st] will be called until the 
    incremented player is valid. Otherwise is Illegal. *)
val next_turn : t -> result 

(** [roll brd st] is Legal st', where st' has a new value for the current 
    location of the current player based on the number of doubles "rolled", if 
    the current player hasn't rolled yet. Otherwise is Illegal.*)
val roll : Board.board -> t -> result

(** [inventory_value brd st] is an integer representing the total value of the 
    properties owned by the current player in [st], combined with the values of 
    any houses and/or hotels on that property *)
val inventory_value : Board.board -> t -> int 

(** [wealthiest_player brd st] is a tuple where the first element is the player 
    with the most accumulated wealth (properties, houses/hotels, and cash) and 
    the second element is that value of the most accumulated wealth *)
val wealthiest_player : Board.board -> t -> (int * int) list

(** [earn_cash t amt] adds [amt] to the current player's wallet in [st] *)
val earn_cash : t -> int -> result 

(** [buy bd prop st] adds [prop] to the current player's inventory and 
    pays the cost of the property. *)
val buy : Board.board -> prop_name -> t -> result 

(** [sell bd prop st] sells the property [prop] from the current player's 
    inventory.  *)
val sell : Board.board -> prop_name -> t -> result 

(** [pay_rent bd prop st] executes the transaction where the current player
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

(** [move_cards brd crd st] moves [crd] to the bottom of the card pile. 
      If [crd] is a get out of jail card, the pile is unchanged *)
val move_cards : board -> card_name -> t -> result

(** [houses st prop] returns the number of houses on [prop] in [st].
      Returns 0 if [prop] is not a valid property or no hotels are built on it
      Requires there are 3 houses on [prop] *)
val houses : t -> Board.prop_name -> int

(** [hotels st prop] returns the number of hotels on [prop] in [st].
      Returns 0 if [prop] is not a valid property or no hotels are built on it
      Requires there are 3 houses on [prop] *)
val hotels : t -> Board.prop_name -> int

(** [prop_available prop st] returns false if [prop] is already owned *)
val prop_available : Board.prop_name -> t -> bool

(** [next_card st] returns the card at the top of the pile 
      Requires: [cards st] is not empty. *)
val next_card : t -> string 

(** [get_out_of_jail brd st] allows the current player to leave jail, returning 
      a new state, [Legal st']. Return [Illegal] if the player cannot use the 
      card to leave jail. *)
val get_out_of_jail : board -> t -> result

(** [pay_tax brd st] pays the tax given by landing on a tax square. *)
val pay_tax : board -> t -> int -> result