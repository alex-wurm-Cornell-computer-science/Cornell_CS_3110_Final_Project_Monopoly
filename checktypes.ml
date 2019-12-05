module type BoardSig = sig
  type square
  type card
  type cardType = | Money | Location | LeaveJail
  type prop_name = string
  type card_name = string
  exception UnknownSquare of prop_name
  exception UnknownCard of card_name
  type board
  type squareType = | Go| Jail  | Parking | GoToJail | Property | Card | Tax 
  val from_json : Yojson.Basic.t -> board
  val square_type  : board -> prop_name -> squareType
  val all_squares : board -> string list
  val cost : board -> string -> int 
  val rent : board -> string -> int 
  val cards : board -> string list 
  val next_card : board -> string 
  val card_description : board -> string -> string 
  val size : board -> int 
  val monopoly_group : board -> string -> string list
  val nth_square : board -> int -> string
  val square_pos : board -> prop_name -> int
  val square_color : board -> prop_name -> string option
  val house_cost : board -> prop_name -> int option
  val hotel_cost : board -> prop_name -> int option
  val is_buildable : board -> prop_name -> bool
  val is_buyable : board -> prop_name -> bool 
  val card_type : board -> card_name -> cardType
  val card_payment : board -> card_name -> int
end

module BoardCheck : BoardSig = Board

module type CommandSig = sig
  type object_phrase = string list
  type command = | Roll  | Quit | Wallet  | Inventory  | Buy  | Sell of object_phrase | Items | Auction of object_phrase  | Next | Build of object_phrase| Game 
  exception Empty
  exception Malformed
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig
  open Board
  exception Unbuildable of prop_name
  type t
  type property 
  type result = Legal of t | Illegal | Win
  val init_state : Board.board -> int -> t
  val current_player : t -> int
  val num_players : t -> int
  val locations : t -> (int * (int * bool)) list
  val doubles_rolled : t -> int
  val current_location : t -> int
  val inventories : t -> (int * Board.prop_name list) list
  val items : t -> (int * Board.card_name list) list 
  val wallets : t -> (int * int) list
  val total_assets : t -> (int * int) list 
  val buildings : t -> (prop_name * ( int * int)) list
  val cards : t -> card_name list 
  val update_state : t -> result -> t 
  val next_turn : t -> result 
  val roll : Board.board -> t -> result
  val curr_player_inventory : t -> prop_name list
  val curr_player_wallet : t -> int 
  val curr_player_items : t -> card_name list 
  val buy : Board.board -> prop_name -> t -> result 
  val sell : Board.board -> prop_name -> t -> result 
  val auction : prop_name -> t -> result 
  val inventory_value : Board.board -> t -> int 
  val wealthiest_player : Board.board -> t -> (int * int) list
  val earn_cash : t -> int -> result 
  val pay_rent : board -> prop_name -> t -> result
  val build_houses : board -> t -> prop_name -> int -> result
  val build_hotels : board -> t -> prop_name -> int -> result
  val card_action : board -> card_name -> t -> result
  val move_cards : board -> card_name -> t -> result
end

module StateCheck : StateSig = State

module type AuthorSig = sig
  val hours_worked : int
end

module AuthorCheck : AuthorSig = Author
