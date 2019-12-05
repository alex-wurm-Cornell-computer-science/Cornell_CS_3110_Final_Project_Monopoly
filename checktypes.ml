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
  type command = Go of object_phrase | Quit | Score | Take of object_phrase | Drop of object_phrase | Inventory
  exception Empty
  exception Malformed
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig
  type t 
  val init_state : Adventure.t -> t
  val current_room_id : t -> string
  val visited : t -> string list
  type result = Legal of t | Illegal
  val go : Adventure.exit_name -> Adventure.t -> t -> result
end

module StateCheck : StateSig = State

module type AuthorSig = sig
  val hours_worked : int
end

module AuthorCheck : AuthorSig = Author
