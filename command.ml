(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Roll (* Used to initiate dice roll to move around the board on your turn. *)
  | Quit (* Used to end the game -> will prompt player to confirm. *)
  | Wallet (* Used to see current liquid assets of the player ($$$). *)
  | Inventory (* Used to see current properties that the player owns. *)
  | Buy (* Used to purchase a property that you are currently on. *) 
  | Sell of object_phrase (* Used to sell any properties in the current inventory. *)
  | Items (* Used to see what special cards the player holds. *)
  | Auction of object_phrase (* Used to participate in property auctions. *)
  | Next 
  | Build of object_phrase
  | Game 

exception Empty

exception Malformed

let format_command lst = 
  match lst with 
  | [] -> raise (Empty)
  | h::t when h = "roll" -> if t = [] then Roll else raise (Malformed)
  | h::t when h = "quit" -> if t = [] then Quit else raise (Malformed)
  | h::t when h = "wallet" -> if t = [] then Wallet else raise (Malformed)
  | h::t when h = "inventory" -> if t = [] then Inventory else raise (Malformed)
  | h::t when h = "buy" -> if t = [] then Buy else raise (Malformed)
  | h::t when h = "sell" -> if t = [] then raise (Malformed) else Sell t
  | h::t when h = "items" -> if t = [] then Items else raise (Malformed)
  | h::t when h = "auction" -> if t = [] then raise (Malformed) else Auction t
  | h::t when h = "next" -> if t = [] then Next else raise (Malformed)
  | h::t when h = "build" -> if t = [] then raise (Malformed) else Build t
  | h::t when h = "game" -> if t = [] then Game else raise Malformed
  | _ -> raise (Malformed)

let parse str = 
  let comm = String.trim str in 
  let comm_list = String.split_on_char ' ' comm in 
  let filtered_list = List.filter (fun x -> (x = "") = false) comm_list in 
  format_command filtered_list