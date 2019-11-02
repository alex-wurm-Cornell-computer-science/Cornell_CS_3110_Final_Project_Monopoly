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

exception Empty

exception Malformed

let parse str =
  let parse_lst = 
    List.filter (fun x -> (x = "") = false) (String.split_on_char ' ' str) 
  in 
  let command =
    match parse_lst with
    | [] -> raise (Empty)
    | h::t -> if h = "roll" || h = "quit" || h = "wallet" || h = "inventory" ||
      h = "buy" || h = "sell" || h = "items" || h = "auction" then h 
      else raise (Malformed)
  in 
  let words =
    match parse_lst with
    | [] -> []
    | h::t -> t
  in 

  if command = "roll" && words = [] then Roll else
  if command = "quit" && words = [] then Quit else
  if command = "wallet" && words = [] then Wallet else
  if command = "inventory" && words = [] then Inventory else
  if command = "buy" && words = [] then Buy else
  if command = "sell" && words != [] then Sell words else
  if command = "items" && words = [] then Items else
  if command = "auction" && words != [] then Auction words else
    raise (Malformed)