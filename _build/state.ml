(* Note: You may introduce new code anywhere in this file. *) 

open Board

(* type player = int *)

type property = {
  name : Board.prop_name;
  houses : int;
  hotels : int;
}

type t = {
  curr_player : int;
  num_players : int;
  locations : (int * (int * bool)) list;
  inventories : (int * property list) list;
  items : (int * Board.card list) list;
  wallets : (int * int) list;
  total_assets : (int * int) list;
}

type result = Legal of t | Illegal

let rec init_lists n v acc =
  match n with 
  | 0 -> acc 
  | _ -> init_lists (n-1) v ((n,v)::acc) 

let init_state brd n =
  {
    curr_player = 1;
    num_players = n;
    locations = init_lists (n) (0,false) [];
    inventories = init_lists (n) [] [];
    items = init_lists (n) [] [];
    wallets = init_lists (n) 0 [];
    total_assets = init_lists (n) 0 [];
  } 

let current_player st =
  st.curr_player

let num_players st = 
  st.num_players

let locations st = 
  st.locations

let current_location st = 
  fst (List.assoc (current_player st) (locations st))

let inventories st =
  st.inventories

let items st = 
  st.items

let wallets st =
  st.wallets

let total_assets st = 
  st.total_assets

let update_state old_st res = 
  match res with 
  | Illegal -> old_st 
  | Legal t -> t 

let next_turn st = 
  let curr_player = current_player st in 
  let total_loc = locations st in 
  let curr_loc = List.assoc curr_player total_loc in 
  if snd curr_loc then (
    let trimmed = List.remove_assoc curr_player total_loc in 
    let new_loc = (curr_player, (fst curr_loc,false))::trimmed in 
    Legal {
      curr_player = ((current_player st) mod (num_players st)) + 1;
      num_players = num_players st;
      locations = new_loc;
      inventories = inventories st;
      items = items st;
      wallets = wallets st;
      total_assets = total_assets st;
    }
  ) else Legal st 

let roll brd st = 
  let die1 = (Random.int 5) + 1 in 
  let die2 = (Random.int 5) + 1 in 
  let curr_player = current_player st in 
  let total_loc = locations st in 
  let curr_loc = List.assoc curr_player total_loc in 
  if snd curr_loc = false then (
    let trimmed = List.remove_assoc curr_player total_loc in 
    let new_loc = (curr_player, ((fst curr_loc + die1 + die2) mod Board.size brd,true))::trimmed in 
    match (List.mem_assoc curr_player new_loc) with 
    | false -> Illegal
    | true -> Legal {
        curr_player = curr_player;
        num_players = num_players st;
        locations = new_loc;
        inventories = inventories st;
        items = items st;
        wallets = wallets st;
        total_assets = total_assets st;
      }
  ) else Illegal

let curr_player_inventory st = 
  let curr_player = current_player st in
  let total_inv = inventories st in 
  List.assoc curr_player total_inv

let curr_player_wallet st = 
  let curr_player = current_player st in
  let total_wallets = wallets st in 
  List.assoc curr_player total_wallets

let curr_player_items st = 
  let curr_player = current_player st in
  let total_items = items st in 
  List.assoc curr_player total_items

let buy st = 
  failwith ("Unimplemented")

let sell st = 
  failwith ("Unimplemented")

let auction st = 
  failwith ("Unimplemented")

let earn_cash st amt =
  let curr_player = current_player st in 
  let total_cash = wallets st in 
  let curr_cash = List.assoc curr_player total_cash in 
  let trimmed = List.remove_assoc curr_player total_cash in 
  let new_cash = (curr_player,curr_cash + amt)::trimmed in 
  Legal {
    curr_player = curr_player;
    num_players = num_players st;
    locations = locations st;
    inventories = inventories st;
    items = items st;
    wallets = new_cash;
    total_assets = total_assets st;
  }