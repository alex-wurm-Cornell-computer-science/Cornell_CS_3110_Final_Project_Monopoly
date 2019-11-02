(* Note: You may introduce new code anywhere in this file. *) 

open Board

exception UnknownProperty of Board.prop_name

type player = int

type property = {
  name : Board.prop_name;
  houses : int;
  hotels : int;
}

type t = {
  curr_player : player;
  num_players : int;
  locations : (player * int) list;
  inventories : (player * property list) list;
  items : (player * Board.card list) list;
  wallets : (player * int) list;
  total_assets : (player * int) list;
}

type result = Legal of t | Illegal

let rec init_lists n v acc =
  match n with 
  | 0 -> acc 
  | _ -> init_lists (n-1) v ((n,v)::acc) 

let init_state brd n =
  {
    curr_player = 0;
    num_players = n;
    locations = init_lists (n-1) 0 [];
    inventories = init_lists (n-1) [] [];
    items = init_lists (n-1) [] [];
    wallets = init_lists (n-1) 0 [];
    total_assets = init_lists (n-1) 0 [];
  } 

let current_player st =
  st.curr_player

let num_players st = 
  st.num_players

let locations st = 
  st.locations

let inventories st =
  st.inventories

let items st = 
  st.items

let wallets st =
  st.wallets

let total_assets st = 
  st.total_assets

let next_turn old_st res = 
  match res with 
  | Illegal -> old_st
  | Legal t -> {
      curr_player = (current_player old_st) + 1;
      num_players = num_players old_st;
      locations = locations old_st;
      inventories = inventories old_st;
      items = items old_st;
      wallets = wallets old_st;
      total_assets = total_assets old_st;
    }

let roll brd st = 
  let die1 = (Random.int 5) + 1 in 
  let die2 = (Random.int 5) + 1 in 
  let curr_player = current_player st in 
  let total_loc = locations st in 
  let curr_loc = List.assoc curr_player total_loc in 
  let trimmed = List.remove_assoc curr_player total_loc in 
  let new_loc = (curr_player, (curr_loc + die1 + die2) mod Board.size brd)::trimmed in 
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

let curr_player_inventory st = 
  let curr_player = current_player st in
  let total_inv = inventories st in 
  List.assoc curr_player total_inv

let disp_curr_player_inventory st = 
  List.iter (fun x -> print_string x.name) (curr_player_inventory st)

let curr_player_wallet st = 
  let curr_player = current_player st in
  let total_wallets = wallets st in 
  List.assoc curr_player total_wallets

let curr_player_items st = 
  let curr_player = current_player st in
  let total_items = items st in 
  List.assoc curr_player total_items

let disp_curr_player_items st = 
  failwith ("Need to get card names from board.ml")
(* List.iter (fun x -> print_string x.c_name) (curr_player_items st) *)

let buy st = 
  failwith ("Unimplemented")

let sell st = 
  failwith ("Unimplemented")

let auction st = 
  failwith ("Unimplemented")
