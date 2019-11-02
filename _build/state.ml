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
  items : (player * Board.prop_name list) list;
  wallets : (player * int) list;
  total_assets : (player * int) list;
}

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

type result = Legal of t | Illegal