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
  items : (player * Board.card_name) list;
  wallets : (player * int) list;
  total_assets : (player * int) list;
}

let rec init_lists n v acc =
  match n with 
  | 0 -> acc 
  | _ -> init_lists (n-1) v ((n*v)::acc) 

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

let go ex adv st =
  let exits = List.filter (fun x -> x = ex) (Adventure.exits adv st.curr) in
  match exits with
  | [] -> Illegal
  | h::t -> let curr = Adventure.next_room adv st.curr ex in 
    Legal {
      curr = curr;
      visited = curr::st.visited;
      score = (Adventure.room_score adv curr) + st.score;
      inventory = st.inventory;
    }

let score st =
  st.score

let get_current_room old_state result =
  match result with 
  | Legal t -> t.curr
  | Illegal -> old_state.curr

let update_state old_state result = 
  match result with
  | Legal t -> t
  | Illegal -> old_state

let print_inventory st room = 
  let items_lst =  List.filter (fun x -> snd x = room) st.inventory in
  if items_lst != []  then 
    String.concat ", " (List.map (fun x -> fst x) items_lst)
  else "Nothing ¯\\_(ツ)_/¯"

let get_inventory st =
  st.inventory

let update_items st adv item room = 
  let item_info = List.filter (fun x -> fst x = item) st.inventory in
  let updated_item = match item_info with
    | [] -> raise (UnknownItem item)
    | h::t -> (fst h, room)
  in
  let items_lst = List.filter (fun x -> ((fst x = item) = false) && ((snd x = Adventure.treasure_room adv) = false)) st.inventory in 
  updated_item::items_lst

let update_item_score st item adv = 
  let items_lst = List.filter (fun x -> fst x = item) st.inventory in
  match items_lst with 
  | [] -> raise (UnknownItem item)
  | h::t -> if st.curr = (Adventure.treasure_room adv) then (Adventure.item_score adv item) + st.score else st.score

let take item adv st = 
  let items_lst = List.filter (fun x -> fst x = item && snd x = st.curr) st.inventory in
  match items_lst with 
  | [] -> Illegal
  | h::t -> 
    Legal {
      curr = st.curr;
      visited = st.visited;
      score = update_item_score st item adv;
      inventory = update_items st adv item "inventory";
    }

let drop item adv st = 
  let items_lst = List.filter (fun x -> fst x = item && snd x = "inventory") st.inventory in
  match items_lst with 
  | [] -> Illegal
  | h::t -> 
    if st.curr = Adventure.treasure_room adv then (
      Legal {
        curr = st.curr;
        visited = st.visited;
        score = update_item_score st item adv;
        inventory = List.filter (fun x -> (fst x = item) = false) st.inventory;
      }) else (
      Legal {
        curr = st.curr;
        visited = st.visited;
        score = st.score;
        inventory = update_items st adv item st.curr;
      }
    )