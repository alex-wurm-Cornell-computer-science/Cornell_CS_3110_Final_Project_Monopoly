(* Note: You may introduce new code anywhere in this file. *) 

open Board

exception UnknownProperty of Board.prop_name

type player = int

type t = {
  curr_player : player;
  locations : (player * int) list;
  inventories : (player * Board.prop_name list) list;
  items : (player * Board.card_name) list;
  wallets : (player * int) list;
  total_assets : (player * int) list;
}

let init_state adv =
  let state start =
    {
      curr_player = 0;
      visited = start::[];
      score = Adventure.room_score adv start;
      inventory = Adventure.items adv;
    } in 
  state (adv |> Adventure.start_room) 

let current_room_id st =
  st.curr

let visited st =
  st.visited

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