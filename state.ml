(* Note: You may introduce new code anywhere in this file. *) 

open Board


exception Unbuildable of prop_name

(* type player = int *)

type property = {
  name : prop_name;
  houses : int;
  hotels : int;
}

type t = {
  curr_player : int;
  num_players : int;
  locations : (int * (int * bool)) list;
  doubles_rolled : int;
  inventories : (int * prop_name list) list;
  items : (int * prop_name list) list;
  wallets : (int * int) list;
  total_assets : (int * int) list;
  buildings : (prop_name * ( int * int)) list 
}

type result = Legal of t | Illegal | Win

let rec init_lists n v acc =
  match n with 
  | 0 -> acc 
  | _ -> init_lists (n-1) v ((n,v)::acc) 

let init_state brd n =
  {
    curr_player = 1;
    num_players = n;
    locations = init_lists (n) (0,false) [];
    doubles_rolled = 0;
    inventories = init_lists (n) [] [];
    items = init_lists (n) [] [];
    wallets = init_lists (n) 0 [];
    total_assets = init_lists (n) 0 [];
    buildings = []
  } 

let current_player st =
  st.curr_player

let num_players st = 
  st.num_players

let locations st = 
  st.locations

let doubles_rolled st =
  st.doubles_rolled

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

let buildings st =
  st.buildings

let update_state old_st res = 
  match res with 
  | Illegal -> old_st 
  | Legal t -> t 
  | Win -> old_st

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
      doubles_rolled = 0;
      inventories = inventories st;
      items = items st;
      wallets = wallets st;
      total_assets = total_assets st;
      buildings = buildings st;
    }
  ) else Legal st 

let roll brd st = 
  let die1 = (Random.int 5) + 1 in 
  let die2 = (Random.int 5) + 1 in 
  (* let die1 = 3 in 
     let die2 = 3 in  *)
  let curr_player = current_player st in 
  let total_loc = locations st in 
  let curr_loc = List.assoc curr_player total_loc in 
  let trimmed = List.remove_assoc curr_player total_loc in
  if snd curr_loc = false then (
    if die1 = die2 then (
      if (doubles_rolled st = 2) then (
        let new_loc = (curr_player, (Board.square_pos brd "Jail",true))::trimmed in 
        Legal {
          curr_player = curr_player;
          num_players = num_players st;
          locations = new_loc;
          inventories = inventories st;
          doubles_rolled = doubles_rolled st + 1;
          items = items st;
          wallets = wallets st;
          total_assets = total_assets st;
          buildings = buildings st;
        }) else (
        let new_loc = if (nth_square brd ((fst curr_loc + die1 + die2) mod Board.size brd) = "Go To Jail") 
          then Board.square_pos brd "Jail" else ((fst curr_loc + die1 + die2) mod Board.size brd) in 
        let new_loc_lst = (curr_player, (new_loc,false))::trimmed in 
        Legal {
          curr_player = curr_player;
          num_players = num_players st;
          locations = new_loc_lst;
          inventories = inventories st;
          doubles_rolled = doubles_rolled st + 1;
          items = items st;
          wallets = wallets st;
          total_assets = total_assets st;
          buildings = buildings st;
        }
      )
    ) else (
      let new_loc = if (nth_square brd ((fst curr_loc) mod Board.size brd) = "Jail") then ((fst curr_loc) mod Board.size brd)
        else if (nth_square brd ((fst curr_loc + die1 + die2) mod Board.size brd) = "Go To Jail") 
        then Board.square_pos brd "Jail" 
        else ((fst curr_loc + die1 + die2) mod Board.size brd) in 
      let new_loc_lst = (curr_player, (new_loc,true))::trimmed in 
      Legal {
        curr_player = curr_player;
        num_players = num_players st;
        locations = new_loc_lst;
        inventories = inventories st;
        doubles_rolled = 0;
        items = items st;
        wallets = wallets st;
        total_assets = total_assets st;
        buildings = buildings st;
      })
  ) else Illegal

let houses st prop = 
  try 
    List.find (fun s -> (fst s) = prop) (buildings st) |> snd |> fst
  with 
  | Not_found -> raise (UnknownSquare prop)

let hotels st prop : int = 
  try 
    List.find (fun s -> (fst s) = prop) (buildings st) |> snd |> snd
  with 
  | Not_found -> raise (UnknownSquare prop)

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

(** [prop_available prop st] returns false if [prop] is already owned *)
let prop_available prop st = 
  let all_owned = List.fold_left (fun acc (a,b) -> b @ acc) [] (inventories st) in 
  not (List.mem prop all_owned) 

(** [enough_funds prop st] returns true if the current player has enough money 
    to buy [prop] in [bd] *)
let enough_funds bd prop st = 
  let price = cost bd prop in (List.assoc (current_player st) (wallets st)) > price

let auction prop st = 
  failwith ("Unimplemented")

let earn_cash st amt =
  let curr_player = current_player st in 
  let total_cash = wallets st in 
  let curr_cash = List.assoc curr_player total_cash in 
  let trimmed = List.remove_assoc curr_player total_cash in 
  let inc_cash = curr_cash + amt in 
  let new_cash = (curr_player,inc_cash)::trimmed in 
  if inc_cash > 500 then Win else
    Legal {
      curr_player = curr_player;
      num_players = num_players st;
      locations = locations st;
      doubles_rolled = doubles_rolled st;
      inventories = inventories st;
      items = items st;
      wallets = new_cash;
      total_assets = total_assets st;
      buildings = buildings st;
    } 

let buy bd prop st = 
  if is_buyable bd prop then 
    if (prop_available prop st) && (enough_funds bd prop st) then 
      match (current_location st = square_pos bd prop) with 
      | false -> Illegal 
      | true -> begin 
          match (earn_cash st ((-1) *(cost bd prop))) with 
          | Legal st' -> 
            let curr_invent = List.assoc st.curr_player st.inventories in 
            let trimmed = List.remove_assoc st.curr_player st.inventories in 
            let new_inv = (st.curr_player, prop ::curr_invent) :: trimmed in 
            if List.length new_inv > 1 then Win else
              Legal {
                curr_player = st.curr_player;
                num_players = num_players st';
                locations = locations st';
                doubles_rolled = doubles_rolled st;
                inventories = new_inv;
                items = items st';
                wallets = wallets st';
                total_assets = total_assets st';
                buildings = st.buildings
              }
          | _ -> Illegal
        end
    else Illegal
  else Illegal

let sell bd prop st = 
  if is_buyable bd prop then 
    if (List.mem prop (List.assoc st.curr_player st.inventories)) then 
      match (earn_cash st (cost bd prop)) with 
      | Legal st' -> 
        let curr_invent = List.assoc st.curr_player st.inventories in 
        let trimmed = List.remove_assoc st.curr_player st.inventories in 
        let new_inv = (st.curr_player, List.filter (fun p -> p <> prop) curr_invent) 
                      :: trimmed in 
        Legal {
          curr_player = st.curr_player;
          num_players = num_players st';
          locations = locations st';
          doubles_rolled = doubles_rolled st';
          inventories = new_inv;
          items = items st';
          wallets = wallets st';
          total_assets = total_assets st';
          buildings = st.buildings
        }
      | _ -> Illegal 

    else Illegal
  else Illegal


let pay_rent bd prop st =  
  let lst = inventories st in 
  let rec owner prop lst =
    match lst with
    | [] -> 0
    | h::t -> if List.mem prop (snd h) then fst h else  
        owner prop t
  in
  let pay_to = owner prop lst in
  if pay_to = 0 then Legal st else 
    match earn_cash st ((-1) * ((rent bd prop) + (rent bd prop) * (houses st prop)/2)) with 
    | Legal st1 -> 
      let total_cash = wallets st in 
      let curr_cash = List.assoc pay_to total_cash in 
      let trimmed = List.remove_assoc pay_to total_cash in 
      let new_cash = (pay_to,curr_cash + (rent bd prop))::trimmed in 
      Legal {
        curr_player = st.curr_player;
        num_players = num_players st;
        locations = locations st;
        doubles_rolled = doubles_rolled st;
        inventories = inventories st;
        items = items st;
        wallets = new_cash;
        total_assets = total_assets st;
        buildings = st.buildings
      } 
    | _ -> Illegal

let build_houses bd st prop n  = 
  if is_buildable bd prop then 
    let monopoly_group = monopoly_group bd prop in 
    let player_prps = List.assoc st.curr_player st.inventories in 
    if List.for_all (fun s -> List.mem s player_prps) monopoly_group then 
      let house_cost = match house_cost bd prop with 
        | Some v -> v * n
        | None -> raise (Unbuildable prop) in
      if not (List.assoc st.curr_player st.wallets >= house_cost) then Illegal else
        let curr_houses = List.assoc prop st.buildings |> fst in 
        if (curr_houses + n) <= 3 then 
          let curr_hotels = List.assoc prop st.buildings |> snd in 
          let trimmed = List.remove_assoc prop st.buildings in 
          let new_buildings = (prop, (curr_houses+n, curr_hotels)) :: trimmed in 
          let st1 = {
            curr_player = st.curr_player;
            num_players = num_players st;
            locations = locations st;
            doubles_rolled = st.doubles_rolled;
            inventories = inventories st;
            items = items st;
            wallets = wallets st;
            total_assets = total_assets st;
            buildings = new_buildings
          } in 
          earn_cash st1 (-n * house_cost) 
        else Illegal
    else Illegal
  else Illegal

let build_hotels bd st prop n  = 
  if is_buildable bd prop then 
    if List.assoc prop st.buildings |> fst = 3 then 
      let hotel_cost = match (hotel_cost bd prop) with 
        | Some v -> n * v 
        | None -> raise (Unbuildable prop) in  
      if not (List.assoc st.curr_player st.wallets >= hotel_cost) then Illegal else
        let curr_hotels = List.assoc prop st.buildings |> snd in
        if (curr_hotels + n) <= 3 then 
          let curr_houses = List.assoc prop st.buildings |> fst in 
          let trimmed = List.remove_assoc prop st.buildings in 
          let new_buildings = (prop, (curr_houses-3, curr_hotels+n)) :: trimmed in 
          let st1 = {
            curr_player = st.curr_player;
            num_players = num_players st;
            locations = locations st;
            inventories = inventories st;
            doubles_rolled = st.doubles_rolled;
            items = items st;
            wallets = wallets st;
            total_assets = total_assets st;
            buildings = new_buildings
          } in 
          match house_cost bd prop with 
          | None -> raise (UnknownCard prop)
          | Some i -> 
            earn_cash st1 ((-n * hotel_cost) + 3 * i) else Illegal

    else Illegal
  else Illegal




