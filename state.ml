open Board

exception Unbuildable of prop_name

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
  buildings : (prop_name * ( int * int)) list;
  cards : card_name list;
  player_status : (int * bool) list;
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
    wallets = init_lists (n) 1500 [];
    total_assets = init_lists (n) 0 [];
    buildings = [];
    cards = cards brd;
    player_status = init_lists (n) true [];
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

let cards st =
  st.cards

let player_status st = 
  st.player_status

let current_status st = 
  List.assoc (current_player st) (player_status st)

let update_state old_st res = 
  match res with 
  | Illegal -> old_st 
  | Legal t -> t 
  | Win -> old_st

let rec next_turn st = 
  let curr_player = current_player st in 
  let total_loc = locations st in 
  let total_status = player_status st in 
  let curr_loc = List.assoc curr_player total_loc in 
  let curr_status = List.assoc curr_player total_status in 
  if (curr_status) then (
    if (snd curr_loc) then (
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
        cards = cards st;
        player_status = player_status st; 
      }
    ) else (
      Legal st
    )
  ) else if (not curr_status) then (
    let next_st = {
      curr_player = ((current_player st) mod (num_players st)) + 1;
      num_players = num_players st;
      locations = locations st;
      doubles_rolled = 0;
      inventories = inventories st;
      items = items st;
      wallets = wallets st;
      total_assets = total_assets st;
      buildings = buildings st;
      cards = cards st;
      player_status = player_status st; 
    } in 
    next_turn next_st
  ) else (
    Illegal
  )

let roll brd st = 
  let die1 = (Random.int 5) + 1 in 
  let die2 = (Random.int 5) + 1 in 
  (* let die1 = 3 in 
     let die2 = 3 in  *)
  let curr_player = current_player st in 
  let total_loc = locations st in 
  let total_status = player_status st in 
  (* let new_status = if (curr_player = 1) then (curr_player,false)::(List.remove_assoc curr_player total_status) else total_status in  *)
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
          cards = cards st;
          player_status = total_status; 
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
          cards = cards st;
          player_status = total_status;
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
        cards = cards st;
        player_status = total_status;
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

let inventory_value brd st = 
  let prop_value p = 
    let num_houses = fst (List.assoc p (buildings st)) in
    let house_costs = match (Board.house_cost brd p) with
      | Some i -> i * num_houses 
      | None -> 0
    in 
    let num_hotels = snd (List.assoc p (buildings st)) in
    let hotel_costs = match (Board.hotel_cost brd p) with 
      | Some i -> i * num_hotels
      | None -> 0
    in
    (Board.cost brd p) + house_costs + hotel_costs
  in

  let inv_values = List.map prop_value (curr_player_inventory st) in
  List.fold_left (fun acc x -> acc + x) 0 inv_values

let wealthiest_player brd st = 
  let rec max_wealth brd st =
    let inv_list = inventories st in 
    match inv_list with
    | [] -> [(0,0)]
    | (p,invs)::t -> let st' = {st with curr_player = p} in 
      let curr_wallet = curr_player_wallet st' in 
      let curr_props = inventory_value brd st' in 
      let curr_wealth = curr_wallet + curr_props in 
      let next_st = {st with inventories = t} in 
      let xs = max_wealth brd next_st in 
      if curr_wealth > snd (List.hd xs) then (current_player st',curr_wealth) :: []
      else if curr_wealth < snd (List.hd xs) then xs
      else (current_player st',curr_wealth) :: xs
  in
  max_wealth brd st

let earn_cash st amt =
  let curr_cash = List.assoc st.curr_player st.wallets in 
  let trimmed = List.remove_assoc st.curr_player st.wallets in 
  let inc_cash = curr_cash + amt in 
  let new_cash = (st.curr_player,inc_cash)::trimmed in 
  if inc_cash > 2000 then Win else
    Legal {
      curr_player = st.curr_player;
      num_players = num_players st;
      locations = locations st;
      doubles_rolled = doubles_rolled st;
      inventories = inventories st;
      items = items st;
      wallets = new_cash;
      total_assets = total_assets st;
      buildings = buildings st;
      cards = cards st;
      player_status = player_status st; 
    } 

let buy bd prop st = 
  if is_buyable bd prop then 
    if (prop_available prop st) && (enough_funds bd prop st) then
      match (current_location st = square_pos bd prop) with 
      | false -> Illegal 
      | true -> begin 
          match (earn_cash st (-(cost bd prop))) with 
          | Legal st' ->  
            let curr_invent = List.assoc st'.curr_player st'.inventories in 
            let trimmed = List.remove_assoc st'.curr_player st'.inventories in 
            let new_inv = (st'.curr_player, prop ::curr_invent) :: trimmed in 
            if List.length (List.assoc st'.curr_player new_inv) > 1 then Win else
              Legal {
                curr_player = st.curr_player;
                num_players = num_players st';
                locations = locations st';
                doubles_rolled = doubles_rolled st;
                inventories = new_inv;
                items = items st';
                wallets = wallets st';
                total_assets = total_assets st';
                buildings = st.buildings;
                cards = cards st;
                player_status = player_status st; 
              }
          | Illegal -> Illegal
          | Win -> Win
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
          buildings = st.buildings;
          cards = cards st;
          player_status = player_status st; 
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
  if pay_to = 0 || pay_to = (current_player st) then Legal st else 
    match earn_cash st ((-1) * ((rent bd prop) + (rent bd prop) * ((houses st prop)) + (hotels st prop))) with 
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
        buildings = st.buildings;
        cards = cards st;
        player_status = player_status st; 
      } ;

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
            buildings = new_buildings;
            cards = cards st;
            player_status = player_status st; 
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
            buildings = new_buildings;
            cards = cards st;
            player_status = player_status st; 
          } in 
          match house_cost bd prop with 
          | None -> raise (UnknownCard prop)
          | Some i -> 
            earn_cash st1 ((-n * hotel_cost) + 3 * i) else Illegal

    else Illegal
  else Illegal


let card_action bd cd st = 
  match card_type bd cd with 
  | Money -> earn_cash st (card_payment bd cd)
  | Location -> 
    let new_loc = card_payment bd cd in 
    let curr_loc = List.assoc st.curr_player st.locations in 
    let trimmed = List.remove_assoc st.curr_player st.locations in
    let new_locations = (st.curr_player, (new_loc,false)) :: trimmed in 
    let st1 = {
      curr_player = st.curr_player;
      num_players = num_players st;
      locations = new_locations;
      inventories = inventories st;
      doubles_rolled = st.doubles_rolled;
      items = items st;
      wallets = wallets st;
      total_assets = total_assets st;
      buildings = st.buildings;
      cards = cards st;
      player_status = player_status st; 
    } in 
    if fst curr_loc > new_loc then earn_cash st1 200 else Legal st1
  | LeaveJail -> 
    let curr_items = List.assoc st.curr_player st.items in 
    let trimmed = List.remove_assoc st.curr_player st.items in 
    let new_items = (st.curr_player, cd :: curr_items) :: trimmed in 
    Legal {
      curr_player = st.curr_player;
      num_players = num_players st;
      locations = locations st;
      inventories = inventories st;
      doubles_rolled = st.doubles_rolled;
      items = new_items;
      wallets = wallets st;
      total_assets = total_assets st;
      buildings = st.buildings;
      cards = cards st;
      player_status = player_status st; 
    }

let move_cards brd crd st = 
  let trimmed = List.filter (fun s -> s <> crd) st.cards in 
  let new_cards = trimmed @ [crd] in 
  match (card_type brd crd) with 
  | Money | Location -> 
    Legal {
      curr_player = st.curr_player;
      num_players = num_players st;
      locations = locations st;
      inventories = inventories st;
      doubles_rolled = st.doubles_rolled;
      items = st.items;
      wallets = wallets st;
      total_assets = total_assets st;
      buildings = st.buildings;
      cards = new_cards;
      player_status = player_status st; 
    }
  | LeaveJail -> 
    let curr_invent = List.assoc st.curr_player st.items in 
    let trimmed = List.remove_assoc st.curr_player st.items in 
    let new_invent = crd :: curr_invent in 
    let new_items = (st.curr_player, new_invent) :: trimmed in 
    Legal {
      curr_player = st.curr_player;
      num_players = num_players st;
      locations = locations st;
      inventories = inventories st;
      doubles_rolled = st.doubles_rolled;
      items = new_items;
      wallets = wallets st;
      total_assets = total_assets st;
      buildings = st.buildings;
      cards = cards st;
      player_status = player_status st; 
    }


