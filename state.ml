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
  in_jail : (int * bool) list 
}

type result = Legal of t | Illegal | Win

(**  [init_lists n v acc] is the association list of length [n] where the keys 
     are integers 1..n and the values are [v] *)
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
    in_jail = init_lists (n) false []
  } 

let current_player st =
  st.curr_player

let num_players st = 
  st.num_players

let locations st = 
  st.locations

let doubles_rolled st =
  st.doubles_rolled

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

let in_jail st =
  st.in_jail

let is_in_jail st = 
  List.assoc (current_player st) (in_jail st)

let current_location st = 
  fst (List.assoc (current_player st) (locations st))

let curr_player_inventory st =  
  List.assoc (current_player st) (inventories st)

let curr_player_wallet st = 
  let curr_player = current_player st in
  let total_wallets = wallets st in 
  List.assoc curr_player total_wallets

let curr_player_items st = 
  let curr_player = current_player st in
  let total_items = items st in 
  List.assoc curr_player total_items

let current_status st = 
  List.assoc (current_player st) (player_status st)

let update_state old_st res = 
  match res with 
  | Illegal -> old_st 
  | Legal t -> t 
  | Win -> old_st

let rec next_turn st = 
  let curr_loc = List.assoc (current_player st) (locations st) in 
  let curr_status = List.assoc (current_player st) (player_status st) in 
  if curr_status then (
    if snd curr_loc then (
      let trimmed = List.remove_assoc (current_player st) (locations st) in 
      let new_loc = ((current_player st), (fst curr_loc,false))::trimmed in 
      Legal {st with 
             curr_player = ((current_player st) mod (num_players st)) + 1;
             locations = new_loc;
             doubles_rolled = 0;
            }
    ) else (
      Illegal
    )
  ) else if (not curr_status) then (
    let next_st = {st with 
                   curr_player = ((current_player st) mod (num_players st)) + 1;
                   doubles_rolled = 0;
                  } 
    in 
    next_turn next_st
  ) else (
    Illegal
  )

let roll brd st = 
  let die1 = (Random.int 5) + 1 in 
  let die2 = (Random.int 5) + 1 in
  (* let die1 = 3 in 
     let die2 = 3 in  *)
  (* let die1 = 0 in 
     let die2 = 1 in  *)
  let curr_player = current_player st in 
  let total_loc = locations st in 
  let total_status = player_status st in 
  let curr_loc = List.assoc curr_player total_loc in 
  let trimmed = List.remove_assoc curr_player total_loc in
  if snd curr_loc = false then (
    if die1 = die2 then (
      if (doubles_rolled st >= 2 || 
          (nth_square brd (current_location st) =  "Go To Jail")) then (
        let new_loc = (curr_player, (square_pos brd "Jail",true))::trimmed in 
        let old_jail = List.remove_assoc curr_player (in_jail st) in 
        let new_jail = (curr_player, true) :: old_jail in 
        Legal { st with 
                locations = new_loc;
                doubles_rolled = doubles_rolled st + 1;
                player_status = total_status; 
                in_jail = new_jail
              }
      ) else (
        let new_loc = ((fst curr_loc + die1 + die2) mod Board.size brd) in 
        let now_in_jail = nth_square brd (current_location st) = "Go To Jail" in 
        let new_loc_lst = (curr_player, (new_loc,now_in_jail))::trimmed in 
        let old_jail = List.remove_assoc curr_player (in_jail st) in 
        let new_jail = (curr_player,now_in_jail) :: old_jail in 
        Legal {st with 
               locations = new_loc_lst;
               doubles_rolled = doubles_rolled st + 1;
               player_status = total_status;
               in_jail = new_jail
              }
      )
    ) else (
      let new_loc, new_jail = 
        if is_in_jail st 
        then ((fst curr_loc) mod Board.size brd), (in_jail st)
        else (
          if (square_type brd (nth_square brd ((fst curr_loc + die1 + die2) 
                                               mod Board.size brd)) = GoToJail) 
          then 
            let old_jail = List.remove_assoc curr_player (in_jail st) in
            let new_jail = (curr_player, true) :: old_jail in 
            Board.square_pos brd "Jail" , new_jail
          else ((fst curr_loc + die1 + die2) mod Board.size brd), (in_jail st)) 
      in 
      let new_loc_lst = (curr_player, (new_loc,true))::trimmed in 
      Legal {st with 
             locations = new_loc_lst;
             doubles_rolled = 0;
             player_status = total_status;
             in_jail = new_jail
            })
  ) else Illegal

let houses st prop = 
  try 
    List.find (fun s -> (fst s) = prop) (buildings st) |> snd |> fst
  with 
  | Not_found -> 0

let hotels st prop = 
  try 
    List.find (fun s -> (fst s) = prop) (buildings st) |> snd |> snd
  with 
  | Not_found -> 0

let prop_available prop st = 
  let all_owned = List.fold_left (fun acc (a,b) -> b @ acc) []
      (inventories st) in 
  not (List.mem prop all_owned) 

(** [enough_funds prop st] returns true if the current player has enough money 
    to buy [prop] in [bd] *)
let enough_funds bd prop st = 
  let price = cost bd prop in 
  (List.assoc (current_player st) (wallets st)) > price

let inventory_value brd st = 
  let prop_value p = 
    if is_buildable brd p then 
      let num_houses = houses st p in
      let house_costs = match (Board.house_cost brd p) with
        | Some i -> i * num_houses 
        | None -> 0
      in 
      let num_hotels = hotels st p in
      let hotel_costs = match (Board.hotel_cost brd p) with 
        | Some i -> i * num_hotels
        | None -> 0
      in
      (Board.cost brd p) + house_costs + hotel_costs else 
      cost brd p
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
      let next_st = {st' with inventories = t} in 
      let xs = max_wealth brd next_st in 
      if curr_wealth > snd (List.hd xs) 
      then (current_player st',curr_wealth) :: []
      else if curr_wealth < snd (List.hd xs) then xs
      else (current_player st',curr_wealth) :: xs
  in
  max_wealth brd st

let earn_cash st amt =
  let curr_cash = List.assoc (current_player st) (wallets st) in 
  let trimmed = List.remove_assoc (current_player st) (wallets st) in 
  let inc_cash = curr_cash + amt in 
  let new_cash = ((current_player st),inc_cash)::trimmed in 
  if inc_cash > 2000 then Win else
    Legal {st with wallets = new_cash;} 

let buy bd prop st = 
  try 
    let can_buy = is_buyable bd prop in 
    if can_buy && not (is_in_jail st) then 
      if (prop_available prop st) && (enough_funds bd prop st) then
        match (current_location st = square_pos bd prop) with 
        | false -> Illegal 
        | true -> begin 
            match (earn_cash st (-(cost bd prop))) with 
            | Legal st' ->  
              let curr_invent = List.assoc (current_player st') 
                  (inventories st') in 
              let trimmed = List.remove_assoc (current_player st') 
                  (inventories st') in 
              let new_inv = ((current_player st'), prop::curr_invent)::trimmed 
              in
              if List.length (List.assoc (current_player st') new_inv) > 10
              then Win else
                Legal {st' with inventories = new_inv;}
            | Illegal -> Illegal
            | Win -> Win
          end
      else Illegal
    else Illegal
  with
    UnknownSquare prop -> Illegal

let sell bd prop st = 
  if is_buyable bd prop && not (is_in_jail st) then
    if (List.mem prop (List.assoc (current_player st) (inventories st))) then 
      match (earn_cash st (cost bd prop)) with 
      | Legal st' -> 
        let curr_invent = List.assoc (current_player st) (inventories st) in 
        let trimmed = List.remove_assoc (current_player st) (inventories st) in 
        let new_inv = ((current_player st), 
                       List.filter (fun p -> p <> prop) curr_invent)::trimmed 
        in 
        Legal {st' with 
               curr_player = (current_player st);
               inventories = new_inv;
               buildings = buildings st;
               cards = cards st;
               player_status = player_status st; 
               in_jail = in_jail st;
              }
      | _ -> Illegal 
    else Illegal
  else Illegal

let pay_rent bd prop st =  
  let lst = inventories st in 
  let rec owner prop lst =
    match lst with
    | [] -> 0
    | h::t -> if List.mem prop (snd h) then fst h else owner prop t
  in
  let pay_to = owner prop lst in
  if pay_to = 0 || pay_to = (current_player st) then Legal st else 
    let pay_amt = (rent bd prop) + 
                  ((rent bd prop) * ((houses st prop) + (hotels st prop))) in
    match earn_cash st (-pay_amt) with 
    | Legal st' -> 
      let total_cash = wallets st' in 
      let curr_cash = List.assoc pay_to total_cash in 
      let trimmed = List.remove_assoc pay_to total_cash in 
      let new_cash = (pay_to,curr_cash + pay_amt)::trimmed in 
      Legal {st' with 
             wallets = new_cash;
             in_jail = (in_jail st) 
            } 
    | _ -> Illegal

let build_houses bd st prop n  = 
  if is_buildable bd prop  && not (is_in_jail st) then 
    let monopoly_group = monopoly_group_named bd prop in
    let player_prps = List.assoc (current_player st) (inventories st) in     
    if List.for_all (fun s -> List.mem s player_prps) monopoly_group then  
      let house_cost = match house_cost bd prop with 
        | Some v -> v * n
        | None -> raise (Unbuildable prop) in
      if not (List.assoc (current_player st) (wallets st) >= n * house_cost) 
      then Illegal else       
        let curr_houses = houses st prop in 
        if (curr_houses + n) <= 3 then 
          let curr_hotels = hotels st prop in 
          let trimmed = List.remove_assoc prop (buildings st) in 
          let new_buildings = (prop, (curr_houses+n, curr_hotels))::trimmed in 
          let st' = {st with buildings = new_buildings;} in 
          earn_cash st' (-n * house_cost) 
        else Illegal
    else Illegal
  else Illegal

let build_hotels bd st prop n  = 
  if is_buildable bd prop && not (is_in_jail st) then 
    if houses st prop = 3 then 
      let hotel_cost = match (hotel_cost bd prop) with 
        | Some v -> n * v 
        | None -> raise (Unbuildable prop) in  
      if not (List.assoc (current_player st) (wallets st) >= n * hotel_cost) 
      then Illegal else
        let curr_hotels = hotels st prop in
        if (curr_hotels + n) <= 3 then 
          let curr_houses = houses st prop in 
          let trimmed = List.remove_assoc prop (buildings st) in 
          let new_buildings = (prop, (curr_houses-3, curr_hotels+n))::trimmed in 
          let st' = {st with buildings = new_buildings;} in 
          match house_cost bd prop with 
          | None -> raise (UnknownCard prop)
          | Some i -> 
            earn_cash st' ((-n * hotel_cost) + 3 * i) 
        else Illegal
    else Illegal
  else Illegal

let card_action bd cd st = 
  match card_type bd cd with 
  | Money -> earn_cash st (card_payment bd cd)
  | Location -> 
    let new_loc = card_payment bd cd in 
    let curr_loc = List.assoc (current_player st) (locations st) in 
    let trimmed = List.remove_assoc (current_player st) (locations st) in
    let new_locations = ((current_player st), (new_loc,false)) :: trimmed in
    let jail = 
      if new_loc |> nth_square bd |> square_type bd = Jail 
      then 
        let old_jail = List.remove_assoc (current_player st) (in_jail st) 
        in 
        ((current_player st), true) :: old_jail 
      else (in_jail st) in 
    let st' = {st with 
               locations = new_locations;
               in_jail = jail
              } 
    in 
    if fst curr_loc > new_loc then earn_cash st' 200 else Legal st'
  | LeaveJail -> 
    let curr_items = List.assoc (current_player st) (items st) in 
    let trimmed = List.remove_assoc (current_player st) (items st) in 
    let new_items = ((current_player st), cd :: curr_items) :: trimmed in 
    let old_jail = List.remove_assoc (current_player st) (in_jail st) in 
    let new_jail = ((current_player st), false) :: old_jail in 
    Legal {st with 
           items = new_items;
           in_jail = new_jail
          }

let move_cards brd crd st = 
  let trimmed = List.filter (fun s -> s <> crd) (cards st) in 
  let new_cards = trimmed @ [crd] in 
  match (card_type brd crd) with 
  | Money | Location -> Legal {st with cards = new_cards;}
  | LeaveJail -> 
    let curr_invent = List.assoc (current_player st) (items st) in 
    let trimmed = List.remove_assoc (current_player st) (items st) in 
    let new_invent = crd :: curr_invent in 
    let new_items = ((current_player st), new_invent) :: trimmed in 
    let new_cards = List.filter (fun s -> s <> crd) (cards st) in
    Legal {st with 
           items = new_items;
           cards = new_cards;
          }

let next_card st = 
  List.hd (cards st)

(** [has_jail_card st] returns [Some crd] if the current player has a get out 
    of jail card, where crd is the name of the card and None otherwise.  *)
let has_jail_card brd st =  
  try  
    Some (List.find (fun card -> card_type brd card = LeaveJail) 
            (curr_player_items st));
  with 
  | _ -> None

let get_out_of_jail brd st = 
  match current_location st |> nth_square brd |> square_type brd  with 
  | Jail -> begin 
      match (has_jail_card brd st) with 
      | None -> Illegal
      | Some card -> 
        let new_loc = (current_location st |> nth_square brd |> 
                       square_pos brd) + 1 in 
        let trimmed_locs = List.remove_assoc ((current_player st)) 
            (locations st) in 
        let loc_list = ((current_player st), (new_loc, true))::trimmed_locs in 
        let pl_items =  List.filter (fun c -> c <> card) 
            (curr_player_items st) in 
        let trimmed_items = List.remove_assoc (current_player st) (items st) in 
        let new_items = ((current_player st), pl_items) :: trimmed_items in 
        let new_cards = (cards st) @ [card] in 
        let old_jail = List.remove_assoc ((current_player st)) (in_jail st) in 
        let new_jail = ((current_player st), false) :: old_jail in 
        Legal {st with 
               locations = loc_list;
               items = new_items;
               cards = new_cards;
               in_jail = new_jail
              }  
    end 
  | _ -> Illegal

let pay_tax brd st n = 
  let sq_name = current_location st |> nth_square brd in 
  let trimmed = List.remove_assoc (current_player st) (wallets st) in 
  let new_money = (curr_player_wallet st) - (n *(rent brd sq_name)) in 
  let new_wallets = ((current_player st), new_money) :: trimmed in 
  Legal {st with wallets = new_wallets;}