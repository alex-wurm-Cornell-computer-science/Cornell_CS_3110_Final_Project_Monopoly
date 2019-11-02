open Yojson.Basic.Util

type prop_name = string
exception UnknownSquare of prop_name


type squareType = 
  | Go
  | Jail 
  | Parking
  | GoToJail
  | Property
  | Chance
  | Chest
  | Tax 

type squareColor = 
  | Brown
  | LBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | DBlue
  | RR
  | Util


type card = 
  { 
    name : string;
    description : string;
    payment : int
  }

type square = { 
  name : string ; 
  cost : int ;
  color : squareColor option ;
  squareType : squareType ;
  owner : string option;
  rent : int
}


type board  =  {
  squares : square list;
  chance_cards : card list;
  chest_cards : card list
}


(** [uniq lst] is the set-like list composed of the elements from [lst] *)
let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

(** [parse_color col] parses [col] into a valid monopoly color *)
let parse_color col = 
  if col = "Brown" then Some Brown else 
  if col = "Light Blue" then Some LBlue else 
  if col = "Pink" then Some Pink else 
  if col = "Orange" then Some Orange else 
  if col = "Red" then Some Red else 
  if col = "Yellow" then Some Yellow else 
  if col = "Green" then Some Green else 
  if col = "Dark Blue" then Some DBlue else 
  if col = "RailRoad" then Some RR else 
  if col = "Utility" then Some Util else None

(** [parse_type s] parses [s] into a valid square type *)
let parse_type s = 
  if s = "GO" then Go else 
  if s = "Jail" then Jail else 
  if s = "Parking" then Parking else 
  if s = "GoToJail" then GoToJail else 
  if s = "Property" then Property else 
  if s = "Chance" then Chance else 
  if s = "Chest" then Chest else Tax

(**[square_of_json json] Parses [json] into a valid monopoly square *)
let square_of_json json= 
  { 
    name = json |> member "name" |> to_string;
    cost = json |> member "cost" |> to_string |> int_of_string;
    color = json |> member "color" |> to_string |> parse_color;
    owner = None;
    squareType = json |> member "type" |> to_string |> parse_type;
    rent = json |> member "rent" |> to_string |> int_of_string

  }

(**[card_of_json json] parses [json] into a card type *)
let card_of_json json = 
  { 
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    payment = json |> member "payment" |> to_string |> int_of_string
  }

let from_json j = 
  {
    squares = j |> member "squares" |> to_list |> List.map square_of_json;
    chance_cards = j |> member "chance cards" |> List.map card_of_json;
    chest_cards = j |> member "chest cards" |> List.map card_of_json;
  }


let cost (b : board) (prop : string) = 
  let rec cost' squares prop =
    match squares with 
    | [] -> raise (UnknownSquare prop)
    | h :: t -> if h.name = prop then h.cost else cost' t prop
  in cost' b.squares prop



(*
let start_room adv =
  adv.start_room

let room_ids adv = 
  let get_name room = room.id in
  uniq (map get_name adv.rooms)

let description adv room =
  let rooms_lst = List.filter (fun x -> x.id = room) adv.rooms in
  match rooms_lst with
  | [] -> raise (UnknownRoom room)
  | h::t -> h.description

let exits adv room = 
  let get_exit_name exit = exit.name in 
  let rooms_lst = List.filter (fun x -> x.id = room) adv.rooms in
  match rooms_lst with
  | [] -> raise (UnknownRoom room)
  | h::t -> uniq(map get_exit_name h.exits)

let next_room adv room ex =
  let rooms_lst = List.filter (fun x -> x.id = room) adv.rooms in
  let exit_lst = 
    match rooms_lst with
    | [] -> raise (UnknownRoom room)
    | h::t -> h.exits
  in 
  let does_it_exist = List.filter (fun x -> x.name = ex) exit_lst in 
  match does_it_exist with
  | [] -> raise (UnknownExit ex)
  | h::t -> h.id

let next_rooms adv room =
  let exits_lst = List.filter (fun x -> x.id = room) adv.rooms in
  let possible =
    match exits_lst with
    | [] -> raise (UnknownRoom room)
    | h::t -> h.exits
  in 
  let get_next_room (exit:exit) = exit.id in 
  uniq (map get_next_room possible)

let treasure_room adv = 
  adv.treasure_room

let win_msg adv = 
  adv.win_msg

let items adv =
  let get_item_info item = (item.item_name,item.room) in
  let items_lst = adv.items in 
  match items_lst with
  | [] -> []
  | h::t -> (get_item_info h)::(map get_item_info t)

let room_score adv room =
  let rooms_lst = List.filter (fun x -> x.id = room) adv.rooms in
  match rooms_lst with
  | [] -> raise (UnknownRoom room)
  | h::t -> h.score

let item_score adv item = 
  let items_lst = List.filter (fun x -> x.item_name = item) adv.items in 
  match items_lst with 
  | [] -> raise (UnknownItem item)
  | h::t -> h.score

  *)