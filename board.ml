open Yojson.Basic.Util

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
    payment : string
  }

type square = { 
  name : string ; 
  cost : int ;
  color : squareColor option ;
  squaretype : squareType ;
  owner : string option;
  rent : string
}


type board  = square list

(** [uniq lst] is the set-like list composed of the elements from [lst] *)
let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []



let square_of_json json= 
  { 
    name = json |> member "name" |> to_string;
    cost = json |> member "cost" |> to_string |> int_of_string;
    color = json |> member "color" |> to_string;
  }


let from_json j = 
  failwith ""


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