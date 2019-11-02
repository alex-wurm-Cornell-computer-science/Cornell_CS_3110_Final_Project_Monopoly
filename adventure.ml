(* Note: You may introduce new code anywhere in this file. *) 
open Yojson.Basic.Util

type room_id = string
type exit_name = string
exception UnknownRoom of room_id
exception UnknownExit of exit_name

(* TODO: replace [unit] with a type of your own design. *)
type exit = {
  name: exit_name;
  id: room_id;
}

type room = {
  id: room_id;
  description: string;
  exits: exit list;
}

type t = {
  start_room: room_id;
  rooms: room list;
}

let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

let rec map f lst = 
  match lst with
  | [] -> []
  | h::t -> (f h)::(map f t)

let from_json j = 

  let exit_of_json j = {
    name = j |> member "name" |> to_string;
    id = j |> member "room id" |> to_string;
  } in 

  let room_of_json j = {
    id = j |> member "id" |> to_string;
    description = j |> member "description" |> to_string;
    exits = j |> member "exits" |> to_list |> List.map exit_of_json;
  } in

  let adventure_of_json j = {
    start_room = j |> member "start room" |> to_string;
    rooms = j |> member "rooms" |> to_list |> List.map room_of_json;
  } in

  let parse j =
    try adventure_of_json j
    with Type_error (s, _) -> failwith ("Parsing error: " ^ s) in

  parse j

let start_room adv =
  adv.start_room

let room_ids adv = 
  let get_name room =
    room.id
  in
  uniq (map get_name adv.rooms)


let description adv room =
  let rooms_lst = List.filter (fun x -> x.id = room) adv.rooms in
  match rooms_lst with
  | [] -> raise (UnknownRoom room)
  | h::t -> h.description

let exits adv room = 
  let get_exit_name exit = 
    exit.name
  in 
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
  let get_next_room (exit:exit) =
    exit.id
  in 
  uniq (map get_next_room possible)