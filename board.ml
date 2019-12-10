open Yojson.Basic.Util

type card_name = string
type prop_name = string
exception UnknownSquare of prop_name
exception UnknownCard of card_name

type squareType = 
  | Go
  | Jail 
  | Parking
  | GoToJail
  | Property
  | Card
  | Tax 

type cardType = 
  | Money 
  | Location 
  | LeaveJail

type card = 
  { 
    c_name : card_name;
    description : string;
    payment : int;
    c_type : cardType 
  }

type square = { 
  name : prop_name ; 
  cost : int ;
  color : string option ;
  squareType : squareType ;
  rent : int;
  house : int option;
  hotel : int option;
}

type board  =  {
  squares : square list;
  cards : card list;
  monopolies : prop_name list Stdlib__map.Make(String).t
}

(** [uniq lst] is the set-like list composed of the elements from [lst] *)
let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

(** [parse_color col] parses [col] into a valid monopoly color *)
let parse_color col = 
  if col = "None" then None 
  else Some col

(** [parse_type s] parses [s] into a valid square type *)
let parse_type s = 
  if s = "GO" then Go else 
  if s = "Jail" then Jail else 
  if s = "Parking" then Parking else 
  if s = "GoToJail" then GoToJail else 
  if s = "Property" then Property else 
  if s = "Card" then Card else 
  if s = "Tax" then Tax else failwith ("RI Violated: " ^ s)

(** [get_house j] returns the house field of the given json square [j] *)
let get_house j = 
  match j |> member "type" |> to_string |> parse_type with 
  | Property -> (if ( to_string (member "color" j) = "Railroad")  ||
                    ( to_string (member "color" j) = "Utility") 

                 then None else 
                   Some (j |> member "house" |> to_string |> int_of_string))
  | _ -> None 

(** [get_house j] returns the hotel field of the given json square [j] *)
let get_hotel j = 
  match j |> member "type" |> to_string |> parse_type with 
  | Property -> (if ( to_string (member "color" j) = "Railroad") ||
                    ( to_string (member "color" j) = "Utility")
                 then None else 
                   Some (j |> member "hotel" |> to_string |> int_of_string))
  | _ -> None 

(**[square_of_json json] Parses [json] into a valid monopoly square *)
let square_of_json json= 
  { 
    name =  json |> member "name" |> to_string; 
    cost = json |> member "cost" |> to_string |> int_of_string;
    color = json |> member "color" |> to_string |> parse_color;
    squareType = json |> member "type" |> to_string |> parse_type;
    rent = json |> member "rent" |> to_string |> int_of_string;
    house = get_house json ;
    hotel = get_hotel json ;
  }

(**[card_of_json json] parses [json] into a card type *)
let card_of_json json = 
  { 
    c_name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    payment = json |> member "payment" |> to_string |> int_of_string;
    c_type =  let tpe = json |> member "type" |> to_string in 
      if tpe = "Money" then Money 
      else if tpe = "Location" then Location else LeaveJail
  }

(** [opt_match op] Returns the v if [op] is Some v and fails 
    with Empty otherwise *)
let opt_match op =
  match op with 
  | None -> failwith "Empty"
  | Some x -> x

(** A module to represent a dictionary with strings as keys *)
module MonopDict = Map.Make(String)

(** [init_monopolies sqs] creates a map from colors to property name lists
    where each color is associated with the properties that are of that 
    color. *)
let init_monopolies sqs = 
  let rec init' sqs acc = 
    match sqs with 
    | [] -> acc
    | h :: t -> if MonopDict.mem (opt_match h.color) acc then 
        let vals = MonopDict.find (opt_match h.color) acc in 
        init' t (MonopDict.add (opt_match h.color) (h.name :: vals) acc)
      else 
        init' t (MonopDict.add (opt_match h.color) [h.name] acc) in
  init' sqs MonopDict.empty

let from_json j = 
  let sqs = j |> member "squares" |> to_list |> List.map square_of_json in 
  {
    squares = sqs;
    cards = if List.length (  j |> member "cards" |> to_list ) > 0 then
        j |> member "cards" |> to_list |> List.map card_of_json else [];
    monopolies = init_monopolies (List.filter (fun k -> 
        match k.squareType with 
        | Property -> true
        | _ -> false) sqs)
  }

let cost (b : board) (prop : string) = 
  let rec cost' squares prop =
    match squares with 
    | [] -> raise (UnknownSquare prop)
    | h :: t -> if h.name = prop then h.cost else cost' t prop
  in cost' b.squares prop

let rent b prop = 
  try 
    (List.find (fun pr -> pr.name = prop) b.squares).rent 
  with 
  | Not_found -> raise (UnknownSquare prop)

let square_color (b : board) (prop : string) = 
  try 
    (List.find (fun pr -> pr.name = prop) b.squares).color 
  with 
  | Not_found -> raise (UnknownSquare prop)

let cards b = 
  List.map (fun x -> x.c_name) b.cards 

let card_description b cd = 
  try 
    let card = List.find (fun k -> k.c_name = cd) b.cards in 
    card.description 
  with 
  | exn -> raise (UnknownCard cd)

let size b =
  List.length b.squares

let all_squares b = 
  List.map (fun s -> s.name) b.squares

let square_type b prop = 
  try 
    let square = List.find (fun k -> k.name = prop) b.squares in 
    square.squareType 
  with 
  | exn -> raise (UnknownSquare prop)

let monopoly_group b col = 
  try 
    MonopDict.find col b.monopolies
  with 
  | Not_found -> raise (UnknownSquare "not a color in the board")

let monopoly_group_named b prop = 
  match square_color b prop with 
  | Some c -> monopoly_group b c
  | _ -> raise (UnknownSquare prop)

let nth_square bd n = 
  try (List.nth bd.squares n).name
  with 
  | exn -> raise (UnknownSquare "not a valid index in board")

let square_pos b p = 
  let rec pos' squares prop acc = 
    match squares with 
    | [] -> raise (UnknownSquare prop)
    | h :: t -> if (h.name = prop) then acc else pos' t prop (acc + 1) in
  pos' b.squares p 0

let hotel_cost b p = 
  try let prop = List.find (fun pr -> pr.name = p) b.squares in 
    prop.hotel
  with 
  | Not_found -> raise (UnknownSquare p)

let house_cost b p = 
  try let prop = List.find (fun pr -> pr.name = p) b.squares in 
    prop.house
  with 
  | Not_found -> raise (UnknownSquare p)

let is_buildable bd prop = 
  try 
    let p = List.find (fun s -> s.name = prop) bd.squares in 
    match p.squareType with 
    | Property -> (if  opt_match p.color <> "Railroad" && 
                       opt_match p.color <> "Utility" &&
                       opt_match p.color <> "None" 
                   then true else false )
    | _ -> false
  with 
  | Not_found -> raise (UnknownSquare prop)

let is_buyable bd prop = 
  try 
    let p = List.find (fun s -> s.name = prop) bd.squares in 
    match p.squareType with 
    | Property -> true 
    | _ -> false 
  with 
  | Not_found -> raise (UnknownSquare prop)

let card_type bd cd = 
  try 
    let card = List.find (fun k -> k.c_name = cd) bd.cards in 
    card.c_type
  with 
  | exn -> raise (UnknownCard cd)

let card_payment bd cd = 
  try 
    let card = List.find (fun k -> k.c_name = cd) bd.cards in 
    card.payment
  with 
  | exn -> raise (UnknownCard cd)