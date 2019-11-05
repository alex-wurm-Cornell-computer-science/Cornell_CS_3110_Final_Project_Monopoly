open Yojson.Basic.Util

type card_name = string
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

type card = 
  { 
    c_name : string;
    description : string;
    payment : int
  }

type square = { 
  name : string ; 
  cost : int ;
  color : string option ;
  squareType : squareType ;
  rent : int
}


type board  =  {
  squares : square list;
  chance_cards : card list;
  chest_cards : card list;
  monopolies : string list list
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
  if s = "Chance" then Chance else 
  if s = "Chest" then Chest else Tax

(**[square_of_json json] Parses [json] into a valid monopoly square *)
let square_of_json json= 
  { 
    name = json |> member "name" |> to_string;
    cost = json |> member "cost" |> to_string |> int_of_string;
    color = json |> member "color" |> to_string |> parse_color;
    squareType = json |> member "type" |> to_string |> parse_type;
    rent = json |> member "rent" |> to_string |> int_of_string;
  }

(**[card_of_json json] parses [json] into a card type *)
let card_of_json json = 
  { 
    c_name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    payment = json |> member "payment" |> to_string |> int_of_string
  }

(** [monopoly_of_json j] transforms [j] into a list of strings, where the strings 
    are properties that together form a monopoly*)
let monopoly_of_json j = 
  j |> to_list |> List.map to_string

let from_json j = 
  {
    squares = j |> member "squares" |> to_list |> List.map square_of_json;

    chance_cards = if List.length (  j |> member "chance cards" |> to_list ) > 0 then
        j |> member "chance cards" |> to_list |> List.map card_of_json else [];

    chest_cards = if List.length (j |> member "chest cards" |> to_list) > 0 then
        j |> member "chest cards" |> to_list |> List.map card_of_json else [];
    monopolies = j |> member "monopolies" |> to_list |> List.map monopoly_of_json
  }

let cost (b : board) (prop : string) = 
  let rec cost' squares prop =
    match squares with 
    | [] -> raise (UnknownSquare prop)
    | h :: t -> if h.name = prop then h.cost else cost' t prop
  in cost' b.squares prop

let rent b prop = 
  let rec rent' squares prop = 
    match squares with 
    | [] -> raise (UnknownSquare prop)
    | h :: t -> if h.name = prop then h.rent else rent' t prop
  in rent' b.squares prop


let chance_cards b = 
  List.map (fun x -> x.c_name) b.chance_cards 

let chest_cards b = 
  List.map (fun x -> x.c_name) b.chest_cards 

let chance_card_description b cd = 
  try 
    let card = List.find (fun k -> k.c_name = cd) b.chance_cards in 
    card.description 
  with 
  | exn -> failwith "Unknown card"

let chest_card_description b cd = 
  try 
    let card = List.find (fun k -> k.c_name = cd) b.chest_cards in 
    card.description 
  with 
  | exn -> failwith "Unknown card"

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


let monopoly_group b prop = 
  let monopolies = List.fold_left 
      (fun acc k -> if List.mem prop k then k else acc) [] b.monopolies in 
  match monopolies with
  | [] -> raise (UnknownSquare prop)
  | l -> l

let nth_square bd n = 
  (List.nth bd.squares n).name