(* Note: You may introduce new code anywhere in this file. *) 

type object_phrase = string list

type command = 
  | Go of object_phrase
  | Quit
  | Score
  | Take of object_phrase
  | Drop of object_phrase
  | Inventory

exception Empty

exception Malformed

let parse str =
  let parse_lst = 
    List.filter (fun x -> (x = "") = false) (String.split_on_char ' ' str) 
  in 
  let command =
    match parse_lst with
    | [] -> raise (Empty)
    | h::t -> if h = "go" || h = "quit" || h = "score" || h = "take" || h = "drop" || h = "inventory" then h else raise (Malformed)
  in 
  let words =
    match parse_lst with
    | [] -> []
    | h::t -> t
  in 
  if command = "quit" && words = [] then Quit else
  if command = "score" && words = [] then Score else
  if command = "go" && words != [] then Go words else 
  if command = "take" && words != [] then Take words else
  if command = "drop" && words != [] then Drop words else
  if command = "inventory" && words = [] then Inventory else
    raise (Malformed)