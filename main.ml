open Yojson.Basic.Util
open Board
open Command
open State

exception NotValid

(** [user_input _] prompts the user for a command. If the command
    is empty the game will inform the player and ask for another command. 
    If the command was malformed, the game will inform the player and ask
    for another command. *)
let rec user_input _ = 
  print_string " > ";
  try parse (read_line ()) with
  | Empty -> print_string "\nNo command was entered, please try again. \
                           \n\n";
    user_input()
  | Malformed -> print_string "\nCommand unclear, please try again. \n\n"; 
    user_input()

let rec number_of_players () =
  print_string " > ";
  let n = read_line () in 
  if n = "2" then 2
  else if n = "3" then 3
  else if n = "4" then 4
  else if n = "5" then 5
  else if n = "6" then 6
  else begin 
    print_string "\nInvalid number of players entered. Monopoly works with 2 to 6 players. Please try again!\n";
    number_of_players ()
  end 

(** [get_board f] converts input file f to a Board. If any exceptions
    are raised from the conversion, the game will notify the player and ask for
    a new file name input. *)
let rec get_board f = 
  if f = "quit" then (print_string "\n Goodbye! \n\n"; Stdlib.exit 0)
  else try f |> Yojson.Basic.from_file |> from_json with
    | _ -> print_string "Invalid adventure. Try again. \n";
      get_board (read_line ())  


(* let format_assoc_list format_key format_val fmt lst =
  Format.fprintf fmt "[";
  List.iter (fun (k,v) -> Format.fprintf fmt "%a -> %a;"
                format_key k format_val v) lst;
  Format.fprintf fmt "]"

let format fmt d =
      format_assoc_list Key.format Value.format fmt d *)

let to_list f acc l = List.fold_left (fun acc (k,v) -> f k v acc) acc

let rec print_string_list l = 
  print_string "[";
  match l with
  | [] -> print_string "]";
  | [h] -> Printf.printf "%s" h;
  | h::t -> Printf.printf "%s, " h; print_string_list t

let rec print_int_list l = 
  print_string "[";
  match l with
  | [] -> print_string "]";
  | [h] -> Printf.printf "%d" h;
  | h::t -> Printf.printf "%d, " h; print_int_list t

let rec disp_wallet wals = 
  print_endline "\n";
  match wals with 
  | [] -> print_endline "\n";
  | (a,b) :: t -> Printf.printf "Player %d has $%d." a b; print_endline "\n";
                  disp_wallet t

let rec disp_inventories invs = 
  print_endline "\n";
  match invs with
  | [] -> print_endline "\n";
  | (a,b) :: t -> Printf.printf "Player %d has the following properties:" a; 
                  print_string_list b; print_endline "\n"; disp_inventories t


let rec disp_items itms = 
  print_endline "\n";
  match itms with
  | [] -> print_endline "\n";
  | (a,b) :: t -> Printf.printf "Player %d has the following cards:" a;
                  print_string_list b; print_endline "\n"; disp_inventories t

(*
(** [update_items adv st] prints the loot of the [current_room] given
the state and the adventure being played. If there is no loot in the [room]
the function will let the player know. *)
let rec update_items adv st =
  let rels = List.assoc (current_room_id st) (current_room_info st) in
  match rels with 
    | [] -> print_string "There are no items in this room. \n";
    | _ -> print_string "You see the following items in this room: \n"; 
           List.iter print_endline rels

(** [update_desc adv st] prints the description of the [current_room] given
the state and the adventure being played. This function also prints the 
loot found in [current_room] given [st]. *)
let update_desc adv st = 
  print_string "\n";
  st |> current_room_id |> description adv |> print_string;
  print_string "\n\n";
  update_items adv st;
  print_string "\n"

(**[update_score st] prints the score of the player given the current state
[st]. *)
let update_score st = 
  st |> current_score |> print_int;
  print_string "\n\n"

(** [revert_exit lst] takes a string list and convert it to a single string. *)
let revert_exit lst = 
  let new_str = List.fold_left (fun p n -> p ^ " " ^ n) "" lst in
  String.trim new_str

(** [disp_inv st] prints the player's current [inventory]. *)
let disp_inv st =
  print_string "\n"; 
  List.iter print_endline (st |> current_inventory);
  print_string "\n"

(** [disp_items st] prints the [loot] of the current room. *)
let disp_items st =
  print_string "\n";
  st |> current_room_loot |> List.iter print_endline;
  print_string "\n"
*)

let disp_inv st = 
  print_string "\n"; 
  (* List.iter print_endline (st |> curr_player_inventory); //NEED TO GET PROPERTY NAMES, NOT PROPERTY OBJ*) 
  print_string "\n"

let roll brd st = 
  let res = State.roll brd st in 
  match res with 
  | Illegal -> print_string "\nIllegal movement, please try again\n"; res
  | Legal t -> Printf.printf "\nPlayer %d is now at space %d\n" (State.current_player t) (State.current_location t); res

let pass_go brd st = 
  let res = State.earn_cash st 200 in
  match res with 
  | Illegal -> print_string "\nIllegal movement, please try again\n"; res
  | Legal t -> Printf.printf "\nPlayer %d now has $%d in cash\n" (State.current_player t) (State.curr_player_wallet t); res

let next_turn res st =
  match res with 
  | Illegal -> st
  | Legal t -> let res' = State.next_turn t in 
    State.update_state t res'

(** [interp_command brd st command] allows the user to play the game by
    printing an exit message if the input command is [Quit] or by inspecting a 
    [Go] message to determine what action to perform. If the command is [Legal]
    the state is updated and the user is prompted for another command. If the 
    command is [Illegal] the game prints an error message and asks the user
    for a new command. *)
let rec interp_command brd res st = 
  Printf.printf "\nPlayer %d, it's your turn!\n" (State.current_player st);
  let command = user_input () in
  match command with
  | Quit -> print_string "\nThank you for playing the Monopoly Game Engine! \
                          \n\n"; exit 0
  | Roll -> (let res = roll brd st in 
             match res with 
             | Illegal ->  Printf.printf "\nYou've already rolled, player %d!\n" (current_player st); 
               interp_command brd (Legal st) st
             | Legal st' ->
               let moved = current_location st' - current_location st in 
               if moved <= 0 then (
                 Printf.printf "\nYou've passed GO, player %d!\n" (current_player st');
                 let res' = pass_go brd st' in 
                 interp_command brd res' st'
               ) else (
                 Printf.printf "\nYou rolled a %d\n" moved;
                 interp_command brd res st'
               )

            )
  | Inventory -> print_string "\nYou own the following properties:\n";
    disp_inventories (State.inventories st);  
    interp_command brd res st
  | Wallet -> Printf.printf "\nYou currently have $%d in cash.\n" 
                (curr_player_wallet st); 
    interp_command brd res st    
  | Items -> print_string "\nYou currently have the following cards:\n"; 
    disp_items (State.items st);
    interp_command brd res st             
  | Buy -> print_string "\nAre you sure you would like to buy this property?\n";
    (* let response = read_line () in 
       if response = "yes" then (* try-catch to see if legal *)
       print_string "\n Congratulations! You are \
                    the owner of %s. \n" (*add functionality then take more input*)
       else if response = "no" then print_string "\n Okay maybe next time! \n"
       (* Take more input. *)
       else print_string "\n Invalid response, please try again. \n"; *)
    interp_command brd res st
  | Sell p -> print_string "\nAre you sure you would like to sell this property?\n";
    (* let response = read_line () in 
       if response = "yes" then (* try-catch to see if legal *)
       print_string "\n Congratulations! You have \
                    sold %s. \n" (*add functionality then take more input*)
       else if response = "no" then print_string "\n Okay maybe next time! \n"
       (* Take more input. *)
       else print_string "\n Invalid response, please try again. \n"; *)
    interp_command brd res st
  | Auction p -> print_string "\nAre you sure you would like to participate in
                               the auction for this property?\n";
    (* let response = read_line () in 
       if response = "yes" then (* try-catch to see if legal *)
       print_string "\n Congratulations! You are \
                    the owner of %s. \n" (*add functionality then take more input*)
       else if response = "no" then print_string "\n Okay maybe next time! \n"
       (* Take more input. *)
       else print_string "\n Invalid response, please try again. \n"; *)
    interp_command brd res st
  | Next -> let st' = next_turn res st in interp_command brd res st'

(** [continue_game adv st result] updates the state of the game, prints the
    description, and prompts the user for another command to continue the game. *)
(* and continue_game_roll brd st result =
   let st' = update_state st result in
   update_desc adv st';
   interp_command adv st' (user_input ())
   and continue_game_check brd st result =
   let st' = update_state st result in
   interp_command adv st' (user_input()) *)

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let brd = get_board f in
  print_string "\nPlease enter a valid number of players for this game. \n";
  let n = number_of_players () in 
  let st = init_state brd n in
  let _ = interp_command brd Illegal st in

  Stdlib.exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string 
    "\n\nWelcome to the 3110 Monopoly Game Engine.\n";
  print_endline "Please enter the name of the board file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
