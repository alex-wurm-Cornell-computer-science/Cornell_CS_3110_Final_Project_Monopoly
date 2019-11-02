open Yojson.Basic.Util
open Board
open Command
open State

(** [user_input _] prompts the user for a command. If the command
    is empty the game will inform the player and ask for another command. 
    If the command was malformed, the game will inform the player and ask
    for another command. *)
let rec user_input _ = 
  try parse (read_line ()) with
  | Empty -> print_string "\n No command was entered, please try again. \
                           \n\n";
    user_input()
  | Malformed -> print_string "\n Command unclear, please try again. \n\n"; 
    user_input()

(** [get_adv f] converts input file f to an Adventure. If any exceptions
    are raised from the conversion, the game will notify the player and ask for
    a new file name input. *)
let rec get_board f = 
  if f = "quit" then (print_string "\n Goodbye! \n\n"; Stdlib.exit 0)
  else try f |> Yojson.Basic.from_file |> from_json with
    | _ -> print_string "Invalid adventure. Try again. \n";
      get_board (read_line ())  

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
  State.disp_curr_player_inventory st 

let roll st = 
  State.roll st 

(** [interp_command brd st command] allows the user to play the game by
    printing an exit message if the input command is [Quit] or by inspecting a 
    [Go] message to determine what action to perform. If the command is [Legal]
    the state is updated and the user is prompted for another command. If the 
    command is [Illegal] the game prints an error message and asks the user
    for a new command. *)
let rec interp_command brd st command = 
  match command with
  | Quit -> print_string "\n Thank you for playing the Monopoly Game Engine! \
                          \n\n"; exit 0
  | Roll -> print_string "\n You rolled a %d: "; (* add functionality then
                                                    take more input. *)
    interp_command brd st (user_input ())
  | Inventory -> print_string "\n Your own the following properties: \n";
    disp_inv st;  
    interp_command brd st (user_input ())
  | Wallet -> print_string "\n You currently have $%d in cash: \n"; 
    (* add functionality then take more input*)
    interp_command brd st (user_input ())    
  | Items -> print_string "\n You currently have the following cards: \n"; 
    (* add functionality then take more input*)
    interp_command brd st (user_input ())              
  | Buy -> print_string "\n Are you sure you would like to buy this property? \n";
    let response = read_line () in 
    if response = "yes" then (* try-catch to see if legal *)
      print_string "\n Congratulations! You are \
                    the owner of %s. \n" (*add functionality then take more input*)
    else if response = "no" then print_string "\n Okay maybe next time! \n"
    (* Take more input. *)
    else print_string "\n Invalid response, please try again. \n";
    interp_command brd st (user_input ())
  | Sell p -> print_string "\n Are you sure you would like to sell this property? \n";
    let response = read_line () in 
    if response = "yes" then (* try-catch to see if legal *)
      print_string "\n Congratulations! You have \
                    sold %s. \n" (*add functionality then take more input*)
    else if response = "no" then print_string "\n Okay maybe next time! \n"
    (* Take more input. *)
    else print_string "\n Invalid response, please try again. \n";
    interp_command brd st (user_input ())
  | Auction p -> print_string "\n Are you sure you would like to participate in \
                               the auction for this property? \n";
    let response = read_line () in 
    if response = "yes" then (* try-catch to see if legal *)
      print_string "\n Congratulations! You are \
                    the owner of %s. \n" (*add functionality then take more input*)
    else if response = "no" then print_string "\n Okay maybe next time! \n"
    (* Take more input. *)
    else print_string "\n Invalid response, please try again. \n";
    interp_command brd st (user_input ())

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
  (*failwith "Unimplemented"*)
  let brd = get_board f in
  let st = init_state brd 6 in
  (* update_desc brd st; *)

  let response = user_input () in

  let _ = interp_command brd st (response) in

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
