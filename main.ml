open Yojson.Basic.Util
open Board
open Command
open State

(** [format_assoc_list fmt_key fmt_val fmt lst] formats an association 
    list [lst] as a dictionary.  The [fmt_key] and [fmt_val] arguments
    are formatters for the key and value types, respectively.  The
    [fmt] argument is where to put the formatted output. 
    Taken from a4 source code. *)
let format_assoc_list format_key format_val fmt lst =
  Format.fprintf fmt "[";
  List.iter (fun (k,v) -> Format.fprintf fmt "%a -> %a; "
                format_key k format_val v) lst;
  Format.fprintf fmt "]"

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

(** [number_of_players] prompts the user for input. If the input isn't an 
    integer in 2..6, the game will inform the player and ask for more input. *)
let rec number_of_players () =
  print_string " > ";
  let n = read_line () in 
  if n = "2" then 2
  else if n = "3" then 3
  else if n = "4" then 4
  else if n = "5" then 5
  else if n = "6" then 6
  else begin 
    print_string "\nInvalid number of players entered. 
    Monopoly works with 2 to 6 players. Please try again!\n";
    number_of_players ()
  end 

(** [get_board f] converts input file f to a Board. If any exceptions
    are raised from the conversion, the game will notify the player and ask for
    a new file name input. *)
let rec get_board f = 
  if f = "quit" then (print_string "\n Goodbye! \n\n"; Stdlib.exit 0)
  else try f |> Yojson.Basic.from_file |> from_json with
    | Not_found -> print_string "Invalid adventure. Try again. \n";
      get_board (read_line ())  

(** [to_list f acc l] is the accumulator result [acc] from applying [f] to list 
    [l] *)
let to_list f acc l = List.fold_left (fun acc (k,v) -> f k v acc) acc

(** [print_string_list l] prints the given string list [l] *)
let rec print_string_list l = 
  print_string "[";
  match l with
  | [] -> print_string "]";
  | [h] -> Printf.printf "%s" h;
  | h::t -> Printf.printf "%s, " h; print_string_list t

(** [print_int_list l] prints the given int list [l] *)
let rec print_int_list l = 
  print_string "[";
  match l with
  | [] -> print_string "]";
  | [h] -> Printf.printf "%d" h;
  | h::t -> Printf.printf "%d, " h; print_int_list t

(** [disp_wallet wals] prints the amount of cash in [wals] that each player has *)
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
    print_string_list b; print_endline "\n"; disp_items t

let parse_obj_phrase lst = 
  let new_str = List.fold_left (fun p n -> p ^ " " ^ n) "" lst in
  String.trim new_str



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

let rec roll_dice brd st = 
  let res = roll brd st in 
  match res with 
  | Illegal -> print_string "\nIllegal movement, please try again\n"; res
  | Legal t -> (let dubs = doubles_rolled t in  
                if dubs = 3 then (
                  Printf.printf "\nPlayer %d rolled 3 pairs of doubles, 
                  which is an illegal movement\n" (State.current_player t); 
                  res
                ) else if dubs > 0 && dubs <= 2 then (
                  Printf.printf "\nPlayer %d rolled %d pair(s) of doubles\n" 
                    (State.current_player t) 
                    (State.doubles_rolled t); 
                  res
                ) else (
                  Printf.printf "\nPlayer %d is now at %s, space %d\n" 
                    (State.current_player t) 
                    (nth_square brd (current_location t)) 
                    (State.current_location t); res
                )
               )
  | Win -> Printf.printf "\nYou've already won!"; res


let pass_go st = 
  State.earn_cash st 200

let next_move res st =
  match res with
  | Win -> Win
  | _ -> 
    match (State.next_turn st) with 
    | Illegal -> Printf.printf "\nYou are not done with your turn. Please roll!\n"; Illegal
    | Legal t -> State.next_turn st
    (* State.update_state t res' *)
    | Win -> Win


let format_board brd locs = 
  let new_locs = List.map (fun (a,b) -> (a, fst b)) locs in 
  let rec format' brd locs = 
    match locs with 
    | [] -> () 
    | h :: t -> 
      Printf.printf "Player %d is at position: " (fst h);
      print_string (nth_square brd (snd h));
      format' brd t in 
  format' brd new_locs



let print_game brd st = 
  disp_inventories (inventories st);
  disp_wallet (wallets st);
  format_board brd (locations st); ()

let check_card pos brd st = 
  match (square_type brd (nth_square brd pos)) with 
  | Card -> 
    let crd = next_card brd in
    print_string ("\nYou have found this card:\n" ^ (card_description brd crd));
    let res = move_cards brd crd st in 
    begin match res with 
      | Legal st' -> card_action brd crd st'  
      | _ -> failwith "shouldnt't happen"
    end
  | _ -> Legal st

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
                          \n\n";
    let lst = State.wealthiest_player brd st in 
    if List.length lst = 1 then let (x,y) = List.hd lst in 
      Printf.printf "\n Player %d, you were winning the game \
                     at the time it ended with a total accumulated wealth \
                     of $%d! \n" x y;
    else Printf.printf "\nThere was a tie between the \
                        following players: \n";
    let rec print_winners = function
      | [] -> Printf.printf "\n\n"
      | (pl,wlt)::t -> Printf.printf "\nPlayer %d : $%d \n" pl wlt;
        print_winners t
    in
    print_winners lst;
    exit 0
  | Build obj -> if nth_square brd (current_location st) <> "Jail" then (
      begin 
        if List.hd obj = "houses" then 
          let () = print_string "\nWhere would you like to build on?\n" in 
          let prop = read_line () in 
          let current_houses = try (houses st prop) with 
            | (UnknownSquare prop) -> Printf.printf "\n %s is not a property\n" (prop);
              interp_command brd (Legal st) st in 
          let () = print_string 
              (prop ^ " currently has" ^ (string_of_int current_houses) ^ " houses on it") in 
          let () = print_string "\nHow many would you like to build?\n" in 
          let n = int_of_string (read_line ()) in 
          let res = build_houses brd st prop n in 
          match res with 
          | Illegal -> print_string "\nYou can't build at the moment, or you entered an invalid command\n";
            interp_command brd (Legal st) st
          | Legal st1 -> print_string ("\n You've built %d hotels on " ^ prop ^ "\n"); 
            interp_command brd res st1
          | Win -> Printf.printf "\nYou won, player %d\n" (current_player st); 
            exit 0;
        else 
        if List.hd obj = "hotels" then 
          let () = print_string "\nWhere would you like to build on?\n" in 
          let prop = read_line () in 
          let current_hotels = try (hotels st prop) with 
            | (UnknownSquare prop) -> Printf.printf "\n %s is not a property\n" (prop);
              interp_command brd (Legal st) st in 
          let () = print_string 
              (prop ^ " currently has" ^ (string_of_int current_hotels) ^ " hotels on it") in
          let () = print_string "\nHow many would you like to build?\n" in 
          let n = int_of_string (read_line ()) in 
          let res = build_hotels brd st prop n in 
          match res with 
          | Illegal -> print_string "\nYou can't build at the moment, or you entered an invalid command\n";
            interp_command brd (Legal st) st
          | Legal st1 -> print_string ("\n You've built %d hotels on " ^ prop ^ "\n"); 
            interp_command brd res st1
          | Win -> Printf.printf "\nYou won, player %d\n" (current_player st); 
            exit 0;
        else 
          print_string "\n Try again!\n"; interp_command brd (Legal st ) st
      end 
    ) else (
      Printf.printf "\nUh oh! You're in Jail, so you can't perform 
    this action\n"; 
      interp_command brd (Legal st) st
    )
  | Roll -> (let res = roll_dice brd st in  
             match res with 
             | Illegal ->  Printf.printf "\nYou've already rolled, player %d!\n" 
                             (current_player st); 
               interp_command brd (Legal st) st
             | Win -> Printf.printf "\nYou won, player %d\n" (current_player st); 
               exit 0;
             | Legal st0 -> 
               let res2 = pay_rent brd (nth_square brd (current_location st0)) st0 in 
               match res2 with 
               | Illegal -> Printf.printf "\nTry again, player %d\n" 
                              (current_player st0);
                 interp_command brd (Legal st) st;
               | Win -> Printf.printf "\nYou won, player %d\n" 
                          (current_player st0); exit 0;
               | Legal st' -> 
                 if (nth_square brd (current_location st') <> "Jail") 
                 then (
                   if nth_square brd (current_location st) =  "Jail" then 
                     Printf.printf "\nYou got out of Jail with a double!\n";
                   let moved = if (current_location st' - current_location st) > 0
                     then current_location st' - current_location st
                     else Board.size brd + 
                          (current_location st' - current_location st) 
                   in 
                   if (current_location st' - current_location st) <= 0 then (
                     Printf.printf "\nYou rolled %d\n" moved;
                     Printf.printf "\nYou are at %s\n" 
                       (Board.nth_square brd (current_location st'));
                     Printf.printf "\nYou've passed GO, player %d!\n" 
                       (current_player st');
                     let res' = earn_cash st' 200 in 
                     let st'' = update_state st' res' in 
                     let res' = check_card (current_location st') brd st in
                     interp_command brd res' st''
                   ) else (
                     Printf.printf "\nYou rolled %d\n" moved;
                     Printf.printf "\nYou are at %s\n" 
                       (Board.nth_square brd (current_location st'));
                     let res' = check_card (current_location st') brd st in
                     interp_command brd res' st'
                   )
                 ) else (
                   Printf.printf "\nYou need to roll a double or use a Get Out of Jail Free card to leave Jail\n";
                   interp_command brd res st'
                 ) 
            )
  | Inventory -> print_string "\nYou own the following properties:\n";
    disp_inventories (State.inventories st);  
    interp_command brd res st
  | Wallet -> print_string "\nYou currently have the following in cash.\n";
    disp_wallet (wallets st); 
    interp_command brd res st    
  | Items -> print_string "\nYou currently have the following cards:\n"; 
    disp_items (State.items st);
    interp_command brd res st             
  | Buy -> if nth_square brd (current_location st) <> "Jail" then (
      print_string "\nAre you sure you would like to buy this property?\n";
      let confirmation = read_line() in 
      if confirmation = "yes" then
        (let prop = (State.current_location st) |> Board.nth_square brd in 
         let res = State.buy brd prop st in
         (match res with 
          | Illegal -> Printf.printf "\nUnfortunately this property cannot
                           be purchased at this time.\n"; interp_command brd (Legal st) st
          | Legal st' -> Printf.printf "\n Congratulations! You are \
                                        the owner of %s." prop; interp_command brd (Legal st') st';
          | Win -> Printf.printf "\n Player %d you have won the game! You were the \
                                  first player to acquire multiple properties!\n" (State.current_player st)))
      else if confirmation = "no" then
        (Printf.printf "Okay, what would you like to do instead?\n"; 
         interp_command brd (Legal st) st)
      else 
        (Printf.printf "\n Invalid response, please try again. \n";
         interp_command brd (Legal st) st)) 
    else (
      (Printf.printf "\nUh oh! You're in Jail, so you can't perform this action\n"; 
       interp_command brd (Legal st) st)
    )


  (* let response = read_line () in 
     if response = "yes" then (* try-catch to see if legal *)
     print_string "\n Congratulations! You are \
                  the owner of %s. \n" (*add functionality then take more input*)
     else if response = "no" then print_string "\n Okay maybe next time! \n"
     (* Take more input. *)
     else print_string "\n Invalid response, please try again. \n"; *)
  | Sell p -> if nth_square brd (current_location st) <> "Jail" then (
      print_string "\nAre you sure you would like to sell this property?\n";
      let confirmation = read_line() in 
      if confirmation = "yes" then
        (let prop = parse_obj_phrase p in 
         let res = State.sell brd prop st in
         match res with
         | Illegal -> Printf.printf "\nUnfortunately this property cannot
                           be sold at this time.\n"; interp_command brd (Legal st) st
         | Legal st' -> Printf.printf "\n Congratulations! You have successfully \
                                       sold %s." prop; interp_command brd (Legal st') st'
         | Win -> Printf.printf "\n Player %d you seem to have won the game... \
                                 but I suspect you may have cheated.\n" (State.current_player st); 
           interp_command brd (Legal st) st)
      else if confirmation = "no" then
        (Printf.printf "Okay, what would you like to do instead?\n"; 
         interp_command brd (Legal st) st)
      else 
        (Printf.printf "\n Invalid response, please try again. \n";
         interp_command brd (Legal st) st))
    else (
      (Printf.printf "\nUh oh! You're in Jail, so you can't perform this action\n"; 
       interp_command brd (Legal st) st)
    )
  (* let response = read_line () in 
     if response = "yes" then (* try-catch to see if legal *)
     print_string "\n Congratulations! You have \
                  sold %s. \n" (*add functionality then take more input*)
     else if response = "no" then print_string "\n Okay maybe next time! \n"
     (* Take more input. *)
     else print_string "\n Invalid response, please try again. \n"; *)
  | Auction p -> if nth_square brd (current_location st) <> "Jail" then 
      (print_string "\nAre you sure you would like to participate in
                               the auction for this property?\n";
       interp_command brd res st)
    else (Printf.printf "\nUh oh! You're in Jail, so you can't perform this action\n"; 
          interp_command brd (Legal st) st
         )
  (* let response = read_line () in 
     if response = "yes" then (* try-catch to see if legal *)
     print_string "\n Congratulations! You are \
                  the owner of %s. \n" (*add functionality then take more input*)
     else if response = "no" then print_string "\n Okay maybe next time! \n"
     (* Take more input. *)
     else print_string "\n Invalid response, please try again. \n"; *)
  | Next -> let res' = next_move res st in 
    let st' = State.update_state st res' in 
    interp_command brd res' st'

  | Game -> print_game brd st; interp_command brd res st

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
  Random.self_init ();
  let brd = get_board f in
  print_string "\nPlease enter a valid number of players for this game. \n";
  let n = number_of_players () in 
  let st = init_state brd n in
  let _ = interp_command brd Illegal st in

  Stdlib.exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  print_string 
    "\n\nWelcome to the 3110 Monopoly Game Engine.\n";
  print_endline "Please enter the name of the board file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> try play_game file_name with 
    | _ -> print_string "Not a game file, try again"; main ()

(* Execute the game engine. *)
let () = main ()


