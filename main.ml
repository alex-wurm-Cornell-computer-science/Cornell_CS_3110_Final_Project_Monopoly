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
  | Empty -> print_string "\nNo command was entered, please try again.\n";
    user_input()
  | Malformed -> print_string "\nCommand unclear, please try again.\n"; 
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
    print_string "\nInvalid number of players entered. \
                  Monopoly works with 2 to 6 players. Please try again!\n";
    number_of_players ()
  end 

(** [rec most_money ()] prompts the user for an input of a number between 1700
    and 20580. If they enter a valid response it converts the string to the 
    corresponding int and returns that value. If they enter an invalid response 
    it re-prompts them for a valid input.*)
let rec most_money () =
  print_string " > ";
  let ui = read_line () in 
  let mon = try int_of_string ui with Failure int_of_string -> 
    print_string "\nInvalid value, please enter a number \
                  between 1700 and 20580.\n";
    most_money ()
  in 
  if mon < 1700 || mon > 20580 then begin
    print_string "\nInvalid quantity of money to win the game. \
                  Please select a quantity between 1700 and 20580.\n";
    most_money ()
  end
  else mon

(** [get_board f] converts input file f to a Board. If any exceptions
    are raised from the conversion, the game will notify the player and ask for
    a new file name input. *)
let rec get_board f = 
  if f = "quit" then (print_string "\nGoodbye!\n"; Stdlib.exit 0)
  else try f |> Yojson.Basic.from_file |> from_json with
    | Not_found -> print_string "\nInvalid board. Try again.\n";
      print_string " > ";
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

(** [disp_wallet wals] prints the amount of cash in [wals] that each player has 
    at the time the function is called. *)
let rec disp_wallet st = 
  match wallets st with 
  | [] -> print_string "\n";
  | (a,b) :: t -> Printf.printf "\nPlayer %d has $%d.\n" a b;
    let st' = {st with wallets = t} in
    disp_wallet st'

(** [disp_inventories st] prints the properties owned by all players at the time
    the function is called as well as the number of houses and hotels on each
    property. *)
let rec disp_inventories st = 
  match inventories st with
  | [] -> ();
  | (a,b) :: t -> Printf.printf "\nPlayer %d:" a; 
    let rec disp_buildings props = 
      if (List.length b) == 0 then print_string " owns no properties\n" else 
        match props with
        | [] -> ();
        | h :: t -> let (hou,hot) = (houses st h, hotels st h) in 
          Printf.printf "\n%s with %d houses and %d hotels\n" h hou hot;
          disp_buildings t
    in
    disp_buildings b;
    let st' = {st with inventories = t} in disp_inventories st'

(** [disp_items st] prints the items held by all players at the time the 
    function is called. *)
let rec disp_items st = 
  match items st with
  | [] -> print_string "\n";
  | (a,b) :: t -> Printf.printf "\nPlayer %d has the following cards:\n" a;
    print_string_list b;
    let st' = {st with items = t} in disp_items st'

(** [parse_obj_phrase lst] takes an input object phrase (a string list of an 
    input with multiple words, such as a property name) and converts it into an 
    single string. *)
let parse_obj_phrase lst = 
  let new_str = List.fold_left (fun p n -> p ^ " " ^ n) "" lst in
  String.trim new_str

(** [roll_dice brd st] is the result of a player rolling on their turn. The 
    function also prints a useful message regarding the number of doubles the 
    player has rolled and which indicates if they are going to jail. *)
let rec roll_dice brd st = 
  let res = roll brd st in 
  match res with 
  | Illegal -> print_string "\nIllegal movement, please try again\n"; res
  | Legal t -> let dubs = doubles_rolled t in  
    if dubs = 3 then (
      Printf.printf "\nPlayer %d rolled 3 pairs of doubles, \
                     which is an illegal movement\n" 
        (current_player t); 
      res) 
    else if dubs > 0 && dubs <= 2 then (
      let moved = 
          if (current_location t - current_location st) >= 0
          then current_location t - current_location st
          else size brd + 
               (current_location t - current_location st) 
        in 
      Printf.printf "\nPlayer %d rolled %d pair(s) of doubles and moved \
                    %d spaces.\n" (current_player t) (doubles_rolled t) moved; 
      let space_name = (current_location st) |> nth_square brd in
      Printf.printf "\nYou are now at %s.\n" space_name;
      print_string "\nYou can perform actions, but you have to roll again.\n";
      res
    ) else (
      Printf.printf "\nPlayer %d is now at %s, space %d\n"
        (current_player t) (nth_square brd (current_location t)) 
        (current_location t); res
    )
  | Win -> Printf.printf "\nYou've already won!\n"; res

(** [pass_go st] adds $200 to the current player's wallet. *)
let pass_go st = 
  earn_cash st 200

(** [next_move res st] is the result of the current player attempting to move 
    onto the next player's turn. The function prints a warning if the current 
    player's turn is not yet over. *)
let next_move res st =
  match res with
  | Win -> Win
  | _ -> 
    match (next_turn st) with 
    | Illegal -> Printf.printf "\nYou are not done with your turn. \
                                Please roll!\n"; 
      Illegal
    | Legal t -> next_turn st
    | Win -> Win

(** [format_board brd locs] prints the current locations of all players
    in the game at the time the function is called.*)
let format_board brd locs = 
  let new_locs = List.map (fun (a,b) -> (a, fst b)) locs in 
  let rec format' brd locs = 
    match locs with 
    | [] -> () 
    | h :: t -> 
      Printf.printf "\nPlayer %d is at position %s\n" (fst h) 
        (nth_square brd (snd h));
      format' brd t in 
  format' brd new_locs

(** [print_game brd st] prints all information about all players in the game.
    These prints include the properties, wallets, and locations of the 
    players.*)
let print_game brd st = 
  disp_inventories st;
  disp_wallet st;
  format_board brd (locations st); ()

(** [check_card post brd st] checks to see if the player's current position
    is a square where you pick up a card. If it is, then it performs the card 
    action but if it isn't it returns the original state unchanged. *)
let check_card pos brd st = 
  match (square_type brd (nth_square brd pos)) with 
  | Card -> 
    let crd = next_card st in
    print_string ("\nYou have found this card:\n" ^ 
                  (card_description brd crd) ^ "\n");
    let res = move_cards brd crd st in 
    begin 
      match res with 
      | Legal st' -> card_action brd crd st'  
      | _ -> failwith "shouldn't happen"
    end
  | _ -> Legal st

(** [rec check_tax brd st] checks to see if the player's current position
    is a square where you pay a tax. If it is, then it collects the tax, but if
    it isn't it returns the original state unchanged. It also prints a 
    corresponding error message if a tax is paid. *)
let rec check_tax brd st = 
  let sq_name = current_location st |> nth_square brd in 
  match square_type brd sq_name with 
  | Tax -> 
    let () = print_string "\nYou landed on a tax square. \
                           Time to pay! Dice are being rolled to calculate \
                           your tax.\n" in 
    let mult = (Random.int 5) + (Random.int 5) + 2 in 
    let () = print_string ("\nYour tax roll was " ^ (string_of_int mult) ^ "\n") 
    in let tax = mult * (rent brd sq_name)
    in let () = Printf.printf "\nPay %d\n" tax in 
    begin 
      match earn_cash st (-tax) with 
      | Legal st' -> st'
      | _ -> st
    end 
  | _ -> st

(** [interp_command brd st command] allows the user to play the game by
    printing an exit message if the input command is [Quit] or by inspecting a 
    [Go] message to determine what action to perform. If the command is [Legal]
    the state is updated and the user is prompted for another command. If the 
    command is [Illegal] the game prints an error message and asks the user
    for a new command. *)
let rec interp_command brd res st wc = 
  let _ = check_wc brd res st wc in 
  Printf.printf "\nPlayer %d, it's your turn!\n" (current_player st);
  let command = user_input () in
  match command with
  | Quit -> quit_helper brd res st wc
  | Build obj -> build_helper brd res st wc obj 
  | Roll -> let res' = roll_dice brd st in  
    roll_helper brd res' st wc 
  | Inventory -> print_string "\nThe following properties are owned:\n";
    disp_inventories st;  
    interp_command brd res st wc 
  | Wallet -> print_string "\nYou currently have the following in cash.\n";
    disp_wallet st; 
    interp_command brd res st wc 
  | Items -> print_string "\nYou currently have the following cards:\n"; 
    disp_items st;
    interp_command brd res st wc     
  | Buy -> buy_helper brd res st wc 
  | Sell p -> sell_helper brd res st wc p
  | Next -> next_helper brd res st wc
  | Game -> print_game brd st; 
    interp_command brd res st wc 
  | Use -> use_helper brd res st wc

(** [check_wc brd res st wc] checks if a player has satisfied the win 
    condition of the game*)
and check_wc brd res st wc =  
  let trimmed_statuses = List.remove_assoc 
      (current_player st) (player_status st) in 
  let last_one_standing = List.for_all (fun (x,y) -> y = false) 
      trimmed_statuses in 
  let current_player_wealth = 
    (curr_player_wallet st) + (inventory_value brd st) in
  if last_one_standing then
    let bank = wealthiest_player brd st in 
    let (a,b) = List.hd bank in 
    Printf.printf "\nPlayer %d, you are the last player standing! \
                   All of your opponents have gone bankrupt. 
                   You have accumulated a total wealth of $%d! \
                   Thank you for playing!\n" a b;exit 0
  else if current_player_wealth >= wc then
    let bank = wealthiest_player brd st in 
    let (a,b) = List.hd bank in 
    Printf.printf "\nPlayer %d, you have accumulated the amount of wealth \
                   voted on as sufficient to win the game! 
                   You have accumulated a total wealth of $%d! \
                   Thank you for playing!\n" a b;exit 0
  else ()

(** [quit_helper brd res st wc] quits the game loop *)
and quit_helper brd res st wc = 
  print_string "\nThank you for playing the Monopoly Game Engine!\n";
  let lst = wealthiest_player brd st in 
  if List.length lst = 1 then let (x,y) = List.hd lst in 
    Printf.printf "\nPlayer %d, you were winning the game \
                   at the time it ended with a total accumulated wealth \
                   of $%d!\n" x y;
  else Printf.printf "\nThere was a tie between the following players:\n";
  let rec print_winners = function
    | [] -> ();
    | (pl,wlt)::t -> Printf.printf "\nPlayer %d : $%d\n" pl wlt;
      print_winners t
  in
  print_winners lst;
  exit 0

(** [build_houses_helper brd st wc prop] builds houses on [prop] in 
    state [st] *)
and build_houses_helper brd res st wc prop =
  (let current_houses = try (houses st prop) with 
      | (UnknownSquare prop) -> Printf.printf "\n%s is not a property\n" (prop);
        interp_command brd (Legal st) st wc in 
   let () = print_string 
       ("\n" ^ prop ^ " currently has " ^ (string_of_int current_houses) 
        ^ " houses on it\n") in 
   let () = print_string "\nHow many would you like to build?\n" in 
   let n = try (
     print_string " > ";
     int_of_string (read_line ())
   )
     with | _ ->  print_string "\nNot a valid number\n"; 
       interp_command brd res st wc in  
   (build_houses brd st prop n, n))

(** [build_houses_helper brd st wc prop] builds hotels on [prop]*)
and build_hotels_helper brd res st wc prop = 
  let current_hotels = try (hotels st prop) with 
    | (UnknownSquare prop) -> Printf.printf "\n%s is not a property\n" (prop);
      interp_command brd (Legal st) st wc in 
  let () = print_string 
      ("\n" ^ prop ^ " currently has" ^ 
       (string_of_int current_hotels) ^ " hotels on it\n") in
  let () = print_string "\nHow many would you like to build?\n" in 
  let n = try (
    print_string " > ";
    int_of_string (read_line ())
  )
    with | _ ->  print_string "\nNot a valid number\n" ; 
      interp_command brd res st wc in 
  (build_hotels brd st prop n, n)

(** [build_helper brd res st wc] executes the action of building houses and 
    hotels *)
and build_helper brd res st wc obj = 
  try 
    if not (is_in_jail st) then (
      let () = print_string "\nWhere would you like to build on?\n" in 
      print_string " > ";
      let prop = String.trim (read_line ()) in 
      begin 
        if List.hd obj = "houses" then 
          let  res' = (build_houses_helper brd res st wc prop) in 
          match fst res' with 
          | Illegal -> print_string "\nYou can't build at the moment, \
                                     or you entered an invalid command\n";
            interp_command brd (Legal st) st wc 
          | Legal st1 -> Printf.printf ("\nYou've built %d houses on %s\n") 
                           (snd res') prop; 
            interp_command brd (fst res') st1 wc 
          | Win -> Printf.printf "\nYou won, player %d\n" (current_player st); 
            exit 0;
        else 
        if List.hd obj = "hotels" then 
          let res' = (build_hotels_helper brd res st wc prop) in 
          match fst res' with 
          | Illegal -> print_string "\nYou can't build at the moment, \
                                     or you entered an invalid command\n";
            interp_command brd (Legal st) st wc 
          | Legal st1 -> Printf.printf ("\nYou've built %d hotels on %s\n") 
                           (snd res') prop; 
            interp_command brd (fst res') st1 wc 
          | Win -> Printf.printf "\nYou won, player %d\n" (current_player st); 
            exit 0;
        else 
          let _ = print_string "\nTry again!\n" in 
          interp_command brd (Legal st ) st wc 
      end 
    ) else (
      Printf.printf "\nUh oh! You're in Jail, so you can't perform \
                     this action\n"; 
      interp_command brd (Legal st) st wc 
    )
  with 
  | UnknownSquare prop -> print_string ("\n" ^ prop ^ " is not a property\n"); 
    interp_command brd res st wc

(** [roll_helper brd res st wc] handles the initial result [res] of the roll 
    function in State. If [res] is Illegal, it means that the current player 
    has just rolled a non-double on their turn and therefore can't roll again. 
    If [res] is Win, then the win condition has been passed and the game ends. 
    If [res] is Legal, then the helper function [pay_on_square] is called. *)
and roll_helper brd res st wc = 
  match res with 
  | Illegal ->  Printf.printf "\nYou've already rolled, player %d!\n" 
                  (current_player st); 
    interp_command brd (Legal st) st wc 
  | Win -> Printf.printf "\nYou won, player %d\n" 
             (current_player st); 
    exit 0;
  | Legal st' -> pay_on_square brd res st' wc    

(** [pay_on_square brd res st wc] calls the functions that deal with paying 
    money at each square on the board, specifically [check_tax] and [pay_rent] 
    from State. If the result of those functions is Illegal, it means that the 
    current player has just rolled a non-double on their turn and therefore 
    can't roll again, and if it's Win, then the win condition has been passed 
    and the game ends. If the result of those functions is Legal, then the 
    current player's wallet in [st] is adjusted accordingly and the helper 
    function [landed_on_square] is called. *)
and pay_on_square brd res st wc = 
  let st' = check_tax brd st in 
  let res' = pay_rent brd 
      (nth_square brd (current_location st')) st' in  
  match res' with 
  | Illegal -> Printf.printf "\nTry again, player %d\n" 
                 (current_player st');
    interp_command brd (Legal st') st' wc ;
  | Win -> Printf.printf "\nYou won, player %d\n" 
             (current_player st'); exit 0;
  | Legal st'' -> 
    if (curr_player_wallet st'' < curr_player_wallet st') 
    then let rent_paid = 
           (curr_player_wallet st' - curr_player_wallet st'') in 
      Printf.printf "\nYou paid %d in rent.\n" rent_paid; 
    else ();
    landed_on_square brd res' st'' wc 

(** [landed_on_square brd res st wc] handles the actual movement of the state 
    [st]'s current player around the board. This includes handling the 
    consequences of rolling doubles, passing GO, and being in Jail. *)
and landed_on_square brd res st wc = 
  let res' = check_card (current_location st) brd st in                
  match res' with 
  | Illegal -> Printf.printf "\nTry again, player %d\n" 
                 (current_player st);
    interp_command brd (Legal st) st wc ;
  | Win -> Printf.printf "\nYou won, player %d\n" 
             (current_player st); exit 0;
  | Legal st' -> 
    if not (is_in_jail st') 
    then (
      if is_in_jail st then (
        Printf.printf "\nYou got out of Jail with a double!\n";
        let moved = 
          if (current_location st' - current_location st) >= 0
          then current_location st' - current_location st
          else size brd + 
               (current_location st' - current_location st) 
        in 
        if (current_location st' - current_location st) < 0 then (
          Printf.printf "\nYou rolled %d\n" moved;
          Printf.printf "\nYou are at %s\n" 
            (nth_square brd (current_location st'));
          Printf.printf "\nYou've passed GO, player %d!\n" 
            (current_player st');
          let res'' = earn_cash st' 200 in 
          let st'' = update_state st' res'' in 
          let res''' = check_card (current_location st'') brd st in
          interp_command brd res''' st'' wc 
        ) else (
          Printf.printf "\nYou rolled %d\n" moved;
          Printf.printf "\nYou are at %s\n" 
            (nth_square brd (current_location st'));
          let res'' = check_card (current_location st') brd st in
          interp_command brd res'' st' wc 
        )
      ) else if not (is_in_jail st) then (
        interp_command brd res st' wc 
      ) else interp_command brd res st' wc 
    ) else (
      Printf.printf "\nYou need to roll a double or use a \
                     Get Out of Jail Free card to leave Jail\n";
      interp_command brd res st' wc 
    ) 

(** [buy_helper brd res st wc] executes the purchase of the property 
    the current player is on *)
and buy_helper brd res st wc = 
  match is_in_jail st with
  | true -> print_string "\nUh oh! You're in Jail, so you can't perform \
                          this action\n"; 
    interp_command brd res st wc
  | false -> (print_string "\nAre you sure you would like to \
                            buy this property?\n";
              Printf.printf "\n spend $%d.\n" 
                (cost brd (nth_square brd (current_location st)));
              print_string " > ";
              let confirmation = String.trim (read_line()) in 
              if confirmation = "yes" then
                (let prop = (current_location st) |> nth_square brd in 
                 let res = buy brd prop st in
                 (match res with 
                  | Illegal -> Printf.printf "\nUnfortunately this property \
                                              cannot be purchased at this \
                                              time.\n"; 
                    interp_command brd (Legal st) st wc 
                  | Legal st' -> Printf.printf "\nCongratulations! You are the \
                                                owner of %s.\n" prop;  
                    interp_command brd (Legal st') st' wc 
                  | Win -> Printf.printf "\nPlayer %d you have won the game! \
                                          You were the first player to acquire \
                                          ten properties!\n" 
                             (current_player st);  
                    exit 0)
                )
              else if confirmation = "no" then
                (Printf.printf "\nNo worries! Choose another action\n"; 
                 interp_command brd (Legal st) st wc )
              else (Printf.printf "\nInvalid response, please try again.\n";
                    interp_command brd (Legal st) st wc )
             )

(** [sell_helper brd res st wc p] executes the sale of [p] *)
and sell_helper brd res st wc p = 
  try 
    let prop = parse_obj_phrase p in 
    let can_buy = is_buyable brd prop && prop_available prop st in 
    if can_buy && not (is_in_jail st) then (
      print_string "\nAre you sure you would like to sell this property?\n";
      Printf.printf "You'd earn $%d.\n" (cost brd prop);
      print_string " > ";
      let confirmation = String.trim (read_line()) in 
      if confirmation = "yes" then
        (let res = sell brd prop st in
         match res with
         | Illegal -> Printf.printf "\nUnfortunately this property cannot \
                                     be sold at this time.\n"; 
           interp_command brd (Legal st) st wc 
         | Legal st' -> Printf.printf "\nCongratulations! You have successfully\
                                       sold %s.\n" prop; 
           interp_command brd (Legal st') st' wc 
         | Win -> Printf.printf "\nPlayer %d you seem to have won the game... \
                                 but we suspect you may have cheated.\n" 
                    (current_player st); 
           interp_command brd (Legal st) st wc )
      else if confirmation = "no" then
        (Printf.printf "\nOkay, what would you like to do instead?\n"; 
         interp_command brd (Legal st) st wc )
      else 
        (Printf.printf "\nInvalid response, please try again.\n";
         interp_command brd (Legal st) st wc ))
    else 
      let () =  print_string "\nYou can't sell this\n" in 
      interp_command brd res st wc
  with 
    UnknownSquare prop -> (Printf.printf "\nInvalid response, \
                                          please try again.\n";
                           interp_command brd (Legal st) st wc 
                          )

(** [next_helper brd res st wc] ends the current players turn and starts the 
    next player's turn. *)
and next_helper brd res st wc = 
  let res' = next_move res st in 
  let st' = update_state st res' in 
  interp_command brd res' st' wc 

(** [use_helper brd res st wc] executes the get out of jail card *)
and use_helper brd res st wc = 
  if is_in_jail st then 
    let new_st = 
      match get_out_of_jail brd st with
      | Legal st' -> print_string "\nYou got out of jail using your \
                                   Get Out of Jail Free Card!\n"; st'
      | Illegal -> print_string "\nYou can't get out of jail right now!\n"; 
        st 
      | Win -> print_string "\nLooks like you've cheated, try again!\n"; 
        st
    in 
    interp_command brd (Legal new_st) new_st wc
  else 
    let () = print_string "\nInvalid action—you're not in jail!\n" in 
    interp_command brd res st wc

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  Random.self_init ();
  let brd = get_board f in
  print_string "\nPlease enter a valid number of players for this game.\n";
  let n = number_of_players () in 
  print_string "\nPlease enter the amount of wealth a player must accumulate \
                to win the game\n";
  let w = most_money () in 
  let st = init_state brd n in
  let _ = interp_command brd Illegal st w in
  Stdlib.exit 0

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  print_string 
    "\nWelcome to the 3110 Monopoly Game Engine.\n";
  print_string "Please enter the name of the board file you want to load.\n";
  print_string  " > ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> try play_game file_name with 
      e ->
      print_string "\nInvalid board provided. Please try again.\n";
      main ()

(* Execute the game engine. *)
let () = main ()