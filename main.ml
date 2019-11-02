let is_game_won st adv =
  let result = State.get_inventory st = [] in 
  if result then (
    print_endline(Adventure.win_msg adv);
    print_endline ("Thank you for playing!\n"); 
    exit 0;) else (
    ();
  )

(** [play_game f] starts the adventure in file [f]. *)
let rec play_game f =

  let rec parse_command adv st =
    let str = read_line () in
    try
      let command = Command.parse str in
      match command with
      | Quit -> print_endline ("Thank you for playing!\n"); exit 0;
      | Score -> print_string ("Score:");
        print_int (State.score st);
        print_string  "\n\n> ";
        parse_command adv st;
      | Go exit -> let potential_exit = String.concat " " exit in 
        let next_move = State.go potential_exit adv st in
        (match next_move with 
         | Illegal -> print_endline ("\nPlease provide a valid exit to go through\n");
           print_string  "> ";
           parse_command adv st;
         | Legal t -> 
           let updated = (State.update_state st next_move) in
           print_endline (Adventure.description adv (State.get_current_room st next_move));
           print_string ("\nThe following items can be found at ");
           print_string (State.get_current_room updated next_move);
           print_string (": ");
           print_endline (State.print_inventory updated (State.get_current_room updated next_move));
           print_string  "\n> ";
           parse_command adv updated;)
      | Take item -> let potential_item = String.concat " " item in 
        let next_item = State.take potential_item adv st in 
        (match next_item with
         | Illegal -> print_endline ("\nPlease provide a valid item to pick up\n");
           print_string "> ";
           parse_command adv st;
         | Legal t -> 
           let updated = (State.update_state st next_item) in 
           print_string ("Your inventory now contains: ");
           print_endline (State.print_inventory updated "inventory");
           print_string ("\nThe following items can be found at ");
           print_string (State.get_current_room updated next_item);
           print_string (": ");
           print_endline (State.print_inventory updated (State.get_current_room updated next_item));
           print_string  "\n> ";
           parse_command adv updated;)
      | Drop item -> let potential_item = String.concat " " item in 
        let next_item = State.drop potential_item adv st in 
        (match next_item with
         | Illegal ->print_endline ("\nPlease provide a valid item to drop\n");
           print_string "> ";
           parse_command adv st;
         | Legal t -> 
           let updated = (State.update_state st next_item) in 
           is_game_won updated adv;
           print_string ("\nYour inventory now contains: ");
           print_endline (State.print_inventory updated "inventory");
           print_string ("\nThe following items can be found at ");
           print_string (State.get_current_room updated next_item);
           print_string (": ");
           print_endline (State.print_inventory updated (State.get_current_room updated next_item));
           print_string  "\n> ";
           parse_command adv updated;)
      | Inventory -> print_string ("\nYour inventory now contains: ");
        print_endline (State.print_inventory st "inventory");
        print_string  "\n> ";
        parse_command adv st;
    with
    | Command.Empty -> print_endline ("Please provide a valid command string\n");
      print_string  "> ";
      parse_command adv st
    | Command.Malformed -> print_endline ("Please provide a valid command string\n");
      print_string  "> ";
      parse_command adv st
  in
  try 
    let j = Yojson.Basic.from_file f in 
    let adv = Adventure.from_json j in 
    let st = State.init_state adv in 
    print_endline (Adventure.description adv (Adventure.start_room adv));
    print_string ("\nThe following items can be found: ");
    print_endline (State.print_inventory st (Adventure.start_room adv));
    print_string  "\n> ";
    parse_command adv st
  with 
    Sys_error _ -> print_endline ("Invalid Adventure file provided. Please make sure your JSON file exists.\n"); 
    print_string  "> ";
    play_game (read_line ())

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
