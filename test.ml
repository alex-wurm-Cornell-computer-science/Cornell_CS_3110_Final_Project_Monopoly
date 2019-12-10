open OUnit2
open Board
open Command
open State


(** TEST PLAN :
    Tests for functions in [Board.ml]. For the purposes of testing these 
    functions we generated [test_board] and the smaller [test_board2]. We 
    tested every function in [Board.mli] using a combination of black box and 
    glass box testing. When a function  was written in a .mli file, tests were 
    created based on the specification. Then, after implementing the function, 
    more tests were add. A smaller board, test_board, was created to make 
    testing easier. It is a truncated version of real_board, which is a standard 
    monopoly board. The functions we implementedbehave the same on all valid 
    boards, so testing on a small board is indicative ofperformance on a full 
    board. The same methodology was also used when testing [State.ml],
    with the addition of play testing. For the purposes of testing these 
    functions we generated [test_goojf], [jail_board], and [tax_board], and 
    still used test_board. These allowed us to separate the testing of jail 
    functions, rent functions, card functions, and tax functions, ensuring they 
    work independently of each other and that they work together in the full 
    game. While doing this, we hard-coded the roll function to roll a 1 each 
    time the player rolls the dice. This strategy allowed us to save time with 
    constructing states by initializing states from each test board and moving 
    quickly to the relevant space we wanted to test actions on. You can 
    hard-code the roll option by commenting out lines 142-143 in State.ml and 
    uncommenting lines 146-147. Make sure to revert this change before playing 
    the game regularly. Functions in this module, as well as functions in main, 
    rely extensively on user input. Thus, we play tested all functions
    with valid inputs, adding addition test cases to Board and State when 
    finding bugs, and inputting invalid inputs to ensure they did not break the 
    system. In sum, we constructed unit tests for board and state functions, 
    and playtested state  and main functions. As with a2, this approach allows 
    us to ensure all functions  exposed in .mli files that can be tested are 
    tested, while allowing us to  pass many edge cases to user dependent 
    functions. Separating components (cards, jail, ...)to test allowed us to 
    debug them individually, and then testing multiple components  ensured that 
    the results of one component did not impact the other components.

    INSTRUCTIONS FOR TESTING: 
    Because we created truncated boards for running tests, testing will require 
    a minor change to the released game. In State.ml, function Roll (Line 141),
    The lines [(* let die1 = (Random.int 5) + 1 in 
     let die2 = (Random.int 5) + 1 in *)
    (* let die1 = 3 in 
     let die2 = 3 in  *)] (142-145) must be commented. Lines [let die1 = 0 in 
    let die2 = 1 in  ] must be uncommented (146-147). This hard codes the 
    dice roll to 1, allowing the test boards to work.
*)

(********************************************************************
   Here are some helper functions for your testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and 
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo = 
  [
    "order is irrelevant" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(** Boards for unit tests. *)
let test_board = from_json (Yojson.Basic.from_file "test_board.json")
let real_board = from_json (Yojson.Basic.from_file "standard_board.json")
let test_board2 = from_json (Yojson.Basic.from_file "test_2.json")
let jail_board = from_json (Yojson.Basic.from_file "test_goojf.json")
let tax_board = from_json (Yojson.Basic.from_file "tax_board.json")

let board_tests_valid = [
  "test size" >:: (fun _ -> assert_equal 12 (size test_board));
  "test cost" >:: (fun _ -> assert_equal 100 (cost test_board "Baltic Avenue"));
  "test cost 2" >:: (fun _ -> assert_equal 700 
                        (cost real_board "Oriental Avenue"));
  "invalid property cost" >:: 
  ( fun _ -> try let _ = cost real_board "bad room" in assert false
    with UnknownSquare r -> assert true);
  "test rent" >:: (fun _ -> assert_equal 100 (rent test_board "Baltic Avenue"));
  "invalid property rent" >:: 
  ( fun _ -> try let _ = rent real_board "bad room" in assert false
    with UnknownSquare r -> assert true);
  "test all squares" >:: (fun _ -> assert_equal true (
      cmp_set_like_lists [ "GO"; "Mediterranean Avenue" ; "Community Chest" ; 
                           "Baltic Avenue" ; "Income Tax" ; "Chance" ; "Jail" ; 
                           "Go To Jail"
                         ; "Park Place" ; "Boardwalk" ; "Luxury Tax"] 
        (all_squares test_board |> List.sort_uniq compare)
    ));
  "test monopoly" >:: (fun _ -> assert_equal true 
                          (cmp_set_like_lists ["Mediterranean Avenue" ; 
                                               "Baltic Avenue"] 
                             (monopoly_group test_board "Brown")));
  "test monopoly2" >:: (fun _ -> assert_equal true 
                           (cmp_set_like_lists ["Mediterranean Avenue" ; 
                                                "Baltic Avenue"] 
                              (monopoly_group test_board2 "Brown")));
  "invalid monopoly group" >:: 
  ( fun _ -> try let _ = monopoly_group real_board "bad room" in assert false
    with UnknownSquare r -> assert true);
  "test house price" >:: (fun _ -> assert_equal (Some 50) 
                             (house_cost test_board "Boardwalk"));
  "test hotel price" >:: (fun _ -> assert_equal (Some 100) 
                             (hotel_cost test_board "Boardwalk"));
  "invalid property hotel price" >:: 
  ( fun _ -> try let _ = hotel_cost real_board "bad room" in assert false
    with UnknownSquare r -> assert true);
  "test square pos" >:: (fun _ -> assert_equal 4 
                            (square_pos test_board "Income Tax"));
  "invalid property square pos" >:: 
  ( fun _ -> try let _ = square_pos real_board "bad room" in assert false
    with UnknownSquare r -> assert true);
  "test nth square" >:: (fun _ -> assert_equal "Chance" 
                            (nth_square test_board 5));
  "invalid nth square" >:: 
  ( fun _ -> try let _ = nth_square real_board 9000 in assert false
    with UnknownSquare r -> assert true);
  "buildable prop" >:: (fun _ -> assert_equal true 
                           (is_buildable test_board "Boardwalk"));
  "not buildable prop" >:: ( fun _ -> assert_equal false 
                               (is_buildable test_board "Chance"));
  "buildable bad prop" >:: 
  ( fun _ -> try let _ = is_buildable test_board "bad square" in assert false
    with UnknownSquare r -> assert true);
  "buyable prop" >:: (fun _ -> assert_equal true 
                         (is_buyable test_board "Baltic Avenue"));
  "buyable bad prop" >:: 
  ( fun _ -> try let _ = is_buyable test_board "bad square" in assert false
    with UnknownSquare r -> assert true);
  "unbuyable prop" >:: (fun _ -> assert_equal false 
                           (is_buyable test_board "Chance"));
  "square color" >:: (fun _ -> assert_equal (Some "Dark Blue")
                         (square_color test_board "Park Place"));
  "invalid square color " >:: 
  ( fun _ -> try let _ = square_color real_board "bad square" in assert false
    with UnknownSquare r -> assert true);
  "card payment" >:: (fun _ -> assert_equal (-100) 
                         (card_payment real_board "lose money"));
  "card type" >:: (fun _ -> assert_equal LeaveJail
                      (card_type real_board "get out"));
  "invalid card type" >:: 
  ( fun _ -> try let _ = card_type real_board "bad card" in assert false
    with UnknownCard r -> assert true);
  "invalid card payment" >:: 
  ( fun _ -> try let _ = card_payment real_board "bad card" in assert false
    with UnknownCard r -> assert true);
  "monopoly_group_named" >:: 
  (fun _ -> assert_equal true (
       cmp_set_like_lists 
         ["Mediterranean Avenue" ; "Baltic Avenue"] 
         (monopoly_group_named test_board "Baltic Avenue")));
  "bad monopoly named" >:: 
  ( fun _ -> try let _ = (monopoly_group_named real_board "bad prop") 
      in assert false with UnknownSquare r -> assert true);
]

(** *)
let state_tests = 
  let st = init_state real_board 2 in 
  let cash_state = match (earn_cash st (-200)) with 
      Legal st' -> st' 
    | _ -> failwith "" in 
  let card_shuffle = match move_cards real_board "lose money" st with 
    | Legal st' -> st'
    | _ -> failwith "" in 
  let pick_up_card = match move_cards real_board "get out" st with 
    | Legal st' -> st'
    | _ -> failwith "" in 
  let rolled_bought = match roll real_board st with 
    | Legal st' -> begin 
        match buy real_board "Mediterranean Avenue" st' with 
        | Legal st' -> st'
        | _ -> failwith "" 
      end  
    | _ -> failwith "" in 
  let jail_st = init_state jail_board 2 in 
  let move11 = match (roll jail_board jail_st) with 
    | Legal st1 -> st1 
    | _ -> failwith "" in
  let pickup_goojf1 = match move_cards jail_board "get out" move11 with
    | Legal st2 -> st2
    | _ -> failwith "" in
  let turn2 = match (next_turn pickup_goojf1) with
    | Legal st3 -> st3
    | _ -> failwith "" in 
  let move21 = match (roll jail_board turn2) with 
    | Legal st4 -> st4
    | _ -> failwith "" in 
  let pickup_goojf2 = match move_cards jail_board "get out" move21 with
    | Legal st5 -> st5
    | _ -> failwith "" in
  let turn3 = match (next_turn pickup_goojf2) with
    | Legal st6 -> st6
    | _ -> failwith "" in 
  let move12 = match (roll jail_board turn3) with 
    | Legal st7 -> st7
    | _ -> failwith "" in
  let turn4 = match (next_turn move12) with
    | Legal st8 -> st8
    | _ -> failwith "" in 
  let move22 = match (roll jail_board turn4) with 
    | Legal st9 -> st9
    | _ -> failwith "" in
  let turn5 = match (next_turn move22) with
    | Legal st10 -> st10
    | _ -> failwith "" in 
  let get_out = match (get_out_of_jail jail_board turn5) with
    | Legal st11 -> st11 
    | _ -> failwith "" in
  let tax_st = init_state tax_board 2 in 
  let move_tax = match (roll tax_board tax_st) with 
    | Legal st' -> st'
    | _ -> failwith "" in
  let tax_paid = match (pay_tax tax_board move_tax 5) with
    | Legal st'' -> st''
    | _ -> failwith "" in
  let cant_buy = match (buy tax_board "Luxury Tax" tax_paid) with 
    | Legal st' -> st'
    | Illegal -> tax_paid 
    | _ -> failwith "" in
  let roll_twice = match roll tax_board tax_paid with 
    | Illegal -> tax_paid
    | _ -> failwith "" in 
  [
    "earning cash" >:: (fun _ -> assert_equal 1300 (List.assoc 1 
                                                      (wallets cash_state)));
    "index of location" >:: (fun _ -> assert_equal 0 
                                (List.assoc 1 (locations st) |> fst));
    "first card" >:: (fun _ -> assert_equal "lose money" (next_card st));
    "rearranged deck first card" >:: (fun _ -> assert_equal "gain money" 
                                         (next_card card_shuffle));
    "jail card removed from deck" >:: (fun _ -> assert_equal 
                                          ["lose money" ; "gain money"; "jail"; 
                                           "Reading"]
                                          (cards pick_up_card));
    "curr player" >:: (fun _ -> assert_equal 1 (current_player st));
    "next player" >:: (fun _ -> assert_equal 2 
                          (let st1 = match roll real_board 
                                             (init_state real_board 2) with
                           | Legal sti -> sti
                           | Illegal -> failwith "" 
                           | Win -> failwith ""
                           in 
                           let st2 = match next_turn st1 with 
                             | Legal stn -> stn
                             | Illegal -> failwith ""
                             | Win -> failwith ""
                           in 
                           current_player st2));
    "num players" >:: (fun _ -> assert_equal 2 (num_players st));
    "locs1" >:: (fun _ -> assert_equal 0 ( 
        List.assoc 2 (locations st) |> fst));
    "property in items when bought" >:: 
    (fun _ -> assert_equal  "Mediterranean Avenue"
        (List.hd (curr_player_inventory rolled_bought)));
    "property paid for" >:: (fun _ -> assert_equal 1440 
                                (curr_player_wallet rolled_bought));
    "houses not built" >:: (fun _ -> assert_equal 0 
                               (houses rolled_bought "Mediterranean Avenue" ));
    "update state Illegal" >:: (fun _ -> assert_equal st
                                   (update_state st Illegal));

    "hotels not built" >:: (fun _ -> assert_equal 0 
                               (hotels rolled_bought "Mediterranean Avenue" ));
    "wealth same after buying" >:: 
    (fun _ -> assert_equal 60 (inventory_value real_board rolled_bought));
    "get out of jail card in items" >:: 
    (fun _ -> assert_equal ["get out"] (curr_player_items pickup_goojf1));
    "get out of jail to next space" >:: 
    (fun _ -> assert_equal "Boardwalk" 
        ((current_location get_out) |> nth_square jail_board));
    "get out of jail card is used" >:: (fun _ -> assert_equal [] 
                                           (curr_player_items get_out));
    "get out of jail card is returned" >:: (fun _ -> 
        assert_equal ["gain money";"get out"] (cards get_out));
    "tax is paid" >:: (fun _ -> assert_equal 1450 
                          (curr_player_wallet tax_paid));
    "unbuyable not added" >:: (fun _ -> assert_equal tax_paid cant_buy);
    "can't roll twice" >:: (fun _ -> assert_equal tax_paid roll_twice)
  ]

(** The test suite encapsulates all test groups generated in [test.ml]. To add 
    a new group of tests to runs when using the make test command, enter the 
    name of that test in the brackets below. The main module was tested through 
    playtesting. Its function relies on direct player input, so we did not test 
    them with an OUnit suite *)
let suite =
  "test suite for A2"  >::: List.flatten [
    board_tests_valid;
    state_tests
  ]

let _ = run_test_tt_main suite