open OUnit2
open Adventure
open Command
open State

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

(* You are welcome to add strings containing JSON here, and use them as the
   basis for unit tests.  Or you can add .json files in this directory and
   use them, too.  Any .json files in this directory will be included
   by [make zip] as part of your CMS submission. *)

let make_start_room_test
    (name:string)
    (adv: Adventure.t)
    (expected_output : Adventure.room_id): test =
  name >:: (fun _ ->
      assert_equal expected_output (start_room adv))

let make_room_ids_test
    (name:string)
    (adv:Adventure.t)
    (expected_output:room_id list): test =
  name >:: (fun _ ->
      assert_equal true (cmp_set_like_lists expected_output (room_ids adv)))

let make_description_test
    (name:string)
    (adv: Adventure.t)
    (room:room_id)
    (expected_output:string): test = 
  name >:: (fun _ ->
      assert_equal expected_output (description adv room))

let make_exits_test
    (name:string)
    (adv:Adventure.t)
    (room:room_id)
    (expected_output:exit_name list): test =
  name >:: (fun _ ->
      assert_equal true (cmp_set_like_lists expected_output (exits adv room)))

let make_next_room_test
    (name:string)
    (adv:Adventure.t)
    (room:room_id)
    (exit:exit_name)
    (expected_output:room_id): test =
  name >:: (fun _ ->
      assert_equal expected_output (next_room adv room exit))

let make_next_rooms_lst_test
    (name:string)
    (adv:Adventure.t)
    (room:room_id)
    (expected_output:room_id list): test = 
  name >:: (fun _ ->
      assert_equal true 
        (cmp_set_like_lists expected_output (next_rooms adv room))
    )

let test_json1 = from_json (Yojson.Basic.from_file "ho_plaza.json")
let test_json2 = from_json (Yojson.Basic.from_file "lonely_room.json")

let adventure_tests =
  [
    (** start_room *)
    make_start_room_test "start room is ho plaza" test_json1 "ho plaza";
    make_start_room_test "start room is the room" test_json2 "the room";

    (** room_ids *)
    make_room_ids_test "should be set like list" 
      test_json1 ["ho plaza";"health";"tower";"nirvana";];
    make_room_ids_test "should be a list of one element"  
      test_json2 ["the room";];

    (** description *)
    make_description_test "ho plaza description" 
      test_json1 "ho plaza" "You are on Ho Plaza. Cornell Health is to the southwest. The chimes are playing a concert in the clock tower. Someone tries to hand you a quartercard, but you avoid them.";
    make_description_test "health description" 
      test_json1 "health" "You are at the entrance to Cornell Health. A sign advertises free flu shots. You briefly wonder how long it would take to get an appointment. Ho Plaza is to the northeast.";
    make_description_test "tower description" 
      test_json1 "tower" "You climbed up all 161 steps to the top of McGraw Tower. A Chimesmaster is playing the Jennie McGraw Rag. You feel inspired to ascend higher.";
    make_description_test "nirvana description" 
      test_json1 "nirvana" "You have reached a higher level of existence.  There are no more words.";
    make_description_test "the room description" 
      test_json2 "the room" "A very lonely room.";

    (** exits *)
    make_exits_test "ho plaza exits" 
      test_json1 "ho plaza" ["southwest";"south west";"Cornell Health";
                             "Gannett";"chimes";"concert";"clock tower";];
    make_exits_test "health exits" 
      test_json1 "health" ["northeast";"north east";"Ho Plaza";];
    make_exits_test "tower exits" 
      test_json1 "tower" ["down";"back";"Ho Plaza";"higher";];
    make_exits_test "nirvana exits" test_json1 "nirvana" [];
    make_exits_test "the room exits" test_json2 "the room" [];

    (** next_room *)
    make_next_room_test "ho plaza -> southwest" 
      test_json1 "ho plaza" "southwest" "health";
    make_next_room_test "ho plaza -> south west" 
      test_json1 "ho plaza" "south west" "health";
    make_next_room_test "ho plaza -> Cornell Health" 
      test_json1 "ho plaza" "Cornell Health" "health";
    make_next_room_test "ho plaza -> Gannett" 
      test_json1 "ho plaza" "Gannett" "health";
    make_next_room_test "ho plaza -> chimes" 
      test_json1 "ho plaza" "chimes" "tower";
    make_next_room_test "ho plaza -> concert" 
      test_json1 "ho plaza" "concert" "tower";
    make_next_room_test "ho plaza -> clock tower" 
      test_json1 "ho plaza" "clock tower" "tower";
    make_next_room_test "health -> northeast" 
      test_json1 "health" "northeast" "ho plaza";
    make_next_room_test "health -> north east" 
      test_json1 "health" "north east" "ho plaza";
    make_next_room_test "health -> Ho Plaza" 
      test_json1 "health" "Ho Plaza" "ho plaza";
    make_next_room_test "tower -> down" 
      test_json1 "tower" "down" "ho plaza";
    make_next_room_test "tower -> back" 
      test_json1 "tower" "back" "ho plaza";
    make_next_room_test "tower -> Ho Plaza" 
      test_json1 "tower" "Ho Plaza" "ho plaza";
    make_next_room_test "tower -> higher" 
      test_json1 "tower" "higher" "nirvana";

    (** next_rooms *)
    make_next_rooms_lst_test "rooms from ho plaza" 
      test_json1 "ho plaza" ["health";"tower";];
    make_next_rooms_lst_test "rooms from health" 
      test_json1 "health" ["ho plaza";];
    make_next_rooms_lst_test "rooms from tower" 
      test_json1 "tower" ["ho plaza";"nirvana";];
    make_next_rooms_lst_test "rooms from nirvana" 
      test_json1 "nirvana" [];
    make_next_rooms_lst_test "rooms from the room" 
      test_json2 "the room" [];
  ]

let make_parse_test_valid
    (name:string)
    (str:string)
    (expected_output:Command.command): test = 
  name >:: (fun _ ->
      assert_equal expected_output (parse str))

let make_parse_test_empty
    (name:string)
    (str:string): test = 
  name >:: (fun _ ->
      assert_raises (Empty) (fun() -> parse str))

let make_parse_test_malformed
    (name:string)
    (str:string): test = 
  name >:: (fun _ ->
      assert_raises (Malformed) (fun() -> parse str))

let command_tests =
  [
    (** valid strings *)
    make_parse_test_valid "valid go command" 
      "go hello world" (Go ["hello";"world";]);
    make_parse_test_valid "valid go command with spaces" 
      "    go  hello     world  " (Go["hello";"world";]);
    make_parse_test_valid "valid quit command" "quit" (Quit);
    make_parse_test_valid "valid quit command with spaces" 
      "  quit      " (Quit);

    (** invalid strings that throw Empty *)
    make_parse_test_empty "empty string" "";
    make_parse_test_empty "empty strings with spaces" "      ";

    (** invalid strings that throw Malformed *)
    make_parse_test_malformed "invalid go command" "go";
    make_parse_test_malformed "invalid go command with spaces" "   go   ";
    make_parse_test_malformed "invalid quit command" "quit hello world";
    make_parse_test_malformed "invalid quit command with spaces" 
      "  quit  hello world";
    make_parse_test_malformed "invalid command word" "hello world";
    make_parse_test_malformed "invalid order of words" "hello go world";
  ]

let make_init_state_test
    (name:string)
    (adv:Adventure.t)
    (expected_current:string): test = 
  name >:: (fun _ ->
      assert_equal expected_current ((init_state adv) |> current_room_id))


let make_current_room_id_test
    (name:string)
    (state:State.t)
    (expected_output:string): test = 
  name >:: (fun _ ->
      assert_equal expected_output (current_room_id state))

let make_visited_test
    (name:string)
    (state:State.t)
    (expected_output:string list): test = 
  name >:: (fun _ ->
      assert_equal true (cmp_set_like_lists expected_output (visited state)))

let make_go_test
    (name:string)
    (exit:exit_name)
    (adv:Adventure.t)
    (state:State.t)
    (expected_output:string): test = 
  name >:: (fun _->
      assert_equal expected_output (get_current_room state (go exit adv state)))

let ho_health = go "Gannett" test_json1 (init_state test_json1) 
let health_state = update_state (init_state test_json1) ho_health
let ho_tower = go "chimes" test_json1 (init_state test_json1) 
let tower_state = update_state (init_state test_json1) ho_tower

let state_tests =
  [
    (** init_state *)
    make_init_state_test "ho plaza starting room" test_json1 "ho plaza";
    make_init_state_test "lonely room starting room" test_json2 "the room";

    (** current_room_id *)
    make_current_room_id_test "in ho plaza" (init_state test_json1) "ho plaza";
    make_current_room_id_test "in the room" (init_state test_json2) "the room";

    (** visited *)
    make_visited_test "ho plaza, visited none" (init_state test_json1) [];
    make_visited_test "the room, visited none" (init_state test_json2) [];

    (** go - Legal tests *)
    make_go_test "ho plaza -> Gannett" "Gannett"
      test_json1 (init_state test_json1)  "health";
    make_go_test "health -> northeast" "northeast" 
      test_json1 health_state "ho plaza";
    make_go_test "ho plaza -> chimes" "chimes" 
      test_json1 (init_state test_json1)  "tower";
    make_go_test "tower -> higher" "higher" 
      test_json1 tower_state "nirvana";

    (** go - Illegal tests *)
    make_go_test "health -> higher" "higher" 
      test_json1 health_state "health";
    make_go_test "ho plaza -> higher" "higher" 
      test_json1 (init_state test_json1) "ho plaza";
  ]

let suite =
  "test suite for A2"  >::: List.flatten [
    adventure_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite
