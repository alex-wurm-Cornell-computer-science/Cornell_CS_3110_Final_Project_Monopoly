open OUnit2
open Board
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

let test_board = from_json (Yojson.Basic.from_file "test_board.json")

let board_tests_valid = [
  "test size" >:: (fun _ -> assert_equal 8 (size test_board));
  "test cost" >:: (fun _ -> assert_equal 100 (cost test_board "Baltic Avenue"));
  "test rent" >:: (fun _ -> assert_equal 100 (rent test_board "Baltic Avenue"));
  "test all squares" >:: (fun _ -> assert_equal true (
      cmp_set_like_lists [ "GO"; "Mediterranean Avenue" ; "Community Chest" ; 
                           "Baltic Avenue" ; "Income Tax" ; "Chance" ; "Jail" ; "Go To Jail"]
        (all_squares test_board)
    ));
  "test monopoly" >:: (fun _ -> assert_equal true 
                          (cmp_set_like_lists ["Mediterranean Avenue" ; 
                                               "Baltic Avenue"] 
                             (monopoly_group test_board "Brown")));
]

let suite =
  "test suite for A2"  >::: List.flatten [
    board_tests_valid
  ]

let _ = run_test_tt_main suite
