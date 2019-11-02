(** 
   The main entry point for the game interface.
*)

(** [is_game_won st adv] ends the adventure [adv] if the adventurer's 
    inventory in state [st] is empty. *)
(* val is_game_won : State.t -> Adventure.t -> unit *)

(** [play_game f] starts the adventure in file [f]. *)
val play_game : string -> unit

(* You are free to add more code here. *)
