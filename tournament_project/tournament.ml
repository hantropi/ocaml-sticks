(** Tournament **)

(*pveLoop : (Run only with 20 sticks) *)
let pveLoop board nbrMaxOfSticksToRemove player =
  (* Display all the informations *)
  Functions.displayBoard !board 0;
  Functions.displayNumbers (List.length(!board)) 0;
  if (player mod 2) == 0 then (* Ask the player *)
    board := Player.askPlayer !board nbrMaxOfSticksToRemove player
  else (* Ask the AI *)
    board := Ai.askAi !board player;;

(* pvpLoop : *)
let pvpLoop board nbrMaxOfSticksToRemove player =
  (* Display all the informations *)
  Functions.displayBoard !board 0;
  Functions.displayNumbers (List.length(!board)) 0;
  (* Ask the player *)
  board := Player.askPlayer !board nbrMaxOfSticksToRemove player;;

(* victory : *)
let victory board =
  if Functions.countSticks board <= 1 then true
  else false;;

(* tournamentPlay : *)
let rec tournamentPlay board config player =
  if (Functions.getPve config) then pveLoop board (Functions.getNbrMaxOfSticksToRemove config) !player
  else pvpLoop board (Functions.getNbrMaxOfSticksToRemove config) !player;
  incr player;
  if not (victory !board) then tournamentPlay board config player;;

(* Main program *)
let config = Functions.{pve = true; nbrOfSticks = 20; nbrMaxOfSticksToRemove = 3; timeoutInMilliseconds = 10};;
let board = ref (Functions.initBoard (Functions.getNbrOfSticks config));;
let player = ref 1;;
tournamentPlay board config player;;
(* End of a game *)
Functions.displayBoard !board 0;
Functions.displayNumbers (List.length(!board)) 0;
print_string "\027[36mPLAYER ";;
if (!player mod 2) == 0 then print_int 1
else print_int 2;; (* On inverse car on incremente a la fin de la partie *)
print_string " WIN !\n";;
