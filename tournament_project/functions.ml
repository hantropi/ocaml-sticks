(** Structures definition **)

type tournamentStick = Intact | Crossed;;
type tournamentBoard = tournamentStick list;;
type tournamentConfiguration = {pve : bool; nbrOfSticks : int; nbrMaxOfSticksToRemove : int; timeoutInMilliseconds : int;};;

(* tournamentConfiguration functions *)
let getPve = function
    {pve = n; nbrOfSticks = _; nbrMaxOfSticksToRemove = _; timeoutInMilliseconds = _} -> n;;

let getNbrOfSticks = function
    {pve = _; nbrOfSticks = n; nbrMaxOfSticksToRemove = _; timeoutInMilliseconds = _} -> n;;

let getNbrMaxOfSticksToRemove = function
    {pve = _; nbrOfSticks = _; nbrMaxOfSticksToRemove = n; timeoutInMilliseconds = _} -> n;;

let getTimeoutInMilliseconds = function
    {pve = _; nbrOfSticks = _; nbrMaxOfSticksToRemove = _; timeoutInMilliseconds = n} -> n;;

(* initBoard :
   int -> tournamentBoard *)
let rec initBoard number =
  if number <= 0 then []
  else Intact::initBoard(number - 1);;

(* displayBoard : Display the content of the board on the screen
   list(int) -> unit () *)
let rec displayBoard board i =
  match board with
  | [] -> print_char '\n'
  | hl::tl ->
    (* Display | : Intact or + : Crossed *)
    if hl == Intact then print_string "\027[33m|"
    else print_string "\027[35m+";
    (* Display spaces between the sticks *)
    if i < 10 then print_string "\027[39m "
    else if i < 100 then print_string "\027[39m  "
    else print_string "\027[39m   ";
    displayBoard tl (i + 1);;

(* displayNumbers : Display the number below each stick
   int -> unit () *)
let rec displayNumbers number i =
  if number > 0 then
    (print_int i;
     print_char ' ';
     displayNumbers (number - 1) (i + 1))
  else print_char '\n';;

(* displayPlayer : Display the current player's number
   int -> unit () *)
let displayPlayer player =
  if (player mod 2) == 0 then print_string "\027[32mPLAYER 2"
  else print_string "\027[34mPLAYER 1";
  print_string "> \027[39m";;

(* removeSticks : Remove the sticks between startPos and endPos and return the board
   tournamentBoard * int * int * int -> tournamentBoard *)
let rec removeSticks board startPos endPos i =
  match board with
  | [] -> []
  | hd::tl ->
    if i >= startPos && i <= endPos then Crossed::(removeSticks tl startPos endPos (i + 1))
    else hd::(removeSticks tl startPos endPos (i + 1));;

(* countSticks : Count the number of intact sticks in the board
   tournamentBoard -> int *)
let rec countSticks board =
  match board with
  | [] -> 0
  | hd::tl ->
    if hd == Intact then 1 + countSticks tl
    else 0 + countSticks tl;;
