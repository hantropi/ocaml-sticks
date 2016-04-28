(* Tournament Functions *)

(* Structures definition *)
type tournamentStick = Intact | Crossed;;
type tournamentBoard = tournamentStick list;;
type tournamentConfiguration = {nbrOfSticks : int; nbrMaxOfSticksToRemove : int; timeoutInMilliseconds : int;};;

let getNbrOfSticks = function
    {nbrOfSticks = n; nbrMaxOfSticksToRemove = _; timeoutInMilliseconds = _} -> n;;

let getNbrMaxOfSticksToRemove = function
    {nbrOfSticks = _; nbrMaxOfSticksToRemove = n; timeoutInMilliseconds = _} -> n;;

let getTimeoutInMilliseconds = function
    {nbrOfSticks = _; nbrMaxOfSticksToRemove = _; timeoutInMilliseconds = n} -> n;;

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
    if hl = Intact then print_string "\027[33m|"
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

let displayPlayer player =
  if (player mod 2) == 0 then print_string "\027[32mPLAYER 2"
  else print_string "\027[34mPLAYER 1";
  print_string "> \027[39m";;

(* askLine : *)
let askLine board =
  let line = read_int () in
  if line >= 0 && line < List.length(board) then line
  else -2;;

(* validMove : *)
let rec validMove board maxRemove i =
  match board with
  | [] -> true
  | hd::tl -> if i == maxRemove then true
    else if hd == Intact && i < maxRemove then validMove tl maxRemove (i + 1)
    else false;;

(* goTo : *)
let rec goTo board startPos endPos =
  if startPos >= endPos then board
  else goTo (List.tl(board)) (startPos + 1) endPos;;

(* validLength : *)
let validLength board startPos endPos maxRemove =
  if endPos - startPos + 1 > maxRemove then false
  else let board = goTo board 0 startPos in
    if endPos - startPos + 1 > List.length(board) then false
    else validMove board (endPos - startPos + 1) 0;;

(* removeSticks : *)
let rec removeSticks board startPos endPos i =
  match board with
  | [] -> []
  | hd::tl ->
    if i >= startPos && i <= endPos then Crossed::(removeSticks tl startPos endPos (i + 1))
    else hd::(removeSticks tl startPos endPos (i + 1));;

(* victory : *)
let rec victory board count =
  match board with
  | [] -> if count <= 1 then true else false
  | hd::tl ->
    if hd == Intact then victory tl (count + 1)
    else victory tl count;;

(* askLoop : *)
let rec askLoop board config player =
  (* Display all the informations *)
  displayBoard !board 0;
  displayNumbers (List.length(!board)) 0;
  displayPlayer !player;
  (* Ask the player *)
  let startLine = askLine !board in
  displayPlayer !player;
  let endLine = askLine !board in
  if validLength !board startLine endLine (getNbrMaxOfSticksToRemove config)
  then (board := removeSticks !board startLine endLine 0; incr player)
  else (print_string "\027[31mINPUT ERROR !\027[39m\n";
        askLoop board config player);;

(* tournamentPlay : *)
let rec tournamentPlay board config player =
  askLoop board config player;
  if not (victory !board 0) then tournamentPlay board config player;;

(* Main program *)
let config = {nbrOfSticks = 20; nbrMaxOfSticksToRemove = 3; timeoutInMilliseconds = 10};;
let board = ref (initBoard (getNbrOfSticks config));;
let player = ref 1;;
tournamentPlay board config player;;
(* End of a game *)
displayBoard !board 0;
displayNumbers (List.length(!board)) 0;
print_string "\027[36mPLAYER ";;
if (!player mod 2) == 0 then print_int 1
else print_int 2;; (* On inverse car on incremente a la fin de la partie *)
print_string " WIN !\n";;
