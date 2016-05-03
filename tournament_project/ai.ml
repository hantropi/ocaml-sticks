(** Ai functions **)

(* numberOfSticksToRemove : Check the number of sticks to remove from the current board
   int -> int *)
let numberOfSticksToRemove count =
  if count <= 20 && count > 17 then 3
  else if count <= 17 && count > 13 then count - 13
  else if count <= 13 && count > 9 then count - 9
  else if count <= 9 && count > 5 then count - 5
  else 3;;

(* defineStartPos : Determine the first position where the AI can remove the sticks depending on numberToRemove
   tournamentBoard * int * int * int -> int *)
let rec defineStartPos board numberToRemove startPos i =
  match board with
  | [] ->
    if (i - startPos) == numberToRemove then startPos
    else -2
  | hd::tl ->
    if (i - startPos) == numberToRemove then startPos
    else if hd == Functions.Intact && startPos == i then defineStartPos tl numberToRemove i (i + 1)
    else if hd == Functions.Intact && startPos >= 0 then defineStartPos tl numberToRemove startPos (i + 1)
    else defineStartPos tl numberToRemove (i + 1) (i + 1);;

(* getStartPos : Get the startPos value
   tournamentBoard * int -> int *)
let rec getStartPos board numberToRemove =
  let startPos = defineStartPos board numberToRemove 0 0 in
  if startPos >= 0 then startPos, numberToRemove
  else getStartPos board (numberToRemove - 1);;

(* askAi :
   tournamentBoard * int -> tournamentBoard *)
let askAi board player =
  let count = Functions.countSticks board in
  let numberToRemove = numberOfSticksToRemove count in
  let startPos, newNumberToRemove = getStartPos board numberToRemove in
  Functions.displayPlayer player;
  print_int startPos;
  print_char '\n';
  Functions.displayPlayer player;
  print_int (startPos + (newNumberToRemove - 1));
  print_char '\n';
  Functions.removeSticks board startPos (startPos + (newNumberToRemove - 1)) 0;;
