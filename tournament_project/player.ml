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
    else if hd == Functions.Intact && i < maxRemove then validMove tl maxRemove (i + 1)
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

(* askPlayer : *)
let rec askPlayer board nbrMaxOfSticksToRemove player =
  Functions.displayPlayer player;
  let startLine = askLine board in
  Functions.displayPlayer player;
  let endLine = askLine board in
  if validLength board startLine endLine nbrMaxOfSticksToRemove
  then Functions.removeSticks board startLine endLine 0
  else (print_string "\027[31mINPUT ERROR !\027[39m\n";
        askPlayer board nbrMaxOfSticksToRemove player);;
