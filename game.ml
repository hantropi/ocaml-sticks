(** Game **)

(* initSticks : Initialize the list of sticks
   int -> list(int) *)
let rec initSticks number =
  if number <= 0 then []
  else 1::initSticks(number - 1);;

(* displaySticks : Display the sticks on the screen
   list(int) -> unit () *)
let rec displaySticks list =
  match list with
  | [] -> print_char '\n'
  | hl::tl -> if hl = 1 then print_char '|'
    else print_char '+';
    print_char ' ';
    displaySticks tl;;

(* displayNumbers : Display the number below each stick
   int -> unit () *)
let displayNumbers number =
  for i = 0 to (number - 1) do
    print_int i;
    print_char ' ';
  done;
  print_char '\n';;

(* displayAll : Display the sticks and the numbers
   list(int) -> unit () *)
let rec displayAll list =
  displaySticks list; (* Voir si on garde l'affichage des numeros *)
  displayNumbers (List.length(list));;

(* askRow : *)
let askRow list =
  print_string "> ";
  let row = read_int () in
  if row >= 0 && row < List.length(list) then row
  else 100;;

(* validMove : *)
let rec validMove list maxBat valInter =
  match list with
  | [] -> true
  | hd::tl -> if valInter = maxBat then true
    else if hd = 1 && valInter < maxBat then validMove tl maxBat (valInter + 1)
    else false;;

(* goTo :
   int * int * list(int) -> list(int)
   startPos, endPos, list -> supprime la liste jusqu'à la position endPos (position endPos exclus) *)
let rec goTo startPos endPos list =
  if startPos >= endPos then list
  else goTo (startPos + 1) endPos (List.tl(list));;

(* validLength : *)
let validLength startPos endPos rmSticks list =
  if endPos - startPos + 1 > rmSticks then false
  else let list = goTo 0 startPos list in
    if endPos - startPos + 1 > List.length(list) then false
    else validMove list (endPos - startPos + 1) 0;;

(* removeSticks : *)
let rec removeSticks startPos endPos valInter list =
  match list with
  | [] -> []
  | hd::tl -> if valInter >= startPos && valInter <= endPos then 0::(removeSticks startPos endPos (valInter+1) tl)
    else hd::(removeSticks startPos endPos (valInter+1) tl);;

(* victory : *)
let victory list =
  let nbrSticks = ref 0 in
  for i = 0 to (List.length(list) - 1) do
    if (List.nth list i) = 1 then incr nbrSticks
  done;
  if !nbrSticks <= 1 then true else false;;

(* loop : *)
let rec loop list =
  displayAll !list;
  let startRow = askRow !list in
  let endRow = askRow !list in
  if validLength startRow endRow Params.rmSticks !list
  then list := removeSticks startRow endRow 0 !list
  else loop list;;

(* main : *)
let rec main list =
  loop list;
  if not (victory !list) then main list;;

let list = ref (initSticks Params.nbrSticks);;
main list;;
displayAll !list;;
