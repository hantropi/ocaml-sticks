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
  | hl::tl -> if hl = 1 then print_string "\027[33m|"
    else print_string "\027[35m+";
    print_string "\027[39m ";
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
let displayAll list =
  displaySticks list; (* Voir si on garde l'affichage des numeros *)
  displayNumbers (List.length(list));;

let displayPlayer player =
  if (player mod 2) == 0 then print_string "\027[32mPLAYER 2"
  else print_string "\027[34mPLAYER 1";
  print_string "> \027[39m";;

(* askRow : *)
let askRow list =
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
let rec askLoop list player =
  displayAll !list;
  displayPlayer !player;
  let startRow = askRow !list in
  displayPlayer !player;
  let endRow = askRow !list in
  if validLength startRow endRow Params.rmSticks !list
  then (list := removeSticks startRow endRow 0 !list; incr player)
  else (print_string "\027[31mINPUT ERROR !\027[39m\n";
        askLoop list player);;

(* main : *)
let rec main list player =
  askLoop list player;
  if not (victory !list) then main list player;;

let list = ref (initSticks Params.nbrSticks);;
let player = ref 1;;
main list player;;
displayAll !list;;
print_string "\027[36mPLAYER ";;
if (!player mod 2) == 0 then print_int 1
else print_int 2;; (* On inverse car on incremente a la fin de la partie *)
print_string " WIN !\n";;
