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
  | hl::tl -> if (hl = 1) then print_char '|'
    else print_char '+';
    print_char ' ';
    displaySticks tl;;

(* displayNumbers : Display the number below each stick
   int -> unit () *)
let displayNumbers number =
  for i = 0 to number do
    print_int i;
    print_char ' ';
  done;
  print_char '\n';;

(* displayAll : Display the sticks and the numbers
   list(int) -> unit () *)
let rec displayAll list =
  displaySticks list; (* Voir si on garde l'affichage des numeros *)
  displayNumbers (List.length(list));;

let askRow () =
  let row = read_int() in row;;

let main list =
  let quit_loop = ref false in
  while not !quit_loop do
    displayAll list;
    print_string "> ";
    let row = read_int () in
    if row >= 0 then () (*print_int row; print_char '\n'*)
    else quit_loop := true
  done;;

(* Main *)
let list = initSticks Params.nbrSticks;;
main list;;
