(** Game **)

(* initSticks : Initialize the list of sticks
   int -> list(int)
*)
let rec initSticks number =
  if number <= 0 then []
  else 1::initSticks(number - 1);;

(* displaySticks : Display the sticks on the screen
   list(int) -> unit ()
*)
let rec displaySticks list =
  match list with
  | [] -> print_char '\n'
  | hl::tl -> if (hl = 1) then print_char '|'
    else print_char '+';
    print_char ' ';
    displaySticks tl;;

(* displayNumbers : Display the number below each stick
   int -> unit ()
*)
let displayNumbers number =
  for i = 0 to number do
    print_int i;
    print_char ' ';
  done;
  print_char '\n';;

(* displayAll : Display the sticks and the numbers
   list(int) -> unit ()
*)
let rec displayAll list =
  displaySticks list;
  displayNumbers (List.length(list));;

let askRow () =
  let row = read_int() in row;;

(* Main *)
let list = initSticks Params.nbrSticks;;
displayAll list;;
