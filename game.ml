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
  | [] -> print_string "\n"
  | hl::tl -> if (hl = 1) then print_string "| "
              else print_string "+ ";
              displaySticks tl;;

let list = initSticks Params.nbrSticks;;
displaySticks list;;
