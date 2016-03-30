(** Game **)

(* initSticks : Initialize the list of sticks
 *)
let rec initSticks number = true;;

(* displaySticks : Display the sticks on the screen
list(int) -> unit () *)
let rec displaySticks list =
  match list with
  | [] -> print_string "\n"
  | hl::tl -> if (hl = 1) then print_string "| "
              else print_string "+ ";
              displaySticks tl;;

(*let list = initSticks params.nbrSticks;;*)
let list = [1; 1; 0; 1];;
displaySticks list;;
