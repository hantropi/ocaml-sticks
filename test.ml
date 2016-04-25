(*
  goTo : int*int*list(int) -> list(int)
             a, b, l1       -> supprime la liste jusqu'à la position a (position a exclus) 
 
 P.S : b est variable intermediare qui vaut 0 à l'initialisation. Si b = 0 alors les batonnets sont numérotés de 0 à n sinon si b = 1 alors les batonnets sont numerotés de 1 à n.
*)

(*
  validMove : int*int*int*int*list(int) -> boolean
                c, d, e, f, l2          -> verifie si le joueur peut retirer les batonnets entre c et d et retourne un boolean

  P.S : c = indice de depart où le joueur veut commencer à retirer les batonnets
        d = indice de fin où le joueur veut s'arreter de retirer les batonnets
        e = le nombre de batonnets que le joueur peut retirer à la fois
        f = valeur intermédiaire qui vaut 1 à l'initialisation
        l2 = liste qui a subit la methode goTo

   Si les batonnets sont numerotés de 0 à n alors if((d-c)+1 > e) then false (...)
 Sinon si les batonnets sont numerotés de 1 à n alors if(d-c > e) then false (...)
*)

let rec validMove(c, d, e, f, l2) =
  if((d-c)+1 > e) then false
  else
    if(f = e || l2 = []) then true
    else
      if(List.hd(l2) = 1 && f < e) then validMove(c, d, e, f, List.tl(l2))
      else false;;

let rec goTo(a, b, l1) =
  if(b >= a) then l1
  else
    goTo(a, b+1, List.tl(l1));;

let l3 = goTo(4, 0, [1;1;1;1;1;0;1]);;
let l4 = goTo(4, 0, [1;1;1;1;1;1;1]);;

validMove(4, 6, 3, 1, l3);;
validMove(4, 6, 3, 1, l4);; 
