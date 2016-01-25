
(***********************************************************************)
(*                                                                     *)
(* Holds synt&treeypes for use in linearize along with utility methods *)
(*                                                                     *)
(***********************************************************************)




(*Coordinate system, origin: top left. x1 ... x3 left to right. y1 ... y3 top to bottom*)
type synt = {name: string; font: string; size: float; x1: int; x2: int; x3: int; y1: int; y2: int; y3: int};;


(*A           tree, represented as a bounding box and linearised string, or an empty tree*)
type iTree = {tleft:int; tright:int; ttop:int; tbot:int; tbase:int;} 
type tree = Null | ITree of iTree



(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   list of synts right bound
    @effects: 
    @output:  largest right bound in list
 *)
let rec getRight t synts right=
  match t,synts with
    | Null,[] -> right
    | ITree iTree,[] -> max right iTree.tright
    | _,hd::tl when hd.x3>right -> getRight t tl hd.x3
    | _,hd::tl -> getRight t tl right

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   list of synts, left bound
    @effects: 
    @output:  smallest left bound in list
 *)
let rec getLeft t synts left=
  match t,synts with
    | Null,[] -> left
    | ITree iTree,[] -> min left iTree.tleft
    | _,hd::tl when hd.x1<left -> getLeft t tl hd.x1
    | _,hd::tl -> getLeft t tl left

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   list of synts bottom bound
    @effects: 
    @output:  largest bottom bound in list
 *)
let rec getBot t synts bot=
  match t,synts with
    | Null,[] -> bot
    | ITree iTree,[] -> max bot iTree.tbot
    | _,hd::tl when hd.y3>bot -> getBot t tl hd.y3
    | _,hd::tl -> getBot t tl bot

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   list of synts, top bound
    @effects: 
    @output:  smallest top bound in list
 *)
let rec getTop t synts top=
  match t,synts with
    | Null,[] -> top
    | ITree iTree,[] -> min top iTree.ttop
    | _,hd::tl when hd.y1<top -> getTop t tl hd.y1
    | _,hd::tl -> getTop t tl top

(** 
    @edited:  22-JUL-2012
    @author:  Josef Baker
    @input:   symbol
    @effects: 
    @output:  linearised form
 *)
let lineariseSynt symbol =
  match symbol.name with
      "line" when symbol.x3 - symbol.x1 > symbol.y3-symbol.y1 -> (" <hline , "^symbol.font^" , "^(string_of_float symbol.size)^"> ")
    | "line" when symbol.y3 - symbol.y1 > symbol.x3-symbol.x1 -> (" <vline , "^symbol.font^" , "^(string_of_float symbol.size)^"> ")
  | _->  (" <"^symbol.name^" , "^symbol.font^" , "^(string_of_float symbol.size)^"> ")

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   symbol
    @effects: 
    @output:  tree (leaf) containing symbol
 *)
let makeLeaf symbol =
   (ITree {tleft =  symbol.x1; 
     tright=  symbol.x3;
     ttop  =  symbol.y1;
     tbot  =  symbol.y3;
     tbase =  symbol.y2;})

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   tree, synts
    @effects: 
    @output:  space between tree and synts
 *)
let getSpace t synts =
  match t,synts with
    | _,[]   -> " "
    | Null,_ -> " "
    | ITree iTree,hd::tl when hd.x1-iTree.tright<20-> " w0 "
    | ITree iTree,hd::tl when hd.x1-iTree.tright<30-> " w1 "
    | ITree iTree,hd::tl when hd.x1-iTree.tright<40-> " w2 "
    | ITree iTree,hd::tl when hd.x1-iTree.tright<50-> " w3 "
    | _,_  -> " w4 "

(** 
    @edited:  14-FEB-2010
    @author:  Josef Baker
    @input:   List, pos
    @effects: 
    @output:  Deletes item in pos from list
 *)
let rec delete inList outList item =
  if item == 0 then (List.append  (List.rev outList) (List.tl inList))
  else delete (List.tl inList) ((List.hd inList)::outList) (item-1)
;;

let delete list item =
delete list [] item
;;

let rec exists element list index =
  match list with 
      head::tail ->( if element = head then index
                     else exists element tail (index+1))
    | [] -> -1
;;

let exists element list =
  exists element list 0
;;

let rec remove subList list =
  match subList with 
      head::tail -> (
        let index = exists head list in
        remove tail (delete  list index)
      )
    | [] -> list
;;
let syntLess s1 s2 =
  if s1.x1 < s2.x1 then true
  else false
;;

(*Sorts based upon left hand side*)
let rec sortSynt = function
  | [] -> []
  | pivot :: rest ->
    let is_less x = syntLess x pivot in
    let left, right = List.partition is_less rest in
      sortSynt left @ [pivot] @ sortSynt right
;;

let syntLessV s1 s2 =
  if s1.y1 < s2.y1 then true
  else false
;;

(*Sorts based upon top side*)
let rec sortSyntV = function
  | [] -> []
  | pivot :: rest ->
    let is_less x = syntLessV x pivot in
    let left, right = List.partition is_less rest in
      sortSyntV left @ [pivot] @ sortSyntV right
;;

let rec checkMatLinesAux line =
  match line with
      hd::tl when hd.name = "line"-> checkMatLinesAux tl
    | hd::tl when hd.name = "minus"-> checkMatLinesAux tl
    | hd::tl -> true
    |_ ->false
;;

let rec checkMatLines lines =
  match lines with
      hd::tl when checkMatLinesAux hd -> checkMatLines tl
    | hd::tl -> false
    | [] -> true
;;

let rec getMatLinesAux synts bottom line lines =
  match synts with
      hd::tl when hd.y1>(bottom+10) -> getMatLinesAux tl hd.y3 [hd] ((sortSynt line)::lines)
    | hd::tl -> getMatLinesAux tl (max bottom hd.y3) (hd::line) lines
    | [] -> (List.rev ((sortSynt line)::lines))

;;

let getMatLines synts =
  let synts =  (sortSyntV synts) in 
    match synts with
	h::t ->( let matLines = getMatLinesAux t h.y3 [h] [] in
		  if checkMatLines matLines then matLines
		  else [])
      | _ -> []
;;

let rec getLinesAux synts bottom line lines =
  match synts with
      hd::tl when hd.y1>bottom -> getLinesAux tl hd.y3 [hd] ((sortSynt line)::lines)
    | hd::tl -> getLinesAux tl (max bottom hd.y3) (hd::line) lines
    | _ -> (List.rev ((sortSynt line)::lines))
;;

let getLines synts =
  let synts = sortSyntV synts in 
    match synts with
	h::t -> getLinesAux t h.y3 [h] []
      | _ -> []
;;

let rec verticallyOverlaps t synts =
  match t,synts with
    | Null,_ -> false
    | _,[] -> false
    | ITree iTree, hd::tl when 
	((hd.y1 >= iTree.ttop) && (hd.y1 <= iTree.tbot)) ||
	  ((hd.y3 >= iTree.ttop) && (hd.y3 <= iTree.tbot)) ||
	  ((hd.y1 <= iTree.ttop) && (hd.y3 >= iTree.tbot))-> true
    | ITree iTree, hd::tl -> verticallyOverlaps t tl

let rec hasNextFence t synts = 
  match t, synts with
    | ITree iTree, hd::tl when 
	(iTree.ttop = hd.y1) && (iTree.tbot = hd.y3)(* &&
	  ((hd.x3-hd.x1)=(iTree.tright-iTree.tleft))*)-> true
    | _,hd::tl -> hasNextFence t tl
    | _,_ -> false
;;

let rec getNextFence t synts = 
  match t, synts with
    | ITree iTree, hd::tl when 
	(iTree.ttop = hd.y1)&&(iTree.tbot = hd.y3)(*&&
	 ((hd.x3-hd.x1)=(iTree.tright-iTree.tleft))*)-> hd
    | _,hd::tl -> getNextFence t tl
;;

let rec getSyntsUpto bound synts upto =
  match synts with
      hd::tl when ((hd.x3 < bound.x1) &&
		     (hd.y1>=bound.y1)&&
		     (hd.y3<=bound.y3))-> getSyntsUpto bound tl (hd::upto)
    | hd::tl when ((hd.x3 < bound.x1) && (	
		     (hd.y1<bound.y1)||
		       (hd.y3>bound.y3)))-> []
    | hd::tl ->  getSyntsUpto bound tl upto
    | _ -> upto
;;

let rec isRemainderVerticallyBound bound synts =
  match synts with
      hd::tl when (hd.y1>=bound.y1)&&
	(hd.y3<=bound.y3)-> isRemainderVerticallyBound bound tl
    | hd::tl ->  false
    | _ -> true
;;



(** 
    @edited:  06-AUG-2012
    @author:  Josef Baker
    @input:   synt list
    @effects: 
    @output:  changes name to overbar if synt list is a single line element
 *)
let fixOverbar synts =
  match synts with
      h::[] when h.name ="hline" -> [{name="overbar"; font=h.font; size=h.size; x1=h.x1; x2=h.x2; x3=h.x3; y1=h.y1; y2=h.y2; y3=h.y3}]
    | _ -> synts

(** 
    @edited:  06-AUG-2012
    @author:  Josef Baker
    @input:   synt list
    @effects: 
    @output:  changes name to underbar if synt list is a single line element
 *)
let fixUnderbar synts =
  match synts with
      h::[] when h.name ="line" ->[{name="underbar"; font=h.font; size=h.size; x1=h.x1; x2=h.x2; x3=h.x3; y1=h.y1; y2=h.y2; y3=h.y3}]
    | _ -> synts


(** 
    @edited:  06-AUG-2012
    @author:  Josef Baker
    @input:   string
    @effects: 
    @output:  changes line to underbar if string ends in a line
 *)
let fixUnderbarString t =
  let tregexp = Str.regexp_string "<hline , none , 0.>" in
    try (Str.search_backward tregexp t ((String.length t)-1);
	 if Str.match_end () >  ((String.length t)-10) 
	 then Str.replace_matched  "<underbar , none , 0.>" t
	 else t)
    with
	Not_found  -> t

(** 
    @edited:  06-AUG-2012
    @author:  Josef Baker
    @input:   string
    @effects: 
    @output:  changes line to overbar if string ends in a line
 *)
let fixOverbarString t =
  let tregexp = Str.regexp_string "<hline , none , 0.>" in
    try (Str.search_backward tregexp t ((String.length t)-1);
	 if Str.match_end () >  ((String.length t)-10) 
	 then Str.replace_matched  "<overbar , none , 0.>" t
	 else t)
	 
    with
	Not_found  -> t



(** 
    @edited:  13-AUG-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let syntsVOverlap s1 s2 =
((s1.y1 < s2.y1) && (s1.y3 > s2.y1)) ||
((s1.y3 > s2.y3) && (s1.y1 < s2.y3)) ||
((s1.y1 > s2.y1) && (s1.y3 <s2.y3))

