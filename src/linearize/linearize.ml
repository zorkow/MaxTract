open Synt;;

let bbstring   = ref ""
let verbose = ref false



(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   symbol
    @effects: Updates BBstring
    @output:  
 *)
let updateBB symbol = 
  bbstring := !bbstring^("<"^symbol.name^" , "
			 ^symbol.font^" , "
			 ^(string_of_float  symbol.size)^ "> "
			 ^(string_of_int symbol.x1)^" "
			 ^(string_of_int symbol.x3)^" "
			 ^(string_of_int symbol.y1)^" "
			 ^(string_of_int symbol.y3)^"\n");
;;

(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   tree, List of synts
    @effects: 
    @output:  all synts above t, bounded by x
 *)
let rec getBoundedAbove t synts above =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when ((hd.y3 <= iTree.ttop) 
			       && (hd.x1 >= iTree.tleft) 
			       && (hd.x3 <= iTree.tright)) -> getBoundedAbove t tl (hd::above)
    | _,hd::tl -> getBoundedAbove t tl above    
    | _,[]   -> above

(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   tree, List of synts
    @effects: 
    @output:  all synts below t, bounded by x
 *)
let rec getBoundedBelow t synts below =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when ((hd.y1 >= iTree.tbot) 
			       && (hd.x1 >= iTree.tleft) 
			       && (hd.x3 <= iTree.tright)) -> getBoundedBelow t tl (hd::below)
    | _,hd::tl -> getBoundedBelow t tl below    
    | _,[]   -> below

(** 
    @edited:  22-JUL-2012
    @author:  Josef Baker
    @input:   tree, List of synts
    @effects: 
    @output:  all synts bounded by t
*)
let rec getRootBody t synts body =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when ((hd.y1 >= iTree.ttop)
			       && (hd.y3 <= iTree.tbot)
			       && (hd.x3 <= iTree.tright)) -> getRootBody t tl (hd::body)
    | _,hd::tl -> getRootBody t tl body    
    | _,[]   -> body


(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   tree, List of synts
    @effects: 
    @output:  all synts subing t,
 *)(*
let rec getSub t synts sub =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when ((hd.y1 > iTree.ttop) 
			       && (hd.y2 > iTree.tbase)
			       && (hd.y3 >= iTree.tbot)
			       && (hd.x1 < ((getRight t sub iTree.tright) + 20) ))
	-> getSub t tl (hd::sub)
    | _,hd::tl -> (*getSub t tl*) sub    
    | _,[]   -> sub
	*)
let rec getSub t synts sub =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when hd.y2 = iTree.tbase -> sub
   | ITree iTree,hd::tl when ((hd.y1 > iTree.ttop) 
			       && (((hd.y2 > iTree.tbase)&& (hd.y3 >= iTree.tbot))||(hd.y1 > ((iTree.ttop+iTree.tbot))/2))
			       && (hd.x1 < ((getRight t sub iTree.tright) + 20) ))
	-> getSub t tl (hd::sub)
    | _,hd::tl -> sub   
    | _,[]   -> sub

(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   tree, List of synts
    @effects: 
    @output:  all synts supering t,
 *)(*
let rec getSuper t synts super =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when ((hd.y3 < iTree.tbot) 
			       && ((hd.y2 < iTree.tbase)||(hd.y2 < ((iTree.ttop+iTree.tbot))/2))
			       && (hd.y1 <= iTree.ttop)
			       && (hd.x1 < ((getRight t super iTree.tright) + 20) ))
	-> getSuper t tl (hd::super)
    | _,hd::tl -> (*getSuper t tl*) super    
    | _,[]   -> super
	*)
let rec getSuper t synts super =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when hd.y2 = iTree.tbase -> super
    | ITree iTree,hd::tl when ((hd.y3 < iTree.tbot)
			       &&   (((hd.y2 < iTree.tbase)&& (hd.y1 <= iTree.ttop))||(hd.y2 < ((iTree.ttop+iTree.tbot))/2))
			       && (hd.x1 < ((getRight t super iTree.tright) + 20) ))
	-> getSuper t tl (hd::super)
    | _,hd::tl -> super   
    | _,[]   -> super


(** 
    @edited:  17-OCT-2012
    @author:  Josef Baker
    @input: tree, List of synts  
    @effects: 
    @output:  all synts supering and subbing t
 *)
let rec getSupSub t synts super sub =
  match t,synts with
    | Null,_ -> ([],[])
    | ITree iTree,hd::tl when (* ((hd.y3 < iTree.tbot) 
			       && ((hd.y2 < iTree.tbase)||(hd.y2 < ((iTree.ttop+iTree.tbot))/2))
			       && (hd.y1 <= iTree.ttop)
			       && (hd.x1 < ((getRight t super iTree.tright) + 20) )) *)
	((hd.y3 < iTree.tbot)
	 &&   (((hd.y2 < iTree.tbase)&& (hd.y1 <= iTree.ttop))||(hd.y2 < ((iTree.ttop+iTree.tbot))/2))
	 && (hd.x1 < ((getRight t super iTree.tright) + 20) ))
	-> getSupSub t tl (hd::super) sub
    | ITree iTree,hd::tl when (* ((hd.y1 > iTree.ttop) 
			       && (hd.y2 > iTree.tbase)
			       && (hd.y3 >= iTree.tbot)
			       && (hd.x1 < ((getRight t sub iTree.tright) + 20) )) *)
	((hd.y1 > iTree.ttop) 
	 && (((hd.y2 > iTree.tbase)&& (hd.y3 >= iTree.tbot))||(hd.y1 > ((iTree.ttop+iTree.tbot))/2))
	 && (hd.x1 < ((getRight t sub iTree.tright) + 20) ))
	-> getSupSub t tl super (hd::sub)
    | _,hd::tl -> (*getSuper t tl*) (super,sub)    
    | _,[]   -> (super,sub)
;;



(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   tree, List of synts
    @effects: 
    @output:  all synts lower limiting t,
 *)
let rec getBottomLimit t synts below =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when ((hd.y1 > iTree.tbot) 
			       && (hd.x1 < ((getRight t below iTree.tright) + 10) ))
	-> getBottomLimit t tl (hd::below)
    | _,hd::tl -> getBottomLimit t tl below    
    | _,[]   -> below
	
(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   tree, List of synts
    @effects: 
    @output:  all synts top limiting t,
 *)
let rec getTopLimit t synts top =
  match t,synts with
    | Null,_ -> []
    | ITree iTree,hd::tl when ((hd.y3 < iTree.ttop) 
			       && (hd.x1 < ((getRight t top iTree.tright) + 10) ))
	-> getTopLimit t tl (hd::top)
    | _,hd::tl -> getTopLimit t tl top    
    | _,[]   -> top


(** 
    @edited:  25-JUL-2012
    @author:  Josef Baker
    @input:   tree, List of synts
    @effects: 
    @output:  all synts overlapping tree horizontally
 *)
let rec getRecursiveXOverlap t synts overlap =
  match t,synts with
    | Null, _ -> []
    | _,[] -> overlap
    | ITree iTree, hd::tl when hd.x1 <= ((getRight t overlap iTree.tright)+10) -> getRecursiveXOverlap t tl (hd::overlap)
    | _,_ -> overlap


(** Doesn't work!!!!!!!!!!
    @edited:  25-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let getLimits t synts =
  let lines = getLines (getRecursiveXOverlap t synts []) in  

    match t,lines with
      | ITree iTree, ln1::ln2::[]  (*Tree top ln1 base ln2 bottom*)
	  when iTree.tbot < getTop Null ln1 (List.hd ln1).y1 -> (1,lines)
      | ITree iTree, ln1::ln2::[]  (*Tree bottom ln1 top ln2 bottom*)
	  when iTree.ttop > getBot Null ln2 (List.hd ln2).y3 -> (2,lines)
      | ITree iTree, ln1::ln2::[]  (*Tree base ln1 top ln2 bottom*)
	  when (iTree.ttop > getBot Null ln1 (List.hd ln1).y3) 
	    && (iTree.tbot < getTop Null ln2 (List.hd ln2).y1) -> (3,lines)
      | ITree iTree, ln1::ln2::ln3::[]  (*Tree ln1 top ln2 base ln3 bottom*)
	  when verticallyOverlaps t ln1 -> (4,lines)
      | ITree iTree, ln1::ln2::ln3::[]  (*ln1 top tree ln2 base ln3 bottom*)
	  when verticallyOverlaps t ln2 -> (5,lines)
      | ITree iTree, ln1::ln2::ln3::[]  (*ln1 top ln2 tree base ln3 bottom*)
	  when verticallyOverlaps t ln3 -> (6,lines)
      | _,_-> (0,[])

(** 
    @edited:  31-JUL-2012
    @author:  Josef Baker
    @input:   tree synts
    @effects: 
    @output:  returns matrix lines
 *)
let getMatrixLines fence synts = 
  let contained = getSyntsUpto fence synts [] in
  getMatLines (sortSynt contained)
;;

(** Aux method of getMatrixCols*)
let rec getColsAux synts right col cols =
  match synts with
      hd::tl when hd.x1<=(right+40) -> getColsAux tl (max right hd.x3) (hd::col) cols
    | hd::tl -> getColsAux tl hd.x3 [hd] ((List.rev col)::cols)
    | _ -> List.rev (col::cols)
;;

(** 
    @edited:  02-AUG-2012
    @author:  Josef Baker
    @input:   list of synts
    @effects: 
    @output:  list of columns
 *)
let getMatrixCols synts =
  let synts = sortSynt synts in 
    match synts with
	h::t -> getColsAux t h.x3 [h] []
      | _ -> []
;;

(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   list of synts
    @effects: 
    @output:  true if they form division
 *)
let isDivision synts =
  let hd = List.hd synts in
    if hd.name = "line" then(
      let t = makeLeaf hd  in
	match (getBoundedAbove t synts []),(getBoundedBelow t synts []) with 
	  |  [],_ | _,[] -> false
	  | _,_ -> true
    )
    else false    

(** 
    @edited:  20-JUL-2012
    @author:  Josef Baker
    @input:   list of synts
    @effects: 
    @output:  true if they form a root
 *)
let isRoot synts =
  match synts with
      hd::tl when ((List.length (getRootBody (makeLeaf hd) tl [])) >0) -> (
	try (Str.search_forward (Str.regexp "radical") hd.name 0;
	     true)
	with Not_found -> false)
    | _ ->false

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   tree, list of synts 
    @effects: 
    @output:  true if they form a subscript
 *)
let isSub t synts =  
  match t,synts with
    | ITree iTree, hd::tl when (List.length (getSub t synts [])>0)-> true
    | _,_ -> false

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   tree, list of synts 
    @effects: 
    @output:  true if they form a superscript
 *)
let isSuper t synts =  
  match t,synts with
    | ITree iTree, hd::tl when (List.length (getSuper t synts [])>0)-> true
    | _,_ -> false

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   tree, list of synts 
    @effects: 
    @output:  true if they form a super-sub-script
 *)
let isSupSub t synts =  
  match t,synts with
    | ITree iTree, hd::tl-> ( match getSupSub t synts [] [] with
				  [],_ -> false
				| _,[] -> false
				| _,_  -> true)
    | _,_ -> false

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   tree, list of synts 
    @effects: 
    @output:  true if hd is a bottom limit of t
 *)
let hasBottomLimit t synts =  
  match t,synts with
    | ITree iTree, hd::tl when (hd.y1 > iTree.tbot) &&
	(hd.y3-hd.y1) < (iTree.tbot-iTree.ttop) &&
	(hd.x1 < iTree.tright)-> true 
    | _,_ -> false

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   tree, list of synts 
    @effects: 
    @output:  true if t is a bottom limit of hd
 *)
let isBottomLimit t synts =  
(*  match t,synts with
    | ITree iTree, hd::tl when (hd.y3 < iTree.ttop) &&
	(hd.y3-hd.y1) > (iTree.tbot-iTree.ttop) &&
	(iTree.tleft < hd.x3)-> true 
    | _,_ -> false	
*)
let lines = getLines (getRecursiveXOverlap t synts []) in 
   match t,lines with
      | ITree iTree, ln1::[]  (*Tree top ln1 base*)
	  when iTree.ttop > getBot Null ln1 (List.hd ln1).y3 &&
	    (((List.hd ln1).y3-((List.hd ln1).y1)) > (iTree.tbot-iTree.ttop)) -> true
      | ITree iTree, ln1::ln2::[]  (*Tree ln1 top ln2 base*)
	  when verticallyOverlaps t ln2  &&
	    (((List.hd ln1).y3-((List.hd ln1).y1)) > (iTree.tbot-iTree.ttop))-> true 
      | _,_-> false


(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   tree, list of synts 
    @effects: 
    @output:  true if hd is a top limit of t
 *)
let hasTopLimit t synts =  
  match t,synts with
    | ITree iTree, hd::tl when (hd.y3 < iTree.ttop) &&
	(hd.y3-hd.y1) < (iTree.tbot-iTree.ttop) &&
	(hd.x1 < iTree.tright)-> true 
    | _,_ -> false

(** 
    @edited:  21-JUL-2012
    @author:  Josef Baker
    @input:   tree, list of synts 
    @effects: 
    @output:  true if t is a top limit of hd
 *)
let isTopLimit t synts =  
 (* match t,synts with
    | ITree iTree, hd::tl when (hd.y1 > iTree.tbot) &&
	(hd.y3-hd.y1) > (iTree.tbot-iTree.ttop) &&
	(iTree.tleft < hd.x3)-> true 
    | _,_ -> false	
 *)

let lines = getLines (getRecursiveXOverlap t synts []) in 
   match t,lines with
      | ITree iTree, ln1::[]  (*Tree top ln1 base*)
	  when (iTree.tbot < getTop Null ln1 (List.hd ln1).y1) &&
	    (((List.hd ln1).y3-((List.hd ln1).y1)) > (iTree.tbot-iTree.ttop)) -> true
      | ITree iTree, ln1::ln2::[]  (*Tree ln1 top ln2 base*)
	  when verticallyOverlaps t ln1 &&
	    (((List.hd ln2).y3-((List.hd ln2).y1)) > (iTree.tbot-iTree.ttop))-> true 
      | _,_-> false

(** 
    @edited:  25-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let hasLimits t synts tstring=

  let lines = getLines (getRecursiveXOverlap t synts []) in
    match t,lines with
      | ITree iTree, ln1::ln2::[]  (*Tree top ln1 base ln2 bottom*)
	  when iTree.tbot < getTop Null ln1 (List.hd ln1).y1 -> true
      | ITree iTree, ln1::ln2::[]  (*Tree bottom ln1 top ln2 bottom*)
	  when iTree.ttop > getBot Null ln2 (List.hd ln2).y3 -> true
      | ITree iTree, ln1::ln2::[]  (*Tree base ln1 top ln2 bottom*)
	  when (iTree.ttop > getBot Null ln1 (List.hd ln1).y3) 
	    && (iTree.tbot < getTop Null ln2 (List.hd ln2).y1) -> true
      | ITree iTree, ln1::ln2::ln3::[]  (*Tree ln1 top ln2 base ln3 bottom*)
	  when verticallyOverlaps t ln1 -> true 
      | ITree iTree, ln1::ln2::ln3::[]  (*ln1 top tree ln2 base ln3 bottom*)
	  when verticallyOverlaps t ln2 -> true
      | ITree iTree, ln1::ln2::ln3::[]  (*ln1 top ln2 tree base ln3 bottom*)
	  when verticallyOverlaps t ln3 -> true
      | _,_-> false

   
;;

(** 
    @edited:  31-JUL-2012
    @author:  Josef Baker
    @input:   tree synts
    @effects: 
    @output:  true if tree forms beginning of a matrix
 *)
let isMatrix t synts = 
  match t,synts with 
      Null,hd::tl -> (
	if (hasNextFence (makeLeaf hd) tl) then (
	  let fence = getNextFence (makeLeaf hd) tl in
	  let contained = getSyntsUpto fence tl [] in
	  let lines = getMatLines contained in
	    if (List.length lines) > 1 then true
	    else false)
	else false)
    | _,_->false
;;

(** 
    @edited:  31-JUL-2012
    @author:  Josef Baker
    @input:   tree synts
    @effects: 
    @output:  true if tree forms beginning of a case
 *)
let isCase t synts = 
  match t,synts with 
      Null,hd::tl -> ( 
	if (isRemainderVerticallyBound hd tl) then 
	 ( let lines = getMatLines tl in
	    if (List.length lines) > 1 then true
	    else false)
	else false)
    | _,_->false
;;

let printRule name synts tstring =
  if (false) then (
    print_newline ();
    print_endline ("*"^name^"*");
    print_endline tstring;
    Util.printSyntList (synts);
    print_string "**";
    print_newline (););
  ()
 ;;
 
let rec linearise t synts tstring linearised= 


  match t,synts with
      (*Nothing left to process*)
    | Null,[] -> linearised
	(*No more input symbols*)
    | ITree iTree,[] -> linearised^tstring
	(*Division*)
    | _,hd::tl when isDivision synts -> ( 
	printRule "div" synts tstring;

	updateBB hd;
	let numerator = sortSynt (getBoundedAbove (makeLeaf hd) tl []) in
	let denominator = sortSynt (getBoundedBelow (makeLeaf hd) tl []) in
	let fraction = (numerator@denominator@[hd]) in
	    linearise (ITree {tleft=(getLeft t fraction hd.x1); 
			      tright=(getRight t fraction hd.x3); 
			      ttop=(getTop t fraction hd.y1); 
			      tbot=(getBot t fraction hd.y3); 
			      tbase=hd.y2;}) 
	      (sortSynt (remove fraction synts))
	      (tstring^(getSpace t synts)^" frac("^(linearise Null numerator "" "")^")("^
		 (linearise Null denominator "" "")^")") linearised
	      
      )
	(*Root*) (*Still need to sort out index and itree*)
    | _,hd::tl when isRoot synts -> ( 
	printRule "root" synts tstring;
	updateBB hd;
	let body = sortSynt (getRootBody (makeLeaf hd) tl []) in
	let root = body@[hd] in
	  linearise (ITree {tleft=(getLeft t root hd.x1); 
			    tright=(getRight t root hd.x3); 
			    ttop=(getTop t root hd.y1); 
			    tbot=(getBot t root hd.y3); 
			    tbase=hd.y2;}) 
	    (sortSynt (remove root synts))
	    (tstring^(getSpace t synts)^" functor("^(lineariseSynt hd)^")( arg("^
	       (linearise Null body "" "")^"))") linearised
      )
	(*Matrix*)
    | _, hd::tl when isMatrix t synts -> (
	printRule "matrix" synts tstring;
	updateBB hd;
	let openFence = hd in
	let closeFence = getNextFence (makeLeaf hd) tl in
	let lines = getMatrixLines closeFence tl in
	let matrix = (List.concat lines)@[openFence]@[closeFence] in
	let matrixCol = ref "" in		     
	    
	let processCol col =
	  let col = ("col("^(linearise Null (sortSynt col) "" "")^")") in
	    matrixCol :=!matrixCol^col
	in
	  
	let matrixStr = ref ("matrix("^(lineariseSynt hd)^")(") in
	  
	let processMatrix row =
	  let cols = getMatrixCols (sortSynt row) in
	    List.iter processCol cols;
	    let row = ("row("^(!matrixCol)^")") in
	      matrixStr :=!matrixStr^row;
	      matrixCol := "";
	in
	  
	  List.iter processMatrix lines;
	    let matString =  !matrixStr^")("^(lineariseSynt closeFence)^") " in
	      updateBB closeFence;
	      
	      linearise (ITree {tleft=(getLeft t matrix hd.x1); 
				tright=(getRight t matrix hd.x3); 
				ttop=(getTop t matrix hd.y1); 
				tbot=(getBot t matrix hd.y3); 
				tbase=hd.y2;})
		(sortSynt (remove matrix synts))
	    (tstring^(getSpace t synts)^matString) linearised
		
		
		
      )
	(*Case*)
    | _, hd::tl when isCase t synts -> (
	printRule "case" synts tstring;

	updateBB hd;
	let openFence = hd in
	let lines = getMatLines tl in
	let case = (List.concat lines)@[openFence] in
	  
	let caseRef = ref ("cases("^(lineariseSynt hd)^")( ") in	 
	  
	let processRow row = 
	  let row = ("line ( alignat "^(linearise  Null (sortSynt row) "" "")^")") in
	    caseRef :=!caseRef^row
	in
	  
	  List.iter processRow (lines);
	    
	  caseRef :=!caseRef^")" ;

	  linearise (ITree {tleft=(getLeft t case hd.x1); 
			    tright=(getRight t case hd.x3); 
			    ttop=(getTop t case hd.y1); 
			    tbot=(getBot t case hd.y3); 
			    tbase=hd.y2;})
	    (sortSynt (remove case synts))
	    (tstring^(getSpace t synts)^(!caseRef)) linearised
      )


	(*Limits*)
    | ITree iTree,hd::tl when hasLimits t synts tstring ->(

	printRule "limits" synts tstring;

	let limits =  getLimits t synts in
	  match limits with
 	    | (1, base::bot::[]) -> ((*Tree top ln1 base ln2 bottom*)
		
		linearise (ITree {tleft=(getLeft t (base@bot) iTree.tleft); 
				  tright=(getRight t (base@bot) iTree.tright); 
				  ttop=(getTop t (base@bot) iTree.ttop); 
				  tbot=(getBot t (base@bot) iTree.tbot); 
				  tbase=iTree.tbase;}) 
		  (sortSynt (remove (base@bot) synts))
		  (" lim("^(linearise Null (sortSynt base) "" "")^")("^(tstring)^")("^(linearise Null (sortSynt bot) "" "")^")")
		  linearised
	      )
	    | (2, top::base::[]) -> ((*Tree bottom ln1 top ln2 bottom*)
		
		linearise (ITree {tleft=(getLeft t (base@top) iTree.tleft); 
				  tright=(getRight t (base@top) iTree.tright); 
				  ttop=(getTop t (base@top) iTree.ttop); 
				  tbot=(getBot t (base@top) iTree.tbot); 
				  tbase=iTree.tbase;}) 
		  (sortSynt (remove (base@top) synts))
		  (" lim("^(linearise Null (sortSynt base) "" "")^")("^(linearise Null (sortSynt top) "" "")^")("^(tstring)^")")
		  linearised	
	      )
	    | (3, top::bot::[]) -> ((*Tree base ln1 top ln2 bottom*)

		linearise (ITree {tleft=(getLeft t (bot@top) iTree.tleft); 
				  tright=(getRight t (bot@top) iTree.tright); 
				  ttop=(getTop t (bot@top) iTree.ttop); 
				  tbot=(getBot t (bot@top) iTree.tbot); 
				  tbase=iTree.tbase;}) 
		  (sortSynt (remove (bot@top) synts))
		  (" lim("^(tstring)^")("^(linearise Null (sortSynt top) "" "")^")("^(linearise Null (sortSynt bot) "" "")^")")
		  linearised
	      )
	    | (4, top::base::bot::[]) -> ((*Tree ln1 top ln2 base ln3 bottom*)
	
		linearise (ITree {tleft=(getLeft t (bot@top@base) iTree.tleft); 
				  tright=(getRight t (bot@top@base) iTree.tright); 
				  ttop=(getTop t (bot@top@base) iTree.ttop); 
				  tbot=(getBot t (bot@top@base) iTree.tbot); 
				  tbase=iTree.tbase;}) 
		  (sortSynt (remove (bot@top@base) synts))
		  (" lim("^(linearise Null (sortSynt base) "" "")^")("^
		     (linearise t (sortSynt top) tstring "")^")("^
		     (linearise Null (sortSynt bot) "" "")^")")
		  linearised	
	      )
	    | (5, top::base::bot::[]) -> ((*ln1 top tree ln2 base ln3 bottom*)
	
		linearise (ITree {tleft=(getLeft t (bot@top@base) iTree.tleft); 
				  tright=(getRight t (bot@top@base) iTree.tright); 
				  ttop=(getTop t (bot@top@base) iTree.ttop); 
				  tbot=(getBot t (bot@top@base) iTree.tbot); 
				  tbase=iTree.tbase;}) 
		  (sortSynt (remove (bot@top@base) synts))
		  (" lim("^(linearise t (sortSynt base) tstring "")^")("^
		     (linearise Null (sortSynt top) "" "")^")("^
		     (linearise Null (sortSynt bot) "" "")^")")
		  linearised	
	      )
	    | (6, top::base::bot::[]) -> ((*ln1 top ln2 tree base ln3 bottom*)
		
		linearise (ITree {tleft=(getLeft t (bot@top@base) iTree.tleft); 
				  tright=(getRight t (bot@top@base) iTree.tright); 
				  ttop=(getTop t (bot@top@base) iTree.ttop); 
				  tbot=(getBot t (bot@top@base) iTree.tbot); 
				  tbase=iTree.tbase;}) 
		  (sortSynt (remove (bot@top@base) synts))
		  (" lim("^(linearise Null (sortSynt base) "" "")^")("^
		     (linearise Null (sortSynt top) "" "")^")("^
		     (linearise t (sortSynt bot) tstring "")^")")
		  linearised
	      )
      )
   

    (*Bottom limit- when tree is base*)
    | ITree iTree,hd::tl when hasBottomLimit t synts ->(
	printRule "under" synts tstring;
	let under = sortSynt (getBottomLimit t synts []) in
	let under = fixUnderbar under in
	  linearise (ITree {tleft=(getLeft t under hd.x1); 
			    tright=(getRight t under hd.x3); 
			    ttop=(getTop t under hd.y1); 
			    tbot=(getBot t under hd.y3); 
			    tbase=iTree.tbase;}) 
	    (sortSynt (remove under synts))
	    (" under("^tstring^")("^(linearise Null under "" "")^")")
	    linearised
      )
	(*Bottom limit- when tree is limit*)
    | ITree iTree,hd::tl when isBottomLimit t synts ->(
	printRule "under-tree" synts tstring;
	let under = sortSynt (getBottomLimit (makeLeaf hd) tl []) in
	let tstring = fixUnderbarString tstring in
	  linearise (ITree {tleft=(getLeft t under hd.x1); 
			    tright=(getRight t under hd.x3); 
			    ttop=(getTop t under hd.y1); 
			    tbot=(getBot t under hd.y3); 
			    tbase=iTree.tbase;}) 
	    (sortSynt (remove (hd::under) synts))
	    (" under("^(linearise Null [hd] "" "")^")("^(linearise t under tstring "")^")")
	    linearised
      )
		(*Top limit- when tree is base*)
    | ITree iTree,hd::tl when hasTopLimit t synts ->(
	printRule "over" synts tstring;
	let limit = sortSynt (getTopLimit t synts []) in
	let limit = fixOverbar limit in
	  linearise (ITree {tleft=(getLeft t limit hd.x1); 
			    tright=(getRight t limit hd.x3); 
			    ttop=(getTop t limit hd.y1); 
			    tbot=(getBot t limit hd.y3); 
			    tbase=iTree.tbase;}) 
	    (sortSynt (remove limit synts))
	    (" over("^tstring^")("^(linearise Null limit "" "")^")")
	    linearised
      )
	(*Top limit- when tree is limit*)
    | ITree iTree,hd::tl when isTopLimit t synts ->(
	printRule "over-tree" synts tstring;
	let limit = sortSynt (getTopLimit (makeLeaf hd) tl []) in
	let tstring = fixOverbarString tstring in
	  linearise (ITree {tleft=(getLeft t limit hd.x1); 
			    tright=(getRight t limit hd.x3); 
			    ttop=(getTop t limit hd.y1); 
			    tbot=(getBot t limit hd.y3); 
			    tbase=iTree.tbase;}) 
	    (sortSynt (remove (hd::limit) synts))
	    (" over("^(linearise Null [hd] "" "")^")("^(linearise t limit tstring "")^")")
	    linearised
      )
	(*Super-sub-script*)
    | ITree iTree,hd::tl when isSupSub t synts ->(
	printRule "supsub" synts tstring;
	match getSupSub t synts [] [] with
	    sup,sub -> (
	let superscript = sortSynt sup in
	let subscript = sortSynt sub in
	let supsub = superscript@subscript in
	  linearise (ITree {tleft=(getLeft t supsub iTree.tleft); 
			    tright=(getRight t supsub iTree.tright); 
			    ttop=(getTop t supsub iTree.ttop); 
			    tbot=(getBot t supsub iTree.tbot); 
			    tbase=iTree.tbase;}) 
	    (sortSynt (remove supsub synts))
	    (" supsub("^tstring^")("^(linearise Null superscript "" "")^")("^(linearise Null subscript "" "")^")")
	    linearised
	    )
      )
	(*Super-script*)
    | ITree iTree,hd::tl when isSuper t synts ->(
	printRule "sup" synts tstring;
	let superscript = sortSynt (getSuper t synts []) in
	  linearise (ITree {tleft=(getLeft t superscript iTree.tleft); 
			      tright=(getRight t superscript iTree.tright); 
			      ttop=(getTop t superscript iTree.ttop); 
			      tbot=(getBot t superscript iTree.tbot); 
			      tbase=iTree.tbase;}) 
	    (sortSynt (remove superscript synts))
	    (" sup("^tstring^")("^(linearise Null superscript "" "")^")") linearised
      )
	(*Sub-script*)
    | ITree iTree,hd::tl when isSub t synts ->(
	printRule "sub" synts tstring;
	let subscript = sortSynt (getSub t synts []) in
	  linearise (ITree {tleft=(getLeft t subscript iTree.tleft); 
			      tright=(getRight t subscript iTree.tright); 
			      ttop=(getTop t subscript iTree.ttop); 
			      tbot=(getBot t subscript iTree.tbot); 
			      tbase=iTree.tbase;}) 
	    (sortSynt (remove subscript synts))
	    (" sub("^tstring^")("^(linearise Null subscript "" "")^")") linearised
      )
	  (*A leaf node*)
    | Null,hd::tl -> (
	printRule "leaf" synts tstring;
	  updateBB hd;
	  linearise (makeLeaf hd) tl (lineariseSynt hd)  linearised
	)
	(*Linearize*)
    | ITree iTree,_ -> 
	printRule "lin" synts tstring;
	(linearise Null synts ""	(linearised^tstring^(getSpace t synts))
	)
;;

let lineariseLine synts bbChan=
 (* Util.printSyntList (sortSynt synts);*)
  match synts with
      h::t -> (
	let x = string_of_int (getLeft Null synts h.x1) in
	let y = string_of_int (getTop Null synts h.y1) in
	let w = string_of_int ((getRight Null synts h.x3) -  (getLeft Null synts h.x1)) in
	let h = string_of_int ((getBot Null synts h.y3) -(getTop Null synts h.y1)) in
	  
	  bbstring := "";
	  let line =  linearise Null (sortSynt synts) "" (x^" "^y^" "^w^" "^h^" ") in
	    output_string bbChan (!bbstring);
	    line)
    |  _ ->  ""

;;
(*linearize (ITree {tleft=1; tright=1; ttop=1; tbot=1; tbase=1;
tstring="Q"})*)
