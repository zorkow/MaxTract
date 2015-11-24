open MatcherUtility;;
open Jsonfio.JsonfIO;;
open Contentparser;;

type symb = {
  glList: bBox list;
  elList: elem list;
}

let test = ref false
let glyphLst = ref []
let elemLst = ref []

let btLn = ref (Ln {stx=0.; sty=99999.; enx=0.; eny=99999.; lnw=0.})
let rtCh = ref (Chr {chname=""; chfont=""; chsize=0.; chx=99999.; chy=0.; chw=0.})




(** 
    @edited:  08-JUN-2010
    @author:  Josef Baker
    @input:   Char and list of symbols
    @effects: 
    @output:  True if char is within symbol list
 *)
let rec checkChar char symbols =
  match symbols with 
      h::t when equalChar char h -> true
    | h::t -> checkChar char t
    | [] -> false
;;


(** 
    @edited:  07-NOV-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  list of chars not in the symbol list
 *)
let rec checkChars inChars outChars symbols =
  match inChars with 
      h::t when checkChar h symbols -> checkChars t outChars symbols
    | h::t -> checkChars t (h::outChars) symbols 
    | [] -> outChars
;;


(** 
    @edited:  08-JUN-2010
    @author:  Josef Baker
    @input:   Char list, symbol list, empty list
    @effects: 
    @output:  Char list without any of the chars that are also present in the symbol list
 *)
let rec removeDupChars chars symbols symbolChars =
  match symbols with
      h::t -> removeDupChars chars t (List.append h.elList symbolChars)
    | [] ->  checkChars chars [] symbolChars
;;


(** 
    @edited:  08-JUN-2010
    @author:  Josef Baker
    @input:   Glyph and list of symbols
    @effects: 
    @output:  True if glyph is within symbol list
 *)
let rec checkGlyph glyph symbols =
  match symbols with 
      h::t -> ( if (equalGlyph glyph h)
		then true
		else checkGlyph glyph t
	      )
    | [] -> false
;;


let rec checkGlyphs inGlyphs outGlyphs symbols =
  match inGlyphs with 
      h::t -> ( if checkGlyph h symbols then checkGlyphs t outGlyphs symbols
		else checkGlyphs t (h::outGlyphs) symbols )
    | [] -> outGlyphs
;;


(** 
    @edited:  08-JUN-2010
    @author:  Josef Baker
    @input:   Glyph list, symbol list, empty list
    @effects: 
    @output:  Glyph list without any of the glyphs that are also present in the symbol list
 *)
let rec removeDupGlyphs glyphs symbols symbolGlyphs =
  match symbols with
      h::t -> removeDupGlyphs glyphs t (List.append h.glList symbolGlyphs)
    | [] ->  checkGlyphs glyphs [] symbolGlyphs
;;





let rec convElems elems out =
  match elems with
      h::t ->(match h with 
		  Chr chr -> convElems t (PDFChar {Jsonfio.JsonfIO.c=chr.chname;

						   bx=(int_of_float chr.chx);
						   by=(int_of_float chr.chy);
						   font=chr.chfont;
						   scale=chr.chsize;}::out)
		| Ln ln -> convElems t (Line {sx=(int_of_float ln.stx);
					      sy=(int_of_float ln.sty);
					      lw=(int_of_float ln.lnw);
					      ex=(int_of_float ln.enx);
					      ey=(int_of_float ln.eny);}::out))
    |_ ->out
       
;;

let rec getBBox xm ym wm hm glyphs =
  match glyphs with
      hd::tl -> ( getBBox (min xm hd.x) 
		    (min ym hd.y) 
		    ((max (xm + wm) (hd.x +hd.w)) - (min xm hd.x) )
		    ((max (ym + hm) (hd.y +hd.h)) - (min ym hd.y) )
		    tl)
    | _ ->{x=xm;y=ym;w=wm;h=hm}
;;

let rec convert symbols out =
  match symbols with
      h::t -> convert t
	({bbox=(getBBox (List.hd h.glList).x 
		  (List.hd h.glList).y
		  (List.hd h.glList).w
		  (List.hd h.glList).h h.glList);glyphs=h.glList;elements=(convElems h.elList [])}::out)
    | _ -> out
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Two symbols
    @effects: 
    @output:  True if symbol 1's y coord is less than 2's
 *)
let symbLessY s1 s2=
  let el1 = s1.elList in
  let el2 = s2.elList in
    charLessY (List.hd el1) (List.hd el2)
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Symbol list
    @effects: 
    @output:  Symbols sorted by y coords
 *)
let rec sortSymbolY = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = symbLessY x pivot in
      let left, right = List.partition is_less rest in
      sortSymbolY left @ [pivot] @ sortSymbolY right
;;



(** 
    @edited:  11-MAY-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  list of elements overlapping glyph
 *)
let rec getGlyphElemOverlap glyph elems overlap hGScale vScale hEScale=
  match elems with 
      h::t when glyphElemOverlap glyph h hGScale vScale hEScale -> 
	getGlyphElemOverlap glyph t (h::overlap) hGScale vScale hEScale
    | h::t -> getGlyphElemOverlap glyph t overlap hGScale vScale hEScale
    | _ -> overlap
;;


(*
let rec multiMatchAux glyphs elems symbs =
 match glyphs with 
      h::t ->(
	let overlap = getOverlap h elems [] 1. 1. 0.8 in
		 if (List.length overlap) > 1 then 
		   multiMatchAux t elems ({glList=([h]); elList = overlap}::symbs)
		 else
		   multiMatchAux t elems symbs)
    | _ -> symbs

let rec multiMatch symbols= 

  let symbols1 = (multiMatchAux (!glyphLst)  (!elemLst) [])@symbols in
  
    elemLst :=  removeDupChars !elemLst symbols1 [];
    glyphLst := removeDupGlyphs !glyphLst symbols1 []; 	
    
    if (List.length symbols1) = (List.length symbols) then symbols1
    else multiMatch symbols1 
;;




let rec upperMatch chars symbols =
  match chars with
      h::t -> (let upper = findUpper h (!glyphLst) [] in
		 if (List.length upper) = 1 then  (
		   let newSymbol = {elList=[h];glList=upper} in
		   glyphLst := removeDupGlyphs (!glyphLst) [newSymbol] [];
		     upperMatch  t (newSymbol::symbols))
		 else upperMatch  t symbols
	      )
	
    | _ -> (getAbove symbols [])
;;

let basicMatch symbols =  
  
  let symbols = (singleMatch  [] 1. 1. 1.)@symbols in
    
  let symbols = (singleMatch  [] 1. 1. 0.4)@symbols in
    
  let symbols = (singleMatch  [] 0.9 1. 0.4)@symbols in
    
  let symbols = (singleMatch   [] 1. 1.1 1.)@symbols in
    
  let symbols = (singleMatch   [] 1. 1.1 0.4)@symbols in
    
  let symbols = sortSymbolY symbols in  
 
  let symbols = getAbove symbols  [] in
    
    symbols
;;

*)

let hasAbove ch =
  (ch ="j") || (ch ="i") || (ch="equal") || (ch="colon") || (ch="greaterequal")
  || (ch="lessequal")|| (ch="semicolon")
;;

let addAbove symbol glyphs =
  let above = findAbove (List.hd symbol.elList) glyphs [] in
    {elList = symbol.elList; glList=(symbol.glList@above)}
;;

let rec getAbove inSymbols outSymbols =
  match inSymbols with
      h::t -> ( let c = (List.hd h.elList) in
		  match c with 
		      Chr ch ->(
			if (hasAbove ch.chname) then (
			  let newSymbol = addAbove h !glyphLst in
			    glyphLst := removeDupGlyphs !glyphLst [newSymbol] [];
			  getAbove t (newSymbol::outSymbols)
		      )
			else getAbove t (h::outSymbols))
		    | _ -> getAbove t (h::outSymbols))
	
    | [] -> outSymbols
;;

(** glyph below elem
    @edited:  18-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec lowerMatch elems symbols =
  match elems with
      h::t -> (let lower = findLower h (!glyphLst) [] in
		 if (List.length lower) = 1 then  (
		   let newSymbol = {elList=[h];glList=lower} in
		     glyphLst := removeDupGlyphs (!glyphLst) [newSymbol] [];
		     elemLst := removeDupChars (!elemLst) [newSymbol] [];
		     lowerMatch  t (newSymbol::symbols))
		 else lowerMatch  t symbols
	      )
	
    | _ -> symbols
;;

(** glyph above elem
    @edited:  18-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec upperMatch elems symbols =
  match elems with
      h::t -> (let upper = findUpper h (!glyphLst) [] in
		 if (List.length upper) = 1 then  (
		   let newSymbol = {elList=[h];glList=upper} in
		     glyphLst := removeDupGlyphs (!glyphLst) [newSymbol] [];
		     elemLst := removeDupChars (!elemLst) [newSymbol] [];
		     upperMatch  t (newSymbol::symbols))
		 else upperMatch  t symbols
	      )
	
    | _ -> symbols
;;


(** glyph contains muliple elems
    @edited:  17-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec bigGlyphMatch glyphs symbols=
  match glyphs with
      h::t ->(let overlap = getGlyphElemOverlap h (!elemLst)  [] 0.8 1.1 0.8 in
	 	 if (List.length overlap) > 1 then(
		   let symbol =  {glList=([h]); elList = overlap} in
		     glyphLst := removeDupGlyphs (!glyphLst) [symbol] [];
		     elemLst  := removeDupChars (!elemLst) [symbol] [];
		   bigGlyphMatch t (symbol::symbols))
		 else bigGlyphMatch t symbols)
    | _ ->    symbols
;;


(** 
    @edited:  11-MAY-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec singleMatchAux glyphs elems symbs hGScale vScale hEScale=
  match glyphs with
      h::t -> (let overlap = getGlyphElemOverlap h elems [] hGScale vScale hEScale in
	 	 if (List.length overlap) = 1 then 
		   singleMatchAux t elems ({glList=([h]); elList =
					       overlap}::symbs) hGScale vScale hEScale
		 else
		   singleMatchAux t elems symbs hGScale vScale hEScale)
    | _ -> symbs
;;

let rec singleMatch symbols elems glyphs hGScale vScale hEScale= 
  
  let symbols1 = (singleMatchAux glyphs elems [] hGScale vScale hEScale)@symbols in
    
  let elems =  removeDupChars elems symbols1 [] in
  let glyphs = removeDupGlyphs glyphs symbols1 [] in  	
    
    if (List.length symbols1) = (List.length symbols) then symbols1
    else singleMatch symbols1 elems glyphs hGScale vScale hEScale
;;

(** 
    @edited:  16-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  true if elem only overlaps one glyph
 *)
let rec getSingleElemsAux elem glyphs overlap= 
  match glyphs with
    |  _ when (List.length overlap) = 2 -> false
    | h::t -> if (glyphElemOverlap h elem 0.8 1.1 0.8) 
      then getSingleElemsAux elem t (h::overlap)
      else getSingleElemsAux elem t overlap
    | [] when (List.length overlap) = 1 -> true
    | _ -> false
;;

(** 
    @edited:  16-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  list of elements only overlapping one glyph
 *)
let rec getSingleElems glyphs elems singles = 
  match elems with
    | h::t -> if (getSingleElemsAux h glyphs []) 
      then getSingleElems glyphs t (h::singles)
      else getSingleElems glyphs t singles
    | _ -> singles
;;

(** 
    @edited:  16-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  true if glyph only overlaps one elem
 *)
let rec getSingleGlyphsAux elems glyph overlap= 
  match elems with
    |  _ when (List.length overlap) = 2 -> false
    | h::t when glyphElemOverlap glyph h 0.8 1.1 0.8-> getSingleGlyphsAux t glyph (h::overlap)
    | h::t -> getSingleGlyphsAux t glyph overlap
    | [] when (List.length overlap) = 1 -> true
    | _ -> false
;;

(**
    @edited:  16-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  List of glyphs only overlapping one elem
*)
let rec getSingleGlyphs glyphs elems singles =
  match glyphs with 
    | h::t when getSingleGlyphsAux elems h [] ->  getSingleGlyphs t elems (h::singles)
    | h::t ->  getSingleGlyphs t elems  singles
    | _ -> singles

;;

(** 
    @edited:  17-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let singlesMatch glyphs elems=
  let glyphs = getSingleGlyphs glyphs elems [] in
  let elems = getSingleElems glyphs elems [] in

  let symbols = singleMatch [] elems glyphs 0.8 1.1 0.8 in
    glyphLst := removeDupGlyphs (!glyphLst) symbols [];
    elemLst  := removeDupChars (!elemLst) symbols [];


    symbols
;;

(** 
    @edited:  17-JUL-2012
    @author:  Josef Baker
    @input:   single glyph
    @effects: 
    @output:  a list of the elements ,if any, making the root in that glyph
 *)
let getEnclosedRoot glyph =
  let line = getTopLine glyph (!elemLst) (!btLn) in
  let root = getLeftRoot glyph (!elemLst) (!rtCh) in
    if (glyphElemOverlap glyph line 1. 1. 1.) 
      && (glyphElemOverlap glyph root 1. 1. 1.)
    then [line;root]
    else []
;;

(** 
    @edited:  17-JUL-2012
    @author:  Josef Baker
    @input:   glyph list
    @effects: removes used glyphs and elems from their respective ref lists
    @output:  list of root symbols
 *)
let rec rootMatch glyphs symbols =
  match glyphs with
    | h::t ->( let root = getEnclosedRoot h in
		 if (List.length root = 2) 
		 then ((*print_string "found";*) 
		       rootMatch t ({glList=([h]); elList =root}::symbols))
		 else rootMatch t symbols)

    | _ -> (   glyphLst := removeDupGlyphs (!glyphLst) symbols [];
	       elemLst  := removeDupChars (!elemLst) symbols [];
	       symbols)
	

(** 
    @edited:  08-MAY-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let makeSymbols glyphs elems =

(*	printElems (elems);*)
  let bottom = findBottom glyphs 0 in


  elemLst :=  sortElemX (cutElems bottom elems []);
  glyphLst := sortGlyphX glyphs;

  if (false)then (
    print_endline ("Before Match");
    printGlyphs (!glyphLst);
    printElems (!elemLst);
  );

  let roots = rootMatch (!glyphLst) [] in
  let singles = singlesMatch (!glyphLst) (!elemLst) in
  let bigGlyphs = bigGlyphMatch (!glyphLst) [] in

  let singles2 = singlesMatch (!glyphLst) (!elemLst) in
  let bigGlyphs2 = bigGlyphMatch (!glyphLst) [] in
 
  let uppers =  upperMatch (!elemLst) []in

  let singles3 = singlesMatch (!glyphLst) (!elemLst) in
  let bigGlyphs3 = bigGlyphMatch (!glyphLst) [] in


  let lowers = lowerMatch (!elemLst) [] in

  let symbols =  roots@singles@bigGlyphs@uppers@singles2@bigGlyphs2@singles3@bigGlyphs3@lowers in

  let symbols = getAbove symbols [] in
(*
    let symbols = basicMatch [] in
      
      
    let symbols = ((upperMatch !elemLst [])@symbols) in
      elemLst :=  removeDupChars !elemLst symbols [];
    
      glyphLst := sortGlyphX !glyphLst;
      elemLst := sortElemX !elemLst;
*)      
      if (List.length (!glyphLst) <(-1))then(
	print_endline ("After Match");
	printGlyphs (!glyphLst);
	printElems (!elemLst);
	print_endline ("Next Line");
      );
(*    printSymbols symbols;*)
symbols

;;
