open Contentparser;;
open Str;;
open Jsonfio.JsonfIO;;

type symb = {
  glList: bBox list;
  elList: elem list;
}

let top = ref 0
let bot = ref 0

(** {                  PRINTING FUNCTIONS                                } *)


(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   List of glyphs
    @effects: Prints list
    @output:  None
 *)
let rec printGlyphs glyphList =
  match glyphList with 
      h::t -> (
	print_string ("G x:"^(string_of_int h.Jsonfio.JsonfIO.x)^" y:"^
			(string_of_int h.Jsonfio.JsonfIO.y)^" w:"^
			(string_of_int h.Jsonfio.JsonfIO.w)^" h:"^
			(string_of_int h.Jsonfio.JsonfIO.h));
	print_newline ();
	printGlyphs t;
      )
    | [] -> ()
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Char list
    @effects: Prints list
    @output:  None
 *)
let rec printChars charList =
  match charList with 
      h::t -> (
	match h with 
	    Chr chr ->(
	      print_string ("C "^chr.chname^" x:"^(string_of_float chr.chx)^" y:"^(string_of_float chr.chy)^" w:"^(string_of_float chr.chw));
	      print_newline ();
	      printChars t;
	    )
	  | Ln ln ->(
	      print_string ("C line stx:"^
			      (string_of_float ln.stx)^" sty:"^
			      (string_of_float ln.sty)^" enx:"^
			      (string_of_float ln.enx)^" eny:"^
			      (string_of_float ln.eny));
	      print_newline ();
	      printChars t;
	    )
      )
	    
    | [] -> ()
;;


let print_chars_only charlist = 
  List.iter 
    (function 
       | {Jsonfio.JsonfIO.bbox = _; Jsonfio.JsonfIO.glyphs = _;  Jsonfio.JsonfIO.elements = charlist} -> 
	   List.iter 
	     (function
		| Jsonfio.JsonfIO.PDFChar c -> Printf.printf "%s " c.Jsonfio.JsonfIO.c)
	     charlist )
    charlist
	 


  

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Symbol List
    @effects: Prints list
    @output:  None
 *)
let rec printSymbols symbolList =
  match symbolList with
      h::t -> ( printChars h.elList;
		printGlyphs h.glList;
		print_string "*-*-*";
		printSymbols t;
	      )

    | [] -> print_newline ();

;;

(** {           SORTING FUNCTIONS                                        } *)


(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Two glyphs
    @effects: 
    @output:  True if first glyph's x is less than second
 *)
let glyphLessX s1 s2 =
  if s1.x < s2.x then true
  else (if (s1.x = s2.x) 
	then (
	  if s1.y <s2.y then true
	  else false)
	else false)
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Glyphs list
    @effects: 
    @output:  Ordered list by x coord
 *)
let rec sortGlyphX = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = glyphLessX x pivot in
      let left, right = List.partition is_less rest in
      sortGlyphX left @ [pivot] @ sortGlyphX right
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Two glyphs
    @effects: 
    @output:  true if first glyph's y is less than second's y
 *)
let glyphLessY s1 s2 =
  if s1.y < s2.y then true
  else false
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Glyph list
    @effects: 
    @output:  Ordered by y co ord
 *)
let rec sortGlyphsY = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = glyphLessY x pivot in
      let left, right = List.partition is_less rest in
      sortGlyphsY left @ [pivot] @ sortGlyphsY right
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Two chars
    @effects: 
    @output:  True if first char's y is less than second
 *)
let charLessY s1 s2 =
  match s1 with
      Ln  ln1 ->(
	match s2 with
	    Ln  ln2 ->( 
	      if ln1.sty < ln2.sty then true
	      else (if (ln1.sty = ln2.sty) 
		    then (
		      if ln1.stx <ln2.stx then true
		      else false)
		    else false) 
	    )
	  | Chr ch2 ->(
	      if ln1.sty < ch2.chy then true
	      else (if (ln1.sty = ch2.chy) 
		    then (
		      if ln1.stx <ch2.chx then true
		      else false)
		    else false)
	    )
      )
    | Chr ch1 ->(
	match s2 with
	    Ln  ln2 ->(
	      if ch1.chy < ln2.sty then true
	      else (if (ch1.chy = ln2.sty) 
		    then (
		      if ch1.chx <ln2.stx then true
		      else false)
		    else false)	
	    )
	  | Chr ch2 ->(
	      if ch1.chy < ch2.chy then true
	      else (if (ch1.chy = ch2.chy) 
		    then (
		      if ch1.chx <ch2.chx then true
		      else false)
		    else false)	    
	    )
      )
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Char list
    @effects: 
    @output:  List ordered by Y coord
 *)
let rec sortCharY = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = charLessY x pivot in
      let left, right = List.partition is_less rest in
      sortCharY left @ [pivot] @ sortCharY right
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Two chars
    @effects: 
    @output:  true if char1. x is less than char2.x
 *)
let charLessX s1 s2 =
  match s1 with
      Ln  ln1 ->(
	match s2 with
	    Ln  ln2 ->( 
	      if ln1.stx < ln2.stx then true
	      else (if (ln1.stx = ln2.stx) 
		    then (
		      if ln1.sty <ln2.sty then true
		      else false)
		    else false) 
	    )
	  | Chr ch2 ->(
	      if ln1.stx < ch2.chx then true
	      else (if (ln1.stx = ch2.chx) 
		    then (
		      if ln1.sty <ch2.chy then true
		      else false)
		    else false)
	    )
      )
    | Chr ch1 ->(
	match s2 with
	    Ln  ln2 ->(
	      if ch1.chx < ln2.stx then true
	      else (if (ch1.chx = ln2.stx) 
		    then (
		      if ch1.chy <ln2.sty then true
		      else false)
		    else false)	
	    )
	  | Chr ch2 ->(
	      if ch1.chx < ch2.chx then true
	      else (if (ch1.chx = ch2.chx) 
		    then (
		      if ch1.chy <ch2.chy then true
		      else false)
		    else false)	    
	    )
      )
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   char list
    @effects: 
    @output:  chars sorted by x coord
 *)
let rec sortCharX = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = charLessX x pivot in
      let left, right = List.partition is_less rest in
      sortCharX left @ [pivot] @ sortCharX right
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Two symbols
    @effects: 
    @output:  True if symbol 1's x coord is less than 2's
 *)
let symbLessX s1 s2=
  let el1 = s1.elList in
  let el2 = s2.elList in
    charLessX (List.hd el1) (List.hd el2)
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Symbol list
    @effects: 
    @output:  Symbols sorted by x coords
 *)
let rec sortSymbolX = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = symbLessX x pivot in
      let left, right = List.partition is_less rest in
      sortSymbolX left @ [pivot] @ sortSymbolX right
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
(*

(** {     CONTAINING FUNCTIONS                                           } *)


(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph and line
    @effects: 
    @output:  True if glyph contains line, else false
 *)
let glyphContainsLine glyph line =
 ((line.stx >= (float_of_int glyph.x)) &&
    (line.enx <= (float_of_int (glyph.x + glyph.w))) &&
    (line.sty >= (float_of_int glyph.y)) &&
    (line.eny <= (float_of_int (glyph.y + glyph.h))))
;;

(*What to do if width is missing*)
let getCharW char =
  if char.chw = 0.0 then 30.0
  else char.chw
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph, char
    @effects: 
    @output:  True if char's centre is contained horizontally within glyph
 *)
let glyphContainsCharHorizontal glyph char =
(*  let cx = char.chx +. ((getCharW char) /. 2.) in
  let clx = char.chx in
  let crx = char.chx +. (getCharW char) in*)

  let cx = char.chx +. ((getCharW char) /. 2.) in
  let clx = char.chx in
  let crx = char.chx +. (getCharW char) in

   (((cx>=(float_of_int glyph.x)) && (cx <= ((float_of_int glyph.x) +. (float_of_int glyph.w))))(* ||
      ((lx>=(float_of_int glyph.x)) && (lx <= ((float_of_int glyph.x) +. (float_of_int glyph.w)))) ||
      ((rx>=(float_of_int glyph.x)) && (rx <= ((float_of_int glyph.x) +. (float_of_int glyph.w))))*) )
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph, char
    @effects: 
    @output:  True if char is contained vertically withing glyph
 *)
let glyphContainsCharVertical glyph char =
  let cy = char.chy in
  let ty = char.chy -. (char.chsize /. 2.) in
  let by = char.chy +. (char.chsize /. 2.)  in
  (((cy >= (float_of_int glyph.y)) && (cy <= ((float_of_int glyph.y) +. (float_of_int glyph.h)))) ||
((by >= (float_of_int glyph.y)) && (by <= ((float_of_int glyph.y) +. (float_of_int glyph.h)))) ||
((ty >= (float_of_int glyph.y)) && (ty <= ((float_of_int glyph.y) +. (float_of_int glyph.h))))) 
;;

(** THIS NEEDS LOOKING AT
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
(*let  higherGlyphContainsCharVertical glyph char =

  let cht = char.chy -. char.chsize in
  let glb = ((float_of_int glyph.y) +. ((float_of_int glyph.h) *. 2.)) in
glb <= cht
;;
*)
(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   Glyph, char
    @effects: 
    @output:  True if glyph contains char's centre
 *)
let glyphContainsChar glyph char =
  (glyphContainsCharHorizontal glyph char) && (glyphContainsCharVertical glyph char)
;;

(** NEEDS LOOKING AT 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
(*let higherGlyphContainsChar glyph char =
  (glyphContainsCharHorizontal glyph char) && (higherGlyphContainsCharVertical glyph char)
;;
*)
(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph, char
    @effects: 
    @output:  True if glyph centre is horizontally contained by char
 *)
let charContainsGlyphHorizontal glyph char =
  let lch = char.chx in
  let rch = char.chx +. (getCharW char) in
  let cgl = (float_of_int glyph.x) +.((float_of_int glyph.w)/.2.) in

    ((lch <= cgl) && (rch >= cgl))
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph, char
    @effects: 
    @output:  True if glyph centre is vertically contained by char
 *)
let charContainsGlyphVertical glyph char =
  let topG = float_of_int glyph.y in
  let botG = topG +. (float_of_int glyph.h) in
  
    ((char.chy >= topG) && (char.chy <= botG))
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph, char
    @effects: 
    @output:  true if char contains glyph
 *)
let charContainsGlyph glyph char =
  match char with
      Chr chr -> (charContainsGlyphVertical glyph chr) && (charContainsGlyphHorizontal glyph chr)
    | Ln ln -> false
;;


(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   char, list of glyph, initialy empty list
    @effects: 
    @output:  list of all glyphs contained by that char
 *)
let rec getAllContainedGlyphs char glyphs outGlyphs =
  match glyphs with
      h::t -> (if charContainsGlyph h char then getAllContainedGlyphs char t (h::outGlyphs)
	       else getAllContainedGlyphs char t outGlyphs)
    | [] -> outGlyphs

;;






let extraVerticalGlyph symbol =
  match symbol with
      Chr c -> (c.chname = "i")||(c.chname = "j")
    | _ -> false
;;
*)
(** 
    @edited:  08-JUN-2010
    @author:  Josef Baker
    @input:   Two glyphs
    @effects: 
    @output:  True if equal else false
 *)
let equalGlyph g1 g2 =
  ((g1.x = g2.x) && (g1.y = g2.y) && (g1.w = g2.w) && (g1.h =g2.h))
;;


(** 
    @edited:  08-JUN-2010
    @author:  Josef Baker
    @input:   Two chars
    @effects: 
    @output:  True if equal else false
 *)
let equalChar c1 c2 =
  match c1 with
      Chr chr -> (
	match c2 with
	    Chr chr2 -> ( (chr.chname = chr2.chname) &&
			    (chr.chx = chr2.chx) && (chr.chy = chr2.chy) &&
			    (chr.chw = chr2.chw) )
	  | _ -> false
      )
    | Ln ln -> (
	match c2 with
	    Ln ln2 -> ( (ln.stx = ln2.stx) && (ln.sty = ln2.sty) &&
			  (ln.enx = ln2.enx) && (ln.eny = ln2.eny) )
	  | _ -> false
      )
;;



(** 
    @edited:  08-JUN-2010
    @author:  Josef Baker
    @input:   Char and list of symbols
    @effects: 
    @output:  True if char is within symbol list
 *)
let rec checkChar char symbols =
  match symbols with 
      h::t -> ( if (equalChar char h)
		then true
		else checkChar char t
	      )
    | [] -> false
;;


let rec checkChars inChars outChars symbols =
  match inChars with 
      h::t -> ( if checkChar h symbols then checkChars t outChars symbols
		else checkChars t (h::outChars) symbols )
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


(*coordinates 0,0 top left, as is x,y w -> h\|/*)







(*





let rec getAllContainedChars glyph chars outChars =
    match chars with
	h::t -> (match h with
		     Chr chr -> (if glyphContainsChar glyph chr then getAllContainedChars glyph t ((Chr chr)::outChars)
				 else getAllContainedChars glyph t outChars)
		   | Ln ln -> ( if glyphContainsLine glyph ln then getAllContainedChars glyph t ((Ln ln)::outChars)
				else getAllContainedChars glyph t outChars))
      | [] -> outChars
;;

let isRoot name =
  try ( search_forward (regexp "radical") name 0;
true)
  with Not_found -> false
;;

let rec getAllContainedRoots glyph chars outChars =
    match chars with
	h::t -> (match h with
		     Chr chr -> (if ((glyphContainsChar glyph chr)&&(isRoot chr.chname)) 
				     then getAllContainedRoots glyph t ((Chr chr)::outChars)
				 else getAllContainedRoots glyph t outChars)
		   | Ln ln -> ( if glyphContainsLine glyph ln then getAllContainedRoots glyph t ((Ln ln)::outChars)
				else getAllContainedRoots glyph t outChars))
      | [] -> outChars
;;

let rec highestChar char chars =
  match chars with
      h::t -> (match h with
		   Chr c1 -> (match char with
				  Chr c2 -> (if c2.chy < c1.chy then highestChar (Chr c2) t
					     else highestChar (Chr c1) t)
				| Ln l2  -> (if l2.sty < c1.chy then highestChar (Ln l2) t
					     else highestChar (Chr c1) t))
		 | Ln l1  -> (match char with
				  Chr c2 -> (if c2.chy < l1.sty then highestChar (Chr c2) t
					     else highestChar (Ln l1) t)
				| Ln l2  -> (if l2.sty < l1.sty then highestChar (Ln l2) t
					     else highestChar (Ln l1) t)))
    | [] -> char
;;



let rec sideMatch chars glyphs symbols =
  match chars with
      h::t -> (
	let cGlyphs = getAllContainedGlyphs h glyphs [] in
	  if (List.length cGlyphs)>1 then (
	    let symbol = {glList=cGlyphs; elList = (h::[])} in
	    let glyphs = removeDupGlyphs glyphs (symbol::[]) [] in
	      sideMatch t glyphs (symbol::symbols))
	  else sideMatch t glyphs symbols)
   | [] -> symbols
;;



(* 1 GLYPH MORE THAN ONE CHAR () SUM *)
let rec multiMatch chars glyphs symbols =
 match glyphs with 
      h::t ->(
	let elems = getAllContainedChars h chars [] in
	  if (List.length elems)> 1 then (
	    let symbol = {glList=(h::[]); elList = elems} in
	    let chars = removeDupChars chars (symbol::[]) [] in
	      multiMatch chars t (symbol::symbols))
	  else multiMatch chars t symbols)
   | [] -> symbols
;;

(* 1 GLYPH MORE THAN ONE CHAR () SUM *)
let rec rootMatch chars glyphs symbols =
 match glyphs with 
      h::t ->(
	let elems = getAllContainedRoots h chars [] in
	  if (List.length elems)> 1 then (
	    let symbol = {glList=(h::[]); elList = elems} in
	    let chars = removeDupChars chars (symbol::[]) [] in
	      rootMatch chars t (symbol::symbols))
	  else rootMatch chars t symbols)
   | [] -> symbols
;;

let rec getAllHigherGlyphs char glyphs highers =
  match glyphs with
      h::t -> (  if ((h.y < (int_of_float char.chy)) && (charContainsGlyphHorizontal h char))
		      then getAllHigherGlyphs char t (h::highers)
	       else getAllHigherGlyphs char t highers)
    | [] -> ((*print_string "********";-
	     printGlyphs highers;
	     print_string "*******";*)
	     highers;)
;;


(*WHEN GLYPH IS HIGHER THAN CHAR  - =*)
let rec highGlyphMatch chars glyphs symbols =
  match chars with
      h::t ->(
	match h with 
	    Chr ch ->( 
	      if ((ch.chy<=(float_of_int !bot)) && (ch.chy>=(float_of_int !top)))then(
		let highGlyphs = getAllHigherGlyphs ch glyphs [] in
		  if (List.length highGlyphs)> 0 
		  then (  
		    let symbol = {glList=(highGlyphs); elList = (Chr ch::[])} in
		      
		    let glyphs = removeDupGlyphs glyphs (symbol::[]) [] in
		      highGlyphMatch t glyphs (symbol::symbols))
		    
		  else (
		    highGlyphMatch t glyphs symbols))
	      else highGlyphMatch t glyphs symbols)
	    | Ln l ->(highGlyphMatch t glyphs symbols))
	  | [] -> symbols
;;




(*SINGLE CHAR SINGLE GLYPH A B C*)
let rec singleMatch chars glyphs symbols =
 match glyphs with 
      h::t ->(
	let elems = getAllContainedChars h chars [] in
	  if (List.length elems)= 1 then (
	    let symbol = {glList=(h::[]); elList = elems} in
	    let chars = removeDupChars chars (symbol::[]) [] in
	      singleMatch chars t (symbol::symbols))
	  else singleMatch chars t symbols)
   | [] -> symbols
;;

(*WHEN SYMBOLS HAVE EXTRA VERTICAL GLYPHS. i,j *)
let rec upperGlyphsMatch inSymbols glyphs outSymbols =
  match inSymbols with
      h::t -> ( let c = (List.hd h.elList) in
		  match c with 
		      Chr ch ->(
			if (extraVerticalGlyph c) then (
			  let extraGlyphs =  getAllHigherGlyphs ch glyphs [] in
			   
			      let symbol = {elList = h.elList; glList = ((extraGlyphs@h.glList))} in
			      let glyphs = removeDupGlyphs glyphs (symbol::[]) [] in
				upperGlyphsMatch t glyphs (symbol::outSymbols)
			)
			else upperGlyphsMatch t glyphs (h::outSymbols))
		    | _ ->upperGlyphsMatch t glyphs (h::outSymbols))
	
    | [] -> outSymbols
;;

let rec basicMatch chars glyphs symbols =

  let symbols1 = (singleMatch chars glyphs [])@symbols in
  let chars =  removeDupChars chars symbols1 [] in
  let glyphs = removeDupGlyphs glyphs symbols1 [] in 	

    if (List.length symbols1) = (List.length symbols) then symbols1
    else basicMatch chars glyphs symbols1
;;

*)


let rec convElems elems out =
  match elems with
      h::t ->(match h with 
		  Chr chr -> convElems t (PDFChar {c=chr.chname;
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

let rec findTop glyphs y =
  match glyphs with 
      h::t -> ( if y < h.y then findTop t y
	     else findTop t h.y)
    | [] -> y
;;

let rec findBot glyphs y =
  match glyphs with
      h::t -> ( if y > (h.y + h.h) then findBot t y
		else findBot t (h.y + h.h))
      | [] -> y
;;

(******************NEW STUFF********)

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph, char
    @effects: 
    @output:  True if glyph and element overlap horizontally
 *)
let glyphElemOverlapX glyph elem =
  let gL = float_of_int glyph.x in
  let gR = gL +. float_of_int glyph.w in

    match elem with
	Chr chr ->( let eL = chr.chx in
		    let eR = eL +. chr.chw in
		      (((eL >= gL) && (eL <= gR)) || 
			 ((eR >= gL) && (eR <= gR)) ||
			 ((gL >= eL) && (gL <= eR)) || 
			 ((gR >= eL) && (gR <= eR))))
      | Ln ln   ->( let eL = ln.stx in
		    let eR = ln.enx in
		      (((eL >= gL) && (eL <= gR)) || 
			 ((eR >= gL) && (eR <= gR)) ||
			 ((gL >= eL) && (gL <= eR)) || 
			 ((gR >= eL) && (gR <= eR))))
;;

(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph, char
    @effects: 
    @output:  True if glyph and element overlap vertically
 *)
let glyphElemOverlapY glyph elem =
  let gB = float_of_int glyph.y in
  let gT = gB +. float_of_int glyph.h in
    match elem with
	Chr chr ->( let e = chr.chy in
		      ((e >= gB) && (e <= gT)) )
      | Ln ln   ->( let e = ln.sty in
		      ((e >= gB) && (e <= gT)) )
;;

let glyphElemOverlap glyph elem =
(glyphElemOverlapY glyph elem) && (glyphElemOverlapX glyph elem)

(** 
    @edited:  11-MAY-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  list of elements overlapping glyph
 *)
let rec getOverlap glyph elems overlap =
  match elems with 
      h::t ->(if glyphElemOverlap glyph h then getOverlap glyph t (h::overlap)
	      else getOverlap glyph t overlap)
    | _ -> overlap
;;

(** 
    @edited:  11-MAY-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec singleMatch glyphs elems symbs =
  match glyphs with
      h::t -> (let overlap = getOverlap h elems [] in
		 if (List.length overlap) = 1 then 
		   singleMatch t elems ({glList=(h::[]); elList = overlap}::symbs)
		 else
		   singleMatch t elems symbs)
    | _ -> symbs
;;

let rec basicMatch chars glyphs symbols =

  let symbols1 = (singleMatch glyphs chars [])@symbols in
  let chars =  removeDupChars chars symbols1 [] in
  let glyphs = removeDupGlyphs glyphs symbols1 [] in 	

    if (List.length symbols1) = (List.length symbols) then symbols1
    else basicMatch chars glyphs symbols1
;;


let matcher chars glyphs =
  
    
    top := findTop glyphs ((List.hd glyphs).y);
    bot := findBot glyphs ((List.hd glyphs).y + (List.hd glyphs).h);
    
  let glyphs = sortGlyphX glyphs in

    let symbols = [] in
      
    (*SINGLE CHAR SINGLE GLYPHS*)
    let symbols = basicMatch chars glyphs symbols in
      
    let chars =  removeDupChars chars symbols [] in
    let glyphs = removeDupGlyphs glyphs symbols [] in 	

    let symbols = sortSymbolY symbols in 
 
 let glyphs = sortGlyphX glyphs in
(*printGlyphs glyphs;
printChars chars;
print_newline ();
print_newline ();
*)

(*
    let symbols = upperGlyphsMatch  symbols glyphs [] in
      
      
    let glyphs = removeDupGlyphs glyphs symbols [] in 

    (*SQUARE ROOTS*)
      
    let chars = sortCharX chars in
    let glyphs = sortGlyphX glyphs in

    let symbols = (rootMatch chars glyphs [])@symbols in
      
    let chars =  removeDupChars chars symbols [] in
    let glyphs = removeDupGlyphs glyphs symbols [] in 

    (*MULTI CHAR SINGLE GLYPHS*)
      
    let chars = sortCharX chars in
    let glyphs = sortGlyphX glyphs in

    let symbols = (multiMatch chars glyphs [])@symbols in
      
    let chars =  removeDupChars chars symbols [] in
    let glyphs = removeDupGlyphs glyphs symbols [] in 
      

    
    (*HIGH GLYPH SYMBOLS*)
    
    let chars = (sortCharY chars) in
    let glyphs = (sortGlyphX glyphs) in

    let symbols = (highGlyphMatch chars glyphs [])@symbols in
      
    let chars =  removeDupChars chars symbols [] in
    let glyphs = removeDupGlyphs glyphs symbols [] in
      
    let chars = sortCharX chars in
    let glyphs = sortGlyphX glyphs in
     
    let symbols = (sideMatch chars glyphs [])@symbols in
      
    let chars =  removeDupChars chars symbols [] in
    let glyphs = removeDupGlyphs glyphs symbols [] in
 
      

(*      print_string "*";
      print_int (List.length glyphs);
      print_string "*\n";
      printGlyphs glyphs ;
      print_string "*\n";
*)    
      (* (*printSymbol symbols;*)
	 print_newline ();
	 print_newline ();
      *)
*)
      let symbols = sortSymbolX symbols in
	(*   printLine symbols;*)
	symbols 
	
;;

