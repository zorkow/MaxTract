open Contentparser;;
open Str;;
open Jsonfio.JsonfIO;;

(******Printing *)

  
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
		      if ln1.sty <=ln2.sty then true
		      else false)
		    else false) 
	    )
	  | Chr ch2 ->(
	      if ln1.stx < ch2.chx then true
	      else (if (ln1.stx = ch2.chx) 
		    then (
		      if ln1.sty <=ch2.chy then true
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
		      if ch1.chy <=ln2.sty then true
		      else false)
		    else false)	
	    )
	  | Chr ch2 ->(
	      if ch1.chx < ch2.chx then true
	      else (if (ch1.chx = ch2.chx) 
		    then (
		      if ch1.chy <=ch2.chy then true
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
let rec sortElemX = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = charLessX x pivot in
      let left, right = List.partition is_less rest in
      sortElemX left @ [pivot] @ sortElemX right
;;

(****Comparison***********)
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
  match c1,c2 with
      Chr chr, Chr chr2 ->(  (chr.chx = chr2.chx) && 
			     (chr.chy = chr2.chy) )
    | Ln ln, Ln ln2     ->((ln.stx = ln2.stx) && 
			     (ln.sty = ln2.sty) &&
			     (ln.enx = ln2.enx) && 
			     (ln.eny = ln2.eny))
    | _, _              -> false
;;



(** 
    @edited:  14-MAR-2011
    @author:  Josef Baker
    @input:   glyph, char, scaling factor
    @effects: 
    @output:  True if glyph and element overlap horizontally
 *)
let glyphElemOverlapX glyph elem eScale gScale=
  let gL = float_of_int glyph.x in
  let gR = gL +. (float_of_int glyph.w*.gScale) in
  let gL = gL +. (float_of_int glyph.w*.(1.-.gScale)) in


    match elem with
	Chr chr ->( let eL = chr.chx in
		    let eR = eL +. (chr.chw *. eScale) in
		    let eL = eL +. (chr.chw *. (1.-.eScale)) in
	(*	    let eR = eL +. chr.chw in*)
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
    @input:   glyph, char, scaling factor
    @effects: 
    @output:  True if glyph and element overlap vertically
 *)
let glyphElemOverlapY glyph elem scale=
  let gT = float_of_int glyph.y in
  let gB = gT +. (float_of_int glyph.h *. scale) in
    
    match elem with
	Chr chr ->( let e = chr.chy in
		      ((e >= gT) && (e <= gB)) )
      | Ln ln   ->( let e = ln.sty in
		      ((e >= gT) && (e <= gB)) )
;;




(** 
    @edited:  29-JUN-2012
    @author:  Josef Baker
    @input:   glyph, element
    @effects: 
    @output:  true if top of glyph is higher than elem basepoint
 *)
let glyphAboveElem glyph elem =
  match elem with
	Chr chr ->( (float_of_int glyph.y) < chr.chy )
      | Ln ln   ->( (float_of_int glyph.y) < ln.sty  )

(** 
    @edited:  29-JUN-2012
    @author:  Josef Baker
    @input:   glyph, element
    @effects: 
    @output:  true if top of glyph is lower than elem basepoint
 *)
let glyphBelowElem glyph elem =
  match elem with
	Chr chr ->( (float_of_int glyph.y) > chr.chy )
      | Ln ln   ->( (float_of_int glyph.y) > ln.sty  )



(** 
    @edited:  29-JUN-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  True if elem is within a glyph
 *)
let glyphElemOverlap glyph elem hGScale vScale hEScale= 
  (glyphElemOverlapY glyph elem vScale) && (glyphElemOverlapX glyph elem hEScale
  hGScale)


let isRoot name =
  try ( search_forward (regexp "radical") name 0;
true)
  with Not_found -> false
;;

(** 
    @edited:  17-JUL-2012
    @author:  Josef Baker
    @input:   glyph and list of elements
    @effects: 
    @output:  leftmost radical enclosed in the glyph
 *)
let rec getLeftRoot glyph elems root=
  match elems with
      (Chr ch)::t -> (if ((glyphElemOverlap glyph (Chr ch) 1. 1. 1.) &&
    (charLessX (Chr ch) root) && (isRoot ch.chname)) 
		     then getLeftRoot glyph t (Chr ch)
		     else getLeftRoot glyph t root)
    | [] -> root 
    | _ -> getLeftRoot glyph (List.tl elems) root
;;

(** 
    @edited:  17-JUL-2012
    @author:  Josef Baker
    @input:   glyph and is of elements
    @effects: 
    @output:  highest line enclosed in the glyph
 *)
let rec getTopLine glyph elems line=
  match elems with
      (Ln ln)::t -> (if ((glyphElemOverlap glyph (Ln ln) 1. 1. 1.) && (charLessY (Ln ln) line)) 
		     then getTopLine glyph t (Ln ln)
		     else getTopLine glyph t line)
    | [] -> line 
    | _ -> getTopLine glyph (List.tl elems) line
;;

(** 
    @edited:  29-JUN-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  A list of glyphs directly above the element
 *)
let rec findAbove elem glyphs above=
  match glyphs with
      h::t -> (  if ((glyphAboveElem h elem) && (glyphElemOverlapX h elem 1. 1.)) 
		 then findAbove elem t (h::above)
		 else findAbove elem t above)
    | _ -> (if List.length above > 0 then [List.hd (List.rev (sortGlyphsY above))]
	    else [])

let rec findLower elem glyphs below=
 match glyphs with
     h::t when (glyphBelowElem h elem) && (glyphElemOverlapX h elem 1. 1.) -> findLower elem t (h::below)
   | h::t  -> findLower elem t below
   | _ when (List.length below) > 0 -> (
       let lower = List.hd (sortGlyphsY below) in
	 match elem with
	     Chr ch when ((float_of_int (lower.y)-. ch.chy) < 50.) -> [lower]
	   | _ -> []
     )
   | _ -> []



let rec findUpper elem glyphs above=
 match glyphs with
     h::t when (glyphAboveElem h elem) && (glyphElemOverlapX h elem 1. 1.) -> findUpper elem t (h::above)
   | h::t  -> findUpper elem t above
   | _ when (List.length above) > 0 -> (
       let upper = List.hd (List.rev (sortGlyphsY above)) in
	 match elem with
	     Chr ch when ((ch.chy -. (float_of_int (upper.y+upper.h))) < 50.) -> [upper]
	   | _ -> []
     )
   | _ -> []



let isChar elem =
  match elem with
      Chr ch ->(
	match ch.chname with
	|"A"|"B"|"C"|"D"|"E"|"F"|"G"|"H"|"I"|"J"|"K"|"L"|"M"|"N"|"O"|"P"|"Q"|"R"|"S"|"T"|"U"|"V"|"W"|"X"|"Y"|"Z"
	|"a"|"b"|"c"|"d"|"e"|"f"|"g"|"h"|"i"|"j"|"k"|"l"|"m"|"n"|"o"|"p"|"q"|"r"|"s"|"t"|"u"|"v"|"w"|"x"|"y"|"z" ->true
	|_ ->false)
    | _ -> false
;;

(** 
    @edited:  29-JUN-2012
    @author:  Josef Baker
    @input:   list of glyphs
    @effects: 
    @output:  lowest coordinate of glyph list
 *)
let rec findBottom glyphs bottom =
  match glyphs with
      h::t -> ( if bottom > (h.y + h.h) then findBottom t bottom
		else findBottom t (h.y + h.h))
      | [] -> bottom
;;



(** 
    @edited:  29-JUN-2012
    @author:  Josef Baker
    @input:   list of elements, limit
    @effects: 
    @output:  list of elements above the limit
 *)
let rec cutElemsAux bound inElems outElems =
  match inElems with
      (Chr ch)::t when ch.chy < (float_of_int (bound +10))-> cutElemsAux bound t ((Chr ch)::outElems)
    | (Ln ln)::t when ln.sty < (float_of_int (bound +10))-> cutElemsAux bound t ((Ln ln)::outElems)
    | [] -> outElems
    | _ -> cutElemsAux bound (List.tl inElems) outElems
;;

let rec joinElems inElems outElems =
  match inElems with
      (Chr ch1)::(Chr ch2)::t when (ch1.chy = ch2.chy) && (ch1.chx = ch2.chx) ->
	joinElems ((Chr {chname = (ch1.chname^"-"^ch2.chname); 
			chx = ch2.chx; 
			chy = ch2.chy;
			chfont = ch2.chfont;
			chsize = ch2.chsize;
			chw = (max ch1.chw ch2.chw)})::t) outElems
    | h::t -> joinElems t (h::outElems)
    | [] -> outElems
;;

let cutElems bound inElems outElems =
  let elems = cutElemsAux bound inElems outElems in
    joinElems (sortElemX elems) []
;;
