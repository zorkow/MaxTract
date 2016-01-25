open Jsonfio;;
open Synt;;
let hTol = 20;;
let test h =
(h.JsonfIO.c);;



(** 
    @edited:  07-OCT-2012
    @author:  Josef Baker
    @input:   a string of characters, separated by spaces
    @effects: 
    @output:  true if its an alpha character, or would form part of a word
 *)
let isAlpha ch =
  let reg = Str.regexp "\\(\\([A-Z]\\|[a-z]\\(acute\\|caron\\|dieresis\\)?\\)\\|\\(f+\\(i\\|l\\)?\\)\\|\\(aif[0-9]*\\)\\)\\( \\(\\([A-Z]\\|[a-z]\\(acute\\|caron\\|dieresis\\)?\\)\\|\\(f+\\(i\\|l\\)?\\)\\|\\(period\\|comma\\|colon\\|semicolon\\|hyphen\\)\\|\\(aif[0-9]*\\)\\)\\)*$" in
    Str.string_match reg ch 0 
;;

let isNum ch = 
  let reg = Str.regexp
  "\\(\\(one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|zero\\)\\( \\(one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|zero\\)\\)*$\\)\\|\\(\\([0-9]\\)\\( \\([0-9]\\)\\)*$\\)" in
    Str.string_match reg ch 0 
;;
	
let isSameType s1 s2 =
  ((isAlpha s1) && (isAlpha s2)) || ((isNum s1) && (isNum s2))
  ;;


(** 
    @edited:  14-FEB-2010
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let mergeSynt s1 s2 =
  let mname = s1.name ^" "^ s2.name in
  let mfont = s1.font in
  let msize = s1.size in
  let mx1 = s1.Synt.x1 in
  let mx2 = (s1.Synt.x2+s2.Synt.x2)/2 in
  let mx3 = s2.Synt.x3 in
  let my1 = (min s1.Synt.y1 s2.Synt.y1) in
  let my2 = (s1.Synt.y2+s2.Synt.y2)/2 in
  let my3 = (max s1.Synt.y3 s2.Synt.y3) in
  {name=mname; font=mfont;size=msize;
 Synt.x1=mx1; Synt.x2=mx2; Synt.x3=mx3; Synt.y1=my1; Synt.y2=my2; Synt.y3=my3;}
;;
(*

(** 
    @edited:  14-FEB-2010
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec hasRightGroup synt list index =
  match list with
      [] -> -1
    | h::t -> (
	  if (((h.font = synt.font) && (h.y2 = synt.y2) &&  ((h.x1 - synt.x3)<hTol)) && 
		(((isChar h.name) && (isChar synt.name)) || ((isNum h.name)&&(isNum synt.name)))) then (index +1)
	  else hasRightGroup synt t (index+1))
;;



(** 
    @edited:  14-FEB-2010
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec group inList outList =
  match inList with
      [] -> outList
    | h::t ->(
	let second = hasRightGroup h t 0 in
	  if (second = -1) then group t (h::outList)
	  else(                   
	    let str = mergeSynt h (List.nth  inList second) in
	    let inList2 = (Synt.delete inList second) in
	      group (str::(List.tl inList2)) outList))
;; 




let rec removeGroup inSynts group outSynts count =
  match group,inSynts with
      grouphd::grouptl,synthd::synttl when grouphd=count 
	-> removeGroup synttl grouptl outSynts (count+1)
    | grouphd::grouptl,synthd::synttl 
	-> removeGroup synttl group (synthd::outSynts) (count+1)
    | [],synthd::synttl -> removeGroup synttl group (synthd::outSynts) (count+1)
    | [],[] -> List.rev outSynts
;;

let rec makeGroup inSynts group outSynt count =
  match group,inSynts with
      grouphd::grouptl,synthd::synttl when grouphd=count 
	-> makeGroup synttl grouptl (mergeSynt outSynt synthd )(count+1)    
    | grouphd::grouptl,synthd::synttl -> makeGroup synttl group outSynt (count+1)
    | [],_ ->outSynt 

(** 
    @edited:  13-AUG-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec getAlphaNumGroup synt syntList group index=
  match syntList with
      [] -> List.rev group
    | h::t when (h.font = synt.font) && (h.y2 = synt.y2) && 
	
	(((float_of_int (h.x1-synt.x3))
	  /.
	  ((float_of_int (h.x3-h.x1 + synt.x3-synt.x1))/.2.0))
	   
	   
	 <=0.3) &&
	(isSameType synt.name h.name)
	-> (
	  getAlphaNumGroup h t (index::group) (index+1))
    | h::t when (syntsVOverlap synt h) -> List.rev group
    | h::t ->(
	getAlphaNumGroup synt t group (index+1))

(** 
    @edited:  13-AUG-2012
    @author:  Josef Baker
    @input:   input list of synts, initially empty output list
    @effects: 
    @output:  
 *)
let rec groupAlphaNum inSynts outSynts =
  match inSynts with
      [] -> outSynts
    | h::t -> (let group = getAlphaNumGroup h t [0] 1 in
	       let inSynts = sortSynt (removeGroup inSynts group [] 0) in
(*Util.printSyntList (sortSynt inSynts);*)
	       let outSynts = ((makeGroup t (List.tl group) h 1)::outSynts) in
		 groupAlphaNum  inSynts outSynts)

	*)
(** 
    @edited:  06-FEB-2010
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  Elements name
 *)
let rec element2name elementList =
  match elementList with
      (JsonfIO.Line line)::[] -> "line"
    | (JsonfIO.PDFChar pdfChar)::[] -> pdfChar.JsonfIO.c
    | (JsonfIO.Line line)::t -> ("line-"^element2name t)
    | (JsonfIO.PDFChar pdfChar)::t -> (pdfChar.JsonfIO.c^"-"^element2name t)
    | _ -> "error"
;;

(** 
    @edited:  06-FEB-2010
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  Element's size
 *)
let rec element2size elementList =
  match elementList with
      (JsonfIO.Line line)::t ->  0.0;
    | (JsonfIO.PDFChar pdfChar)::t -> pdfChar.JsonfIO.scale
    | _ -> 0.0
;;

(** 
    @edited:  06-FEB-2010
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  Element's font
 *)
let rec element2font elementList =
  match elementList with
      (JsonfIO.Line line)::t ->  "none";
    | (JsonfIO.PDFChar pdfChar)::t -> pdfChar.JsonfIO.font
    | _ -> "none"
;;

(** 
    @edited:  06-FEB-2010
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  Element's base x coord
 *)
let rec element2x symbol =
  let elementList = symbol.JsonfIO.elements in
    match elementList with
	(JsonfIO.Line line)::[] ->  (symbol.JsonfIO.bbox.JsonfIO.x + (symbol.JsonfIO.bbox.JsonfIO.w/2)) ;
      | (JsonfIO.PDFChar pdfChar)::[] -> pdfChar.JsonfIO.bx
      | _ -> (symbol.JsonfIO.bbox.JsonfIO.x + (symbol.JsonfIO.bbox.JsonfIO.w/2))
;;

(** 
    @edited:  23-JUL-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec element2yaux elems base =
  match elems with
    | JsonfIO.Line line::[] ->  (max base line.JsonfIO.ey)
    | JsonfIO.PDFChar pdfChar::[] -> (max  base pdfChar.JsonfIO.by)
    | _ -> base


(** 
    @edited:  06-FEB-2010
    @author:  Josef Baker
    @input:   symbol
    @effects: 
    @output:  Element's base y coord
 *)
let rec element2y symbol =
  let elementList = symbol.JsonfIO.elements in
  let baseY =  element2yaux elementList 0 in
    if baseY < symbol.JsonfIO.bbox.JsonfIO.y then (symbol.JsonfIO.bbox.JsonfIO.y + (symbol.JsonfIO.bbox.JsonfIO.h/2))
    else baseY
;; 

(** 
    @edited:  06-FEB-2010
    @author:  Josef Baker
    @input:   List of symbols from jsonf file
    @effects: 
    @output:  corresponding list of synt objects
 *)
let rec symbol2synt symbolList syntList =
   match symbolList with
      h::t -> symbol2synt t ({Synt.name=(element2name h.JsonfIO.elements); 
			      Synt.font=(element2font h.JsonfIO.elements);
			      Synt.size=(element2size h.JsonfIO.elements);
			      Synt.x1=h.JsonfIO.bbox.JsonfIO.x;
			      Synt.x2=(element2x h);
			      Synt.x3=((h.JsonfIO.bbox.JsonfIO.x)+(h.JsonfIO.bbox.JsonfIO.w));
			      Synt.y1=h.JsonfIO.bbox.JsonfIO.y;
			      Synt.y2=(element2y h);
			      Synt.y3=((h.JsonfIO.bbox.JsonfIO.y)+(h.JsonfIO.bbox.JsonfIO.h))}::syntList)
    | _ -> syntList


let getWidth synt =
  let sList = Str.split (Str.regexp " ") synt.name  in
float_of_int (List.length sList)
;;
(** 
    @edited:  08-OCT-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec hasGroup synt list =
  match list with
      h::t when ((h.font = synt.font) && (h.y2 = synt.y2) && 
	(((float_of_int (h.x1-synt.x3)) /. ((float_of_int (h.x3-h.x1 + synt.x3-synt.x1))/.(1.0 +. (getWidth synt))))
	 <=0.3) && (isSameType synt.name h.name)) -> true
    | h::t when (((h.y1>synt.y1) && (h.y1<synt.y3)) || ((h.y3>synt.y1) && (h.y3<synt.y3)) 
		 ||((synt.y1>h.y1) && (synt.y1<h.y3)) || ((synt.y3>h.y1) && (synt.y3<h.y3))) -> false
    |[] -> false
    | h::t -> hasGroup synt t
;;



(** 
    @edited:  08-OCT-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec mergeGroup synt inList outList =
  match inList with
      h::t when ((h.font = synt.font) && (h.y2 = synt.y2) && 
	(((float_of_int (h.x1-synt.x3)) /. ((float_of_int (h.x3-h.x1 +
    synt.x3-synt.x1))/.(1.0 +. (getWidth synt))))
	 <=0.3) && (isSameType synt.name h.name)) -> ((mergeSynt synt h)::((List.rev outList)@t)) 
    | h::t -> mergeGroup synt t (h::outList)
;;

(** 
    @edited:  08-OCT-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec group syntList grouped =
  match syntList with
      h::t when hasGroup h t -> group (mergeGroup h t []) grouped
    | h::t -> group t (h::grouped)
    | [] -> grouped



(** 
    @edited:  13-AUG-2012
    @author:  Josef Baker
    @input:   List of jsonf symbols (basically a jsonf file)
    @effects: 
    @output:  List of synts for linearize, grouped together where appropraite
 *)
let preprocess symbolList =

  let syntList = symbol2synt symbolList [] in
(*Util.printSyntList (sortSynt syntList);*)
 (* let syntList =  sortSynt (groupAlphaNum (sortSynt syntList) [])  in*)
(*Util.printSyntList (syntList);*)
let syntList =  sortSynt (group (sortSynt syntList) [])  in
(*Util.printSyntList (syntList);*)
syntList
;;
