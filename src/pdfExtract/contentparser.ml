open Pdfextractor;;

(*For testing*)
let verbose = ref false

(*Input streams*)
let contents = ref ""
let textStr = ref ""

(*Text state*)
let charSpace = ref 0.0
let wordSpace = ref 0.0
let scale = ref 100.0
let leading = ref 0.0
let font = ref ""
let size = ref 0.0
let rise = ref 0.0



type matrix = {a:float;b:float;c:float;d:float;e:float;f:float}

let textMatrix = ref {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;f=0.0}
let lineMatrix = ref {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;f=0.0}
let ctMatrix = ref {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;f=0.0}

let identity = {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;f=0.0};;

(*Graphics state*)
let lineWidth = ref 0.0 

type chr = {chname:string; chfont:string; chsize:float; chx:float; chy:float; chw:float}
type ln = {stx:float; sty:float; enx:float; eny:float; lnw:float}

type elem =
  | Chr of chr
  | Ln of ln



(** 
    @edited:  28-FEB-2012
    @author:  Josef Baker
    @input:   two 3x3 matrices 
    @effects: 
    @output:  result of matrx multiplication
 *)
let matrixX m1 m2 =
  {a= ((m1.a*.m2.a)+.(m1.b*.m2.c));
   b= ((m1.a*.m2.b)+.(m1.b*.m2.d));
   c= ((m1.c*.m2.a)+.(m1.d*.m2.c));
   d= ((m1.c*.m2.b)+.(m1.d*.m2.d));
   e= ((m1.e*.m2.a)+.(m1.f*.m2.c)+.(m2.e));
   f= ((m1.e*.m2.b)+.(m1.f*.m2.d)+.(m2.f))}
;;

let matrixXtd x y m =
  {a= m.a;
   b= m.b;
   c= m.c;
   d= m.d;
   e= ((m.a*.x)+.(m.c*.y)+.(m.e));
   f= ((m.b*.x)+.(m.d*.y)+.(m.f))}
;;

let printMatrix m =
  print_float m.a;print_char ',';
    print_float m.b;print_char ',';
    print_float m.c;print_char ',';
    print_float m.d;print_char ',';
    print_float m.e;print_char ',';
    print_float m.f;print_char ',';
print_newline ();
;;
(** 
    @edited:  16-JUN-2009
    @author:  Josef Baker
    @input:   Font list, font name
    @effects: 
    @output:  List of chars from font
 *)
let rec getFont fonts font =
  if (font = ((List.hd fonts).Pdfextractor.fname)) then((List.hd fonts).Pdfextractor.chars)
  else getFont (List.tl fonts) font
;;

(** 
    @edited:  16-JUN-2009
    @author:  Josef Baker
    @input:   Font list, font name
    @effects: 
    @output:  Font family name
 *)
let rec getFontFamily fonts font =
  if (font = ((List.hd fonts).Pdfextractor.fname)) then((List.hd fonts).Pdfextractor.ffamily)
  else getFontFamily (List.tl fonts) font
;;
 


(** 
    @edited:  26-MAY-2010
    @author:  Josef Baker
    @input:   font list and ASCII code of a character
    @effects: 
    @output:  Character and attributes
 *)
let makeChar fonts code space=
(*print_string  (Str.string_after ((List.nth (getFont fonts !font) code).cname) 1);*)
  let tx = ((-.(space)) *. (!size) +. (!charSpace) +. (!wordSpace)) *. ((!scale)/.100./.1000.) in
    textMatrix := matrixX {a=1.0;b=0.0;c=0.0;d=1.0;e=tx;f=0.0} (!textMatrix);

    let tm = !textMatrix in
    let cm = !ctMatrix in
    let x = (tm.e +. cm.e) *. cm.a (* *. tm.a*) in
    let y = (tm.f +. cm.f) *. cm.d *. tm.d in
    (* let x = 0.8 in *)
    (* let y = 0.8 in *)
      
    let w0 = (List.nth (getFont fonts !font) code).Pdfextractor.cwidth in
  
      
    let tx = (((w0)*.(!size)) +. ((!charSpace)*.(!size)*.100.) +. (!wordSpace)) *. ((!scale)/.100./.1000.) in
      
      textMatrix := matrixX {a=1.0;b=0.0;c=0.0;d=1.0;e=tx;f=0.0} tm;
      
      let tm = !textMatrix in
      Chr {chname=( Str.string_after ((List.nth (getFont fonts !font) code).cname) 1);
	   chfont = getFontFamily fonts !font;
	   chsize = (!size);
	   chx = x;
	   chy = y;
	   (* chw = 0.8;} *)
	   chw = (((List.nth (getFont fonts !font) code).cwidth)*.(cm.a)*.(tm.a) *.(!size)*.((!scale)/.100./.1000.)) ;}
;;



(** 
    @edited:  26-APR-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec makeLineAux pointsList left right top bottom =
 match pointsList with
      x::y::t -> makeLineAux t (min left x) (max right x) (max top y) (min bottom y)
   | _ -> 
       let cm = !ctMatrix in
	 Ln {stx =((left+.(cm.e))*.(cm.a)); 
	     sty=((top+.(cm.f))*.(cm.d)); 
	     enx=((right+.(cm.e)+.left)*.(cm.a)); 
	     eny=((bottom+.(cm.f)+.top)*.(cm.d)); 
	     lnw=((!lineWidth)*.(cm.d))}

(** 
    @edited:  26-APR-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let makeLine pointsList=
  match pointsList with
      x1::y1::x2::y2::t -> makeLineAux t (min x1 x2) (max x1 x2) (max y1 y2) (min y1 y2)
;;


let octValue string =
  let len = String.length string in
    match len with
        3 -> ((int_of_string (Char.escaped string.[0])) * 64) + ((int_of_string (Char.escaped string.[1])) * 8) + ((int_of_string (Char.escaped (string.[2]))))
      | 2 -> ((int_of_string (Char.escaped string.[0])) * 8) + ((int_of_string (Char.escaped (string.[1]))))
      | 1 -> ((int_of_string (Char.escaped string.[0])))
;;

let decValue string =
(*print_endline ("*"^string^"*");*)
(int_of_string string)
;;

(** 
    @edited:  09-MAR-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let makeOctChar fonts space=

  if (Str.string_match (Str.regexp "[0-9][0-9][0-9]") (!contents) 0) then(
    let code = octValue (Str.matched_string (!contents)) in
      contents := Str.string_after (!contents) ((Str.match_end ()));
      makeChar  fonts code space;)
  else if (Str.string_match (Str.regexp "[0-9][0-9]") (!contents) 0) then(
    let code = octValue (Str.matched_string (!contents)) in
      contents := Str.string_after (!contents) ((Str.match_end ()));
      makeChar fonts code space;)
  else (
    let code = octValue (Str.matched_string (!contents)) in

      contents := Str.string_after (!contents) ((Str.match_end ()));
      makeChar fonts code space;)

(** 
    @edited:  09-MAR-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let makeDecChar fonts space=
  if (Str.string_match (Str.regexp "[0-9][0-9][0-9]") (!contents) 0) then(
    let code = decValue (Str.matched_string (!contents)) in
      contents := Str.string_after (!contents) ((Str.match_end ()));
      makeChar  fonts code space;)
  else if (Str.string_match (Str.regexp "[0-9][0-9]") (!contents) 0) then(
    let code = decValue (Str.matched_string (!contents)) in
      contents := Str.string_after (!contents) ((Str.match_end ()));
      makeChar fonts code space;)
  else (
    let code = decValue (Str.matched_string (!contents)) in

      contents := Str.string_after (!contents) ((Str.match_end ()));
      makeChar fonts code space;)


(** 
    @edited:  28-FEB-2012
    @author:  Josef Baker
    @input:   
    @effects: reduces size of REF contents, updates textMatrix
    @output:  
 *)
let rec parseStringAux fonts output space= 
(*print_endline ("2"^(!contents));*)
  let first = String.get (!contents) 0 in
 
  contents := (Str.string_after (!contents) 1);
    if first = ')' then  (
contents := (")"^(!contents));
output)
    else if first = '\\' then  ( 
      if (Str.string_match (Str.regexp "[0-9]") (!contents) 0) 
      then(
	parseStringAux fonts ((makeDecChar fonts space)::output) 0.)
      else(
	let second = String.get (!contents) 0 in
	  contents := (Str.string_after (!contents) 1);
	  match second with
	    |'n' -> (
		parseStringAux fonts ((makeChar fonts 10 space)::output) 0.0)
	    |'r' -> (
		parseStringAux fonts ((makeChar fonts 13 space)::output) 0.)
	    |'t' -> (
		parseStringAux fonts ((makeChar fonts 9 space)::output) 0.)
	    |'b' -> (
		parseStringAux fonts ((makeChar fonts 8 space)::output) 0.)
	    |'f' -> (
		parseStringAux fonts ((makeChar fonts 12 space)::output) 0.)
	    |'(' -> (
		parseStringAux fonts ((makeChar fonts 40 space)::output) 0.)
	    |')' -> (
		parseStringAux fonts ((makeChar fonts 41 space)::output) 0.)
	    |'\\'-> ( 
		if (Str.string_match (Str.regexp "[0-9]") (!contents) 0) 
		then(
		  parseStringAux fonts ((makeOctChar fonts space)::output) 0.)
		else(
		  let third = String.get (!contents) 0 in
		    contents := (Str.string_after (!contents) 1);
		    match third with
		      |'n' -> (
			  parseStringAux fonts ((makeChar fonts 10 space)::output) 0.0)
		      |'r' -> (
			  parseStringAux fonts ((makeChar fonts 13 space)::output) 0.)
		      |'t' -> (
			  parseStringAux fonts ((makeChar fonts 9 space)::output) 0.)
		      |'b' -> (
			  parseStringAux fonts ((makeChar fonts 8 space)::output) 0.)
		      |'f' -> (
			  parseStringAux fonts ((makeChar fonts 12 space)::output) 0.)
		      |'(' -> (
			  parseStringAux fonts ((makeChar fonts 40 space)::output) 0.)
		      |')' -> (
			  parseStringAux fonts ((makeChar fonts 41 space)::output) 0.)
		      |'\\'-> ( 
			  parseStringAux fonts ((makeChar fonts 92 space)::output) 0.)
		))
	    | _ ->
		parseStringAux fonts ((makeChar fonts (Char.code second) space)::output) space))



(*( parseStringAux fonts ((makeChar fonts (Char.code second) space)::output) space))*)
    else (
      
      parseStringAux fonts ((makeChar fonts (Char.code first) space)::output)) 0.
      
let rec parseString fonts output operator space =
 (* print_endline (!contents);*)
(*  print_char (String.get (!contents) 0 );*)
  if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]*") (!contents) 0) 
  then (
    let space = float_of_string (Str.matched_string (!contents)) in
      contents := Str.string_after (!contents) (Str.match_end ());
      parseString fonts output operator space;)
  else(
    let first = String.get (!contents) 0 in
      
      if (first = ']') || ((first = ')') & (operator != "TJ")) then (
	(*print_endline ("::"^operator);*)
	if (Str.string_match (Str.regexp ") *Tj") (!contents) 0) 
	then (contents := Str.string_after (!contents) (Str.match_end ());
	      output)
	else if (Str.string_match (Str.regexp ") *'") (!contents) 0) 
	then (contents := Str.string_after (!contents) (Str.match_end ());
	      output;)
	else if (Str.string_match (Str.regexp ") *\"") (!contents) 0) 
	then (contents := Str.string_after (!contents) (Str.match_end ());
	      output;)
	else if (Str.string_match (Str.regexp "] *TJ") (!contents) 0) 
	then (contents := Str.string_after (!contents) (Str.match_end ());
	      output;)
	else (
	  contents := (Str.string_after (!contents) 1);
	  parseString fonts output operator space;))
	
      
      else if first = '[' ||  first = ' ' then  (
	contents := (Str.string_after (!contents) 1);
	parseString fonts output operator space;)
	
      else if first = '(' then (
	contents := (Str.string_after (!contents) 1);
	parseString fonts (parseStringAux fonts output space) operator 0.)
      else parseString fonts (parseStringAux fonts output space) operator 0.)
    (*
      if (first = ']') && (Str.string_match (Str.regexp "] *TJ") (!contents) 0)
      then (contents := Str.string_after (!contents) (Str.match_end ());
	    output;)
      else if (first = ')')  && (Str.string_match (Str.regexp ") *Tj") (!contents) 0) 
      then (print_string "£££££££££££££££";
	contents := Str.string_after (!contents) (Str.match_end ());
	    output)
      else if (first = ')') && (operator = "'") && (Str.string_match (Str.regexp ") *'") (!contents) 0) 
      then (contents := Str.string_after (!contents) (Str.match_end ());
	    output)
      else if (first = ')') && (operator = "\"") && (Str.string_match (Str.regexp ") *\"") (!contents) 0) 
      then (contents := Str.string_after (!contents) (Str.match_end ());
	    output)
      else if (first = ')') && (operator = "Tj") && (Str.string_match (Str.regexp ") *Tj") (!contents) 0) 
      then (contents := Str.string_after (!contents) (Str.match_end ());
	    output)
      else  if (first = ']') 
      then (contents := (Str.string_after (!contents) 1);
	    parseString fonts output operator space;)
      else if first = '[' ||  first = ' ' 
      then (contents := (Str.string_after (!contents) 1);
	    parseString fonts output operator space;)
      else if first = '(' then (
	contents := (Str.string_after (!contents) 1);
	parseString fonts (parseStringAux fonts output space) operator 0.)
      else parseString fonts (parseStringAux fonts output space) operator 0.)*)
(** 
    @edited:  13-MAR-2012
    @author:  Josef Baker
    @input:   a string containing text showing operator
    @effects: 
    @output:  the operator as a string
 *)
let rec getShowOperator inString = 
(*print_string (Str.first_chars  inString 1);*)
  if (Str.string_match (Str.regexp ") *Tj") inString 0) then "Tj"
(* else if (Str.string_match (Str.regexp "\\") inString 0) then getShowOperator ((Str.string_after (inString) 3))*)
  else if (Str.string_match (Str.regexp ") *'") inString 0) then "'"
  else if (Str.string_match (Str.regexp ") *\"") inString 0) then "\""
  else getShowOperator ((Str.string_after (inString) 1))


   
let rec btReader fonts output =
 (* print_string "BT";*)
  if String.length (!contents) = 0 
  then output
  
      

  else if (Str.string_match (Str.regexp "ET") (!contents) 0)
  then (
contents := Str.string_after (!contents) ((Str.match_end ()));
	output)



  (*Text state operators*)
    
  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Tc") (!contents) 0) 
  then (match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
 	    tc::"Tc"::[] -> (charSpace := (float_of_string tc);
			     Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Tc") (!contents) 0;
			     contents := Str.string_after (!contents) (Str.match_end ());
			     btReader fonts output))
    
  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Tw") (!contents) 0) 
  then (match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    tw::"Tw"::[] -> (wordSpace := (float_of_string tw);
			     Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Tw") (!contents) 0;
			     contents := Str.string_after (!contents) (Str.match_end ());
			     btReader fonts output))

  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Tz") (!contents) 0) 
  then (match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    tz::"Tz"::[] -> (scale := (float_of_string tz);
			     Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Tz") (!contents) 0;
			     contents := Str.string_after (!contents) (Str.match_end ());
			     btReader fonts output))

  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +TL") (!contents) 0) 
  then (match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    tl::"TL"::[] -> (leading := (float_of_string tl);
			     Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +TL") (!contents) 0;
			     contents := Str.string_after (!contents) (Str.match_end ());
			     btReader fonts output))

  else if (Str.string_match (Str.regexp "/[A-Z][0-9]+ +[0-9]+\\.?[0-9]* +Tf") (!contents) 0) 
  then ((*print_string " Tf  ";*)
	match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    f::s::"Tf"::[] -> (font := (Str.string_after f 1);
			       size := (float_of_string s);
			       Str.string_match (Str.regexp "/[A-Z][0-9]+ +[0-9]+\\.?[0-9]* +Tf") (!contents) 0;
			       contents := Str.string_after (!contents) (Str.match_end ());
			       btReader fonts output))

  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Tr") (!contents) 0) 
  then (match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    tr::"Tr"::[] -> (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Tr") (!contents) 0;
			     contents := Str.string_after (!contents) (Str.match_end ());
			     btReader fonts output))

  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Ts") (!contents) 0) 
  then (match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    ts::"Ts"::[] -> (rise := (float_of_string ts);
			     Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +Ts") (!contents) 0;
			     contents := Str.string_after (!contents) (Str.match_end ());
			     btReader fonts output))

 (*Text positioning operators*)
  
  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +Td") (!contents) 0) 
  then ((*print_string "Td ";*)
	match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    tx::ty::"Td"::[] -> (

				 let transMatrix = {a=1.0;b=0.0;c=0.0;d=1.0;
						    e=(float_of_string tx);
						    f=(float_of_string ty)} in
				
				   lineMatrix := matrixX transMatrix (!lineMatrix);
				   textMatrix := (!lineMatrix);

				   Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +Td") (!contents) 0;
				   contents := Str.string_after (!contents) (Str.match_end ());
				   btReader fonts output))

  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +TD") (!contents) 0) 
  then ((*print_string "TD ";*)
	match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    tx::ty::"TD"::[] -> (leading := (-.(float_of_string ty));
				let transMatrix = {a=1.0;b=0.0;c=0.0;d=1.0;
						    e=(float_of_string tx);
						    f=(float_of_string ty)} in
				   lineMatrix := matrixX transMatrix (!lineMatrix);
				   textMatrix := (!lineMatrix);
				   Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +TD") (!contents) 0;
				 contents := Str.string_after (!contents) (Str.match_end ());
				 btReader fonts output))
    
  else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +Tm")
	     (!contents) 0)  
  then ((*print_string "Tm ";*)
	match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    ta::tb::tc::td::te::tf::"Tm"::[] -> 
	      (textMatrix := {a = (float_of_string ta);
			      b = (float_of_string tb);
			      c = (float_of_string tc);
			      d = (float_of_string td);
			      e = (float_of_string te);
			      f = (float_of_string tf)};
	       lineMatrix := {a = (float_of_string ta);
			      b = (float_of_string tb);
			      c = (float_of_string tc);
			      d = (float_of_string td);
			      e = (float_of_string te);
			      f = (float_of_string tf)};
	       Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +Tm")
	     (!contents) 0;
	       contents := (Str.string_after (!contents) (Str.match_end ()));
	       btReader fonts output))

     else if (Str.string_match (Str.regexp "T\\*") (!contents) 0)
     then(let transMatrix = {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;
			     f=(!leading)} in
	    lineMatrix := matrixX transMatrix (!lineMatrix);
	    textMatrix := (!lineMatrix);
	    Str.string_match (Str.regexp "T\\*") (!contents) 0;
	    contents := Str.string_after (!contents) (Str.match_end ());
	    btReader fonts output)
       
     (*Text showing operators regexp won't work here for some, so check
     characters one by one*)

     else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* ") (!contents) 0) 
     then  (match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
		tw::tc::[] -> (wordSpace := (float_of_string tw);
			       charSpace := (float_of_string tc);
			       Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* ") (!contents) 0;
			       contents := Str.string_after (!contents) (Str.match_end ());
			       let transMatrix = {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;
						  f=(!leading)} in
				 lineMatrix := matrixX transMatrix (!lineMatrix);
				 textMatrix := (!lineMatrix);
				 btReader fonts output;))
       
     else if (String.get (!contents) 0) = '('
     then ((*print_string "Tj?";*)
       btReader fonts ( parseString fonts output (getShowOperator (!contents)) 0. ))
	 
     else if (String.get (!contents) 0) = '['
     then  ((* print_string "TJ ";*)
       let op = parseString fonts output "TJ" 0. in
	 btReader fonts op)
  
     else (
       contents := (Str.string_after (!contents) 1);
       btReader fonts output)

;;

let rec streamReader fonts output graphicStack linePoints=
(*print_string "!";*)
  (*Empty stream*)
  if String.length (!contents) = 0 
  then (output)
    
  (*Text object*)
  else if (Str.string_match (Str.regexp "BT") (!contents) 0)
  then (
    contents := Str.string_after (!contents) (Str.match_end ());
    (*btReader fonts output;*)
    lineMatrix := identity;
    textMatrix := identity;
    streamReader fonts (btReader fonts (output)) graphicStack linePoints
  )

 else if (Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +cm")
	     (!contents) 0)  
  then (match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	    ta::tb::tc::td::te::tf::"cm"::[] -> 
	      (let tempMatrix = {a = (float_of_string ta);
			      b = (float_of_string tb);
			      c = (float_of_string tc);
			      d = (float_of_string td);
			      e = (float_of_string te);
			      f = (float_of_string tf)} in 
	       ctMatrix := (matrixX tempMatrix !ctMatrix );
	       Str.string_match (Str.regexp "-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +-?[0-9]+\\.?[0-9]* +cm")
	     (!contents) 0;
	       contents := (Str.string_after (!contents) (Str.match_end ()));
	       streamReader fonts output graphicStack linePoints))

  (*Graphic state operators*)
  else if (Str.string_match (Str.regexp "q") (!contents) 0)
  then (
    contents := Str.string_after (!contents) (Str.match_end ());
    let currentctMatrix = !ctMatrix in
    streamReader fonts output (currentctMatrix::graphicStack) linePoints
  )
  else if (Str.string_match (Str.regexp "Q") (!contents) 0)
  then (
    contents := Str.string_after (!contents) (Str.match_end ());
    ctMatrix := List.hd (graphicStack);
    streamReader fonts output (List.tl graphicStack) linePoints
  )

  (*Line drawing operators*)
  else if (Str.string_match (Str.regexp "[0-9]+\\.?[0-9]* +w") (!contents) 0) 
  then ( 
    match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	width::"w"::[] -> (
	  lineWidth := (float_of_string width);
	  Str.string_match (Str.regexp "[0-9]+\\.?[0-9]* +w") (!contents) 0);
	  contents := Str.string_after (!contents) (Str.match_end ());
	  streamReader fonts output graphicStack linePoints
  )
  else if (Str.string_match (Str.regexp "[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +m") (!contents) 0) 
  then ( 
    match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	x::y::"m"::[] -> (
	  Str.string_match (Str.regexp "[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +m") (!contents) 0);
	  contents := Str.string_after (!contents) (Str.match_end ());
	  streamReader fonts output graphicStack ((float_of_string y)::(float_of_string x)::[])
  )
  else if (Str.string_match (Str.regexp "[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +l") (!contents) 0) 
  then ( 
    match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	x::y::"l"::[] -> (
	  Str.string_match (Str.regexp "[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +l") (!contents) 0);
	  contents := Str.string_after (!contents) (Str.match_end ());
	  streamReader fonts output graphicStack ((float_of_string y)::(float_of_string x)::linePoints)
  )
  else if (Str.string_match (Str.regexp "[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +re") (!contents) 0) 
  then ( 
    match (Str.split (Str.regexp " +") (Str.matched_string (!contents))) with 
	x::y::w::h::"re"::[] -> (
	  Str.string_match (Str.regexp "[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +[0-9]+\\.?[0-9]* +re") (!contents) 0);
	  contents := Str.string_after (!contents) (Str.match_end ());
	  let fx = float_of_string x in
	  let fy = float_of_string y in
	  let fw = float_of_string w in
	  let fh = float_of_string h in
	  streamReader fonts output graphicStack ((fy+.fh)::(fx+.fw)::fy::fx::[])
  )
  else if (Str.string_match (Str.regexp "h") (!contents) 0)
  then (
    contents := Str.string_after (!contents) (Str.match_end ());
    streamReader fonts output graphicStack linePoints
  )

(*Line painting operators*)
  else if (Str.string_match (Str.regexp "S\\|B\\|b") (!contents) 0)
  then (
    contents := Str.string_after (!contents) (Str.match_end ());
    if (List.length linePoints) > 3 then
      streamReader fonts ((makeLine (List.rev linePoints))::output) graphicStack linePoints
    else
      streamReader fonts output graphicStack linePoints
  )
 
  (*Anything else*)
  else (
    contents := (Str.string_after (!contents) 1);
    streamReader fonts output graphicStack linePoints)
;;

let rec printElems elems =
  match elems with
      h::t -> (
	match h with
	    Chr chr -> (print_string chr.chname; 
			print_string ",";
			print_float chr.chx;
			print_string ",";
			print_float chr.chy;
			print_string ":";
			(*print_float chr.chw;
			print_string "\n";*)
			printElems t;)
	  | Ln ln   -> (print_string "line";
			print_string ",";
			print_float ln.stx;
			print_string ",";
			print_float ln.sty;
			print_string ":";
			(*print_float ln.enx;
			print_string ",";
			print_float ln.eny;
			print_string ",";
			print_float ln.lnw;
			print_string "\n";*)

			printElems t;)
      )
    | [] -> print_string ("\n\n");
;;
let rec parse pageList elemLists test =



verbose := false;
  match pageList with
      h::t -> (
(*print_string "!£!";*)
contents := "";
textStr := "";

(*Text state*)
charSpace :=  0.0;
wordSpace :=  0.0;
scale :=  100.0;
leading :=  0.0;
font :=  "";
size :=  0.0;
rise :=  0.0;
textMatrix :=  {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;f=0.0};
lineMatrix :=  {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;f=0.0};
ctMatrix := {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;f=0.0};


contents := h.Pdfextractor.contents;

(*print_string (!contents);*)

	       let elems = (List.rev ( streamReader h.Pdfextractor.fonts [] [] [])) in
		 if (!verbose) then printElems elems;
		 parse t (elems::elemLists) test;
	      )
    | _ -> (elemLists)
;;
(*
  let m1 = {a=1.0;b=0.0;c=0.0;d=1.0;e=148.712;f=707.125} in
  let m2 = {a=1.0;b=0.0;c=0.0;d=1.0;e=0.0;f=0.0} in
  let m3 = matrixX m1 m2 in
    print_float m3.a;print_char ',';
    print_float m3.b;print_char ',';
    print_float m3.c;print_char ',';
    print_float m3.d;print_char ',';
    print_float m3.e;print_char ',';
    print_float m3.f;print_char ',';
;;

*)
