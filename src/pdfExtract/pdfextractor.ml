open Str;;


let verbose = ref false

let tempObj = ref ""

type char = {cname: string ; cwidth: float};;
type font = {fname: string ; chars: char list ; ffamily:string};;
type mediabox = {mx:float; my:float; mwidth:float; mheight:float}
type page = {dimensions: mediabox; fonts: font list; contents: string}



let rec p l =
  match l with
      h::t ->(print_string h; 
	      print_string " \n";
	      p t)
    | []-> print_newline ()
;;

let rec pf l i=
  match l with
      h::t ->(print_string ((string_of_int i)^" "^h.cname^" "^(string_of_float h.cwidth)^": "); 
	      pf t (i+1))
    | []-> print_newline ()
;;

let rec pfs l =
  match l with
      h::t ->(print_string (h.fname^":: ");
	      pf h.chars 0;
	      pfs t)
    | []-> print_newline ()
;;


let trim str =   if str = "" then "" else   let search_pos init p next =
    let rec search i =
      if p i then raise(Failure "empty") else
      match str.[i] with
      | ' ' | '\n' | '\r' | '\t' -> search (next i)
      | _ -> i
    in
    search init   in   let len = String.length str in   try
    let left = search_pos 0 (fun i -> i >= len) (succ)
    and right = search_pos (len - 1) (fun i -> i < 0) (pred)
    in
    String.sub str left (right - left + 1)   with   | Failure "empty" -> "" ;;

(** 
    @edited:  03-JUN-2009
    @author:  Josef Baker
    @input:   Input channel object name
    @effects: Sets channel to line of the object
    @output:  
 *)
let findObj obj chan =
  if (!verbose) then print_string ("Searching for "^obj^"\n");  
  seek_in chan 0;
  let name = ref (regexp obj) in
    
    if (obj <> "trailer") then name := regexp (obj^" obj");

    tempObj := "";

    while (string_match (!name) (!tempObj) 0) = false
    do ( tempObj:= (input_line chan)) done;;   

(** 
    @edited:  07-FEB-2012
    @author:  Josef Baker
    @input:   Input channel, object name
    @effects: Sets input channel to end of object
    @output:  String containing object
 *)
let getObj obj chan =
   
  findObj obj chan;
 if (!verbose) then print_string ("Getting  "^obj^"\n");
  let objStr = ref "" in

  let objCount = ref 0 in
objStr := (!tempObj);

objStr := global_replace (regexp "[0-9]* [0-9]* obj") "" (!objStr);
    
    if (obj = "trailer") then(
     let endObj = regexp_string "%%EOF" in
      let line = ref (input_line chan) in
      try 
	while ((string_match endObj !line 0) = false)
	do 
	  objStr := (!objStr^" "^(!line));
	    line := (input_line chan);
	done;
	!objStr
      with End_of_file -> !objStr
    )
    else(
      let endObj = regexp "\\(>> \\)*endobj" in
      let line = ref (input_line chan) in
	while (((string_match endObj !line 0) = false) && ((!objCount < 1000)))
	do 
	  objCount := !objCount+1;
	  objStr := (!objStr^" "^(!line));
	  line := (input_line chan);
	done;
	Str.global_replace (regexp "\\\\r") " " (String.escaped (!objStr))
    );;

(** 
    @edited:  09-FEB-2012
    @author:  Josef Baker
    @input:   An object string
    @effects: 
    @output:  A string with the first array encountered
 *)
let getArray obj =
  let arrayStr  = ref "" in
  let count = ref 1 in
    while (obj.[!count] <> ']')
    do
      arrayStr := (!arrayStr^(Char.escaped obj.[!count]));
      count := (!count +1);
    done;
    !arrayStr;;

(** 
    @edited:  09-FEB-2012
    @author:  Josef Baker
    @input:   An object string
    @effects: 
    @output:  A string with the first dictionary encountered
 *)
let getDic obj =
  let dicStr  = ref "" in
  let dicCount = ref 1 in
  let count = ref 1 in
    while (!dicCount > 0)
    do
      if (obj.[!count] = '<' ) && (obj.[!count+1] = '<' ) then dicCount := (!dicCount+1);
      if (obj.[!count] = '>' ) && (obj.[!count+1] = '>' ) then dicCount := (!dicCount-1);
      if (!dicCount > 0) then  dicStr := (!dicStr^(Char.escaped obj.[!count]));
      count := (!count +1);
    done;

    !dicStr;;

(** 
    @edited:  09-FEB-2012
    @author:  Josef Baker
    @input:   Object as string, key value, input channel
    @effects: 
    @output:  The value of the key, as a string
 *)
let getKeyValue obj key chan =

  if (!verbose) then print_string ("Getting  "^key^"\n");
  try(  
    Str.search_forward (regexp key) obj 0 ;
    let obj = trim (Str.string_after obj (Str.match_end ())) in
      if obj.[0] = '[' then getArray obj
      else if ((obj.[0] = '<') && (obj.[1] = '<')) then getDic (Str.string_after obj 2)
      else if  (Str.string_match (regexp "[0-9]+ [0-9]+ R") obj 0 ) then getObj
	(Str.string_before (Str.matched_string obj) ((String.length
							(Str.matched_string obj))-2)) chan
      else if obj.[0] = '/' then List.hd (Str.split (regexp "[ \t>]+\\|/") (Str.string_after obj 1))
      else List.hd (Str.split (regexp "[ \t]+\\|>\\|/") obj)
  )
  with
      Not_found -> "Not_found";;



(** Gets entire object with respect to a key value.
    @edited:  26-JUN-2012
    @author:  Volker Sorge, Josef Baker
    @input:   Object as string, key value, input channel.
    @effects: 
    @output:  The remainder of the object as a string.
 *)
let getObjRemainder obj key chan = 
  if (!verbose) then print_string ("Getting  "^key^"\n");
  try
    ignore(Str.search_forward (regexp key) obj 0);
    let endObj = regexp "\\(>> \\)*endobj" in
    let line = ref (input_line chan) in
    let objStr = ref "" in
      while ((string_match endObj !line 0) = false)
      do 
	objStr := (!objStr^" "^(!line));
	line := (input_line chan);
      done;
      Str.global_replace (regexp "\\\\r") " " (String.escaped (!objStr))
  with 
      Not_found -> "Not_found";;

let getObjRemainder2 obj key = 
  Str.string_after obj (Str.search_forward (regexp key) obj 0)



(** 
    @edited:  09-FEB-2012
    @author:  Josef Baker
    @input:   Object as string, key value, input channel
    @effects: 
    @output:  The value of the key, as a string if it exists (To be used for
    inheritable attributes)
 *)
let rec getOptionalKeyValue obj key chan =
  let keyValue = getKeyValue obj key chan in
    match keyValue with
	"Not Found" -> (
	  let parent = getKeyValue obj "Parent" chan in
	    match parent with
		"Not Found" -> parent
	      | _ -> getOptionalKeyValue parent key chan)
      | _ -> keyValue;;

(** 
    @edited:  19-FEB-2012
    @author:  Josef Baker
    @input:   input channel, a list of page nodes and an initially empty list of
    page objects
    @effects: 
    @output:  a string list of page objects in order
 *)
let rec getPages chan pageTree pageList =
  match pageTree with
      num::rev::"R"::tail -> (
	let pageNode = getObj (num^" "^rev) chan in
	  if (getKeyValue pageNode "Type" chan) = "Pages" then(
	    let pageSubTree = Str.split (regexp "[ \t]+") (getKeyValue pageNode "Kids" chan) in
	    getPages chan (pageSubTree@tail) pageList)
	  else(
	    getPages chan tail (pageNode::pageList))
      )
    | _ -> pageList;;

(** 
    @edited:  15-NOV-2010
    @author:  Josef Baker
    @input:   input channel of a pdf file
    @effects: 
    @output:  string list of pages
 *)
let getPageTree chan test =
  verbose := test;
  let trailer = getObj "trailer" chan in
  let root = getKeyValue trailer "Root" chan in
  let catalog = getKeyValue root "Pages" chan in
  let pageTree = Str.split (regexp "[ \t]+") (getKeyValue catalog "Kids" chan) in
    List.rev (getPages chan pageTree []);;    

(*
let stripFront stream =
;;
let stripBack stream =
;;
let stripBoth stream =
;;
*)

let rec fixContentsAux list stream chan=
  match list with
    | obj::"0"::"R"::t -> fixContentsAux t (stream^" "^(getObj (obj^" 0") chan)) chan
    | _ ->stream

(** 
    @edited:  14-NOV-2012
    @author:  Josef Baker
    @input:   Content stream
    @effects: 
    @output:  If content stream is array, then catenate the targets, else return stream
 *)
let fixContents stream chan =
  let contents = (Str.split (regexp " /\\|[ \t]+\\|/\\|>>") stream) in
    match contents with
      | obj::"0"::"R"::t -> fixContentsAux contents "" chan
      | _ -> stream
;;


(** 
    @edited:  21-FEB-2012
    @author:  Josef Baker
    @input:   pdf array as string
    @effects: 
    @output:  a list containing each element of the array without delimiters
 *)
let makeListOfArray array =
  let array = Str.global_replace (regexp "/") " /" array in
  let array = trim array in
    if (array.[0] = '[') && (array.[(String.length array)-1] = ']') then (Str.split (regexp "[ \t]+") (String.sub array 1 ((String.length array)-2)))
    else if (array.[0] = '[') then (Str.split (regexp "[ \t]+") (String.sub array 1 ((String.length array)-1)))
    else if (array.[(String.length array)-1] = ']') then (Str.split (regexp "[ \t]+") (String.sub array 0 ((String.length array)-1)))
    else (Str.split (regexp "[ \t]+") array)
;;

(** 
    @edited:  21-FEB-2012
    @author:  Josef Baker
    @input:   encoding list and initially empty list
    @effects: 
    @output:  encoding list with missing blanks inserted
 *)
let rec fixEncoding encoding fixed count =

  match encoding with
      h::t -> ((*print_string h; print_string " ";*)
	if h.[0] = '/' then fixEncoding t (h::fixed) (count+1)
	else if  (List.length fixed) < (int_of_string h)  then
	  fixEncoding encoding ((Ascii.getAsciiChar ((List.length fixed)))::fixed) (count+1)
	else fixEncoding t fixed (count+1))
    | _ -> (if (List.length fixed) < 256 then fixEncoding encoding
      ((Ascii.getAsciiChar ((List.length fixed)))::fixed) (count+1)
	      else fixed)
;;

let fixEncoding encoding fixed =
  fixEncoding encoding fixed 0
;;

let rec findDup dups pos  =
 match dups with
     num::char::t when pos = (int_of_string num)  -> char
   | num::char::t ->findDup t pos
   | _ -> ".notdef"
;;

let rec makeDupEncoding dups encoding =
  let length = List.length encoding in
    match length with
	256 -> encoding
      | _ -> makeDupEncoding dups ((findDup dups length)::encoding)
;;



(** 
    @edited:  21-FEB-2012
    @author:  Josef Baker
    @input:   widths and initially empty list
    @effects: 
    @output:  widths list with missing blanks inserted
 *)
let rec fixWidths firstChar widths fixed =
  if  (List.length fixed) < firstChar  then fixWidths firstChar widths ("0"::fixed)
  else (
    match widths with
	h::t ->(fixWidths firstChar t (h::fixed))
      | _ -> (if (List.length fixed) < 256 then fixWidths firstChar widths ("0"::fixed)
	      else fixed))
;;

(** 
    @edited:  21-FEB-2012
    @author:  Josef Baker
    @input:   differences list, widths list empty list
    @effects: 
    @output:  list of chars
 *)
let rec makeEncoding  differences widths chars =
  match differences with
      h::t -> makeEncoding t (List.tl widths) ({cname =h;cwidth=(float_of_string (List.hd widths))}::chars)
    | _ -> chars

;;
    


(** Get dup values from a string.
    @edited:  26-JUN-2012
    @author:  Volker Sorge
    @input:   A string.
    @effects: None.
    @output:  A pair list of strings associating a number to a character name.
 *)
let getDup str = 
    match (SPLIT "dup") str with
      | [] -> []
      | _::dups -> List.flatten (List.map (function | RE blank* (digit+ as num) blank* ("/"alpha* as name) _* -> [num;name]) dups);;



(** 
    @edited:  20-FEB-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec getFonts chan fontList fonts =
  match fontList with
      name::num::rev::"R"::tail -> (
	let font = getObj (num^" "^rev) chan in
	  if (!verbose) then print_string font;
	  let baseFont = getKeyValue font "BaseFont" chan in
	  let baseFont = Str.global_replace (Str.regexp ".*\\+") "" baseFont in
	    if (!verbose) then print_string ("Basefont: "^baseFont^"\n");
	    let firstChar = getKeyValue font "FirstChar" chan in
	    let lastChar = getKeyValue font "LastChar" chan in
	    let widths = getKeyValue font "Widths" chan in
	    let font_descr = getKeyValue font "FontDescriptor" chan in
	      if (!verbose) then print_string ("FirstChar: ."^firstChar^". LastChar: ."^lastChar^". Widths: "^widths^"\n");
	      let encoding = getKeyValue font "Encoding" chan in
		if (!verbose) then print_string ("Encoding: "^encoding^"\n");
		let differences = getKeyValue encoding "Differences" chan in
		  if (!verbose) then print_string ("Differences: "^differences^"\n");
		  let diffs = ref [] in		  
		    if differences = "Not_found" then 
		      let font_file = getKeyValue font_descr "FontFile" chan in 
		      let font_encoding = getObjRemainder2 font_file "/Encoding" in 
		      let dups = getDup font_encoding in
			if (!verbose) then print_endline "Dups Start: ";
			if (!verbose) then print_string "Font File: ";
			if (!verbose) then print_endline font_file;
			if (!verbose) then print_string "Font Encoding: ";
			if (!verbose) then print_endline font_encoding;
			if (!verbose) then print_string "Dups: ";
			if (!verbose) then print_endline (String.concat " " dups);
    			if dups <> [] then 
			  diffs := (makeDupEncoding dups [])
			else
			  diffs := Ascii.ascii
		    else diffs := (fixEncoding (makeListOfArray differences) []);
		    let charList = makeEncoding (!diffs)  (fixWidths (int_of_string firstChar) (makeListOfArray widths) []) [] in
		      (*print_string name; print_newline ();		     
			pf charList;*)
		      getFonts chan tail ({fname=name;chars=charList;ffamily=baseFont}::fonts))
    | h::t -> getFonts chan t fonts
    | _ -> fonts
;;

(** 
    @edited:  19-FEB-2012
    @author:  Josef Baker
    @input:   input channel of pdf file, page tree, initially empty page list
    @effects: 
    @output:  page list
*)
let rec extractPDF chan pageTree pageList =

  match pageTree with
      h::t -> (
	try(
	let mediaBox = getOptionalKeyValue h "MediaBox" chan in
	  if (!verbose) then print_string ("MediaBox:  "^mediaBox^"\n");
	  let contentStream = getOptionalKeyValue h "Contents" chan in
	  let contentStream = fixContents contentStream chan in

	    if (!verbose) then print_string ("Contents:  ..."^contentStream^"\n");
	    let resources = getOptionalKeyValue h "Resources" chan in
	      
	      if (!verbose) then print_string ("Resources:  "^resources^"\n");
	    (*  print_string (getOptionalKeyValue resources "Font" chan);*)
	      let fontList = getFonts chan (Str.split (regexp " /\\|[ \t]+\\|/\\|>>") (getOptionalKeyValue resources "Font" chan)) [] in
		(*	let fontList = getFonts chan (Str.split (regexp "[ \t]+\\|/") (getOptionalKeyValue resources "Font" chan)) [] in*)
		if (!verbose) then (print_string ("Fonts:  ");
				    pfs fontList ;
				    print_newline (););
		match (makeListOfArray mediaBox) with
		    x::y::w::h::[] ->(
		      let mBox = {mx=(float_of_string x);
				  my=(float_of_string y);
				  mwidth=(float_of_string w);
				  mheight=(float_of_string h)} in

			extractPDF chan  t ({dimensions=mBox;fonts=fontList; contents=(contentStream)}::pageList)
		    ))
	with error -> (extractPDF chan  t ({dimensions={mx=(-1.); my=(-1.);mwidth=(-1.); mheight=(-1.)}; fonts=[]; contents=""}::pageList))
      )
    | _ -> (pageList)
;;
