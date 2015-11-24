open Unix;;


let pdf = ref ""
let name = ref ""
let test = ref false
let uncomp = ref false
let print = ref false
let directory = ref ""
let jsondir = ref ""



(** 
    @edited:  22-FEB-2012
    @author:  Josef Baker
    @input:   input PDF file
    @effects: creates random temp directory in /tmp with PDF, uncompressed if
    not already also sets name as file prefix
    @output:  dir name of new file
 *)
let prepFile file directory=
  name := Filename.basename file;

  if (Filename.check_suffix (!name) ".pdf")
  then  name := Filename.chop_extension (!name);
  
  if (!jsondir) ="" then (
    system ("mkdir "^directory); 
    let dir = directory in
      if (!uncomp = false)then( 
(*	print_string ("pdftk "^(!pdf)^" output "^dir^"/"^(!name)^".pdf uncompress");*)
	system ("pdftk "^(!pdf)^" output "^dir^"/"^(!name)^".pdf uncompress"))
      else(
	system ("cp "^(!pdf)^" "^dir^"/"^(!name)^".pdf"));
      (dir^"/")
  )
  else (!jsondir)
;;


(** 
    @edited:  06-MAR-2011
    @author:  Josef Baker
    @input:   an integer
    @effects: 
    @output:  string representation of integer , 3 digits long
 *)
let stringInt i =
  if i < 10 then ("00"^(string_of_int i))
  else if i < 100 then ("0"^(string_of_int i))
  else (string_of_int i)


(** 
    @edited:  22-FEB-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let rec makeJson dir count =
  if count < 0 then ()
  else (let subdir = (dir^(stringInt count)) in
(*	  system ("./connectedcomp_ci.opt-STATIC -d "^(stringInt count)^" "^dir^(!name)^".tif");*)
	  system ("./ccl "^dir^(!name)^".tif "^(string_of_int count));
	  system ("mkdir "^subdir);
	  system ("mv "^dir^"*.json "^subdir);
	makeJson dir (count-1))
;;

(** 
    @edited:  06-MAR-2011
    @author:  Josef Baker
    @input:   list of lines, list of pdf chars, empty list
    @effects: 
    @output:  a list of lines of matched glyphs and pdf chars
 *)
let rec matchLines lines chars matchedLines =
  match lines with
     h::t -> (let matched = Matcher.makeSymbols h chars in
		if matched = [] then matchLines t chars matchedLines
		else  matchLines t (Matcher.removeDupChars chars matched [])
		   ((Matcher.convert matched [])::matchedLines))
    | _ -> ((*Match.printChars chars;*)

List.rev matchedLines)
;;

(*
let rec matchLines lines chars matchedLines =
  match lines with
     h::t -> (let matched = Match.matcher chars h in
		if matched = [] then matchLines t chars matchedLines
		else  matchLines t (Match.removeDupChars chars matched [])
		   ((Match.convert matched [])::matchedLines))
    | _ -> ((*Match.printChars chars;*)

List.rev matchedLines)
;;
*)

(** 
    @edited:  06-MAR-2011
    @author:  Josef Baker
    @input:   json clip, matched chars,  pdf file name, page number, directory
    to be saved in
    @effects: creates jsonf file
    @output:  none
 *)
let rec saveClips clip matched count dir=
  match matched with
      h::t -> ( 
 (*  print_string dir;
print_newline;*)
SaveCharClip.saveNewFClip clip h (!name) (dir^(stringInt count)^".jsonf");
		saveClips clip t (count+1) dir;)
    | _ -> ((*print_string "error";*))
;;

let rec matchPage dir count pageHd elemHd=
  try (
    let jsonfile = (dir^"/"^(stringInt count)^".json") in
print_endline jsonfile;

    let clip = LoadClip.getClip jsonfile in
    let aligned = Align.alignElems clip pageHd elemHd in
    let glyphs = Align.convertGlyphs clip.LoadClip.glyphs [] in
    let matched = Matcher.makeSymbols glyphs aligned in

      SaveCharClip.saveNewFClip clip (Matcher.convert matched []) (!name) (dir^"/"^(stringInt count)^".jsonf");
      matchPage dir (count+1) pageHd (Matcher.removeDupChars elemHd matched [])
  )
  with error -> ()
;;


let rec matchPages dir count pageList elementList=
  match pageList,elementList with
      pageHd::pageTl,[]::elemTl -> (
	matchPages dir (count+1) pageTl elemTl
      )
    | pageHd::pageTl,elemHd::elemTl -> (
	matchPage (dir^(stringInt count)) 0 pageHd elemHd);

	matchPages dir (count+1) pageTl elemTl
    | _,_ -> () 
;;

let rec extractPages dir count pageList elementList=
  match pageList,elementList with
      pageHd::pageTl,[]::elemTl -> (
	extractPages dir (count+1) pageTl elemTl
      )
  |      pageHd::pageTl,elemHd::elemTl -> (
(*print_endline ("Page");*)
	     let jsondir = (dir^(stringInt count)^"/"^(!name)^"-"^(string_of_int count)^".json") in
	     let clip = LoadClip.getClip jsondir in
	     let aligned = Align.alignElems clip pageHd elemHd in
	     let glyphs = Align.convertGlyphs clip.LoadClip.glyphs [] in
	       (*   let matched = Matcher.makeSymbols glyphs aligned in
		    ()
	       *)
	     let lines = LineFinder.findLines glyphs in
	       (*  print_string "*";	
		   print_int (List.length lines);
		   print_string "*";
		   print_newline ();*)
	     let matched = matchLines lines aligned [] in
	       
	       (*print_string "$$";
		 print_int  (List.length matched);
		 print_string "$$";
		 print_newline ();*)
	       (*		
				
				print_string (jsondir);
				print_newline ();
				print_string (String.sub pageHd.Pdfextractor.contents 0 200);
				print_newline ();
				Contentparser.printElems ((List.hd  elemHd)::[]);
				print_newline ();
				print_newline ();
	       *)
	       
 	       saveClips clip matched 0 (dir^(stringInt count)^"/");
	       
	       extractPages dir (count+1) pageTl elemTl 
	   )
	     
    | _,_ -> () 
;;


let extractFile inFile inDirectory=
  
  let dir = ref "" in
  let file = ref "" in
    
 (*   try( *)
      
      (*print_string inDirectory;
	print_newline ();*)
      dir := prepFile inFile inDirectory;
      file := (!dir)^(!name)^".pdf";
      (*
	print_string ((!dir)^" "^(!file));
	print_newline ();
      *)
    
      let inCh = open_in_bin (!file) in
      let pageTree = Pdfextractor.getPageTree inCh !test in	  
      let pageList = Pdfextractor.extractPDF inCh pageTree [] in
(*	print_string "!";*)
	close_in inCh;
	let elements = Contentparser.parse pageList [] !test in
	  
	  if (List.length (List.flatten elements)) >50
	  then(
	    
	    if (!jsondir) = "" then(	    
(* pre mo	      system ("./pdf2tiff "^(!file));
	      makeJson !dir ((List.length pageTree)-1););
*)
	      system ("./extractLines.opt -f "^(!file)^" -o "^(!dir));
();
);
	    
	    (*      print_int (List.length pageTree);*)
	    
	    (*print_string ("elems: "^(string_of_int (List.length pageList))^" pages "^(string_of_int (List.length elements))^"\n");
	    *)
	    
	    matchPages !dir 0 (List.rev pageList) elements;
	    
	    ())
	  else ()
  (*  )
    with error -> (print_endline "0";(*system ("rm -fR "^(!dir));*) ();)
  *)  
;;



let extractElements () =

  (* let pdf = ref "" in *)
  (* let name = ref "" in *)
  (* let test = ref false in *)
  (* let uncomp = ref false in *)
  (* let print = ref false in *)
  (* let directory = ref "" in *)
  (* let jsondir = ref "" in *)

  let usage = "usage: " ^ Sys.argv.(0) ^ " [-f file] [-t] [-u] [-p] [-d dir] [-j dir]" in
  let speclist = [
    ("-d", Arg.Set_string directory, ": -d Name of directory");
    ("-f", Arg.Set_string pdf, ": -f Name of PDF file");
    ("-u", Arg.Set uncomp, ": -u If file is uncompressed");
    ("-t", Arg.Set test, ": -t Set verbose mode on");
    ("-p", Arg.Set print, ": -p Print output to sdout");
    ("-j", Arg.Set_string jsondir, ": -j Name of json directory");
  ]
  in
    (* Read the arguments *)
    Arg.parse
      speclist
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      usage;
    ignore(extractFile (!pdf) (!directory))
    
    
let _ = extractElements ()
