open Jsonfio;;
open Preprocessor;;
open Linearize;;

let indir = ref "" 
let infile = ref ""  
let extension = ref "lin" 
let bbox = ref true
let sdout = ref false
let rest = ref false

let pages = ref 0
let lines = ref 0

  
let rec fileInt count = Printf.sprintf "%03d.jsonf" count

let rec dirInt count =  Printf.sprintf "%03d/" count

let linearizeFile file =

    let symbols = JsonfIO.getSymbols file in
    let synts =  Preprocessor.preprocess symbols  in


    let outBB = ((String.sub file 0 ((String.length file)-5))^"bb") in
    let outBBCh = open_out outBB in
    let outStr = ((Linearize.lineariseLine synts outBBCh)^"\n") in
      close_out outBBCh;
 

    let outFile = ((String.sub file 0 ((String.length file)-5))^(!extension)) in
    let outCh = open_out outFile in
      output_string outCh outStr;
      close_out outCh;
      if (!sdout) then(  
	print_newline ();
	print_string outStr;
	print_newline ();)
    
;;

let rec linearizeDir dir count =

  try (

  let file = dir^(fileInt count) in
(*print_newline ();
print_string file;*)
linearizeFile file;
lines := (!lines +1);
linearizeDir dir (count+1);
 
  )
   with
       error  -> () 
;;

let rec linearizeDirs dir count =

  try (
    
    let file = dir^"/"^(dirInt count) in
      (*  print_string file;*)
      if (Sys.file_exists file) then(
	linearizeDir file 0;
pages := (!pages +1);
	linearizeDirs dir (count+1)
      )
      else ()
	
  )
  with
      error  -> () 
;;


let usage = "usage: " ^ Sys.argv.(0) ^ " [-d string] [-e string] [-f string]
[-b] [-s]"
  
let speclist = [
  ("-d", Arg.Set_string indir,     ": Name of the input directory.");
    ("-f", Arg.Set_string infile,    ": Name of the input file (obsolete)!");
    ("-e", Arg.Set_string extension, ": Output file extension. Default is "^(!extension)^".");
    ("-b", Arg.Clear bbox,           ": Sets BBox file off");
    ("-s", Arg.Set sdout,            ": Outputs linearised file to stdout");
]
  
let linearizer () =
  Arg.parse
    speclist
    (* (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) *)
    (fun f -> rest := true; 
       try linearizeFile f with
error ->())
(*	 | Failure(s) -> print_endline ("Error `Failure("^s^")' in file "^f) 
	 | _ -> print_endline ("Error in "^f)) *)
    usage;
  
  match !indir,!infile with
    | "","" when !rest -> exit 0
    | "",""            -> print_string "Either input directory or input file must be specified\n"; exit(0)
    | "",file          -> linearizeFile file
    | dir,""           -> (linearizeDirs !indir 0;
(*Testing to see if anything has been prodeuces, and if more than 3 lines per page are being extracted*)
			   if (!lines>0)&& (!pages>0)&&((!lines) / (!pages) >3) then ( 
			     let logFile = ((!indir)^"/lin.log") in
			     let outCh = open_out logFile in
			       output_string outCh (("Lines: "^(string_of_int (!lines)))^("\nPages: "^(string_of_int (!pages))));
			       close_out outCh;))
	
    | _,_              -> print_string "Specify either an input directory or an
  input file only\n"; exit(0)
  (*
let linearizer () =
if (Preprocessor.isAlpha "p s e u d o hyphen w") then print_endline "true" else print_endline "false";
if (Preprocessor.isAlpha "ffl comma a b period") then print_endline "true" else print_endline "false";
if (Preprocessor.isAlpha "aif12333") then print_endline "true" else print_endline "false";
if (Preprocessor.isAlpha "acaron hyphen b") then print_endline "true" else print_endline "false";
if (Preprocessor.isAlpha "caron") then print_endline "true" else print_endline "false";
if (Preprocessor.isAlpha "comma") then print_endline "true" else print_endline "false";
if (Preprocessor.isAlpha "commacaron") then print_endline "true" else print_endline "false";
if (Preprocessor.isAlpha "A") then print_endline "true" else print_endline "false";
if (Preprocessor.isNum "one 1") then print_endline "true" else print_endline "false";
if (Preprocessor.isNum "one two") then print_endline "true" else print_endline "false";
if (Preprocessor.isNum "1 1") then print_endline "true" else print_endline "false";
;;	
  *)	
let _ = linearizer ()
;;  
(*
  
  let rec main ofile count =
  
  let file = ofile^(fileInt count) in
  
  try (
  
  let symbols = JsonfIO.getSymbols file in
  
  let synts =  Preprocessor.preprocess symbols  in
  
  let outBB = ((String.sub file 0 ((String.length file)-5))^"bb") in
  
  let outBBCh = open_out outBB in
  
  let outStr = ((Linearize.lin outBBCh synts)^"\n") in
  close_out outBBCh;
  
  let outFile = ((String.sub file 0 ((String.length file)-5))^"txt4") in
  let outCh = open_out outFile in
  output_string outCh outStr;
  close_out outCh;
(*      print_newline ();
  print_string outStr;
  print_newline ();*)
  main ofile (count+1);
  )
  with
  Sys_error e -> ()     
  ;;



main Sys.argv.(1) 0
  
*)
