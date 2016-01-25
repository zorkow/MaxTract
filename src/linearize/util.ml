open Synt;;

type bounds = {x1:int;x2:int;x3:int;y1:int;y2:int;y3:int;size:float};;


let rec contains string substring =
  let strlength = String.length string in
  let sublength = String.length substring in
    if sublength > strlength then false
    else (
      if substring = string then true
      else contains (String.sub string 1 (strlength-1)) substring )
;;


let boundsMin a b =
  if a=0 then b else
    if a<b then a
    else b
;;

let boundsMax a b =
  if a=0 then b else
    if a>b then a
    else b
;;

let boundsMid a b =
  if a =0 then b else a
;;

let rec findBounds curBounds synts =
  match synts with
  [] -> curBounds
    | h::t -> (
	findBounds ({x1=(boundsMin curBounds.x1 h.Synt.x1);
		     x2=h.Synt.x2;
		     x3=(boundsMax curBounds.x3 h.Synt.x3);
		     y1=(boundsMin curBounds.y1 h.Synt.y1);
		     y2=(boundsMid curBounds.y2 h.Synt.y2);
		     y3=(boundsMax curBounds.y3 h.Synt.y3);
		     size=h.Synt.size}) t)




let vertLess s1 s2 =
  if s1.Synt.y1 < s2.Synt.y1 then true
  else false
;;

(*Sorts based upon top side*)
let rec sortVert = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = vertLess x pivot in
      let left, right = List.partition is_less rest in
      sortVert left @ [pivot] @ sortVert right
;;

let y3Less s1 s2 =
  if s1.Synt.y3 < s2.Synt.y3 then true
  else false
;;

(*Sorts based upon top side*)
let rec sortY3 = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = y3Less x pivot in
      let left, right = List.partition is_less rest in
      sortY3 left @ [pivot] @ sortY3 right
;;



(** 
    @edited:  14-FEB-2010
    @author:  Josef Baker
    @input:   Synt list
    @effects: 
    @output:  Prints Synt list
 *)
let rec printSyntList inList =
  match inList with
      h::t -> (print_string h.Synt.name;
	       print_string " : ";
	       print_string h.Synt.font;
	       print_string  " : ";
	       print_float  h.Synt.size;
	       print_newline();
	       print_string  "X: ";
	       print_int h.Synt.x1;
	       print_string  " ";
	       print_int  h.Synt.x2;
	       print_string  " ";
	       print_int  h.Synt.x3;
	       
	       print_string  " Y: ";
	       print_int h.Synt.y1;
	       print_string  " ";
	       print_int  h.Synt.y2;
	       print_string  " ";
	       print_int  h.Synt.y3;
	       print_newline();
	       printSyntList t)
    | [] -> print_string "END OF LIST"; 
;;

let rec print_test inList =

  match inList with

      h::t -> (print_string "(( ";
	       print_string h.Synt.name;
	       print_string " )( ";
	       print_string h.Synt.font;
	       print_string " )( -1 ))";
	       print_test t)
    |	[] -> print_newline()
;;

let test inList = 
 print_test (sortSynt inList)
;;
