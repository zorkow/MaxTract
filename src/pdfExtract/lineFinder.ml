let lineTol = 5 ;;


(*Comparison of top sides of a glyph*)
let glyphLessY s1 s2 =
  (s1.Jsonfio.JsonfIO.y)>
    (s2.Jsonfio.JsonfIO.y)
;;

(*Sorts in ascending vertical*)
let rec sortGlyphY = function
  | [] -> []
  | pivot :: rest ->
      let is_less x = glyphLessY x pivot in
      let left, right = List.partition is_less rest in
      sortGlyphY left @ [pivot] @ sortGlyphY right
;;


(** 
    @edited:  21-JAN-2011
    @author:  Josef Baker
    @input:   top coordinate of highest glyph in curLine. List of
    glyphs, current line and previous lines
    @effects: 
    @output:  a list of lists of glyphs representing lines
 *)
let rec getLines bottom glyphs curLine lines =
  match glyphs with
      [] -> (curLine::lines)
    | h::t  ->(
	if (h.Jsonfio.JsonfIO.y) < bottom + lineTol
	then getLines (max bottom (h.Jsonfio.JsonfIO.h + h.Jsonfio.JsonfIO.y)) t (h::curLine) lines
	else getLines (h.Jsonfio.JsonfIO.y + h.Jsonfio.JsonfIO.h) t (h::[]) (curLine::lines)
      )
;;


(** 
    @edited:  10-MAR-2011
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let findLines glyphs = 
  let glyphs = List.rev (sortGlyphY glyphs) in
    match glyphs with
	[] -> []
      | h::t -> List.rev( getLines  (h.Jsonfio.JsonfIO.y + h.Jsonfio.JsonfIO.h) t (h::[]) []	
	)
	  
;;



