open Pdfextractor;;
open Contentparser;;
open LoadClip;;
open Str;;



(** Not used anymore....
    @edited:  02-JUN-2010
    @author:  Josef Baker
    @input:   Coordinates of clip and list of pdf elements
    @effects: 
    @output:  All pdf elements within clip area
 *)
let rec clipChars x1 x2 y1 y2 inList outList =
  match inList with
      h::t ->( match h with
		   Ln ln ->
		     ( if ((ln.Contentparser.stx  >= x1) 
			   && (ln.Contentparser.enx <= x2) 
			   && (ln.Contentparser.sty >= y1) 
			   && (ln.Contentparser.eny <= y2)) then 
			 clipChars x1 x2 y1 y2 t (h::outList)
		       else clipChars x1 x2 y1 y2 t outList)
		 | Chr chr ->
		     ( if ((((chr.Contentparser.chx) +. (chr.Contentparser.chw /. 2.)) >= x1) 
			   && (chr.Contentparser.chx <= x2) 
			   && ((chr.Contentparser.chy >= y1) || ((chr.Contentparser.chy +. chr.Contentparser.chsize/.2.) >=y1)) 
			   && ((chr.Contentparser.chy <= y2) || ((chr.Contentparser.chy -. chr.Contentparser.chsize/.2.) <=y2))) then 
			 clipChars x1 x2 y1 y2 t (h::outList)
		       else clipChars x1 x2 y1 y2 t outList)
	     )
    | [] ->  outList
;;



(** 
    @edited:  02-JUN-2010
    @author:  Josef Baker
    @input:   x and y ratio of pdf file to clip image height of pdf file and list of pdf elements
    @effects: 
    @output:  list of pdf elements scaled andin correct coordinate system
 *)
let rec alignChars xRatio yRatio pHeight inList outList =
  match inList with
      h::t ->( match h with
		   Ln ln ->       
		     alignChars xRatio yRatio pHeight t (Ln {Contentparser.stx=(ln.Contentparser.stx /. xRatio);
							     Contentparser.sty=((pHeight -. ln.Contentparser.sty) /.yRatio);
							     Contentparser.enx=(ln.Contentparser.enx /. xRatio);
							     Contentparser.eny=((pHeight -. ln.Contentparser.eny) /.yRatio);
							     Contentparser.lnw=((ln.Contentparser.lnw) /. xRatio);}::outList)
		 | Chr chr ->  
		     alignChars xRatio yRatio pHeight t (Chr {Contentparser.chname = chr.Contentparser.chname;
							      Contentparser.chfont = chr.Contentparser.chfont;
							      Contentparser.chsize = chr.Contentparser.chsize;
							      Contentparser.chx=(chr.Contentparser.chx /. xRatio);
							      Contentparser.chy=((pHeight -. chr.Contentparser.chy) /.yRatio);
							      Contentparser.chw=((chr.Contentparser.chw) /. xRatio);}::outList)
	     )
	
    |	[] -> outList
;;


(** 
    @edited:  27-JUN-2012
    @author:  Josef Baker
    @input:   Character list
    @effects: prints character list
    @output:  none
 *)
let rec printChLst chars =
  match chars with
      (Ln ln)::t ->(print_endline ((string_of_float ln.stx)^" "^(string_of_float ln.enx)^" "^(string_of_float ln.sty)^" "^(string_of_float ln.eny));
		    printChLst t;)
    | (Chr c)::t ->(print_endline (c.chname^" "^(string_of_float c.chx)^" "^(string_of_float c.chy)^" "^(string_of_float c.chw));
		    printChLst t;)
    | _ -> (print_newline ();)




let align  pdfList mediaBox pageSize clipSize=
(*  
  let x1 = float_of_int clipSize.LoadClip.x in
  let x2 = (float_of_int clipSize.LoadClip.w) +. x1 in
  let y1 = float_of_int clipSize.LoadClip.y in
  let y2 = (float_of_int clipSize.LoadClip.h) +. y1 in
*)
    
    
  let xRatio = ((mediaBox.mwidth) /. (float_of_int pageSize.LoadClip.w)) in
  let yRatio = ((mediaBox.mheight) /. (float_of_int pageSize.LoadClip.h)) in
    
    
  let alignedChars =  alignChars xRatio yRatio (mediaBox.mheight) pdfList [] in
(*
  let clipped = clipChars x1 x2 y1 y2 alignedChars [] in*)
(*
printChLst pdfList;
print_newline ();
*)
    alignedChars
;;

(** 
    @edited:  08-MAR-2009
    @author:  Josef Baker
    @input:   List of Loadclip glyphs
    @effects: 
    @output:  List of jsonf glyphs
 *)
let rec convertGlyphs inList outList =
  match inList with
      h::t -> convertGlyphs t ({Jsonfio.JsonfIO.x = (h.LoadClip.x*2);
				Jsonfio.JsonfIO.y = (h.LoadClip.y*2);
				Jsonfio.JsonfIO.w = (h.LoadClip.w*2);
				Jsonfio.JsonfIO.h = (h.LoadClip.h*2);}::outList) 
    | _ -> outList
;;

(** 
    @edited:  08-MAY-2012
    @author:  Josef Baker
    @input:   
    @effects: 
    @output:  
 *)
let alignElems clip page elems =
    align elems page.Pdfextractor.dimensions 
      {LoadClip.x= 0;
       LoadClip.y=0; 
       LoadClip.w=(clip.LoadClip.pageWidth*2); 
       LoadClip.h=(clip.LoadClip.pageHeight*2)}
      {LoadClip.x=(clip.LoadClip.clipX*2);
       LoadClip.y=(clip.LoadClip.clipY*2);
       LoadClip.w=(clip.LoadClip.clipWidth*2); 
       LoadClip.h=(clip.LoadClip.clipHeight*2)}
;;
