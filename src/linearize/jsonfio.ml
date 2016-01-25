(*
 *  JsonCharClip.ml
 *
 *  Made by (Alan Sexton)
 *  Login   <aps@scathach>
 *
 *)


open Json_type



(** 

    Module to encapsulate loading from and saving to a .jsonf file. Jsonf files
    contains the low level character and line information extracted from a clip
    of a PDF file and with the true bounding boxes of the characters, lines etc
    attached.
    @edited:  06-DEC-2009
    @author:  Alan P. Sexton
*)
module JsonfIO = struct

  (** A single character as extracted from a PDF file  *) 
  type pdfChar = {
    c: string;     (* Name of character  *) 
    bx: int;       (* Basepoint x  *) 
    by: int;       (* Basepoint y  *) 
    font: string;  (* Name of font  *)
    scale: float;  (* Scaling factor to be used in rendering this character  *)
  }
    
  (** A line as extracted from a PDF file  *) 
  type line = {
    sx: int;       (* Start point x  *)
    sy: int;       (* Start point y  *)
    lw: int;       (* Width (thickness) of line  *)
    ex: int;       (* End point x  *)
    ey: int;       (* End point y  *)
  }
      
  (** A  true bounding box of an object found in a PDF file as discovered by
      rendering the PDF file and analysing the image  *) 
  type bBox = {
    x: int;       (* Horizontal coordinate  *)
    y: int;       (* Vertical coordinate  *)
    w: int;       (* Width  *)
    h: int        (* Height  *)
  }
      
  (** An element in a PDF file can be a line or a character  *) 
  type element =
    | Line of line       (* line element  *)
    | PDFChar of pdfChar (* pdfChar element  *)
        
  (** A symbol is a list of elements (usually touching each other if more than
      one) with the true bounding box of the whole group of elements. We also
      include the list of bounding boxes of each individual glyph discovered in the
      rendered image of the PDF page that coresponds to each element or part
      thereof in the symbol *) 
  type symbol = {
    bbox:      bBox;         (* The bounding of the entire symbol *) 
    glyphs:    bBox list;    (* The list of bounding boxes of all glyphs in this symbol  *)
    elements:  element list; (* The list of elements in this symbol  *)
  }
      
  (** The full information about a clip of a PDF file.  *) 
  type clip = {
    srcPDF:     string;      (* Name of the original PDF file that this clip was taken from *)
    page:       int;         (* Page number in PDF file of this clip  *)
    pageHeight: int;         (* Height of Page  *)
    pageWidth:  int;         (* Width of Page  *)
    clipX:      int;         (* Horizontal coordinate of this clip  *)
    clipY:      int;         (* Vertical coordinate of this clip  *)
    clipWidth:  int;         (* Width of this clip  *)
    clipHeight: int;         (* Height of this clip  *)
    clipImage:  string;      (* File name of the tif image of this clip  *)
    symbols:    symbol list; (* List of symbols found in this clip  *)
  }
      
      
      
  (** 
      Jasonf to Ocaml translation of a pdfChar
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let j2oPdfChar v = 
    let tbl = Browse.make_table (Browse.objekt v) in
      {
        c     =  Browse.string (Browse.field tbl "c") ;
        bx    =  Browse.int (Browse.field tbl "bx") ;
        by    =  Browse.int (Browse.field tbl "by") ;
        font  =  Browse.string (Browse.field tbl "font") ;
        scale =  Browse.float (Browse.field tbl "scale") ;
      }
        
  (** 
      Jasonf to Ocaml translation of a line
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let j2oLine v = 
    let tbl = Browse.make_table (Browse.objekt v) in
      {
        sx =  Browse.int (Browse.field tbl "sx") ;
        sy =  Browse.int (Browse.field tbl "sy") ;
        lw  =  Browse.int (Browse.field tbl "lw") ;
        ex =  Browse.int (Browse.field tbl "ex") ;
        ey =  Browse.int (Browse.field tbl "ey") ;
      }
        
  (** 
      Jasonf to Ocaml translation of a bounding box
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let j2oBBox v =
    let tbl = Browse.make_table (Browse.objekt v) in
      {
        x =  Browse.int (Browse.field tbl "x") ;
        y =  Browse.int (Browse.field tbl "y") ;
        w =  Browse.int (Browse.field tbl "w") ;
        h =  Browse.int (Browse.field tbl "h")
      }
        
  (** 
      Jasonf to Ocaml translation of an element (note that this is a sum type)
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let j2oElement v =
    let lst = Browse.array v in
    let len = List.length lst in
      match Browse.string (List.hd lst) with
        | "line" when len = 2
            -> Line (j2oLine (List.nth lst 1))
        | "pdfChar" when len = 2
            -> PDFChar (j2oPdfChar (List.nth lst 1))
        | _ -> raise (Json_error ("Invalid entry in json input: " ^
                                    (Json_io.string_of_json v)))
            
  (** 
      Jasonf to Ocaml translation of a symbol
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let j2oSymbol v =
    let tbl = Browse.make_table (Browse.objekt v) in
      {
        bbox     = j2oBBox (Browse.field tbl "bbox") ;
        glyphs   = Browse.list j2oBBox (Browse.field tbl "glyphs") ;
        elements = Browse.list j2oElement (Browse.field tbl "elements") ;
      }
        
  (** 
      Jasonf to Ocaml translation of a clip
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let j2oClip jsonClip =
    let clipTbl = Browse.make_table (Browse.objekt jsonClip) in
      {
        srcPDF      = Browse.string         (Browse.field clipTbl "srcPDF") ;
        page        = Browse.int            (Browse.field clipTbl "page") ;
        pageWidth   = Browse.int            (Browse.field clipTbl "pageWidth") ;
        pageHeight  = Browse.int            (Browse.field clipTbl "pageHeight") ;
        clipX       = Browse.int            (Browse.field clipTbl "clipX") ;
        clipY       = Browse.int            (Browse.field clipTbl "clipY") ;
        clipWidth   = Browse.int            (Browse.field clipTbl "clipWidth") ;
        clipHeight  = Browse.int            (Browse.field clipTbl "clipHeight") ;
        clipImage   = Browse.string         (Browse.field clipTbl "clipImage") ;
        symbols     = Browse.list j2oSymbol (Browse.field clipTbl "symbols") ;
      }
        
  (** 
      Given the filename of a jsonf file, read it in and translate it into a clip.
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let loadClip file = 
    let jsonClip = Json_io.load_json file in
      j2oClip jsonClip
        
  (** 
      Ocaml to Jasonf translation of a pdfChar
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let o2jPdfChar o =
    Object [    
      "c",     String  o.c ;
      "bx",    Int  o.bx ;
      "by",    Int  o.by ;
      "font",  String  o.font;
      "scale", Float o.scale ;
    ]
      
  (** 
      Ocaml to Jasonf translation of a line
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let o2jLine o =
    Object    [
      "sx" ,  Int o.sx ;
      "sy" ,  Int o.sy ;
      "lw" ,  Int o.lw ;
      "ex" ,  Int o.ex ;
      "ey" ,  Int o.ey ;
    ]

  (** 
      Ocaml to Jasonf translation of a bounding box
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let o2jBBox o =
    Object [
      "x", Int o.x;
      "y", Int o.y;
      "w", Int o.w;
      "h", Int o.h
    ]
      
  (** 
      Ocaml to Jasonf translation of an element
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let o2jElement = function
    | Line(lin) -> Build.array [ Build.string "line"; o2jLine lin ]
    | PDFChar(pc) -> Build.array [ Build.string "pdfChar"; o2jPdfChar pc ]
        
  (** 
      Ocaml to Jasonf translation of a symbol
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let o2jSymbol o =
    Object [
      "bbox", o2jBBox o.bbox ;
      "glyphs", Build.list o2jBBox o.glyphs;
      "elements", Build.list o2jElement o.elements;
    ]
      
  (** 
      Ocaml to Jasonf translation of a clip
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let o2jClip clip =
    Object [
      "srcPDF"     ,  String               clip.srcPDF   ;
      "page"       ,  Int                  clip.page       ;
      "pageWidth"  ,  Int                  clip.pageWidth  ;
      "pageHeight" ,  Int                  clip.pageHeight ;
      "clipX"      ,  Int                  clip.clipX      ;
      "clipY"      ,  Int                  clip.clipY      ;
      "clipWidth"  ,  Int                  clip.clipWidth  ;
      "clipHeight" ,  Int                  clip.clipHeight ;
      "clipImage"  ,  String               clip.clipImage  ;
      "symbols"    ,  Build.list o2jSymbol clip.symbols
    ]
      
  (** 
      Given the filename of a jsonf file and a clip, translate the clip into
      jsonf format and write it to the file.
      @edited:  06-DEC-2009
      @author:  Alan P. Sexton
  *)
  let saveClip file clip  = 
    let jsonClip = o2jClip clip in
      Json_io.save_json file jsonClip 

  let printClip  clip =
    let jsonClip = o2jClip clip in
      print_string (Json_io.string_of_json jsonClip)
  
  let getSymbols file = 
    (loadClip file).symbols

end


(* For testing only: *)

(* let x = JsonfIO.loadClip "tst.jsonf" *)

(* let _= JsonfIO.saveClip "tstSave.jsonf" x *)

(* let y = JsonfIO.loadClip "tstSave.jsonf" *)

(* let _= JsonfIO.saveClip "tstSave2.jsonf" y *)

