(*
*  loadClip.ml
*
*  Made by (Alan Sexton)
*  Login   <aps@scathach>
*
*  Started on  Tue Feb 10 13:26:11 2009 Alan Sexton
*  Last update Tue Feb 10 16:02:49 2009 Alan Sexton
*)

open Json_type
open Json_type.Browse


type glyphBoundBox = {
  x:int;
  y:int;
  w:int;
  h:int}

type clip = {
  srcImage:string;
  page:int;
  pageHeight:int;
  pageWidth:int;
  clipX:int;
  clipY:int;
  clipWidth:int;
  clipHeight:int;
  clipImage:string;
  glyphs: glyphBoundBox list}


let getClip file = 
  let getGBB v =
    let tbl = make_table (objekt v) in
      {
        x =  int (field tbl "x") ;
        y =  int (field tbl "y") ;
        w =  int (field tbl "w") ;
        h =  int (field tbl "h")
      } in
  let jsonClip = Json_io.load_json file in

(*  let jsonClip = Json_io.json_of_string file in
*)
  let clipTbl = make_table (objekt jsonClip) in
  let optClipImage = Browse.optional Browse.string (Browse.fieldx clipTbl "ClipImage") in
    {
      srcImage = string (field clipTbl "SrcImage") ;
      page = int (field clipTbl "Page") ;
      pageWidth = int (field clipTbl "PageWidth") ;
      pageHeight = int (field clipTbl "PageHeight") ;
      clipX = int (field clipTbl "ClipX") ;
      clipY = int (field clipTbl "ClipY") ;
      clipWidth = int (field clipTbl "ClipWidth") ;
      clipHeight = int (field clipTbl "ClipHeight") ;
      clipImage = (match optClipImage with | None -> "" | Some s -> s) ;
      glyphs = list getGBB (field clipTbl "glyphs") ;
    }

let getGlyphs file = 
  let clip = getClip file in
    clip.glyphs;;

let getPageNo file =
  let clip = getClip file in
    clip.page
;;

let getClipBox file =
  let clip = getClip file in
    {x=clip.clipX ; y=clip.clipY; w=clip.clipWidth; h=clip.clipHeight}
;;    
 
let getPageSize file =
  let clip = getClip file in
    {x=0 ; y=0; w=clip.pageWidth; h=clip.pageHeight}
;;

 
 
