open Match;;
open Jsonfio;;
open LoadClip;;


let myNewFontClipFn glyphClip chs file=
  {
    JsonfIO.srcPDF      = file    ;
    JsonfIO.page        = glyphClip.page        ;
    JsonfIO.pageWidth   = glyphClip.pageWidth   ;
    JsonfIO.pageHeight  = glyphClip.pageHeight  ;
    JsonfIO.clipX       = glyphClip.clipX       ;
    JsonfIO.clipY       = glyphClip.clipY       ;
    JsonfIO.clipWidth   = glyphClip.clipWidth   ;
    JsonfIO.clipHeight  = glyphClip.clipHeight  ;
    JsonfIO.clipImage   = glyphClip.clipImage   ;
    JsonfIO.symbols       = chs ;
  }


let rec toText inp outp =
  try toText inp (outp^"\n"^(input_line inp))
  with End_of_file -> outp
;;

let printNewFClip cl fb  file=


  let myNewCharClip = myNewFontClipFn cl fb file in
    JsonfIO.printClip  myNewCharClip;
;;

let saveNewFClip cl fb  file name=


  let myNewCharClip = myNewFontClipFn cl fb file in
    JsonfIO.saveClip name  myNewCharClip;
;;


