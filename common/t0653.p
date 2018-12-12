&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/******************************************************************************
  Program t0653.p - converts a text file into a PDF format.
  
  Program runs thru the file and looks for maximum width and page breaks,
  determines orientation and font point size dependong on requirements.
  
  Note: any report wider than 160 chars will be cropped.
  
  columns  Rows  Orientation  Point Size
    91     65        P           11.0
    95     70        P           10.5
   100     70        P           10.0
   105     78        P            9.5
   112     78        P            9.0
   124     52        L           10.5
   130     53        L           10.0
   137     59        L            9.5
   145     59        L            9.0
   160     65        L            8.0

 Date      By   Issue  Remark                       
 ========  ===  ====== ================================================
 01/25/11  djt   55107 new
 03/23/11  djt   55796 last line repeats, seeing some blank pages
 04/25/11 zakh   56149 Using right-trim() instead of trim() to find line length
 05/23/11  djt   56532 Missing last line on some reports
 09/18/13  cjs   63576 Add pcOption to add barcode.
 02/04/14  djt   65459 Problems printing picklists with spec images
******************************************************************************/

DEFINE INPUT PARAMETER p_TextFile AS CHARACTER NO-UNDO.
DEFINE input PARAMETER p_PDFFile  AS CHARACTER NO-UNDO.
define input parameter pcOption   as character no-undo.

define variable cProp       as character  no-undo.
define variable cVal        as character  no-undo.
define variable iIdx        as integer    no-undo.
define variable cbarval     as character  no-undo.
define variable cbartext    as character  no-undo.
define variable ixPos       as integer    no-undo.
define variable iyPos       as integer    no-undo.
define variable conPage     as character  no-undo.
define variable cFontFile1  as character  no-undo.
define variable cFontFile2  as character  no-undo.
define variable cTest       as character  no-undo.

DEFINE VARIABLE v_line  AS CHARACTER NO-UNDO.
define variable iMaxCols as int no-undo.
define variable iMaxRows as int no-undo.
define variable iTotalLines as int no-undo.
define variable iPageRows as int no-undo.
define variable eFontSize as decimal no-undo.
define variable cOrient as char no-undo.
DEFINE VARIABLE lNeedPage AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iPos AS INTEGER     NO-UNDO.
define variable rfile as memptr.
DEFINE VARIABLE iSize AS INTEGER   NO-UNDO.
define variable iByte as int no-undo.
define variable cChar as char no-undo.
DEFINE VARIABLE iLoop AS INTEGER     NO-UNDO.
define variable iPage as integer no-undo.
define stream in-file .

{ includes/pdf_inc.i "NOT SUPER"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
run ParseOpts.
run FindSize.
assign
    iPage = 1
    v_Line = ''.

/*   MESSAGE substitute('Total Lines: &1~nMaxRows: &2~nMaxCols: &3~nOrientation: &4~nFontSize: &5',  */
/*                    string(iTotalLines),                                                           */
/*                    string(iMaxrows),                                                              */
/*                    string(iMaxCols),                                                              */
/*                    cOrient,                                                                       */
/*                    string(eFontSize,">9.9")) view-as alert-box buttons ok.                        */

  RUN pdf_new IN h_PDFinc ("Spdf",p_PDFFile).

  /* The following line ensures that we don't go less than 36 points from the 
     bottom of the page -- this is mostly for readability purposes.  If we didn't
     have this then the PDF would have text on the very last line of the page.
     Ugly.  This just moves it up a couple of lines. */
  RUN pdf_set_BottomMargin  IN h_PDFinc ("Spdf",36).
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf",cOrient). 
  if cBarVal ne ? and cBarVal > '' then run BarFont.  /* Load the barcode font if we need it */

  run pdf_set_font IN h_PDFinc ("Spdf","Courier",eFontSize).

  /* if we see blank pages, try turning on these margins */
/*   run pdf_set_BottomMargin IN h_PDFinc ("Spdf",30). */
/*   run pdf_set_TopMargin IN h_PDFinc ("Spdf",30).    */

  RUN pdf_new_page IN h_PDFinc ("Spdf"). /* Instantiate a new page */
  RUN pdf_text_color IN h_PDFinc ("Spdf",0.0,.0,.0).

  /* ascii '12' character on end of line messes up the 'import' command,
     need to read file character by character to avoid the issue. 
     progress issue p176632 */

  file-info:file-name = p_Textfile.
  set-size(rFile) = file-info:file-size.
  assign iSize = file-info:file-size.
  input from value(p_Textfile) binary no-convert.
  import rFile.
  input close.
  
   do iLoop = 1 to iSize:
         assign
         ibyte = get-byte(rfile, iLoop)
         cchar = get-string(rFile, iLoop, 1).
         if iByte > 31 then do:
             v_Line = v_Line + cChar.
             next.
         end.
         if iByte = 12 then do:
             if v_Line ne '' then do:
                 RUN pdf_text IN h_PDFinc ("Spdf", v_Line).
                 RUN pdf_skip IN h_PDFinc ("Spdf").
             end.
             assign lNeedPage = yes.
         end.
         if iByte = 13 then next.
         if iByte = 10 then do:
             if lNeedPage then do:
                if iPage = 1 then run PlaceBarcode .
                RUN pdf_new_page IN h_PDFinc ("Spdf").
                run pdf_set_font IN h_PDFinc ("Spdf","Courier",eFontSize).
                assign
                    iPage = iPage + 1
                    lNeedPage = no.
             end.
             RUN pdf_text IN h_PDFinc ("Spdf", v_Line).
             RUN pdf_skip IN h_PDFinc ("Spdf").
             assign v_Line = ''.
         end.
   end.
   if iPage = 1 then run PlaceBarcode .
RUN pdf_close IN h_PDFinc ("Spdf").
IF VALID-HANDLE(h_PDFinc) THEN DELETE PROCEDURE h_PDFinc.
set-size(rFile) = 0.
/* end of conv2pdf.p */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BarFont) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BarFont Procedure 
PROCEDURE BarFont :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

assign cFontFile1 = {fnarg getAppStr '"SDIR"'} + 'bin/code39.ttf'
       cFontFile2 = {fnarg getAppStr '"SDIR"'} + 'bin/code39.afm'.
 /*If Font file unavailable, don't try to load it */
 file-info:file-name = cFontfile1.
 cTest = FILE-INFO:FULL-PATHNAME.

 if cTest = ? then do:
   /* may be running on devel mode, try looking in :\tools */
    assign
       cFontFile1 = 'P:\tools\code39.ttf'
       cFontFile2 = 'p:\tools\code39.afm'.
    file-info:file-name = cFontFile1.
    cTest = FILE-INFO:FULL-PATHNAME.
 end.
 if cTest ne ? then
     RUN pdf_load_font  IN h_PDFinc ("Spdf"
                                    ,"Code39"
                                    ,cFontFile1
                                    ,cFontFile2
                                    ,"").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindSize Procedure 
PROCEDURE FindSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

input stream in-file from value(p_TextFile).
assign
    iMaxRows = 0
    iTotalLines = 0
    iMaxCols = 0
    eFontSize = 9.5
    cOrient = 'Landscape'
    lNeedPage = no.

  first-pass:
  repeat:
      do transaction on error undo, next:
          import stream in-file unformatted v_Line.
          assign
              iTotalLines = iTotalLines + 1
              iPageRows = iPageRows + 1
              iMaxRows = max(iMaxRows, iPageRows)
              iMaxCols = max(iMaxCols, length(right-trim(v_Line))).
          if INDEX(v_Line,CHR(12)) > 0 then iPageRows = 0.
      end.
  end.
  INPUT stream in-file CLOSE.
  if iMaxRows > 78 then iMaxRows = 0.  /* too many don't take into account */

  case true:
      when iMaxCols < 92 and iMaxRows < 66 then assign
          cOrient = 'Portrait'
          eFontSize = 11.
      when iMaxCols < 96 and iMaxRows < 71 then assign
          cOrient = 'Portrait'
          eFontSize = 10.5.
      when iMaxCols < 101 and iMaxRows < 71 then assign
          cOrient = 'Portrait'
          eFontSize = 10.
      when iMaxCols < 106 and iMaxRows < 79 then assign
          cOrient = 'Portrait'
          eFontSize = 9.5.
      when iMaxCols < 113 and iMaxRows < 79 then assign
          cOrient = 'Portrait'
          eFontSize = 9.
      when iMaxCols < 125 and iMaxRows < 53 then assign
          cOrient = 'Landscape'
          eFontSize = 10.5.
      when iMaxCols < 131 and iMaxRows < 54 then assign
          cOrient = 'Landscape'
          eFontSize = 10.0.
      when iMaxCols < 138 and iMaxRows < 60 then assign
          cOrient = 'Landscape'
          eFontSize = 9.5.
      when iMaxCols < 146 and iMaxRows < 60 then assign
          cOrient = 'Landscape'
          eFontSize = 9.
      otherwise assign
          cOrient = 'Landscape'
          eFontSize = 8.
  end case.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ParseOpts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParseOpts Procedure 
PROCEDURE ParseOpts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if pcOption ne "" then do:
    do iIdx = 1 to num-entries(pcOption,"|"):
      cProp = entry(iIdx,pcOption,"|").
      if num-entries(cProp,"=") gt 1 then do:
        assign cVal  = entry(2,cProp,"=")
               cProp = entry(1,cProp,"=").
          case cProp:
              when 'barval' then do:
                  assign cbarval = cVal.
              end.
              when 'bartext' then do:
                  assign cbartext = cVal.
              end.
              when 'xPos' then do:
                  assign ixPos = int(cVal).
              end.
              when 'yPos' then do:
                  assign iyPos = int(cVal).
              end.
              when 'onPage' then do:
                  assign conPage = cVal.
              end.
          end case.
      end.
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PlaceBarcode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlaceBarcode Procedure 
PROCEDURE PlaceBarcode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  if pcOption ne "" then do:
  if cTest ne ? then do:
         run pdf_set_font IN h_PDFinc ("Spdf","Code39",17).
         if cOrient = 'Landscape' then do:
             RUN pdf_text_center IN h_PDFinc ("Spdf", substitute('*&1*',cbarval) ,ixPos,iyPos - 175).
             RUN pdf_text_center IN h_PDFinc ("Spdf", substitute('*&1*',cbarval) ,ixPos,iyPos - 189).
             if cBartext ne ? and cBarText > '' then do:
                run pdf_set_font IN h_PDFinc ("Spdf","Courier",18).
                RUN pdf_text_center IN h_PDFinc ("Spdf", cBarText ,ixPos,iyPos - 207).
             end.
         end.
         else do:
             RUN pdf_text_center IN h_PDFinc ("Spdf", substitute('*&1*',cbarval) ,ixPos,iyPos).
             RUN pdf_text_center IN h_PDFinc ("Spdf", substitute('*&1*',cbarval) ,ixPos,iyPos + 14).
             if cBartext ne ? and cBarText > '' then do:
                run pdf_set_font IN h_PDFinc ("Spdf","Courier",18).
                RUN pdf_text_center IN h_PDFinc ("Spdf", cBarText ,ixPos,iyPos - 17).
             end.
         end.
     end.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

