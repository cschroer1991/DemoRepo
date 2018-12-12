&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0611.p
    Purpose     : misc server side utilities

    Syntax      :
   Date     By    TIWO  Description
 ========  ===  ====== =================================================
 06/28/10  djt   52390  new
 01/18/18  Mas   76553  Added 14 new fields to accomodate the Copy Specs Screen
                        In the FetchNotes Module.  If the Owner = 55 then the
                        Pckg-Code field will Copy otherwise it will not.
 02/13/18  Mas   76553  Added 2 New Input Fields piCopyToPartId and piCopyToPartId which will allow you to
                        get the Partner/Owner-Id of the Owner You are Copying To.
04/17/18   tjn   77564  Set initial mandrel size to ? instead of 0. Mandrels with 0 have been being put on hold.

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
define input  parameter pcAction        as character  no-undo.
define input  parameter pcOptionList    as character  no-undo.
define input  parameter piCopyToPartnerId  as Integer  no-undo.
define input  parameter piCopyToPartId  as Integer  no-undo.
define output parameter pcSetupText     as char no-undo.
define output parameter pcProcessText   as char no-undo.
define output parameter pcWrapText      as char no-undo.
define output parameter pcPckgText      as char no-undo.
define output parameter pcShipText      as char no-undo.
define output parameter peGauge-Neg     as decimal no-undo.
define output parameter peGauge-Pos     as decimal no-undo.
define output parameter peWdth-Neg      as decimal no-undo.
define output parameter peWdth-Pos      as decimal no-undo.
define output parameter peLngth-Neg     as decimal no-undo.
define output parameter peLngth-Pos     as decimal no-undo.
define output parameter peLngth2-Neg    as decimal no-undo.
define output parameter peLngth2-Pos    as decimal no-undo.
define output parameter piMandrel1      as integer no-undo.
define output parameter piMandrel2      as integer no-undo.
define output parameter piMandrel3      as integer no-undo.
define output parameter piStencilType   as integer no-undo.
define output parameter piInterleaving  as integer no-undo.
define output parameter pcPckgCode      as character no-undo.
define output parameter pcErr           as char no-undo.


define buffer bPart for Part.
define buffer bPartner for Partner.

define variable iSpecID as int no-undo.
define variable cText as char no-undo.
define variable cLastLine as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNotes Procedure 
FUNCTION getNotes RETURNS CHARACTER
  ( input piNoteID as int)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
run parseOpts.

if pcAction = "":u 
or pcAction = ? 
or not can-do(this-procedure:internal-entries,pcAction) then 
  pcErr = "**Procedure " + pcAction + ' Not available in ' + program-name(1).
else 
  run value(pcAction).

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FetchNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FetchNotes Procedure 
PROCEDURE FetchNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first spec no-lock where spec.spec-id = iSpecID no-error.
if not avail spec then do:
    assign pcErr = 'Invalid spec-id passed into t0611.p'.
    return.
end.
find first Partner no-lock where Partner.Partner-Id = Spec.Partner-Id no-error.
if not avail Partner then do:
    assign pcErr = 'Invalid Partner-Id passed into t0611.p'.
    return.
end.

assign
    pcSetupText     = ''
    pcProcessText   = ''
    pcWrapText      = ''
    pcPckgText      = ''
    pcShipText      = ''
    peGauge-Neg     = 0
    peGauge-Pos     = 0
    peWdth-Neg      = 0
    peWdth-Pos      = 0
    peLngth-Neg     = 0
    peLngth-Pos     = 0
    peLngth2-Neg    = 0
    peLngth2-Pos    = 0
    piMandrel1      = ?
    piMandrel2      = ?
    piMandrel3      = ?
    piStencilType   = 0
    piInterleaving  = 0
    pcPckgCode      = "".

if spec.setup-note-id ne ? and spec.setup-note-id ne 0 then
    assign pcSetupText = getNotes(spec.setup-note-id).

if spec.proc-note-id ne ? and spec.proc-note-id ne 0 then
    assign pcProcessText = getNotes(spec.proc-note-id).

if spec.wrap-note-id ne ? and spec.wrap-note-id ne 0 then
    assign pcWrapText = getNotes(spec.wrap-note-id).

if spec.pckg-note-id ne ? and spec.pckg-note-id ne 0 then
    assign pcPckgText = getNotes(spec.pckg-note-id).

if spec.ship-note-id ne ? and spec.ship-note-id ne 0 then
    assign pcShipText = getNotes(spec.ship-note-id).

if Spec.Gauge-Neg ne ? and Spec.Gauge-Neg ne 0 then
    assign peGauge-Neg = Spec.Gauge-Neg.
    
if Spec.Gauge-Pos ne ? and Spec.Gauge-Pos ne 0 then
    assign peGauge-Pos = Spec.Gauge-Pos.
    
if Spec.Wdth-Neg ne ? and Spec.Wdth-Neg ne 0 then
    assign peWdth-Neg = Spec.Wdth-Neg.

if Spec.Wdth-Pos ne ? and Spec.Wdth-Pos ne 0 then
    assign peWdth-Pos = Spec.Wdth-Pos.
    
if Spec.Lngth-Neg ne ? and Spec.Lngth-Neg ne 0 then
    assign peLngth-Neg = Spec.Lngth-Neg.

if Spec.Lngth-Pos ne ? and Spec.Lngth-Pos ne 0 then
    assign peLngth-Pos = Spec.Lngth-Pos.

if Spec.Lngth2-Neg ne ? and Spec.Lngth2-Neg ne 0 then
    assign peLngth2-Neg = Spec.Lngth2-Neg.

if Spec.Lngth2-Pos ne ? and Spec.Lngth2-Pos ne 0 then
    assign peLngth2-Pos = Spec.Lngth2-Pos.

if Spec.Mandrel[1] ne ? and Spec.Mandrel[1] ne 0 then
    assign piMandrel1   = Spec.Mandrel[1].

if Spec.Mandrel[2] ne ? and Spec.Mandrel[2] ne 0 then
    assign piMandrel2   = Spec.Mandrel[2].

if Spec.Mandrel[3] ne ? and Spec.Mandrel[3] ne 0 then
    assign piMandrel3   = Spec.Mandrel[3].

if Spec.StencilType ne ? and Spec.StencilType ne 0 then
    assign piStencilType = Spec.StencilType.

if Spec.Interleav ne ? and Spec.Interleav ne 0 then
    assign piInterleaving = Spec.Interleav.

if piCopyToPartnerId > 0 or piCopyToPartId > 0 then
do:
    if piCopyToPartnerId > 0 then
        find first bPartner no-lock where bPartner.Partner-Id = piCopyToPartnerId no-error.
    else
        do:
            find first Part no-lock where Part.Part-ID = piCopyToPartId no-error.
            if avail Part then
            do:    
                find first bPartner no-lock where bPartner.Partner-Id = Part.Partner-Id no-error.
            end.
        end.

    /*  Don't Blank Out The Package Code If The Partner From and The Partner To are Both = 55   */
    if Spec.Pckg-Code ne ? and Spec.Pckg-Code ne "" and Partner.Owner-Id = 55 and bPartner.Owner-Id = 55 then
        assign pcPckgCode  = Spec.Pckg-Code.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-parseOpts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parseOpts Procedure 
PROCEDURE parseOpts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var iIdx as int no-undo.
def var cVal as char no-undo.
def var cprop as char no-undo.

do iIdx = 1 to num-entries(pcOptionList,"|"):
    assign
       cVal = ?
       cProp = entry(iIdx,pcOptionList,"|").

  if num-entries(cProp,"=") gt 0 then do:
  
     assign cVal  = entry(2,cProp,"=")
            cProp = entry(1,cProp,"=").
     case cProp:
        when 'SpecID' then assign iSpecID = int(cVal).
     end case.

  end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNotes Procedure 
FUNCTION getNotes RETURNS CHARACTER
  ( input piNoteID as int) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
assign
    cText = ''
    cLastLine = ''.
for each note-page no-lock 
    where note-page.note-id = piNoteID:
    if trim(note-page.txt) = '' and cLastLine = '' then next.
    assign
        cText = cText + note-page.txt
        cLastLine = trim(cText).
end.
  RETURN cText.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

