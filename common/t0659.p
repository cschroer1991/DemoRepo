&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0659.p
    Purpose     : Compare Old/New Spec when doing change partner.

    Syntax      :
   Date     By    TIWO  Description
 ========  ===  ====== =================================================
 03/30/11  nlbu  55290  new
 04/20/11  nlbu  55290  Modifications
 04/28/11  nlbu  55290  Stick location check duplicated. Removed 1 check.
 05/31/11 nlbu   56215  Picklist Exception-Trip/Part/Partner change spec validations.
 10/10/11 nlbu   58130  Printed copy is not doing a line feed.  Need to change chr(13) to chr(10)
 06/20/13  cjs   56765  Add ODMin and ODMax fields.   
 07/02/13  cjs   63882  Compare mandrel values at the time a partner / part is changed.
 08/01/16  bmf   71087  If previous inv-id was processed at another plant
                        compare prev plant specs with current specs.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
define input  parameter pcAction     as character  no-undo.
define input  parameter pcOptionList as character  no-undo.
DEFINE OUTPUT PARAMETER pcSpecInfo   AS character  NO-UNDO.

define variable iOldSpecID as int no-undo.
define variable iNewSpecID as int no-undo.
DEFINE VARIABLE iNewInvID  AS INT NO-UNDO.
DEFINE VARIABLE iNewPartId AS INT NO-UNDO.
DEFINE VARIABLE iOldInvID  AS INT NO-UNDO.
DEFINE VARIABLE iOldPartID  AS INT NO-UNDO.



DEFINE VARIABLE cNote       AS CHAR NO-UNDO.
DEFINE VARIABLE lSpecsValid AS LOGICAL INIT TRUE.
DEFINE VARIABLE cErrMsg     AS CHAR.
DEFINE VARIABLE iOrigOrderId AS INTEGER.
DEFINE BUFFER bNewSpec FOR spec.
DEFINE BUFFER bOrigSpec FOR spec.
DEFINE BUFFER bOrigInv FOR inv.
/* 71087 */
define variable iPrevPlantSpecID as int no-undo.
define variable iCountOld as int no-undo.
define variable iCountNew as int no-undo.
define variable i as int no-undo.
define variable lCompareAll as logical no-undo.
define buffer bPrevPlantInv for inv.
define buffer bPrevPlantSpec for spec.

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
run parseOpts.

if pcAction = "":u 
or pcAction = ? 
or not can-do(this-procedure:internal-entries,pcAction) then 
  pcSpecInfo = "**Procedure " + pcAction + ' Not available in ' + program-name(1).
else 
  run value(pcAction).

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CheckPicklistExceptions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckPicklistExceptions Procedure 
PROCEDURE CheckPicklistExceptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* min/max mult wt */
  FIND FIRST picklistexcp WHERE picklistexcp.order-id = iOrigOrderId
       AND picklistexcp.coil-id = bOrigInv.prev-inv-id
       AND picklistexcp.part-id = iOldPartId
       AND picklistexcp.fieldlabel = "min-mult-wt" NO-LOCK NO-ERROR.
  IF AVAILABLE picklistexcp AND INTEGER(picklistexcp.fieldvalue) <> bNewSpec.min-mult-wt then
       ASSIGN lSpecsValid = FALSE cNote = cNote + chr(10) + SUBSTITUTE(" Picklist Exception on Min Mult Wt. Old: &1 New: &2 ", picklistexcp.fieldvalue, bNewSpec.min-mult-wt). 
  ELSE
    if bOrigSpec.min-mult-wt <> bNewSpec.min-mult-wt then
       ASSIGN lSpecsValid = false
              cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Min Mult Wt. Old: &1 New: &2 ", bOrigSpec.min-mult-wt, bNewSpec.min-mult-wt).  
  FIND FIRST picklistexcp WHERE picklistexcp.order-id = iOrigOrderId
        AND picklistexcp.coil-id = bOrigInv.prev-inv-id
        AND picklistexcp.part-id = iOldPartId
        AND picklistexcp.fieldlabel = "max-mult-wt" NO-LOCK NO-ERROR.
  IF AVAILABLE picklistexcp AND INTEGER(picklistexcp.fieldvalue) <> bNewSpec.max-mult-wt then
       ASSIGN lSpecsValid = FALSE cNote = cNote + chr(10) + SUBSTITUTE(" Picklist Exception on Max Mult Wt. Old: &1 New: &2 ", picklistexcp.fieldvalue, bNewSpec.max-mult-wt).
  ELSE
    if bOrigSpec.max-mult-wt <> bNewSpec.max-mult-wt then
       ASSIGN lSpecsValid = false
              cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Max Mult Wt. Old: &1 New: &2 ", bOrigSpec.max-mult-wt, bNewSpec.max-mult-wt).  
  /* min/max od */
  FIND FIRST picklistexcp WHERE picklistexcp.order-id = iOrigOrderId
       AND picklistexcp.coil-id = bOrigInv.prev-inv-id
       AND picklistexcp.part-id = iOldPartId
       AND picklistexcp.fieldlabel = "ODMin" NO-LOCK NO-ERROR.
  IF AVAILABLE picklistexcp AND INTEGER(picklistexcp.fieldvalue) <> bNewSpec.ODMin then
       ASSIGN lSpecsValid = FALSE cNote = cNote + chr(10) + SUBSTITUTE(" Picklist Exception on Min OD. Old: &1 New: &2 ", picklistexcp.fieldvalue, bNewSpec.ODMin). 
/*   Below is commented out because it is duplicated in the CompareSpecs Proecedure                                                     */
/*   ELSE                                                                                                                               */
/*     if bOrigSpec.ODMin <> bNewSpec.ODMin then                                                                                        */
/*        ASSIGN lSpecsValid = false                                                                                                    */
/*               cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Min OD. Old: &1 New: &2 ", bOrigSpec.ODMin, bNewSpec.ODMin). */
  FIND FIRST picklistexcp WHERE picklistexcp.order-id = iOrigOrderId
        AND picklistexcp.coil-id = bOrigInv.prev-inv-id
        AND picklistexcp.part-id = iOldPartId
        AND picklistexcp.fieldlabel = "ODMax" NO-LOCK NO-ERROR.
  IF AVAILABLE picklistexcp AND INTEGER(picklistexcp.fieldvalue) <> bNewSpec.ODMax then
       ASSIGN lSpecsValid = FALSE cNote = cNote + chr(10) + SUBSTITUTE(" Picklist Exception on Max OD. Old: &1 New: &2 ", picklistexcp.fieldvalue, bNewSpec.ODMax).
/*   Below is commented out because it is duplicated in the CompareSpecs Proecedure                                                     */
/*   ELSE                                                                                                                               */
/*     if bOrigSpec.ODMax <> bNewSpec.ODMax then                                                                                        */
/*        ASSIGN lSpecsValid = false                                                                                                    */
/*               cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Max OD. Old: &1 New: &2 ", bOrigSpec.ODMax, bNewSpec.ODMax). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkPrevPlantSpecs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkPrevPlantSpecs Procedure 
PROCEDURE checkPrevPlantSpecs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Validate target specs against previous plant processed specs */
  If bPrevPlantSpec.pckg-type <> bNewSpec.pckg-type then
       ASSIGN lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Package Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 38, bPrevPlantSpec.pckg-type), DYNAMIC-FUNCTION('getDesc', 38, bNewSpec.pckg-type)).
  If bPrevPlantSpec.unwind <>  bNewSpec.unwind then
       ASSIGN lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Package Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 36, bPrevPlantSpec.unwind), DYNAMIC-FUNCTION('getDesc', 36, bNewSpec.unwind)).
  If bPrevPlantSpec.stky-loc <> bNewSpec.stky-loc then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Sticky loc. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 43, bPrevPlantSpec.stky-loc), DYNAMIC-FUNCTION('getDesc', 43, bNewSpec.stky-loc)).
  If bPrevPlantSpec.tape-type <> bNewSpec.tape-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Tape Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 270, bPrevPlantSpec.tape-type), DYNAMIC-FUNCTION('getDesc', 270, bNewSpec.tape-type)).
  if bPrevPlantSpec.max-mult-cnt <> bNewSpec.max-mult-cnt THEN
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Max Mult Count. Old: &1 New: &2 ", bPrevPlantSpec.max-mult-cnt, bNewSpec.max-mult-cnt).        
  if bPrevPlantSpec.min-mult-cnt <> bNewSpec.min-mult-cnt THEN
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Min Mult Count. Old: &1 New: &2 ", bPrevPlantSpec.min-mult-cnt, bNewSpec.min-mult-cnt).
  If bPrevPlantSpec.od-bnd-type <> bNewSpec.od-bnd-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on OD Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 273, bPrevPlantSpec.od-bnd-type), DYNAMIC-FUNCTION('getDesc', 273, bNewSpec.od-bnd-type)).
  If bPrevPlantSpec.od-bnd-no <>  bNewSpec.od-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on OD Band No. Old: &1 New: &2 ", bPrevPlantSpec.od-bnd-no, bNewSpec.od-bnd-no).
  If bPrevPlantSpec.od-bnd-sz <> bNewSpec.od-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on OD Band Size. Old: &1 New: &2 ", bPrevPlantSpec.od-bnd-sz, bNewSpec.od-bnd-sz).
  If bPrevPlantSpec.od-bnd-ep <> bNewSpec.od-bnd-ep then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on OD Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 275, bPrevPlantSpec.od-bnd-ep), DYNAMIC-FUNCTION('getDesc', 275, bNewSpec.od-bnd-ep)).
  If bPrevPlantSpec.od-bnd-loc <> bNewSpec.od-bnd-loc then
       ASSIGN lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on OD Band Location. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 274, bPrevPlantSpec.od-bnd-loc), DYNAMIC-FUNCTION('getDesc', 274, bNewSpec.od-bnd-loc)).
  If bPrevPlantSpec.core-bnd-type <>  bNewSpec.core-bnd-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Core Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 276, bPrevPlantSpec.core-bnd-type), DYNAMIC-FUNCTION('getDesc', 276, bNewSpec.core-bnd-type)). 
  If bPrevPlantSpec.core-bnd-no <> bNewSpec.core-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Core Band No. Old: &1 New: &2 ", bPrevPlantSpec.core-bnd-no, bNewSpec.core-bnd-no).
  If bPrevPlantSpec.core-bnd-sz <> bNewSpec.core-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Core Band Size. Old: &1 New: &2 ", bPrevPlantSpec.core-bnd-sz, bNewSpec.core-bnd-sz).
  If bPrevPlantSpec.core-bnd-ep <> bNewSpec.core-bnd-ep THEN
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Core Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 278, bPrevPlantSpec.core-bnd-ep), DYNAMIC-FUNCTION('getDesc', 278, bNewSpec.core-bnd-ep)). 
  If bPrevPlantSpec.core-bnd-loc <> bNewSpec.core-bnd-loc then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Core Band Location. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 277, bPrevPlantSpec.core-bnd-loc), DYNAMIC-FUNCTION('getDesc', 277, bNewSpec.core-bnd-loc)). 
  If bPrevPlantSpec.grp-bnd-type <>  bNewSpec.grp-bnd-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Group Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 279, bPrevPlantSpec.grp-bnd-type), DYNAMIC-FUNCTION('getDesc', 279, bNewSpec.grp-bnd-type)). 
  If bPrevPlantSpec.grp-bnd-no <> bNewSpec.grp-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Group Band No. Old: &1 New: &2 ", bPrevPlantSpec.grp-bnd-no, bNewSpec.grp-bnd-no).
  If bPrevPlantSpec.grp-bnd-sz <> bNewSpec.grp-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Group Band Size. Old: &1 New: &2 ", bPrevPlantSpec.grp-bnd-sz, bNewSpec.grp-bnd-sz).
  If bPrevPlantSpec.grp-bnd-ep <> bNewSpec.grp-bnd-ep then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Group Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 280, bPrevPlantSpec.grp-bnd-ep), DYNAMIC-FUNCTION('getDesc', 280, bNewSpec.grp-bnd-ep)). 
  If bPrevPlantSpec.grp-bnd-loc <> bNewSpec.grp-bnd-loc then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Group Band Location. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 301, bPrevPlantSpec.grp-bnd-loc), DYNAMIC-FUNCTION('getDesc', 301, bNewSpec.grp-bnd-loc)). 
  If bPrevPlantSpec.bel-bnd-type <>  bNewSpec.bel-bnd-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Belly Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 281, bPrevPlantSpec.bel-bnd-type), DYNAMIC-FUNCTION('getDesc', 281, bNewSpec.bel-bnd-type)). 
  If bPrevPlantSpec.bel-bnd-no <> bNewSpec.bel-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Belly Band No. Old: &1 New: &2 ", bPrevPlantSpec.bel-bnd-no, bNewSpec.bel-bnd-no).
  If bPrevPlantSpec.bel-bnd-sz <> bNewSpec.bel-bnd-sz then
                  assign lSpecsValid = false
                   cNote = cNote + chr(10)+ SUBSTITUTE(" Spec Diff from Original Plant on Belly Band Size. Old: &1 New: &2 ", bPrevPlantSpec.bel-bnd-sz, bNewSpec.bel-bnd-sz).
  If bPrevPlantSpec.bel-bnd-ep <> bNewSpec.bel-bnd-ep then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Belly Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 282, bPrevPlantSpec.bel-bnd-ep), DYNAMIC-FUNCTION('getDesc', 282, bNewSpec.bel-bnd-ep)).  
  IF bPrevPlantSpec.skid-bnd-type <>  bNewSpec.skid-bnd-type then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Skid Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 283, bPrevPlantSpec.skid-bnd-type), DYNAMIC-FUNCTION('getDesc', 283, bNewSpec.skid-bnd-type)).  
  If bPrevPlantSpec.skid-bnd-no <> bNewSpec.skid-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Skid Band No. Old: &1 New: &2 ", bPrevPlantSpec.skid-bnd-no, bNewSpec.skid-bnd-no).
  If bPrevPlantSpec.skid-bnd-sz <> bNewSpec.skid-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Skid Band Size. Old: &1 New: &2 ", bPrevPlantSpec.skid-bnd-sz, bNewSpec.skid-bnd-sz).
  If bPrevPlantSpec.skid-bnd-ep <> bNewSpec.skid-bnd-ep then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Skid Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 284, bPrevPlantSpec.skid-bnd-ep), DYNAMIC-FUNCTION('getDesc', 284, bNewSpec.skid-bnd-ep)).  
  If bPrevPlantSpec.skid-bnd-loc <> bNewSpec.skid-bnd-loc then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Skid Band Lcation. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 302, bPrevPlantSpec.skid-bnd-loc), DYNAMIC-FUNCTION('getDesc', 302, bNewSpec.skid-bnd-loc)).  
  If bPrevPlantSpec.post-bnd-type <>  bNewSpec.post-bnd-type then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Post Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 285, bPrevPlantSpec.post-bnd-type), DYNAMIC-FUNCTION('getDesc', 285, bNewSpec.post-bnd-type)).  
  If bPrevPlantSpec.post-bnd-no <> bNewSpec.post-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Post Band No. Old: &1 New: &2 ", bPrevPlantSpec.post-bnd-no, bNewSpec.post-bnd-no).    
  If bPrevPlantSpec.post-bnd-sz <> bNewSpec.post-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Post Band Size. Old: &1 New: &2 ", bPrevPlantSpec.post-bnd-sz, bNewSpec.post-bnd-sz).
  If bPrevPlantSpec.post-bnd-ep <> bNewSpec.post-bnd-ep then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Post Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 286, bPrevPlantSpec.post-bnd-ep), DYNAMIC-FUNCTION('getDesc', 286, bNewSpec.post-bnd-ep)).  
  If bPrevPlantSpec.cntr-bnd-type <> bNewSpec.cntr-bnd-type then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Center Band Type. Old: &1 New: &2 ",  DYNAMIC-FUNCTION('getDesc', 287, bPrevPlantSpec.cntr-bnd-type), DYNAMIC-FUNCTION('getDesc', 287, bNewSpec.cntr-bnd-type)).  
  If bPrevPlantSpec.cntr-bnd-no <> bNewSpec.cntr-bnd-no then
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Center Band No. Old: &1 New: &2 ", bPrevPlantSpec.cntr-bnd-no, bNewSpec.cntr-bnd-no).
  If bPrevPlantSpec.cntr-bnd-sz <> bNewSpec.cntr-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Center Band Size. Old: &1 New: &2 ", bPrevPlantSpec.cntr-bnd-sz, bNewSpec.cntr-bnd-sz).
  If bPrevPlantSpec.cntr-bnd-ep <> bNewSpec.cntr-bnd-ep then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Centr Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 288, bPrevPlantSpec.cntr-bnd-ep), DYNAMIC-FUNCTION('getDesc', 288,bNewSpec.cntr-bnd-ep)).
  If bPrevPlantSpec.lw-bnd-type <> bNewSpec.lw-bnd-type then
        ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on LW Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 289, bPrevPlantSpec.lw-bnd-type), DYNAMIC-FUNCTION('getDesc', 289,bNewSpec.lw-bnd-type)).
  If bPrevPlantSpec.lw-bnd-no <> bNewSpec.lw-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on LW Band No. Old: &1 New: &2 ", bPrevPlantSpec.lw-bnd-no, bNewSpec.lw-bnd-no).
  If bPrevPlantSpec.lw-bnd-sz <> bNewSpec.lw-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on LW Band Size. Old: &1 New: &2 ", bPrevPlantSpec.lw-bnd-sz, bNewSpec.lw-bnd-sz).
  If bPrevPlantSpec.lw-bnd-ep <> bNewSpec.lw-bnd-ep then
         assign lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on LW Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 290, bPrevPlantSpec.lw-bnd-ep), DYNAMIC-FUNCTION('getDesc', 290,bNewSpec.lw-bnd-ep)).
  If bPrevPlantSpec.CW-bnd-type <> bNewSpec.CW-bnd-type then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on CW Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 291, bPrevPlantSpec.cw-bnd-type), DYNAMIC-FUNCTION('getDesc', 291,bNewSpec.cw-bnd-type)).
  If bPrevPlantSpec.CW-bnd-no <> bNewSpec.CW-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on CW Band No. Old: &1 New: &2 ", bPrevPlantSpec.cw-bnd-no, bNewSpec.cw-bnd-no).
  If bPrevPlantSpec.CW-bnd-sz <> bNewSpec.CW-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on CW Band Size. Old: &1 New: &2 ", bPrevPlantSpec.cw-bnd-sz, bNewSpec.cw-bnd-sz).
  If bPrevPlantSpec.CW-bnd-ep <> bNewSpec.CW-bnd-ep then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on CW Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 292, bPrevPlantSpec.cw-bnd-ep), DYNAMIC-FUNCTION('getDesc', 292,bNewSpec.cw-bnd-ep)).
  If bPrevPlantSpec.coil-sep <> bNewSpec.coil-sep then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Coil Separators. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 35, bPrevPlantSpec.coil-sep), DYNAMIC-FUNCTION('getDesc', 35,bNewSpec.coil-sep)).
  If bPrevPlantSpec.coil-sep-loc <> bNewSpec.coil-sep-loc then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Coil Separator Loc. Old: &1 New: &2 ", bPrevPlantSpec.coil-sep-loc, bNewSpec.coil-sep-loc).
  /* Processing Spec */
  if bPrevPlantSpec.gauge-pos  <> bNewSpec.gauge-pos then 
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Gauge Pos. Old: &1 New: &2 ", bPrevPlantSpec.gauge-pos, bNewSpec.gauge-pos).
  if bPrevPlantSpec.gauge-neg <> bNewSpec.gauge-neg then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Gauge Neg. Old: &1 New: &2 ", bPrevPlantSpec.gauge-neg, bNewSpec.gauge-neg).
  if bPrevPlantSpec.wdth-pos <> bNewSpec.wdth-pos then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Wdth Pos. Old: &1 New: &2 ", bPrevPlantSpec.wdth-pos, bNewSpec.wdth-pos).
  if bPrevPlantSpec.wdth-neg <> bNewSpec.wdth-neg then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Wdth Neg. Old: &1 New: &2 ", bPrevPlantSpec.wdth-neg, bNewSpec.wdth-neg).   
  If bPrevPlantSpec.prime-side <> bNewSpec.prime-side then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Prime Side. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 268, bPrevPlantSpec.prime-side), DYNAMIC-FUNCTION('getDesc', 268,bNewSpec.prime-side)). 
  if bPrevPlantSpec.ODMin  <> bNewSpec.ODMin then
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Min OD. Old: &1 New: &2 ", bPrevPlantSpec.ODMin, bNewSpec.ODMin).   
  if bPrevPlantSpec.ODMax  <> bNewSpec.ODMax then
                   ASSIGN lSpecsValid = FALSE 
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Max OD. Old: &1 New: &2 ", bPrevPlantSpec.ODMax, bNewSpec.ODMax).   
  If bPrevPlantSpec.interleav <> bNewSpec.interleav then
        ASSIGN lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Diff from Original Plant on Interleav. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 267, bPrevPlantSpec.interleav), DYNAMIC-FUNCTION('getDesc', 267,bNewSpec.interleav)). 


    repeat i = 1 to 3:
      if bPrevPlantSpec.mandrel[i] ne ? then
        iCountOld = iCountOld + 1. 
    end.

     repeat i = 1 to 3:
      if bNewSpec.mandrel[i] ne ? then
        iCountNew = iCountNew + 1.
    end.
 

  if iCountOld > 1 then
        assign lSpecsValid = false
           cNote = CNote + chr(10) + "More than one mandrel exist on Prev Plant spec". 


  if icountNew = 1 and iCountOld = 1 and bPrevPlantSpec.mandrel[1] <> bNewSpec.mandrel[1] then
        assign lSpecsValid = false
           cNote = CNote + chr(10) + substitute(" Spec Diff from Original Plant on Mandrel. Old: &1 New: &2" , bPrevPlantSpec.mandrel[1], bNewSpec.mandrel[1]).

  if iCountOld = 1 and iCountNew > 1 then
  do:
      if bPrevPlantSpec.mandrel[1] <> bNewSpec.mandrel[1] and bPrevPlantSpec.mandrel[1] <> bNewSpec.mandrel[2] and bPrevPlantSpec.mandrel[1] <> bNewSpec.mandrel[3] then
          assign lSpecsValid = false
          cNote = CNote + chr(10) + substitute(" Spec Diff from Original Plant on Mandrel. Old: &1 New: &2 &3 &4" ,
                                                bPrevPlantSpec.mandrel[1],
                                                bNewSpec.mandrel[1],
                                                bNewSpec.mandrel[2],
                                                bNewSpec.mandrel[3]).
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckSpecFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckSpecFields Procedure 
PROCEDURE CheckSpecFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* Validate original spec against target spec */
  If bOrigSpec.pckg-type <> bNewSpec.pckg-type then
       ASSIGN lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Package Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 38, bOrigSpec.pckg-type), DYNAMIC-FUNCTION('getDesc', 38, bNewSpec.pckg-type)).
  If bOrigSpec.unwind <>  bNewSpec.unwind then
       ASSIGN lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Package Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 36, bOrigSpec.unwind), DYNAMIC-FUNCTION('getDesc', 36, bNewSpec.unwind)).
  If bOrigSpec.stky-loc <> bNewSpec.stky-loc then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Sticky loc. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 43, bOrigSpec.stky-loc), DYNAMIC-FUNCTION('getDesc', 43, bNewSpec.stky-loc)).
  If bOrigSpec.tape-type <> bNewSpec.tape-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Tape Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 270, bOrigSpec.tape-type), DYNAMIC-FUNCTION('getDesc', 270, bNewSpec.tape-type)).
  if bOrigSpec.max-mult-cnt <> bNewSpec.max-mult-cnt THEN
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Max Mult Count. Old: &1 New: &2 ", bOrigSpec.max-mult-cnt, bNewSpec.max-mult-cnt).        
  if bOrigSpec.min-mult-cnt <> bNewSpec.min-mult-cnt THEN
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Min Mult Count. Old: &1 New: &2 ", bOrigSpec.min-mult-cnt, bNewSpec.min-mult-cnt).
  if bOrigSpec.max-pckg-wt <> bNewSpec.max-pckg-wt THEN
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Max Package Wt. Old: &1 New: &2 ", bOrigSpec.max-pckg-wt, bNewSpec.max-pckg-wt).        
  if bOrigSpec.min-pckg-wt <> bNewSpec.min-pckg-wt THEN
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Min Package Wt. Old: &1 New: &2 ", bOrigSpec.min-pckg-wt, bNewSpec.min-pckg-wt).
  If bOrigSpec.od-bnd-type <> bNewSpec.od-bnd-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on OD Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 273, bOrigSpec.od-bnd-type), DYNAMIC-FUNCTION('getDesc', 273, bNewSpec.od-bnd-type)).
  If bOrigSpec.od-bnd-no <>  bNewSpec.od-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on OD Band No. Old: &1 New: &2 ", bOrigSpec.od-bnd-no, bNewSpec.od-bnd-no).
  If bOrigSpec.od-bnd-sz <> bNewSpec.od-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on OD Band Size. Old: &1 New: &2 ", bOrigSpec.od-bnd-sz, bNewSpec.od-bnd-sz).
  If bOrigSpec.od-bnd-ep <> bNewSpec.od-bnd-ep then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on OD Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 275, bOrigSpec.od-bnd-ep), DYNAMIC-FUNCTION('getDesc', 275, bNewSpec.od-bnd-ep)).
  If bOrigSpec.od-bnd-loc <> bNewSpec.od-bnd-loc then
       ASSIGN lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on OD Band Location. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 274, bOrigSpec.od-bnd-loc), DYNAMIC-FUNCTION('getDesc', 274, bNewSpec.od-bnd-loc)).
  If bOrigSpec.core-bnd-type <>  bNewSpec.core-bnd-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Core Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 276, bOrigSpec.core-bnd-type), DYNAMIC-FUNCTION('getDesc', 276, bNewSpec.core-bnd-type)). 
  If bOrigSpec.core-bnd-no <> bNewSpec.core-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Core Band No. Old: &1 New: &2 ", bOrigSpec.core-bnd-no, bNewSpec.core-bnd-no).
  If bOrigSpec.core-bnd-sz <> bNewSpec.core-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Core Band Size. Old: &1 New: &2 ", bOrigSpec.core-bnd-sz, bNewSpec.core-bnd-sz).
  If bOrigSpec.core-bnd-ep <> bNewSpec.core-bnd-ep THEN
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Core Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 278, bOrigSpec.core-bnd-ep), DYNAMIC-FUNCTION('getDesc', 278, bNewSpec.core-bnd-ep)). 
  If bOrigSpec.core-bnd-loc <> bNewSpec.core-bnd-loc then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Core Band Location. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 277, bOrigSpec.core-bnd-loc), DYNAMIC-FUNCTION('getDesc', 277, bNewSpec.core-bnd-loc)). 
  If bOrigSpec.grp-bnd-type <>  bNewSpec.grp-bnd-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Group Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 279, bOrigSpec.grp-bnd-type), DYNAMIC-FUNCTION('getDesc', 279, bNewSpec.grp-bnd-type)). 
  If bOrigSpec.grp-bnd-no <> bNewSpec.grp-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Group Band No. Old: &1 New: &2 ", bOrigSpec.grp-bnd-no, bNewSpec.grp-bnd-no).
  If bOrigSpec.grp-bnd-sz <> bNewSpec.grp-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Group Band Size. Old: &1 New: &2 ", bOrigSpec.grp-bnd-sz, bNewSpec.grp-bnd-sz).
  If bOrigSpec.grp-bnd-ep <> bNewSpec.grp-bnd-ep then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Group Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 280, bOrigSpec.grp-bnd-ep), DYNAMIC-FUNCTION('getDesc', 280, bNewSpec.grp-bnd-ep)). 
  If bOrigSpec.grp-bnd-loc <> bNewSpec.grp-bnd-loc then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Group Band Location. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 301, bOrigSpec.grp-bnd-loc), DYNAMIC-FUNCTION('getDesc', 301, bNewSpec.grp-bnd-loc)). 
  If bOrigSpec.bel-bnd-type <>  bNewSpec.bel-bnd-type then
       assign lSpecsValid = false
         cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Belly Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 281, bOrigSpec.bel-bnd-type), DYNAMIC-FUNCTION('getDesc', 281, bNewSpec.bel-bnd-type)). 
  If bOrigSpec.bel-bnd-no <> bNewSpec.bel-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Belly Band No. Old: &1 New: &2 ", bOrigSpec.bel-bnd-no, bNewSpec.bel-bnd-no).
  If bOrigSpec.bel-bnd-sz <> bNewSpec.bel-bnd-sz then
                  assign lSpecsValid = false
                   cNote = cNote + chr(10)+ SUBSTITUTE(" Spec Difference on Belly Band Size. Old: &1 New: &2 ", bOrigSpec.bel-bnd-sz, bNewSpec.bel-bnd-sz).
  If bOrigSpec.bel-bnd-ep <> bNewSpec.bel-bnd-ep then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Belly Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 282, bOrigSpec.bel-bnd-ep), DYNAMIC-FUNCTION('getDesc', 282, bNewSpec.bel-bnd-ep)).  
  IF bOrigSpec.skid-bnd-type <>  bNewSpec.skid-bnd-type then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Skid Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 283, bOrigSpec.skid-bnd-type), DYNAMIC-FUNCTION('getDesc', 283, bNewSpec.skid-bnd-type)).  
  If bOrigSpec.skid-bnd-no <> bNewSpec.skid-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Skid Band No. Old: &1 New: &2 ", bOrigSpec.skid-bnd-no, bNewSpec.skid-bnd-no).
  If bOrigSpec.skid-bnd-sz <> bNewSpec.skid-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Skid Band Size. Old: &1 New: &2 ", bOrigSpec.skid-bnd-sz, bNewSpec.skid-bnd-sz).
  If bOrigSpec.skid-bnd-ep <> bNewSpec.skid-bnd-ep then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Skid Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 284, bOrigSpec.skid-bnd-ep), DYNAMIC-FUNCTION('getDesc', 284, bNewSpec.skid-bnd-ep)).  
  If bOrigSpec.skid-bnd-loc <> bNewSpec.skid-bnd-loc then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Skid Band Lcation. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 302, bOrigSpec.skid-bnd-loc), DYNAMIC-FUNCTION('getDesc', 302, bNewSpec.skid-bnd-loc)).  
  If bOrigSpec.post-bnd-type <>  bNewSpec.post-bnd-type then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Post Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 285, bOrigSpec.post-bnd-type), DYNAMIC-FUNCTION('getDesc', 285, bNewSpec.post-bnd-type)).  
  If bOrigSpec.post-bnd-no <> bNewSpec.post-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Post Band No. Old: &1 New: &2 ", bOrigSpec.post-bnd-no, bNewSpec.post-bnd-no).    
  If bOrigSpec.post-bnd-sz <> bNewSpec.post-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Post Band Size. Old: &1 New: &2 ", bOrigSpec.post-bnd-sz, bNewSpec.post-bnd-sz).
  If bOrigSpec.post-bnd-ep <> bNewSpec.post-bnd-ep then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Post Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 286, bOrigSpec.post-bnd-ep), DYNAMIC-FUNCTION('getDesc', 286, bNewSpec.post-bnd-ep)).  
  If bOrigSpec.cntr-bnd-type <> bNewSpec.cntr-bnd-type then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Center Band Type. Old: &1 New: &2 ",  DYNAMIC-FUNCTION('getDesc', 287, bOrigSpec.cntr-bnd-type), DYNAMIC-FUNCTION('getDesc', 287, bNewSpec.cntr-bnd-type)).  
  If bOrigSpec.cntr-bnd-no <> bNewSpec.cntr-bnd-no then
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Center Band No. Old: &1 New: &2 ", bOrigSpec.cntr-bnd-no, bNewSpec.cntr-bnd-no).
  If bOrigSpec.cntr-bnd-sz <> bNewSpec.cntr-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Center Band Size. Old: &1 New: &2 ", bOrigSpec.cntr-bnd-sz, bNewSpec.cntr-bnd-sz).
  If bOrigSpec.cntr-bnd-ep <> bNewSpec.cntr-bnd-ep then
        assign lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Centr Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 288, bOrigSpec.cntr-bnd-ep), DYNAMIC-FUNCTION('getDesc', 288,bNewSpec.cntr-bnd-ep)).
  If bOrigSpec.lw-bnd-type <> bNewSpec.lw-bnd-type then
        ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on LW Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 289, bOrigSpec.lw-bnd-type), DYNAMIC-FUNCTION('getDesc', 289,bNewSpec.lw-bnd-type)).
  If bOrigSpec.lw-bnd-no <> bNewSpec.lw-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on LW Band No. Old: &1 New: &2 ", bOrigSpec.lw-bnd-no, bNewSpec.lw-bnd-no).
  If bOrigSpec.lw-bnd-sz <> bNewSpec.lw-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on LW Band Size. Old: &1 New: &2 ", bOrigSpec.lw-bnd-sz, bNewSpec.lw-bnd-sz).
  If bOrigSpec.lw-bnd-ep <> bNewSpec.lw-bnd-ep then
         assign lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on LW Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 290, bOrigSpec.lw-bnd-ep), DYNAMIC-FUNCTION('getDesc', 290,bNewSpec.lw-bnd-ep)).
  If bOrigSpec.CW-bnd-type <> bNewSpec.CW-bnd-type then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on CW Band Type. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 291, bOrigSpec.cw-bnd-type), DYNAMIC-FUNCTION('getDesc', 291,bNewSpec.cw-bnd-type)).
  If bOrigSpec.CW-bnd-no <> bNewSpec.CW-bnd-no then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on CW Band No. Old: &1 New: &2 ", bOrigSpec.cw-bnd-no, bNewSpec.cw-bnd-no).
  If bOrigSpec.CW-bnd-sz <> bNewSpec.CW-bnd-sz then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on CW Band Size. Old: &1 New: &2 ", bOrigSpec.cw-bnd-sz, bNewSpec.cw-bnd-sz).
  If bOrigSpec.CW-bnd-ep <> bNewSpec.CW-bnd-ep then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on CW Band EP. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 292, bOrigSpec.cw-bnd-ep), DYNAMIC-FUNCTION('getDesc', 292,bNewSpec.cw-bnd-ep)).
  If bOrigSpec.coil-sep <> bNewSpec.coil-sep then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Coil Separators. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 35, bOrigSpec.coil-sep), DYNAMIC-FUNCTION('getDesc', 35,bNewSpec.coil-sep)).
  If bOrigSpec.coil-sep-loc <> bNewSpec.coil-sep-loc then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Coil Separator Loc. Old: &1 New: &2 ", bOrigSpec.coil-sep-loc, bNewSpec.coil-sep-loc).
  /* Processing Spec */
  if bOrigSpec.gauge-pos  <> bNewSpec.gauge-pos then 
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Gauge Pos. Old: &1 New: &2 ", bOrigSpec.gauge-pos, bNewSpec.gauge-pos).
  if bOrigSpec.gauge-neg <> bNewSpec.gauge-neg then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Gauge Neg. Old: &1 New: &2 ", bOrigSpec.gauge-neg, bNewSpec.gauge-neg).
  if bOrigSpec.wdth-pos <> bNewSpec.wdth-pos then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Wdth Pos. Old: &1 New: &2 ", bOrigSpec.wdth-pos, bNewSpec.wdth-pos).
  if bOrigSpec.wdth-neg <> bNewSpec.wdth-neg then
                   assign lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Wdth Neg. Old: &1 New: &2 ", bOrigSpec.wdth-neg, bNewSpec.wdth-neg).   
  If bOrigSpec.prime-side <> bNewSpec.prime-side then
         ASSIGN lSpecsValid = false
           cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Prime Side. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 268, bOrigSpec.prime-side), DYNAMIC-FUNCTION('getDesc', 268,bNewSpec.prime-side)). 
  if bOrigSpec.ODMin  <> bNewSpec.ODMin then
                   ASSIGN lSpecsValid = false
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Min OD. Old: &1 New: &2 ", bOrigSpec.ODMin, bNewSpec.ODMin).   
  if bOrigSpec.ODMax  <> bNewSpec.ODMax then
                   ASSIGN lSpecsValid = FALSE 
                     cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Max OD. Old: &1 New: &2 ", bOrigSpec.ODMax, bNewSpec.ODMax).   
  If bOrigSpec.interleav <> bNewSpec.interleav then
        ASSIGN lSpecsValid = false
          cNote = cNote + chr(10) + SUBSTITUTE(" Spec Difference on Interleav. Old: &1 New: &2 ", DYNAMIC-FUNCTION('getDesc', 267, bOrigSpec.interleav), DYNAMIC-FUNCTION('getDesc', 267,bNewSpec.interleav)). 


    repeat i = 1 to 3:
      if bOrigSpec.mandrel[i] ne ? then
        iCountOld = iCountOld + 1. 
    end.

     repeat i = 1 to 3:
      if bNewSpec.mandrel[i] ne ? then
        iCountNew = iCountNew + 1.
    end.
 

  if iCountOld > 1 then
        assign lSpecsValid = false
           cNote = CNote + chr(10) + "More than one mandrel exist on old spec". 


  if icountNew = 1 and iCountOld = 1 and bOrigSpec.mandrel[1] <> bNewSpec.mandrel[1] then
        assign lSpecsValid = false
           cNote = CNote + chr(10) + substitute(" Spec Difference on Mandrel. Old: &1 New: &2" , bOrigSpec.mandrel[1], bNewSpec.mandrel[1]).

  if iCountOld = 1 and iCountNew > 1 then
  do:
      if bOrigSpec.mandrel[1] <> bNewSpec.mandrel[1] and bOrigSpec.mandrel[1] <> bNewSpec.mandrel[2] and bOrigSpec.mandrel[1] <> bNewSpec.mandrel[3] then
          assign lSpecsValid = false
          cNote = CNote + chr(10) + substitute(" Spec Difference on Mandrel. Old: &1 New: &2 &3 &4" ,
                                                bOrigSpec.mandrel[1],
                                                bNewSpec.mandrel[1],
                                                bNewSpec.mandrel[2],
                                                bNewSpec.mandrel[3]).
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CompareSpecs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompareSpecs Procedure 
PROCEDURE CompareSpecs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: tiwo 56215- Min/Max mult.      
------------------------------------------------------------------------------*/
 /* find Specs for Orig Part */
iOldSpecId = DYNAMIC-FUNCTION('getSpec',0,iOldPartId,iOldInvId).
find first bOrigSpec where bOrigSpec.spec-id = iOldSpecID no-lock no-error.
if lCompareAll
and NOT AVAILABLE bOrigSpec THEN 
DO:
  ASSIGN cNote = cNote + chr(10) + 'Missing Spec for Original Part! Part Spec compare Not Done.'
      lSpecsValid = false.
END.
 /* find Specs for Target Part */
iNewSpecId = DYNAMIC-FUNCTION('getSpec',0,iNewPartId,0).
find first bNewSpec where bNewSpec.spec-id = iNewSpecID NO-LOCK NO-ERROR.
if lCompareAll
and NOT AVAILABLE bNewSpec THEN
DO:
   ASSIGN cNote = cNote + chr(10) + 'Missing Spec for New Part! Part Spec Compare Not Done.'
      lSpecsValid = false.
END.
FIND FIRST inv WHERE inv.inv-id = iNewInvID NO-LOCK NO-ERROR.
IF NOT AVAILABLE inv THEN
DO:
   assign pcSpecInfo = pcSpecInfo + chr(10) + 'Inv ID:' + string(iNewInvid) + ' Could not be found. Could not apply Spec Compare.'.
      return.
END.
FIND FIRST bOrigInv WHERE bOrigInv.inv-id = iOldInvId NO-LOCK NO-ERROR.
  IF AVAILABLE bOrigInv THEN ASSIGN iOrigOrderId = bOrigInv.order-id.
     ELSE ASSIGN iOrigOrderId = 0.
/* 71087 */
/* find inv/specs from previous plant if processed at another plant */
find rcpt where rcpt.rcpt-id eq bOrigInv.rcpt-id no-lock no-error.
if  rcpt.prev-inv-id <> ""
and rcpt.prev-inv-id <> ? then
do:
  find bPrevPlantInv where bPrevPlantInv.inv-id eq int(rcpt.prev-inv-id) no-lock no-error.
  if available bPrevPlantInv
  and bPrevPlantInv.plant-id <> bOrigInv.plant-id
  and (bPrevPlantInv.order-id > 0
   or  bPrevPlantInv.morder-id > 0) then
  do:

      iPrevPlantSpecId = DYNAMIC-FUNCTION('getSpec',0,
                                          bPrevPlantInv.part-id,
                                          bPrevPlantInv.inv-id).
      find first bPrevPlantSpec where bPrevPlantSpec.spec-id = iPrevPlantSpecID no-lock no-error.
      if not available bPrevPlantSpec then
      do:
         assign cNote = cNote + chr(10) + 'Missing Spec for Prev Plant Part! Part Spec Compare Not Done.'
                lSpecsValid = false.
      end.
  end.
end.
IF lSpecsValid THEN
DO:
  if lCompareAll then
    run checkSpecFields.

  if available bPrevPlantSpec then
      run checkPrevPlantSpecs.

  if lCompareAll then
  run CheckPicklistExceptions.

END.
if not lSpecsValid then do:
                   run inv/t0114.p (input 'hold',    /* pcTransType      */
                   input string(rowid(Inv)),    /* rowid(s)         */
                   input 'inv',                  /* table name       */
                   input 26,                     /* hold code        */
                   input cNote,                  /* note text        */
                   input ?,                      /* damage code      */
                   input ?,                      /* cond code        */
                   input '',                     /* options          */
                   output cErrMsg).              /* error message    */
pcSpecInfo = "Coil has been placed on hold 26."
                            + chr(10) + '  -' + cNote.
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
        when 'OldPart'    then assign iOldPartID  = int(cVal).
        when 'OldInvId'   then assign iOldInvID   = int(cVal).
        when 'NewPart'    then assign iNewPartID  = int(cVal).
        when 'NewInvId'   then assign iNewInvID   = int(cVal).
        when 'CompareAll' then assign lCompareAll = logical(cVal).
     end case.

  end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

