&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common.t0652.p
    Purpose     : Preload Temp Tables for Mass Spec Updata 

    Description : One-shot server hit to populate field table and choices
    
    Date     By   Issue   remark 
  ========  ===  ======  ================================================
  01/17/11  DJT   54893  New  
  
 ***************************  Definitions  ************************** */
define temp-table gttFieldTable like ttFieldTable.
define temp-table gttChoice like choice.

define input parameter pcAction as char.
define output parameter table for gttFieldTable.
define output parameter table for gttChoice.
define output parameter pcRtn as char.

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
         HEIGHT             = 9.19
         WIDTH              = 49.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

if pcAction = "":u 
or pcAction = ? 
or not can-do(this-procedure:internal-entries,pcAction) then 
  pcRtn = "**Procedure " + pcAction + ' Not available in ' + program-name(1).
else 
  run value(pcAction).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PreLoad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreLoad Procedure 
PROCEDURE PreLoad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var cChoice as char no-undo.
for each choice no-lock
    where choice.field-no = 603:
    create gttFieldTable.
    assign
        gttFieldtable.cTab = entry(1, choice.misc1, '|')
        gttFieldtable.iSeq = int(entry(2, choice.misc1, '|'))
        gttFieldtable.cDbField = entry(1, choice.misc2, '|')
        gttFieldtable.cPrompt = replace(entry(2, choice.misc2, '|'),",","")
        gttFieldtable.cFieldType = entry(1, choice.misc3, '|')
        cChoice = entry(2, choice.misc3, '|')
        gttFieldtable.iChoiceField = ?.
    if cChoice ne ? and cChoice ne '' then
        assign gttFieldtable.iChoiceField = int(cChoice) no-error.
end.
for each gttFieldTable where gttFieldtable.iChoiceField ne ?:
    for each choice no-lock where choice.field-no = gttFieldtable.iChoiceField:
        if can-find(gttChoice where gttChoice.field-no = choice.field-no
                              and gttChoice.val = choice.val) then next.
        create gttChoice.
        buffer-copy choice to gttChoice.
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

