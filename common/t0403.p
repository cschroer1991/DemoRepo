&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{includes/ttsupport.i}
 
/* &scoped-define LegReadStart 5     */
/* &scoped-define GenReadStart 50    */
/* &scoped-define UnasnReadStart 80  */
/* &scoped-define LegWriteStart 1    */
/* &scoped-define GenWriteStart 50   */
/* &scoped-define UnasnWriteStart 80 */

define input  parameter pcAction     as character  no-undo.
define input  parameter pcOptionList as character  no-undo.
define input-output parameter table for ttSupport.

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

if pcAction gt "" then
  run value(pcAction).

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-clearSched) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearSched Procedure 
PROCEDURE clearSched :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define buffer bChoice for choice.

for each bChoice exclusive-lock
    where bChoice.field-no eq 451:
  delete bChoice.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-clearTTSupport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clearTTSupport Procedure 
PROCEDURE clearTTSupport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
empty temp-table ttSupport.

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
define variable cProp as character  no-undo.
define variable cVal  as character  no-undo.
define variable iIdx  as integer    no-undo.

do iIdx = 1 to num-entries(pcOptionList,"|"):
  cProp = entry(iIdx,pcOptionList,"|").
  if num-entries(cProp,"=") gt 1 then do:
    assign cVal  = entry(2,cProp,"=")
           cProp = entry(1,cProp,"=").
  end.
  else
    next.

/*   case cProp:  */
/*     when  then */
/*   end case.    */

end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-readSched) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readSched Procedure 
PROCEDURE readSched :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define buffer bChoice for choice.

define variable cType     as character  no-undo.
define variable iDefaultOrder as integer    no-undo.
define variable iOrderNum as integer    no-undo.

iDefaultOrder = 1.
for each bChoice no-lock
    where bChoice.field-no eq 451
    by bChoice.misc3:
  if SUBSTRING(bchoice.misc3,1,1) EQ "M" THEN NEXT.
  if bChoice.misc3 gt "" then
    assign cType = substring(bChoice.misc3,1,1)
           iOrderNum = integer(substring(bChoice.misc3,2)).
  else
    assign cType = "U"  /* Unassigned - for ordering purposes */
           iOrderNum = iDefaultOrder
           iDefaultOrder = iDefaultOrder + 1.

  create ttSupport.
  assign ttSupport.name    = bChoice.descr
         ttSupport.order   = iOrderNum
         ttSupport.phone   = bChoice.misc1
         ttSupport.exten   = entry(1,bChoice.misc2)
         ttSupport.appType = cType.
  validate ttSupport.
end.

if can-find(alert where alertid = 2
                and alert.alerttext ne ?
                and alert.alerttext gt '') then do:
  create ttSupport.
  assign
      ttSupport.name = '***  A HARDWARE ISSUE IS'
      ttSupport.order = 99
      ttSupport.phone = 'IN PROGRESS ***'
      ttSupport.exten = ''
      ttSupport.apptype = 'H'.
end.

if can-find(alert where alertid = 3
                and alert.alerttext ne ?
                and alert.alerttext gt '') then do:
  create ttSupport.
  assign
      ttSupport.name = '***  A SOFTWARE ISSUE IS'
      ttSupport.order = 99
      ttSupport.phone = ' IN PROGRESS ***'
      ttSupport.exten = ''
      ttSupport.apptype = 'S'.
end.


 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateSched) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateSched Procedure 
PROCEDURE updateSched :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do transaction:
/*   run clearSched. */
  run writeSched.
end.

/* Rebuild TT */
run clearTTSupport.
run readSched.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-writeSched) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE writeSched Procedure 
PROCEDURE writeSched :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define buffer bChoice for choice.
define variable iNewVal as integer    no-undo.

find last bChoice no-lock
     where bChoice.field-no eq 451
     use-index file-key no-error.

if available bChoice then
  iNewVal = bChoice.val + 1.
else
  iNewVal = 1.

for each ttSupport no-lock:
/*     break by ttSupport.appType        */
/*           by ttSupport.order:         */
/*   if first-of(ttSupport.appType) then */
/*   do:                                 */
/*     case ttSupport.appType:           */
/*       when "Genesis" then             */
/*         iNewVal = {&GenWriteStart}.   */
/*       when "Legacy" then              */
/*         iNewVal = {&LegWriteStart}.   */
/*       when "" then                    */
/*         iNewVal = {&UnasnWriteStart}. */
/*     end case.                         */
/*   end.                                */

  find bChoice exclusive-lock
       where bChoice.field-no eq 451
       and   bChoice.descr    eq ttSupport.name no-error.
  if not available bChoice then
  do:
    create bChoice.
    assign bChoice.field-no = 451
           bChoice.descr    = ttSupport.name
           bChoice.val      = iNewVal
           iNewVal          = iNewVal + 1.
  end.

  assign bChoice.misc1    = ttSupport.phone
         bChoice.misc2    = ttSupport.exten
         bChoice.misc3    = ttSupport.appType + string(order).
end.

/* iNewVal = 99.                     */
/* /* Preserve our magic numbers */  */
/* repeat preselect                  */
/*   each bChoice exclusive-lock     */
/*     where bChoice.field-no eq 451 */
/*     by bChoice.val descending:    */
/*   find next bChoice.              */
/*   assign bChoice.val = iNewVal    */
/*          iNewVal = iNewVal - 1.   */
/*   if iNewVal lt 98 then           */
/*     leave.                        */
/* end.                              */
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

