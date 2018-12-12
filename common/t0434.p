&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0434.p
    Purpose     : Release Hold Code

    Syntax      :

    Description :

    Author(s)   : Chris Longo
    
04/17/07 -cel- TIWO 29734 Modified to add Activity Record when a hold is released. 
               Also, added gttOldHolds to track hold that get released in case we need
               to re-estabish the hold should work order creation fail.
  
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table gttOldHolds no-undo 
  field InvID as integer
  field rHoldCode as rowid
index InvIDIdx InvID.  
    

define input parameter piInvID as int.
define input parameter pcMode as char.
define output parameter pcHoldStatus as char.
define output parameter piNoteID as int.
define output parameter pcRowID as char.
define output parameter table for gttOldHolds.

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

run value(pcMode).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getHoldStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getHoldStatus Procedure 
PROCEDURE getHoldStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define buffer bHold for hold.

find first bHold where bHold.inv-id = piInvID and
                       bHold.active = true no-lock no-error.
if avail bHold then
  assign pcHoldStatus = 'OnHold'
         piNoteID = bHold.note-id
         pcRowID = string(rowid(bHold)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReleaseHold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReleaseHold Procedure 
PROCEDURE ReleaseHold :
define buffer bInv for Inv.
define buffer bHold for Hold.
DEFINE BUFFER bHoldCodes FOR holdcodes.
DEFINE VAR crtnmsg AS CHAR.



do  transaction:
  find binv where binv.inv-id = piInvID exclusive-lock no-wait no-error.
  if not locked binv and avail binv then do:

     for each bhold where bhold.inv-id = piInvID and bhold.active = true exclusive-lock:

         create gttOldHolds.
         assign gttOldHolds.InvID = bhold.inv-id
                gttOldHolds.rHoldCode = rowid(bhold).
          

         assign
         bhold.rls-logon = dynamic-function('getGlobalVar', input 'UserID')
         bhold.active = false
         bhold.rls-date = today
         bhold.rls-time = integer(replace(string(time,"HH:MM"),":","")).   

           
     end.

     binv.hold-code = 0.

  end. /* not locked binv */
  release binv.
end.
/* tiwo 57625 */
FOR EACH gttOldHolds,
    EACH bhold WHERE ROWID(bhold) = gttOldHolds.rHoldCode,
    EACH bholdcodes WHERE bholdcodes.hold-code = bhold.hold-code:
    ASSIGN crtnmsg = crtnmsg + " " + string(bholdcodes.hold-code) + " - " + bholdcodes.descr + "~n".
END.
/*    ASSIGN crtnmsg = "Hold(s) removed for: " + STRING(piInvID) + "~n" + crtnmsg. */
/*  RETURN crtnmsg. */

dynamic-function("SetMessage", input "WARNING: Hold(s) removed for: " + STRING(piInvID) + "~n" + crtnmsg).
return {fn getUserMsg}.
ASSIGN crtnmsg = " ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

