&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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
define input parameter pcClaimIDString as character no-undo.

define buffer bClaim for psi.claim.

define variable iLCV    as integer no-undo.
define variable iClaim  as integer no-undo.

define variable cMesg   as character no-undo.

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

 do for bClaim iLCV = 1 to num-entries(pcClaimIDString,chr(2)) transaction:
             
     assign
         iClaim = integer(entry(iLCV,pcClaimIDString,chr(2))).
     find bClaim exclusive-lock
         where bClaim.Claim-ID eq iClaim no-error.
     if not available bClaim then next.

     if bClaim.stat eq 9 then
     assign
        cMesg = 'Claim :' + string(iClaim) + ' is already closed'.
     else
     assign 
         bClaim.stat = 9
         bClaim.prep-date = (if bClaim.prep-date eq ? then today else bClaim.prep-date).

 end.

if cMesg ne '' then
 return error cMesg.
else
return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


