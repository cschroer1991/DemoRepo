&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0087.p
    Purpose     : Get claim information for an inventory id

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter piInvId as integer    no-undo.
define output parameter piClaimID as integer    no-undo.

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

find first inv where inv.inv-id = piInvId no-lock no-error.

if inv.inv-code = 1 then do:
  for each claim where claim.coil-id = inv.inv-id no-lock,
      each claim-item where claim-item.claim-id = claim.claim-id no-lock:
    if claim-item.inv-id = inv.inv-id then do:
      assign piClaimId = claim-item.claim-id.
      leave.
    end.
  end.
  for each claim where claim.coil-id = inv.prev-inv-id no-lock,
      each claim-item where claim-item.claim-id = claim.claim-id no-lock:
    if claim-item.inv-id = inv.inv-id then do:
      assign piClaimId = claim-item.claim-id.
      leave.
    end.
  end.
end.
else do:
  for each claim where claim.coil-id = inv.prev-inv-id no-lock,
      each claim-item where claim-item.claim-id = claim.claim-id no-lock:
    if claim-item.inv-id = inv.inv-id then do:
      assign piClaimId = claim-item.claim-id.
      leave.
    end.
  end.
  for each claim where claim.coil-id = inv.inv-id no-lock,
      each claim-item where claim-item.claim-id = claim.claim-id no-lock:
    if claim-item.inv-id = inv.inv-id then do:
      assign piClaimId = claim-item.claim-id.
      leave.
    end.
  end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


