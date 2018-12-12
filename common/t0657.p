&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0654.p
    Purpose     : check for expiring messaging entries
    
 Date     By   Issue Remark
 ======== ==== ===== =================================================
 03/15/11  djt 55210 new
 11/19/12  djt 62224 schema change - make part-id 7 characters
 12/06/16 zakh 70511 Added logic for line-id.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
def var tExpireOn as datetime no-undo.
def var tNow as datetime no-undo.
def var cMsg as char no-undo.
def var lResult as logical no-undo.

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
assign
    tNow = now
    tExpireOn = tNow + 90000000.

for each messages no-lock
    where messages.expiredate ge tNow
    and messages.expiredate le tExpireOn:
    
    find first choice no-lock
        where choice.field-no = 604
        and choice.val = messages.TriggerAction no-error.

    assign cMsg = substitute("Message ID: &1 will expire &2~n~n&3 for:&4&5&6&7&8&9",
                             string(messages.MessageID),
                             string(messages.expiredate,"99/99/99 HH:MM"),
                             'Triggered on ' + (if avail choice then choice.descr else 'Unknown'),
                             (if messages.owner-id ne ? and messages.owner-id ne 0 then '~nOwner ID: ' + string(messages.owner-id) else ''),
                             (if messages.user-id ne ? and messages.user-id ne 0 then '~nUser ID: ' + string(messages.user-id) else ''),
                             (if messages.partner-id ne ? and messages.partner-id ne 0 then '~nPartner ID: ' + string(messages.partner-id) else ''),
                             (if messages.part-id ne ? and messages.part-id ne 0 then '~nPart ID: ' + string(messages.part-id) else ''),
                             (if messages.line-id ne ? and messages.line-id ne 0 then '~nLine ID: ' + string(messages.line-id) else ''),
                             (if messages.WatchReqd then '~n~nThis message sets Watch Required on the prod sched.' else '')).

    if messages.createby ne ?
        and messages.createby ne 0
        and messages.user-id ne 0 then
        run alert (input 53,
                   input cMsg,
                   input '',
                   input 'U',
                   input messages.createby,
                   input '',
                   output lResult).

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


