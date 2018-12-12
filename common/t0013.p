&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :   server/common/t0013.p 
    Purpose     :   set the value of a "global session" variable in the
                    sesContext db table on the AppServer.

    Syntax      :

    Description :   Uses the session:server-connection-id value to tie
                    the var to a specific client session.

    Author(s)   :   Rick Terrell, Bravepoint Inc.
    Created     :   01/28/04
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER pcVarNameList  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcVarValueList AS CHARACTER  NO-UNDO.

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
define variable cConxID   as character  no-undo.
define variable cVarName  as character  no-undo.
define variable cVarValue as character  no-undo.
define variable cLogon    as character  no-undo.
define variable iRetry    as integer    no-undo.
define variable iIdx      as integer    no-undo.

cConxID = {fnarg getSessionID ''}.
if cConxID eq ? then
/* if (not(can-query(session,"server-connection-id") ) */
/*     or session:server-connection-id eq ?            */
/*     or session:server-connection-id eq '') then     */
do:
    return error "No valid session:server-connection-id on AppServer.".
end.


do iIdx = 1 to num-entries(pcVarNameList,chr(1)):
    assign cVarName  = entry(iIdx,pcVarNameList,chr(1))
           cVarValue = entry(iIdx,pcVarValueList,chr(1)).
    /* If we are setting the UserID/User-Id/logon value, we */
    /* need to set it to a consistent varName that doesn't  */
    /* conflict with P4GL keyword.  If not, then find it to */
    /* set the value later.                                 */
    cVarName = dynamic-function("chkAttrName",cVarName).
    if can-do("UserID,Logon",replace(cVarName,'-','')) then
        assign cVarName = "Logon"
               cLogon    = cVarValue.
    else
    do:
        find sesContext no-lock
             where sesContext.connectID eq cConxID
             and   sesContext.attrName  eq "Logon"
             no-error.
        if available sesContext then
            cLogon = sesContext.attrValue.
        else
            cLogon = ?.
    end.

    iRetry = 0.
    do on error undo, retry:
        if retry then
            iRetry = iRetry + 1.
        /* In theory, no one else can update this */
        find sesContext exclusive-lock
             where sesContext.connectID eq cConxID
             and   sesContext.attrName  eq cVarName
             no-error.

        /* Even though no one else can update, we want to   */
        /* test to distinguish between locked and not there */
        if locked sesContext then
        do:
            if iRetry gt 3 then
                return error "Record locked. Cannot update value.".
            else
                undo, retry.
        end.

        if not available sesContext then
        do:
            create sesContext.
            assign sesContext.connectID = cConxID.
        end.

        assign sesContext.attrName  = cVarName
               sesContext.attrValue = cVarValue
               sesContext.attrDate  = today
               sesContext.attrTime  = time
               sesContext.logon     = cLogon.
    end.

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


