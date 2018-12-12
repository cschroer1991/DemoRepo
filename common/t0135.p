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
define input        parameter pcUserName  as character  no-undo.
define input-output parameter pcFieldValueList as character  no-undo.

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
define variable cFldList as character  no-undo.
define variable hBfrHdl as handle     no-undo.
define variable hFldHdl as handle     no-undo.
define variable iIdx    as integer    no-undo.

find psi-user no-lock
     where psi-user.logon eq pcUserName no-error.
if not available psi-user then
do:
    pcFieldValueList = ?.
    return.
end.

assign hBfrHdl = buffer psi-user:handle
       cFldList = pcFieldValueList
       pcFieldValueList = ''.

if cFldList eq '' or cFldList eq '*' then
do iIdx = 1 to hBfrHdl:num-fields:
    assign hFldHdl = hBfrHdl:buffer-field(iIdx)
           pcFieldValueList = pcFieldValueList
                            + (if pcFieldValueList eq '' then '' else chr(1))
                            + hFldHdl:name
                            + chr(1)
                            + (if hFldHdl:buffer-value eq ? then '?' else string(hFldHdl:buffer-value)).
end.
else
do: 
    if num-entries(cFldList) eq 1 then
    do:
        assign hFldHdl = hBfrHdl:buffer-field(cFldList).
        if valid-handle(hFldHdl) then
            pcFieldValueList = string(hFldHdl:buffer-value). /* Doesn't matter if its ? */
        else 
            pcFieldValueList = ?.
    end.
    else
    do iIdx = 1 to num-entries(cFldList):
        assign hFldHdl = hBfrHdl:buffer-field(entry(iIdx,cFldList)).
        if valid-handle(hFldHdl) then
               pcFieldValueList = pcFieldValueList
                          + (if pcFieldValueList eq '' then '' else chr(1))
                          + hFldHdl:name
                          + chr(1)
                          + (if hFldHdl:buffer-value eq ? then '?' else string(hFldHdl:buffer-value)).
    end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


