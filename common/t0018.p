&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0018.p
    Purpose     : Get DB Table Definition

    Syntax      :

    Description : Use this program to send empty temp-table
                  defined like the input parameterr. The Table-table
                  handle can then be used to create other temp-tables.

     Modified   : Create by cel.
    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter pcTables                 as character  no-undo.                                              
define input  parameter pcCols                   as character  no-undo.
define output parameter TABLE-HANDLE phTempTable.
define output parameter pcObsoleteField           as character no-undo.

define variable cTableField                       as character no-undo.
define variable cFieldName                        as character no-undo.
define variable hDBBuffer                         as handle    no-undo.
define variable hDBField                          as handle    no-undo.
define variable i                                 as integer   no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetSysError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSysError Procedure 
FUNCTION GetSysError RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

CREATE TEMP-TABLE phTempTable.
DO i = 1 TO NUM-ENTRIES(pcCols):
  
  cTableField = entry(i, pcCols).
  if index(cTableField, '[') > 0 then
      cTableField = entry(1, cTableField, '[').

  cFieldName = replace(cTableField, '.', '_').
  
  IF NOT phTempTable:ADD-LIKE-FIELD(cFieldName, cTableField)
  THEN DO:
      DELETE OBJECT phTempTable.
      RETURN ERROR "Could not create column:" + cFieldName.
  END.
  
END.


phTempTable:TEMP-TABLE-PREPARE('ttDBSchema').

DELETE OBJECT phTempTable.

/* Locate the first obsolete field */

Blk1: DO i = 1 TO NUM-ENTRIES(pcTables).
    CREATE BUFFER hDBBuffer FOR TABLE ENTRY(i, pcTables).
    hDBField = hDBBuffer:BUFFER-FIELD('Obsolete') NO-ERROR.
    IF hDBField <> ? THEN DO:
      pcObsoleteField = ENTRY(i, pcTables) + '.obsolete'.
      DELETE OBJECT hDBBuffer.
      LEAVE Blk1.
    END.
    DELETE OBJECT hDBBuffer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetSysError) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSysError Procedure 
FUNCTION GetSysError RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    define variable cMesgString as character no-undo.
  
    define variable iLCV as integer no-undo.
    do iLCV = 1 to error-status:num-messages:
        
        assign
            cMesgString = cMesgString + (if cMesgString ne '' then '~n' else '') +
                    error-status:get-message(iLCV).
    end.
    
    return cMesgString.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

