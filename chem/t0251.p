&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : hi
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
{chem/ttmechdefs.i}
{chem/dsmechdefs.i}
{chem/srcmechdefs.i}

DEFINE INPUT        PARAMETER  piTestId AS INT  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMechSet.
/* DEFINE OUTPUT PARAMETER TABLE FOR ttIndx. */
/* DEFINE OUTPUT PARAMETER TABLE FOR ttMeas. */

DEFINE VARIABLE vQ1WhereStr AS CHAR.
DEFINE VAR test-code-list AS CHAR INIT "1,2,6,7,8,9,10,11,14,31".

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
         HEIGHT             = 6.81
         WIDTH              = 52.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* MESSAGE "piTestId = " piTestId VIEW-AS ALERT-BOX. */

/* If we passed in unknown, then why are we */
/* going through the overhead of defining   */
/* all this and going thru the "FILL" motions? */
/* Just get the heck outta here! */
IF piTestId = ? THEN
DO:
  return.
/*   BUFFER ttIndx:FILL-MODE = "NO-FILL". */
/*   BUFFER ttMeas:FILL-MODE = "NO-FILL". */
END.
ELSE
DO:
  vQ1WhereStr = 'FOR EACH t-indx WHERE t-indx.test-id = ' + STRING(piTestId).
  QUERY qIndx:QUERY-PREPARE(vQ1WhereStr).                               
END.

BUFFER ttMeas:SET-CALLBACK-PROCEDURE("BEFORE-ROW-FILL","preMeasRowFill").
BUFFER ttIndx:SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL","postIndxRowFill").
BUFFER ttMeas:SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL","postMeasRowFill").

BUFFER ttIndx:ATTACH-DATA-SOURCE(DATA-SOURCE srcIndx:HANDLE).
BUFFER ttMeas:ATTACH-DATA-SOURCE(DATA-SOURCE srcTMeas:HANDLE).

DATASET dsMechSet:FILL().

BUFFER ttIndx:DETACH-DATA-SOURCE().
BUFFER ttMeas:DETACH-DATA-SOURCE().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-postIndxRowFill) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postIndxRowFill Procedure 
PROCEDURE postIndxRowFill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET FOR dsMechSet.

  ttIndx.oRowId = ROWID(t-indx).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-postMeasRowFill) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postMeasRowFill Procedure 
PROCEDURE postMeasRowFill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET FOR dsMechSet.

  ttMeas.oRowId = ROWID(t-meas).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-preMeasRowFill) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preMeasRowFill Procedure 
PROCEDURE preMeasRowFill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET FOR dsMechSet.
/*   DEFINE BUFFER bttMeas FOR ttMeas.  */

  IF t-meas.t-code = 1 AND
    (t-meas.side <> 6 AND t-meas.side <> 20) THEN
    RETURN NO-APPLY.

  /* The PDS will do this by itself */
/*   CREATE ttMeas.                */
/*   BUFFER-COPY t-meas TO ttMeas. */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

