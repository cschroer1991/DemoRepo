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
{chem/ttchemdefs.i}
{chem/dschemdefs.i}
{chem/srcchemdefs.i}

DEFINE INPUT  PARAMETER  pcChemTestId AS CHAR  NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsChemSet.

DEFINE VARIABLE vWhereStr AS CHAR.

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
/* MESSAGE "pcChemTestId = " pcChemTestId VIEW-AS ALERT-BOX. */

IF pcChemTestId = "?" THEN
DO:
  BUFFER ttChemMstr:FILL-MODE = "NO-FILL".
  BUFFER ttChemMeas:FILL-MODE = "NO-FILL".
END.
ELSE
DO:
  vWhereStr = "FOR EACH chemMstr WHERE chemMstr.chemTestId = " + pcChemTestId.
  QUERY qChemMstr:QUERY-PREPARE(vWhereStr).
END.

BUFFER ttChemMstr:ATTACH-DATA-SOURCE(DATA-SOURCE srcCMstr:HANDLE).
BUFFER ttChemMeas:ATTACH-DATA-SOURCE(DATA-SOURCE srcCMeas:HANDLE).
BUFFER ttChemCode:ATTACH-DATA-SOURCE(DATA-SOURCE srcCCode:HANDLE).

DATASET dsChemSet:FILL().

BUFFER ttChemMstr:DETACH-DATA-SOURCE().
BUFFER ttChemMeas:DETACH-DATA-SOURCE().
BUFFER ttChemCode:DETACH-DATA-SOURCE().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


