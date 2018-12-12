&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      input-output pcComments - comment string to be edited.  Returns string
                                delimited by chr(1) at last word break (i.e.space)
                                before piLineLength.  That space is removed so
                                it wraps correctly.
      input piLineLength      - the length of each line
      input piMaxChars        - the maximum total characters that can be input.
                                ? = no limit (except those imposed by the system).

  Output Parameters:
      <none>

  Author: Rick Terrell, Bravepoint

  Created: 04/23/04
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
define input-output parameter pcComments as character  no-undo.
define input  parameter piLineLength     as integer    no-undo.
define input  parameter piMaxChars       as integer    no-undo.

/* Local Variable Definitions ---                                       */

{includes/sessionlib1.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Cancel edComments Btn_OK RECT-24 
&Scoped-Define DISPLAYED-OBJECTS edComments 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEditorSize gDialog 
FUNCTION setEditorSize RETURNS LOGICAL
  ( input piWidth as integer,
    input piHght  as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMaxChars gDialog 
FUNCTION setMaxChars RETURNS LOGICAL
  ( input piMaxChars as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY  NO-FOCUS
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO  NO-FOCUS
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE edComments AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 76 BY 11.91 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 91.4 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     Btn_Cancel AT ROW 16.24 COL 76
     edComments AT ROW 1.71 COL 5 NO-LABEL
     Btn_OK AT ROW 16.24 COL 60
     RECT-24 AT ROW 16 COL 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Comments"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
                                                                        */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Comments */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
DO:
  define variable cTextIn  as character  no-undo.
  define variable cTextOut as character  no-undo.

  assign cTextIn    = edComments:screen-value in frame {&frame-name}
         cTextOut   = ''
         pcComments = ''.

  do while true:
      run lib/l0092.p (piLineLength,input-output cTextIn, output cTextOut).
      if cTextOut eq ? then
          leave.
      pcComments = pcComments + trim(cTextOut) + chr(1).
  end.

  pcComments = right-trim(pcComments,' ' + chr(1)).

  apply 'go':U to frame {&frame-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */
setEditorSize(piLineLength,?).
setMaxChars(piMaxChars).

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY edComments 
      WITH FRAME gDialog.
  ENABLE Btn_Cancel edComments Btn_OK RECT-24 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEditorSize gDialog 
FUNCTION setEditorSize RETURNS LOGICAL
  ( input piWidth as integer,
    input piHght  as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Written as a function that can be called externally.
------------------------------------------------------------------------------*/
  define variable deTest as decimal    no-undo.

  deTest = font-table:get-text-width-chars(fill("A",piwidth),frame {&frame-name}:font).
  edComments:width = min(deTest,frame {&frame-name}:width - 1).
  if piHght ne ? and piHght gt 0 then
  do: 
      deTest = font-table:get-text-height-chars(frame {&frame-name}:font) * piHght.
      edComments:height = min(piHght,((frame {&frame-name}:height - rect-24:row) - 1)).
  end.
       
  assign edComments:row = (((frame {&frame-name}:height - rect-24:row) - 1) - edComments:height) / 2
         edComments:col = (frame {&frame-name}:width - edComments:width) / 2.
        
  return true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMaxChars gDialog 
FUNCTION setMaxChars RETURNS LOGICAL
  ( input piMaxChars as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  Written as a function that can be called externally.  
            ? = don't set
------------------------------------------------------------------------------*/
  if piMaxChars ne ? then
      edComments:max-chars in frame {&frame-name} = piMaxChars.

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

