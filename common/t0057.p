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

define input parameter pcZip as character no-undo.

define output parameter pcCity   as character no-undo.
define output parameter pcCounty as character no-undo.
define output parameter pcState  as character no-undo.
define output parameter plDST    as logical   no-undo.
define output parameter piTZ     as integer   no-undo.

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
if pcZip eq 'INTL' then
assign
    pcState  = ''
    pcCity   = ''
    pcCounty = ''
    plDST    = no
    piTZ     = 5 /* EASTERM */
    .
else do  for zipcode:
    if pcZip matches ".....-*" then
       assign pcZip = substr(pcZip,1,5).
    find first zipcode no-lock
        where zipcode.zip eq pcZip 
        and   zipcode.preferred eq 'P' /* City/Stat preferreed by USPS */
        no-error.
    if available zipcode then
    assign
        pcState  = zipcode.state
        pcCity   = zipcode.city
        pcCounty = zipcode.cntyName
        plDST    = zipcode.DST
        piTZ     = zipcode.TZ
        .
    else
    assign
        pcState  = ''
        pcCity   = 'Unknown Zip'
        pcCounty = ''
        plDST    = no
        piTZ     = 0 /* Zulu Time */
        .
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


