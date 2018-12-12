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

{includes/gttchoice.i}


define input parameter pcdescr    as character.
define input parameter pifield-no as integer. 
define input parameter pcmisc1    as character.
define input parameter pcmisc2    as character.
define input parameter pcmisc3    as character.
define input parameter piplant-id as integer. 
define input parameter pival      as integer.

define output parameter table for gttChoice.

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

for each Choice
      where Choice.descr    = (if pcdescr <> ? then
                             pcdescr
                           else
                             choice.descr)
        and Choice.field-no = (if pifield-no <> ? then
                             pifield-no
                           else
                             choice.field-no) 
        and Choice.misc1    = (if pcmisc1 <> ? then
                             pcmisc1
                           else
                             choice.misc1)
        and Choice.misc2    = (if pcmisc2 <> ? then
                             pcmisc2
                           else
                             choice.misc2)
        and Choice.misc3    = (if pcmisc3 <> ? then
                             pcmisc3
                           else
                             Choice.misc3)
        and Choice.plant-id = (if piplant-id <> ? then
                             piplant-id
                           else
                             choice.plant-id)
        and Choice.val      = (if pival <> ? then
                             pival
                           else
                             choice.val)
      no-lock:

          buffer-copy choice to gttChoice.

end. /* for each */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


