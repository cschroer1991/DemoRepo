&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  common/t0136.p
    Purpose     :  return temp table of requested list of process codes

    Syntax      :

    Description :

    Author(s)   :  Jenni Lammers
    Created     :  09/15/04
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

def temp-table gttProcessCode no-undo like ProcessCode
    field ListFor as char
    index idx1 is primary ListFor.

define input  parameter pcRequestList as character  no-undo.
define input  parameter pcDelimiter   as character  no-undo.
define output parameter table for gttProcessCode.

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
         HEIGHT             = 11.29
         WIDTH              = 43.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

for each ProcessCode no-lock:
    if (lookup("UseForOrders",pcRequestList,pcDelimiter) > 0 and
        ProcessCode.UseForOrders) or 
       (lookup("UseForActv",pcRequestList,pcDelimiter) > 0 and
        ProcessCode.UseForActv) or
       (lookup("UseForPricing",pcRequestList,pcDelimiter) > 0 and
        ProcessCode.UseForPricing) or
       (lookup("UseForSpec",pcRequestList,pcDelimiter) > 0 and
        ProcessCode.UseForSpec) then do:

        find gttProcessCode where
            gttProcessCode.Proc-Code = ProcessCode.Proc-Code no-error.
        if not available gttProcessCode then do:
            create gttProcessCode.
            buffer-copy ProcessCode to gttProcessCode.
        end.

        assign gttProcessCode.ListFor = gttProcessCode.ListFor + "," + pcRequestList.
    end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


