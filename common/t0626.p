&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0626.p
    Purpose     : Read Menu Data for common/w0274.w

    Syntax      :

    Description :

    Author(s)   : C. Longo
    
    Date     Pgrm   TIWO
    -------- ------ -----------------------------------------------------
    09/21/10 cel    53685 - Initial TIWO
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define temp-table ttReportMenu no-undo like mbMenu
  field hHandle as handle
  field hLabel as handle
  field hBullet as handle
  field WindowTab as integer
  field HistAccess as integer.

define input parameter piUniqueKey as integer no-undo.
define output parameter table for ttReportMenu.

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

for each mbMenu no-lock where mbmenu.Key = piUniqueKey:


   

    run MenuDigger (input mbMenu.key).
            
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-MenuDigger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuDigger Procedure 
PROCEDURE MenuDigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 define input parameter piKey as int no-undo.
 define buffer bMenu1 for mbMenu.
 define buffer bReportRegister for ReportRegister.


 for each bMenu1 no-lock where bMenu1.ParentKey = piKey:

     create ttReportMenu.
     buffer-copy bMenu1 to ttReportMenu.

     
    

     if bMenu1.type = 'sm' then
         run MenuDigger (input bMenu1.key).
     else do:
         find bReportRegister where bReportRegister.ReportName = bMenu1.miProcName no-lock no-error.
         if avail bReportRegister then
             ttReportMenu.HistAccess = bReportRegister.HistoricalAccess.
     end.

 end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

