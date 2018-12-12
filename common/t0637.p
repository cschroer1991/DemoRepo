&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0637.p
    Purpose     : Send Text Messages

    Syntax      :

    Description :

    Author(s)   : C. Longo
   
  Date      Pgrm    Remarks
  --------  ----    ------------------------------------------------------
  10/18/10  cel     TIWO 54143 Intial. 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
routine-level on error undo, throw.

define input parameter piDriverID as integer no-undo.
define input parameter pcMessage as char no-undo.

define output parameter pcErrorMsg as char no-undo.

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

define var cTextTarget as char no-undo.

find Drivers where Drivers.DriverID = piDriverID no-lock no-error.
if not avail Drivers then
    undo, throw new progress.lang.apperror(substitute('Could not find Driver: &1', piDriverID), 101).

if Drivers.CellNo <> '' and Drivers.CellNo <> ? then do: 

    case Drivers.plant-id:
        when 1 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 2 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 3 then
             cTextTarget = substitute('&1@txt.att.net', Drivers.CellNo).
        when 4 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 5 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 6 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 7 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 8 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 9 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 10 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 11 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).
        when 12 then
             cTextTarget = substitute('&1@vtext.com', Drivers.CellNo).



    end case.

    

    run lib/t0373.p (input cTextTarget,
                     input pcMessage,
                     input 'Text Message',
                     input ?).
end.

catch eSysError as progress.lang.error:

  define var i as int no-undo.
 
  do i = 1 to eSysError:NumMessages:
      pcErrorMsg = substitute('&1~nErrorNum: &2 ErrorMsg: &3',
                          pcErrorMsg,
                          eSysError:GetMessageNum(i),
                          eSysError:GetMessage(i)).
        
  end.

end. /* catch */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


