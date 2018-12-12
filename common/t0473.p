&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0473.p
    Purpose     : Read Browser Settings

    Syntax      :

    Description :

    Author(s)   : Rick Terrell, Bravepoint, Inc.
    Created     : 01/30/07
    History     :
  Date   Pgmr Issue Description
-------- ---- ----- -----------------------------------------------------  
01/30/07 rct  25157 Initial code
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{includes/gttbrowsesetting.i}
define input  parameter pcFunction as character  no-undo.
define input  parameter pcOptions  as character  no-undo.
define input  parameter piUsrID    as integer    no-undo.
define output parameter table      for gttBrowseSetting.

define variable cUserString as character  no-undo.

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
if pcFunction gt "" 
and lookup(pcFunction,this-procedure:internal-entries) gt 0 then
case entry(1,this-procedure:get-signature(pcFunction)):
  when "PROCEDURE" then
    run value(pcFunction).
  when "FUNCTION" then
    dynamic-function(pcFunction in target-procedure).
end.

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BuildBrowseSettingTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildBrowseSettingTT Procedure 
PROCEDURE BuildBrowseSettingTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
cUserString = string(piUsrID,"9999") + "$".

empty temp-table gttBrowseSetting.

find psi-user no-lock
     where psi-user.PSIUserID eq piUsrID no-error.

for each choice no-lock
    where choice.field-no eq 487
    and   choice.descr    begins cUserString:
  create gttBrowseSetting.
  assign gttBrowseSetting.logon = psi-user.logon when available psi-user
         gttBrowseSetting.usrID = piUsrID
         gttBrowseSetting.BrowseName = entry(2,choice.descr,"$")
         gttBrowseSetting.ColumnList = choice.misc1
         gttBrowseSetting.WidthList  = choice.misc2.
end.

return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveBrowseSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RemoveBrowseSettings Procedure 
PROCEDURE RemoveBrowseSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iVal as integer    no-undo.

define buffer bChoice for choice.
define buffer bPSIUser for psi-user.

/* Empty anyway, but just in case... */
empty temp-table gttBrowseSetting.

cUserString = string(piUsrID,"9999") + "$" + entry(1,pcOptions,chr(1)).

find bChoice exclusive-lock
     where bChoice.field-no eq 487
     and   bChoice.descr    eq cUserString no-error.

if available bChoice then
  delete bChoice.

return.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveBrowseSettings) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveBrowseSettings Procedure 
PROCEDURE SaveBrowseSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iVal as integer    no-undo.

define buffer bChoice for choice.
define buffer bPSIUser for psi-user.

/* Empty anyway, but just in case... */
empty temp-table gttBrowseSetting.

cUserString = string(piUsrID,"9999") + "$" + entry(1,pcOptions,chr(1)).

find bChoice exclusive-lock
     where bChoice.field-no eq 487
     and   bChoice.descr    eq cUserString no-error.

if not available bChoice then
do:
  find last bChoice no-lock
       where bChoice.field-no eq 487
       use-index file-key no-error.
  if available bChoice then
    iVal = bChoice.val + 1.
  else 
    iVal = 1.

  create bChoice.
  assign bChoice.field-no = 487
         bChoice.descr    = cUserString
         bChoice.val      = iVal.
end.

assign bChoice.misc1    = entry(2,pcOptions,chr(1))
       bChoice.misc2    = entry(3,pcOptions,chr(1)).

return.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

