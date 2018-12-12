&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0457.p
    Purpose     : Copy Images to new Key.

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
    
   date     by   issue  remark
 ========  ===  ======  ================================================   
 10/25/16  djt   73181  copy routine was not checking for image type
                        added routine for copying spec images
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input parameter pcOldKey as char no-undo.
define input parameter pcNewKey as char no-undo.
define input parameter piKeyType as int no-undo.
define input parameter pcAction as char no-undo.

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
         WIDTH              = 49.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


if not can-find(first ImageDir where ImageDir.iKey = pcOldKey) then return.


run value(pcAction).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CopyImages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyImages Procedure 
PROCEDURE CopyImages :
define buffer OldImageLib for ImageLib.
define buffer NewImageLib for ImageLib.

define buffer OldImageDir for ImageDir.
define buffer NewImageDir for ImageDir.

/* Copy Image Dir to New Target */

for each OldImageDir no-lock 
    where OldImageDir.iKeyType = piKeyType
    and OldImageDir.iKey = pcOldKey:

  create NewImageDir.

  buffer-copy OldImageDir except iKey
    to NewImageDir
    assign NewImageDir.iKey = pcNewKey.

end.

/* Copy Image Lib to New Target */

for each OldImageLib no-lock 
    where OldImageLib.iKeyType = piKeyType
    and OldImageLib.iKey = pcOldKey:

  create NewImageLib.

  buffer-copy OldImageLib except iKey
    to NewImageLib
    assign NewImageLib.iKey = pcNewKey.

end.

/* repeat preselect                     */
/* each ImageDir exclusive-lock         */
/*      where ImageDir.iKey = pcOldKey: */
/*   find next ImageDir.                */
/*   ImageDir.iKey = pcNewKey.          */
/* end.                                 */
/*                                      */
/* repeat preselect                     */
/* each ImageLib exclusive-lock         */
/*      where ImageLib.iKey = pcOldKey: */
/*   find next ImageLib.                */
/*   ImageLib.iKey = pcNewKey.          */
/* end.                                 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CopySpecImages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopySpecImages Procedure 
PROCEDURE CopySpecImages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define buffer OldImageLib for ImageLib.
define buffer NewImageLib for ImageLib.

define buffer OldImageDir for ImageDir.
define buffer NewImageDir for ImageDir.

/* Copy Image Dir to New Target */

for each OldImageDir no-lock 
    where OldImageDir.iKey = pcOldKey:

    if not can-do('1,2,4,5,6,7,12,13', string(oldImageDir.iKeyType)) then next.

  create NewImageDir.

  buffer-copy OldImageDir except iKey
    to NewImageDir
    assign NewImageDir.iKey = pcNewKey.

end.

/* Copy Image Lib to New Target */

for each OldImageLib no-lock 
    where OldImageLib.iKey = pcOldKey:

    if not can-do('1,2,4,5,6,7,12,13', string(oldImageLib.iKeyType)) then next.

  create NewImageLib.

  buffer-copy OldImageLib except iKey
    to NewImageLib
    assign NewImageLib.iKey = pcNewKey.

end.

/* repeat preselect                     */
/* each ImageDir exclusive-lock         */
/*      where ImageDir.iKey = pcOldKey: */
/*   find next ImageDir.                */
/*   ImageDir.iKey = pcNewKey.          */
/* end.                                 */
/*                                      */
/* repeat preselect                     */
/* each ImageLib exclusive-lock         */
/*      where ImageLib.iKey = pcOldKey: */
/*   find next ImageLib.                */
/*   ImageLib.iKey = pcNewKey.          */
/* end.                                 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MoveImages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveImages Procedure 
PROCEDURE MoveImages :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
repeat preselect 
each ImageDir exclusive-lock 
     where ImageDir.iKey = pcOldKey:
  find next ImageDir.
  ImageDir.iKey = pcNewKey.
end.

repeat preselect 
each ImageLib exclusive-lock 
     where ImageLib.iKey = pcOldKey:
  find next ImageLib.
  ImageLib.iKey = pcNewKey.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

