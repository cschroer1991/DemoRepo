&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0475.p
    Purpose     : Check for the presents of a DB Connection

    Syntax      :

    Description :

    Author(s)   : C. Longo
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input parameter pcDatabase as char no-undo.
define output parameter plResult as logical no-undo.

define var gcEnv as char no-undo.

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

find AppAttr no-lock where AppAttr.AttrKey = 'Environment' no-error.
if avail AppAttr then
  gcEnv = AppAttr.AppStr.
else
  gcEnv = "".

plResult = connected(pcDatabase).

if plResult = false then do:

  if gcEnv = "" then return.

  case gcEnv:
      when 'genesis' then 
          run ConnectTest.
      when 'preprod' then
          run ConnectPreProd.
      when 'PSI' then
          run ConnectProduction.
  end case.



end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ConnectPreProd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectPreProd Procedure 
PROCEDURE ConnectPreProd :
/* if opsys = 'unix' then do:                           */
/*   if pcDataBase = 'main' or pcDataBase = 'mods' then */
/*     connect -pf /test/bin/syteline-gen.pf no-error.  */
/*   else                                               */
/*     connect -pf /test/bin/preprodcon.pf no-error.    */
/* end.                                                 */
/* else                                                 */
/*   connect -pf p:\config\genpreproddb.pf no-error.    */
/*                                                      */


plResult = connected(pcDatabase).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConnectProduction) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectProduction Procedure 
PROCEDURE ConnectProduction :
/* if opsys = 'unix' then do:                           */
/*                                                      */
/*  if pcDataBase = 'main' or pcDataBase = 'mods' then  */
/*    connect -pf /prod/bin/syteline-gen.pf no-error.   */
/*  else                                                */
/*    connect -pf /prod/bin/genesiscon.pf no-error.     */
/*                                                      */
/* end.                                                 */
/* else                                                 */
/*   connect -pf p:\config\genesisprod.pf no-error.     */



plResult = connected(pcDatabase).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConnectTest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectTest Procedure 
PROCEDURE ConnectTest :
/* if opsys = 'unix' then do:                           */
/*                                                      */
/*   if pcDataBase = 'Main' or pcDataBase = 'Mods' then */
/*      connect -pf /test/bin/syteline-gen.pf no-error. */
/*   else                                               */
/*      connect -pf /test/bin/genesiscon.pf no-error.   */
/*                                                      */
/* end.                                                 */
/* else                                                 */
/*   connect -pf p:\config\genesisdevl.pf no-error.     */



plResult = connected(pcDatabase).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

