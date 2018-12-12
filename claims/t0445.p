&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : claims/t0445.p
    Purpose     : Return the SpecID of a claim.

    Syntax      :

    Description :

    Author(s)   : Chris Longo
   
07/31/06 -cel- Issue 24820 Created.

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input parameter piClaimID as int.
define output parameter piSpecID as int.
define output parameter piPartnerID as int.

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

find claim where claim.claim-id = piClaimID no-lock no-error.

find first claim-item of claim no-lock no-error.
if avail claim-item then do:


   find inv where inv.inv-id = claim-item.inv-id no-lock.
   
   find work-item where work-item.order-id = claim.order-id and
                        work-item.part-id = inv.part-id no-lock no-error.
   if avail work-item then
     assign piSpecID = work-item.spec-id
            piPartnerID = work-item.partner-id.

           

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


