&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0389.p
    Purpose     : Retrieve Groups for a user

    Syntax      :

    Description :

    Author(s)   : Rick Terrell, Bravepoint, Inc.
    Created     : 10/21/05
    Notes       :
    History     : 10/21/05 - rct - Issue 2953 Initial code
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{includes/gttgroup.i}

define input  parameter pcUserID as character  no-undo.
define output parameter table for gttGroup.

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
find psi-user no-lock
     where psi-user.logon eq pcUserID no-error.

if available psi-user then
do:
  for each groupXref no-lock
      where groupXref.user-Id eq psi-user.PSIUserID,
      each groups no-lock
           where groups.groupID eq groupXref.groupID:
    create gttGroup.
    assign gttGroup.usrID   = pcUserID
           gttGroup.grpName = groups.descr
           gttGroup.grpID   = groupXref.groupID
           gttGroup.grpType = groupXref.type.
  end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


