&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0022.p
    Purpose     : load security temp-table

    Syntax      :

    Description :

    Author(s)   : Rick Terrell, Bravepoint, inc.
    Created     : 03/01/2004
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Security TT def */
{common/objsectt.i}

define input  parameter pcUserID as character  no-undo.
define output parameter table for ttObjectSecurity.

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
/* TIWO 26342 - We now need to load the entire objectSecurity table */
/*              to know if there is security for any given object   */
for each objectSecurity no-lock:
  create ttObjectSecurity.
  buffer-copy objectSecurity
    to ttObjectSecurity.
end.

for each userSecurity no-lock
    where userSecurity.UID = pcUserID:

  for each ttObjectSecurity
      where ttObjectSecurity.tokenName eq userSecurity.tokenName:

    /* If this user is allowed to view and update, */
    /* then don't build record for this token      */
    if userSecurity.allowView and userSecurity.allowUpdate then
    do:
      delete ttObjectSecurity.
      next.
    end.

    if ttObjectSecurity.tokenValue eq '' then
    do:
        /* If the default is no security, is there any for this user */
        if not userSecurity.allowUpdate then
          ttObjectSecurity.tokenValue = 'ViewOnly'.
        else  /* TIWO 29209 */
        if not userSecurity.allowView then 
          ttObjectSecurity.tokenValue = 'Hidden'.
    end.
    else
    do:
      /* Now see if there are any overrides for this container */
      if ttObjectSecurity.containerName ne ''
      and ttObjectSecurity.tokenValue eq 'Hidden' 
      and userSecurity.allowView then 
        ttObjectSecurity.tokenValue = 'ViewOnly'.

      /* If they are allowed to update in this container, then no security */
      if ttObjectSecurity.containerName ne ''
      and ttObjectSecurity.tokenValue eq 'ViewOnly'
      and userSecurity.allowUpdate then
        delete ttObjectSecurity.
    end.
  end.

end.

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


