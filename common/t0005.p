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

DEF INPUT PARAMETER pcVals AS CHAR.
DEF OUTPUT PARAMETER TABLE FOR gttChoice.

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
         HEIGHT             = 7.24
         WIDTH              = 42.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR i AS INT.
DEF VAR gcField AS char.
def var giFieldNumber as int.

DO i = 1 TO NUM-ENTRIES(pcVals):
    
    gcField = ENTRY(i,pcVals).
    
    find data-field where data-field.FieldName = gcField no-lock no-error.
    if avail data-field  then
        /* Handle Field Name */
        giFieldNumber = data-field.field-no.
    else do:
        /* Handle Field number */
        giFieldNumber = int(gcField) no-error.
        if error-status:error then do:
            /* Handle when FieldName is comma delimited */
            find first data-field where can-do(data-field.FieldName, gcField)  
            no-lock no-error.
            if avail data-field then
              giFieldNumber = data-field.field-no.
        end.

        if not avail data-field then
           find first data-field where data-field.field-no = giFieldNumber no-lock no-error.  
    end.
       
    if giFieldNumber = 0 then return.

    /* kmk - 04/05 - Pull back in order by Choice Name (Description) */

    
    for each choice WHERE choice.field-no = giFieldNumber NO-LOCK BY choice.descr:
         create gttChoice.
         buffer-copy choice TO gttChoice.
         if avail data-field then
             assign gttChoice.FieldName = data-field.FieldName.
    end.
   
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


