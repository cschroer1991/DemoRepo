&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0491.p
    Purpose     : Concatenate PDF files.

    Syntax      :

    Description : This program will accept two individual files to be combined, 
                  or you can just pass a space separated list into the first 
                  parameter and it will combine all of the files in the list.  
                  If you pass in a list into the first parameter, the program 
                  will ignore the second parameter.  Also, it will not check to
                  see if the file exists when a list is passed in.  You will 
                  want to check that when you build the list.

    Author(s)   : C. Longo
    Created     : 04/25/07
    Notes       :

10/06/11 zakh 57494 added ability to combine several pdfs at once.

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */




define input parameter pcSourcePDF1 as char no-undo.
define input parameter pcSourcePDF2 as char no-undo.
define input parameter pcTargetPDF1 as char no-undo.
define output parameter pcInfo as char no-undo.


define var cCommandLine as char no-undo.

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

define var cJoinPDFCommand as char no-undo.
define var cTargetPDF1 as char no-undo.


find AppAttr where AppAttr.Attrkey = 'JoinPDFCommand' no-lock no-error.
if avail AppAttr then 
    cJoinPDFCommand = AppAttr.AppStr.

else do:
    pcInfo = "Could not find AppAttr JoinPDFCommand. Join Failed.".
    return.
end.

if num-entries(pcSourcePDF1, " ") = 1 then do:
    file-info:file-name = pcSourcePDF1.
    if file-info:file-size = ? then do:
       pcInfo = 'PDF Concatenation failed. Could not locate: ' + pcSourcePDF1.
       return.
    end.
    
    file-info:file-name = pcSourcePDF2.
    if file-info:file-size = ? then do:
       pcInfo = 'PDF Concatenation failed. Could not locate: ' + pcSourcePDF2.
       return.
    end.
    
    
    if pcTargetPDF1 = pcSourcePDF1 then 
      cTargetPDF1 = pcSourcePDF1 + 'cat'.
    else if pcTargetPDF1 = pcSourcePDF2 then
      cTargetPDF1 = pcSourcePDF2 + 'cat'.
    else
      cTargetPDF1 = pcTargetPDF1.
    
    cCommandLine =  cJoinPDFCommand + ' ' + cTargetPDF1 +
        ' ' + pcSourcePDF1 + ' ' + pcSourcePDF2.
    
    message program-name(1) cCommandLine
      view-as alert-box info buttons ok.
    
    
    os-command silent value(cCommandLine).
    
    
    file-info:file-name = cTargetPDF1.
    
    if file-info:file-size = ? then
        pcInfo = 'PDF Concatenation failed.'.
    else do:
        if pcTargetPDF1 = pcSourcePDF1 then do:
            os-copy value(cTargetPDF1) value(pcSourcePDF1).
            os-delete value(cTargetPDF1).
        end.
        if pcTargetPDF1 = pcSourcePDF2 then do:
            os-copy value(cTargetPDF1) value(pcSourcePDF2).
            os-delete value(cTargetPDF1).
        end.
    
        pcInfo = 'Created: ' + pcTargetPDF1.
    end.
end.
else do: /*pcSourcePDF1 is a list*/
    cCommandLine =  cJoinPDFCommand + ' ' + pcTargetPDF1 +
        ' ' + pcSourcePDF1.
    os-command silent value(cCommandLine).

    file-info:file-name = pcTargetPDF1.
    if file-info:file-size = ? then
        pcInfo = 'PDF Concatenation failed.'.
    else do:
        pcInfo = 'Created: ' + pcTargetPDF1.
    end.
end.

message program-name(1) pcInfo
  view-as alert-box info buttons ok.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


