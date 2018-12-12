&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0407.p
    Purpose     : Finds file names on server

    Syntax      :

    Description :

    Author(s)   : Jenni Egbert
    Created     : 12/28/05
    Notes       : 
    
11/30/06  jke  27591  Change equality to "Matches" for form type of "Invc"    
06/10/10  cel  50264  Modfified to pickup PODs
02/27/12 zakh  59419  Find BBL's like BOL's now.  
03/18/15 lcs   68154  Remove Crystal references.
03/31/15 jhl   68118  If POD does not exist check Archive directory.
04/29/15 lcs   68168  Renamed appattr CEBOLUnixDir to BOLUnixDir, CEInvoiceUnixDir to InvoiceUnixDir
                      and CETempUnixDir to TempUnixDir.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input  parameter piKey      as integer    no-undo.
define input  parameter piSubKey   as integer    no-undo.
define input  parameter pcFormType as character  no-undo.
define input  parameter pcFileType as character  no-undo.
define input  parameter plGetOrg   as logical    no-undo.
define output parameter pcFilename as character  no-undo.

define variable cPath as character  no-undo.
define variable cArcPath as character no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-ReturnFilename) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReturnFilename Procedure 
FUNCTION ReturnFilename RETURNS CHARACTER
  ( input pcPathname as char, input pcKeyname as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 6.86
         WIDTH              = 51.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

  define variable cCombName  as character  no-undo.
  cArcPath = ?.

  if pcFormType eq "Slip" or 
     pcFormType eq "BOL"  or 
     pcFormType eq "BBL" then
      assign 
           cCombName = dynamic-function("combineLoadSlip",piKey,piSubKey)
           cPath     = dynamic-function('GetAppAttribute':U, "BOLUnixDir", "Str").
  else if pcFormType matches "*Invc*" then
      assign
           cCombName = string(piKey) + string(piSubKey)
           cPath     = dynamic-function('GetAppAttribute':U, "InvoiceUnixDir", "Str").
  else if pcFormType eq "POD" then do:
      cPath = dynamic-function('GetAppAttribute':U, "PODServerDir", "Str").
      if not cPath matches '*/' then
          cPath = cPath + '/'.
      
      cCombName = substitute('PD&1&2', piKey, string(piSubKey, "99")).
     
  end.
  else 
      assign 
          cCombName = string(piKey)
          cPath     = dynamic-function('GetAppAttribute':U, "TempUnixDir", "Str").


pcFilename =  ReturnFilename (input cPath,
                              input cCombName).


/* If did not find file in original directory,  then check Archive dirctory */
if pcFilename = ? or pcFilename = "" then do:
 
 if pcFormType eq "Slip" or 
     pcFormType eq "BOL"  or 
     pcFormType eq "BBL" then
      assign 
           cCombName = dynamic-function("combineLoadSlip",piKey,piSubKey)
           cArcPath     = dynamic-function('GetAppAttribute':U, "BOLArchiveDir", "Str").
 else if pcFormType matches "*Invc*" then
      assign
           cCombName = string(piKey) + string(piSubKey)
           cArcPath     = dynamic-function('GetAppAttribute':U, "InvoicesArchiveDir", "Str").
 else if pcFormType = "POD" then do:
   cArcPath = dynamic-function('GetAppAttribute':U, "PODArchiveDir", "Str").
   if not cArcPath matches '*/' then
            cArcPath = cArcPath + '/'.
   cCombName = substitute('PD&1&2', piKey, string(piSubKey, "99")).
 end.


 if cArcPath <> ? then do:
      pcFilename =  ReturnFilename (input cArcPath,
                                    input cCombName).
 end.
                                
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-ReturnFilename) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReturnFilename Procedure 
FUNCTION ReturnFilename RETURNS CHARACTER
  ( input pcPathname as char, input pcKeyname as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  define variable cFilePath as character  no-undo.


  if not plGetOrg then   /* Look for rebuilt doc first */
  do:

      case pcFormType:
          when 'POD' then do:
          
             file-info:file-name = substitute("&1&2&3&4",pcPathname,cCombName,pcFileType).
             cFilePath = file-info:full-pathname.
             
          end.
          otherwise do:
             file-info:file-name = substitute("&1&2&3&4",pcPathname,cCombName,pcFormType,pcFileType).
             if file-info:full-pathname ne ? and file-info:file-size > 0 then   /* For consistency only look for slip if BOL found */
               cFilePath = file-info:full-pathname.

             else do: /* Look using old naming convention */
               file-info:file-name = substitute("&1&2&3&4",pcPathname,pcFormType,cCombName,pcFileType).

               if file-info:full-pathname ne ? and file-info:file-size > 0
                 then cFilePath = file-info:full-pathname.
             end.
          end. /* otherwise */
             
      end case.
      

      
  end. /* not plGetOrg */


  /* Below if the paths are unknown, it wasn't found above because either */
  /*     1)  We passed in to only look for org doc or */
  /*     2)  We didn't find a rebuilt doc             */
  /* In any case, we set the values to the full-pathname so ? is returned if not found */
  if cFilePath eq ? and pcFormType <> 'POD' then
  do:
      pcFileType = ".org" + pcFileType.
      file-info:file-name = substitute("&1&2&3&4",pcPathname,cCombName,pcFormType,pcFileType).

      if file-info:full-pathname ne ? and file-info:file-size > 0
          then cFilePath = file-info:full-pathname.

      else do: /* Look using old naming convention */

          file-info:file-name = substitute("&1&2&3&4",pcPathname,pcFormType,cCombName,pcFileType).

          if file-info:full-pathname ne ? and file-info:file-size > 0
              then cFilePath = file-info:full-pathname.

      end.
  end.

  
  return cFilePath.




END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

