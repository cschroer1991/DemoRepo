&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server.common/t0364.p
    Purpose     : Finds BOL & PS file names on server

    Syntax      :

    Description :

    Author(s)   : Rick Terrell, Bravepoint, Inc.
    Created     : 07/27/05
    Notes       : plGetOrg - YES always returns .org.PDF file name
                             NO  returns rebuilt file name if exists, otherwise
                                 returns org name.
    Modified    : 10/12/05 -  fh - Issue 2888 Added check for file length   
                  12/07/05 -  fh - Look for new naming convention first.
                  02/28/06 - cel - Issue 20610 Added function FileByteCountStable.                          
                  04/10/15 - jhl - 68118 - Check archive directory if document does not exit in original directory.
                  04/29/15 - lcs - 68168 Renamed appattr CEBOLUnixDir to BOLUnixDir and CETraceLog to TraceLog.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input  parameter piLoadID   as integer    no-undo.
define input  parameter piSlipID   as integer    no-undo.
define input  parameter plGetOrg   as logical    no-undo.
define output parameter pcBOLPath  as character  no-undo.
define output parameter pcSlipPath as character  no-undo.

define variable lLogging as logical    no-undo initial no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FileByteCountStable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FileByteCountStable Procedure 
FUNCTION FileByteCountStable RETURNS LOGICAL
  ( )  FORWARD.

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
         HEIGHT             = 6.05
         WIDTH              = 51.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

  define variable cCombName  as character  no-undo.
  define variable cPathName  as character  no-undo.
  define variable cArcPath   as character  no-undo.
  define variable cBOLPrefix as character  no-undo initial "BOL".
  define variable cSlpPrefix as character  no-undo initial "Slip".
  define variable cSuffix    as character  no-undo.
  define variable cTestName  as character  no-undo.
  define variable startTime as integer     no-undo initial 0.
  define variable gcTracerFile as character no-undo.
  define stream tracer.

  assign cCombName = dynamic-function("combineLoadSlip",piLoadID,piSlipID)
         pcBOLPath  = ?
         pcSlipPath = ?
         cPathName = {fnarg getAppStr 'BOLUnixDir'}
         cArcPath  = {fnarg getAppStr 'BOLArchiveDir'}
         lLogging   = dynamic-function("getAppLog","TraceLog").

  if lLogging ne yes then
    lLogging = no.

  if lLogging then
  do:
    gcTracerFile = substitute("&1plant&2.txt",
                              (if opsys eq "UNIX" then "/tmp/" else session:temp-directory),
                              string(integer({fnarg getGlobalVar 'plantID'}),"99")).
    output stream tracer to value(gcTracerFile) append.
    put stream tracer unformatted 
      string(now) " " 
      substitute("StartGetFile BOL elapse Time: &1 ", time - startTime) skip.

  end.
  startTime = time.

  if not plGetOrg then   /* Look for rebuilt doc first */
  do:
    cSuffix = ".pdf".
    cTestName = substitute("&1&2&3&4",cPathName,cCombName,cBOLPrefix,cSuffix).
    if lLogging then
      put stream tracer unformatted cTestName skip.
    
    file-info:file-name = cTestName.

    /*   look in Archive */
    if file-info:full-pathname = ? then do:
      cTestName = substitute("&1&2&3&4",cArcPath,cCombName,cBOLPrefix,cSuffix).
      file-info:file-name = cTestName.

    end.
    if  file-info:full-pathname ne ? 
    and file-info:file-size gt 0 
    and dynamic-function('FileByteCountStable':U) = true then   /* For consistency only look for slip if BOL found */
    do:
      pcBOLPath = file-info:full-pathname.
      cTestName = substitute("&1&2&3&4",cPathName,cCombName,cSlpPrefix,cSuffix).
      if lLogging then
        put stream tracer unformatted cTestName skip.

      file-info:file-name = cTestName.

      /*  Look in Archive */
      if file-info:filename = ? then do:
         cTestName = substitute("&1&2&3&4",cArcPath,cCombName,cSlpPrefix,cSuffix).
         file-info:file-name = cTestName.
      end.
      if  file-info:full-pathname ne ? 
      and file-info:file-size gt 0 
      and dynamic-function('FileByteCountStable':U) eq true then
        pcSlipPath = file-info:full-pathname.
    end.

    if pcBOLPath eq ? then 
    do: /* Look using old naming convention */
      cTestName = substitute("&1&2&3&4",cPathName,cBOLPrefix,cCombName,cSuffix).
      if lLogging then
        put stream tracer unformatted cTestName skip.

      file-info:file-name = cTestName.

      /*   look in Archive */
      if file-info:full-pathname = ? then do:
        cTestName = substitute("&1&2&3&4",cArcPath,cBOLPrefix,cCombName,cSuffix).
        file-info:file-name = cTestName.
      end.

      if  file-info:full-pathname ne ? 
      and file-info:file-size gt 0 
      and dynamic-function('FileByteCountStable':U) eq true then   /* For consistency only look for slip if BOL found */
      do:
        pcBOLPath = file-info:full-pathname.
        cTestName = substitute("&1&2&3&4",cPathName,cSlpPrefix,cCombName,cSuffix).
        if lLogging then
          put stream tracer unformatted cTestName skip.

        file-info:file-name = cTestName.

         /*  Look in Archive */
         if file-info:filename = ? then do:
           cTestName = substitute("&1&2&3&4",cArcPath,cSlpPrefix,cCombName,cSuffix).
           file-info:file-name = cTestName.
        end.

        if  file-info:full-pathname ne ?  
        and file-info:file-size gt 0 
        and dynamic-function('FileByteCountStable':U) = true then
          pcSlipPath = file-info:full-pathname.
      end.
    end.
  end.

  /* Below if the paths are unknown, it wasn't found above because either */
  /*     1)  We passed in to only look for org doc or */
  /*     2)  We didn't find a rebuilt doc             */
  /* In any case, we set the values to the full-pathname so ? is returned if not found */
  if pcBOLPath eq ? then
  do:
    cSuffix = ".org.pdf".
    cTestName = substitute("&1&2&3&4",cPathName,cCombName,cBOLPrefix,cSuffix).
    if lLogging then
      put stream tracer unformatted cTestName skip.

    file-info:file-name = cTestName.
      
    /*   look in Archive */
    if file-info:full-pathname = ? then do:
        cTestName = substitute("&1&2&3&4",cArcPath,cCombName,cBOLPrefix,cSuffix).
        file-info:file-name = cTestName.
    end.

    if  file-info:full-pathname ne ? 
    and file-info:file-size gt 0 
    and dynamic-function('FileByteCountStable':U) eq true then 
      pcBOLPath = file-info:full-pathname.

    if pcBOLPath eq ? then /* Look using old naming convention */
    do:
      cTestName = substitute("&1&2&3&4",cPathName,cBOLPrefix,cCombName,cSuffix).
      if lLogging then
        put stream tracer unformatted cTestName skip.

      file-info:file-name = cTestName.

      /*   look in Archive */
      if file-info:full-pathname = ? then do:
        cTestName = substitute("&1&2&3&4",cArcPath,cBOLPrefix,cCombName,cSuffix).
        file-info:file-name = cTestName.
      end.

      if  file-info:full-pathname ne ? 
      and file-info:file-size gt 0 
      and dynamic-function('FileByteCountStable':U) eq true then 
        pcBOLPath = file-info:full-pathname.
    end.
  end.

  if pcSlipPath eq ? then
  do:
    cTestName = substitute("&1&2&3&4",cPathName,cCombName,cSlpPrefix,cSuffix).
    if lLogging then
      put stream tracer unformatted cTestName skip.

    file-info:file-name = cTestName.

    /* look in Archive */
    if file-info:full-pathname = ? then do:
       cTestName = substitute("&1&2&3&4",cArcPath,cCombName,cSlpPrefix,cSuffix).
       file-info:file-name = cTestName.
    end.

    if  file-info:full-pathname ne ? 
    and file-info:file-size gt 0 
    and dynamic-function('FileByteCountStable':U) eq true then 
      pcSlipPath = file-info:full-pathname.

    if pcSlipPath eq ? then /* Look using old naming convention */
    do:
      cTestName = substitute("&1&2&3&4",cPathName,cSlpPrefix,cCombName,cSuffix).
      if lLogging then
        put stream tracer unformatted cTestName skip.

      file-info:file-name = cTestName.

      /* look in Archive */
      if file-info:full-pathname = ? then do:
         cTestName = substitute("&1&2&3&4",cArcPath,cSlpPrefix,cCombName,cSuffix).
         file-info:file-name = cTestName.
      end.

      if  file-info:full-pathname ne ? 
      and file-info:file-size gt 0 
      and dynamic-function('FileByteCountStable':U) eq true then 
        pcSlipPath = file-info:full-pathname.
    end.
  end.

  if lLogging then
  do:
    put stream tracer unformatted string(now) " " 
      substitute("StartGetFile BOL elapse Time: &1 ", time - startTime)
      substitute(" BOL Path: &1 PS Path: &2",pcBOLPath,pcSlipPath) skip.
    output stream tracer close.
  end.
  
  return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FileByteCountStable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FileByteCountStable Procedure 
FUNCTION FileByteCountStable RETURNS LOGICAL
  ( ) :

  define variable i as int no-undo.
  define variable iBytes-1 as int no-undo init 1.
  define variable iBytes-2 as int no-undo init 2.

  if file-info:file-size gt 0 then 
    iBytes-1 = file-info:file-size.
  pause 1 no-message.
  if file-info:file-size gt 0 then 
    iBytes-2 = file-info:file-size.
  
  return (iBytes-1 = iBytes-2).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

