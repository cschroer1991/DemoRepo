&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0470.p
    Purpose     : Log file parser

    Syntax      :

    Description :

    Author(s)   : Rick Terrell, Bravepoint, Inc.
    Created     : 01/16/07
    History     :
  Date   Pgmr Issue Description
-------- ---- ----- ----------------------------------------------------
01/16/07 rct  30295 Initial code    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{includes/ttlogdata.i}

define input  parameter ptStartDate as date       no-undo.
define input  parameter ptEndDate   as date       no-undo.
define output parameter pcRtnMsg    as character  no-undo.
define output parameter table for ttLogData.

define variable cASLog   as character  no-undo.
define variable cOSCmd   as character  no-undo.
define variable cLineIn  as character  no-undo.
define variable cLineOut as character  no-undo.
define variable cLogFile as character  no-undo.
define variable iIdx     as integer    no-undo.
define variable iPlantID as integer    no-undo.
define variable iPos     as integer    no-undo.
define variable iSeq     as integer    no-undo.

define stream stLogIn.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fileSizeStable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fileSizeStable Procedure 
FUNCTION fileSizeStable RETURNS LOGICAL
  ( input pcFileName as character )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
assign iPlantID = integer({fnarg getGlobalVar 'plantID'})
       cASLog   = replace(log-manager:logfile-name,"~\","~/")
       iSeq     = 1.

file-info:file-name = cASLog.
if file-info:full-pathname eq ? then
do:
  pcRtnMsg = substitute("Cannot open log file: &1.",cASLog).
  return.
end.

/* Since the appserver has the log file open, */
/* we won't be able to read it directly plus  */
/* it has more entries than we want anyway.   */
/* grep it out to another file for parsing    */
assign cASLog = file-info:full-pathname
       cLogFile = substitute("&1&2&3&4.log",
                             session:temp-directory,
                             {fnarg getGlobalVar 'userID'},
                             iPlantID,
                             replace(replace(entry(1,iso-date(now),"."),":",""),"-","")).

cOSCmd = substitute("grep -F '*' &1 > &2",cASLog,cLogFile).
message cOSCmd.
os-command /* silent no-console */ value(cOSCmd).

do while not(fileSizeStable(cLogFile)):
  pause 1 no-message.  /* give time for os to close file */
end.

file-info:file-name = cLogFile.
if file-info:full-pathname eq ? then
do:
  pcRtnMsg = substitute("Cannot open parse file: &1.",cLogFile).
  return.
end.
/* pcRtnMsg = cLogFile. */

input stream stLogIn from value(cLogFile).

repeat:
  import stream stLogIn unformatted cLineIn.
  if cLineIn gt "" then
    run parseLine(cLineIn).
end.

input stream stLogIn close.

/* os-delete value(cLogFile). */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-parseLine) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parseLine Procedure 
PROCEDURE parseLine :
/*------------------------------------------------------------------------------
  Purpose:     Parse a line of log file
  Parameters:  pcInfoIn - line to parse
  Notes:       Writes a ttLogData record if passes scrutiny.
               Chops down line as it goes.
------------------------------------------------------------------------------*/
define input  parameter pcInfoIn as character  no-undo.

define variable IPUDFName    as character  no-undo.
define variable cProgramName as character  no-undo.
define variable cWrk     as character  no-undo.
define variable tWrkDate as datetime   no-undo.
define variable iErrorNo as integer    no-undo.
define variable iLineNo  as integer    no-undo.
define variable iTmp     as integer    no-undo.

define buffer bLogData for ttLogData.

/* Valid lines begin with [ */
if substring(pcInfoIn,1,1) ne "[" then
  return.

tWrkDate = datetime(integer(substring(pcInfoIn,5,2)),
                    integer(substring(pcInfoIn,8,2)),
                    2000 + integer(substring(pcInfoIn,2,2)),
                    integer(substring(pcInfoIn,11,2)),
                    integer(substring(pcInfoIn,14,2)),
                    integer(substring(pcInfoIn,17,2))).

if tWrkDate eq ? then  /* invalid datetime */
  return.

if ptStartDate ne ?
and date(tWrkDate) lt ptStartDate then
  return.
if ptEndDate ne ?
and date(tWrkDate) gt ptEndDate then
  return.

/* Any error lines have an error number in parentheses at the end */
if substring(pcInfoIn,length(pcInfoIn),1) ne ")" then
  return.

pcInfoIn = trim(substring(pcInfoIn,index(pcInfoIn,"(") + 1)).

/* If we're not looking at a procedure, bail */
if not(entry(1,pcInfoIn," ") begins "Procedure:") then
  return.

/* Now that we have what looks like a correct line, parse * build */
do for bLogData:

  iPos = index(pcInfoIn,"'").
  if iPos gt 0 then
    pcInfoIn = trim(substring(pcInfoIn,iPos + 1)).

  cWrk = trim(entry(1,pcInfoIn,"'")).
  if num-entries(cWrk," ") gt 1 then
    assign IPUDFName = trim(entry(1,cWrk," "))
           cProgramName = trim(entry(2,cWrk," ")).
  else
    assign IPUDFName = ""
           cProgramName = cWrk.

  /* Trim off up to & including Line: */
  iPos = index(pcInfoIn,":").
  if iPos gt 0 then
    pcInfoIn = trim(substring(pcInfoIn,iPos + 1)).

  assign iLineNo = ?
         iLineNo = integer(trim(entry(1,pcInfoIn,")"))) no-error.

  pcInfoIn = trim(substring(pcInfoIn,index(pcInfoIn,")") + 1)).

  /* Now we are down to the error message and number   */
  /* Since we don't know what is in the message, (e.g. */
  /* there may be embedded parens, start at the right and get the number. */

  assign cWrk = ""
         iIdx = length(pcInfoIn) - 1.
  do iPos = iIdx to 1 by -1:
    if substring(pcInfoIn,iPos,1) eq "(" then
      leave.
    cWrk = substring(pcInfoIn,iPos,1) + cWrk.
  end.

  assign iErrorNo = ?
         iErrorNo = integer(cWrk) no-error.

  iPos = iPos - 1.

  if not(can-find(first ttLogData
                  where ttLogData.procName eq cProgramName
                  and   ttLogData.lineNum  eq iLineNo
                  and   ttLogData.errNum   eq iErrorNo)) then
  do:
    create bLogData.
    assign bLogData.seq      = iSeq
           bLogData.plantID  = iPlantID
           bLogData.logDate  = tWrkDate
           bLogData.procName = cProgramName
           bLogData.IPName   = IPUDFName
           bLogData.lineNum  = iLineNo
           bLogData.errNum   = iErrorNo
           bLogData.errText  = trim(left-trim(substring(pcInfoIn,1,iPos)," *"))
           bLogData.done     = no
           iSeq              = iSeq + 1.
  end.
end.

return.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fileSizeStable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fileSizeStable Procedure 
FUNCTION fileSizeStable RETURNS LOGICAL
  ( input pcFileName as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  define variable iFileSize as integer    no-undo.

  file-info:file-name = pcFileName.

  if file-info:full-pathname eq ? then
    return true.  /* return true and let main block handle */

  iFileSize = file-info:file-size.
  pause 1 no-message.

  return (iFileSize eq file-info:file-size).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

