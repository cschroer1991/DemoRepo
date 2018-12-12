&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/showappseerver.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
   02/07/06  Issue 3693 - cel - Added logic to show buffer private data this
             might be used for resolving memory leaks.
   05/08/08  TIWO 41909 - jhl - Added logic to display PID of appserver and increased
                                Buffers to 1000.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define output parameter pcInfo as character  no-undo.

define variable cCharTmp1 as character  no-undo.
define variable cCharTmp2 as character  no-undo.
define variable cSysName  as character  no-undo.
define variable hHdlTmp1  as handle     no-undo.
define variable iIdx      as integer    no-undo.

define variable cBuffers as char no-undo.

define variable hBuffer as handle no-undo.
define variable hTempTable as handle no-undo.
define variable cTables as character no-undo.
define variable k as int no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getPID) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPID Procedure 
FUNCTION getPID RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

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
/* MESSAGE "Running showAppServer.p" cSuper VIEW-AS ALERT-BOX. */

if opsys = "UNIX" then
do:
  input through uname -n no-echo.
  set cSysName.
  input close.
end.
else
  cSysName = "local".

assign pcInfo = "Session Handle: "
              + string(session:handle)
              + (if can-query(session,"server-connection-id") and session:server-connection-id gt ''
                 then "~nConnection ID: " + session:server-connection-id
                 else "")
              + substitute("~n~nPID: &1", getPID())
              + substitute("~n~nConnected databases (on &1):~n",cSysName) 
             .

do iIdx = 1 to num-dbs:
    pcInfo = pcInfo 
           + "   " 
           + pdbname(iIdx)
           + " connected as "
           + ldbname(iIdx)
           + "~n".
end.

assign pcInfo = pcInfo + "~nSuper Procedures:~n"
       cCharTmp1 = ''
       hHdlTmp1 = session:first-procedure.

do while valid-handle(hHdlTmp1):
    if can-do(session:super-procedures,string(hHdlTmp1)) then
        cCharTmp1 = cCharTmp1
                  + "   " + hHdlTmp1:file-name + "~n".
    else 
        cCharTmp2 = cCharTmp2
                  + "   " + hHdlTmp1:file-name + "~n".

    hHdlTmp1 = hHdlTmp1:next-sibling.
end.

assign pcInfo   = pcInfo 
                + cCharTmp1 
                + "~nPersistent Procedures running:~n"
                + cCharTmp2 
                + "~nGlobal Vars:~n"
       cCharTmp1 = ''
       cCharTmp2 = ''.

if  can-query(session,"server-connection-id")
and session:server-connection-id ne ? 
and session:server-connection-id ne '' then
     run common/getallsesglobals.p (output cCharTmp1).
else   /* Running C/S so no db values */
do:
    run getAllGlobalVars (output cCharTmp1).
    assign cCharTmp1 = "   " + replace(cCharTmp1,chr(4)," = ")
           cCharTmp1 = replace(cCharTmp1,chr(3),"~n   ") + "~n".
end.

hBuffer = SESSION:FIRST-BUFFER.

cTables = "~nBuffers: ".

blk1: 
do while hBuffer ne ? 
on error undo, leave blk1:

  k = k + 1.

  if k <= 1000 then 
  do:
    if hBuffer:table-handle eq ? THEN
      assign cTables = cTables + "~n   " 
                     + hBuffer:table 
                     + " Table Number: " + string(hBuffer:table-number)
        /*
                     + " Recid: " + (if string(hBuffer:recid) eq ? then "Unknown" else string(hBuffer:recid)) 
                     + substitute(" &1 ",string(hBuffer:dynamic,"Dynamic/Static")) 
                     + (if valid-handle(hBuffer:instantiating-procedure) then
                        substitute(" By: &1 ",hBuffer:instantiating-procedure:file-name) 
                        else "XXX") 
                     + hBuffer:private-data  */
                     .
    else 
    do:
      hTempTable = hBuffer:TABLE-HANDLE.
       
      assign cTables = cTables + "~n   " 
                     + hTempTable:name + " TempTable" 
          /*
                     + substitute(" &1 ",string(hTempTable:dynamic,"Dynamic/Static"))
                     + (if valid-handle(hTempTable:instantiating-procedure) then
                        substitute(" By: &1 ",hTempTable:instantiating-procedure:file-name)
                        else "***") 
                     + hBuffer:private-data */
                     .
    end.
  end.

  hBuffer = hBuffer:next-sibling. 

end.

if k ge 50 then 
  run deleteBuffers.

if cTables eq ? then 
  cTables = '~nBuffers: UnknownValue ~n'.

cTables = cTables + "~n   Number of Buffers in Memory: " + string(k).

assign pcInfo = pcInfo
              + cCharTmp1
              + cTables
              + "~n~nPropath:~n"
              + "   " + replace(propath,",","~n   ")
       cCharTmp1 = ''.

pcInfo = pcInfo 
       + substitute("~n~nTemp Directory: &1~n",session:temp-directory).

pcInfo = pcInfo
       + substitute("~nLog Info:&1   File Name: &2&1   Entry Types: &3&1   Types List: &4&1   Level: &5&1   Threshold: &6&1",
                       "~n",
                       log-manager:logfile-name,
                       log-manager:log-entry-types,
                       log-manager:entry-types-list,
                       log-manager:logging-level,
                       log-manager:log-threshold).
    
return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-deleteBuffers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteBuffers Procedure 
PROCEDURE deleteBuffers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR hBuffer1 AS HANDLE.
DEFINE VAR hBuffer2 AS HANDLE.
DEFINE VAR hDataset AS HANDLE.
DEFINE VAR hTableHandle AS HANDLE.
DEFINE VAR cTables AS CHAR.
DEFINE VAR hTempTable AS HANDLE.

hBuffer1 = SESSION:FIRST-BUFFER.

blk1: DO WHILE hBuffer1 <> ?:

  hBuffer2 = hBuffer1.
  hBuffer1 = hBuffer1:NEXT-SIBLING.
  
  IF hBuffer2:TABLE-HANDLE = ? THEN 
      DELETE OBJECT hBuffer2.
  ELSE DO:
    
    hTableHandle = hBuffer2:TABLE-HANDLE.

    hDataSet = hBuffer2:DATASET.
    IF hDataSet <> ? THEN DO: 
        DELETE OBJECT hDataSet.
        hBuffer1 = SESSION:FIRST-BUFFER.
    END.

    IF VALID-HANDLE(hTableHandle) THEN DELETE OBJECT hTableHandle.
  END.
      

 

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getPID) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPID Procedure 
FUNCTION getPID RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var myPid     as integer no-undo init -1.
   def var hConBuf   as handle  no-undo.
   def var tableName as char    no-undo.
   def var ix        as integer no-undo.

   /* loop connected dbs */      
   dbBlock:   
   do ix = 1 to num-dbs
      :
      /* only consider PSC DBs */
      if dbtype(ix) eq 'Progress' then /* doc says 'OpenEdge' */
      do: 
         /* get handle to the _myconnection meta-schema table */
         assign 
            tablename = subst('&1._myconnection',ldbname(ix))
            .
         
         create buffer hConBuf for table tablename no-error.
         
         /* if we have a valid handle, retrieve the record */
         if valid-handle(hConBuf) then
         do:
            hConBuf:find-first('',no-lock).
            
            /* return the PID (_myconn-pid) when the record is found */
            if hConBuf:available then
               assign myPid = hConBuf::_myconn-pid.
            
            /* release the retrieve buffer : this probably isn't necessary */
            hConBuf:buffer-release no-error.
         end. /* valid-handle */
         
         /* clean up the buffer handle */
         delete object hConBuf no-error.   
         assign hConBuf = ?.
         
         /* we have our PID : leave */
         leave dbBlock.
      end. /* dbtype = PSC */
   end. /* dbBlock */
      
   return myPid.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

