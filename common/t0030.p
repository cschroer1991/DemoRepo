&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0030.p
    Purpose     : Create Audit Record
    Author(s)   : Rick Terrell, Bravepoint, Inc.
    Created     : 03/22/04
    Notes       : Based on a previous work in progress.
                  Written as a stand-alone .p to avoid any buffer, transaction,
                  and/or locking scope issues that may be associated with this
                  running in a persistent procedure.  Buffer parameters are
                  treated as a "free reference", so their scope is to the
                  entire procedure.  This could create problems in a PP.
    History     : 
10/25/05 jhl        Added message when pcLogon is null or blank to write to appserver server log.
10/09/18 zakh 74210 Kicks out if called by t0827.p

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input parameter pcTableName as character  no-undo.
define input parameter phNewBuffer as handle     no-undo.
define input parameter phOldBuffer as handle     no-undo.
define input parameter pcLogon     as character  no-undo.
define input parameter pcTranType  as character  no-undo.
define input parameter pcCallProgram as character  no-undo.

define variable cBufferValue as character  no-undo.
define variable cAuditDelim  as character  no-undo initial "|".
define variable cKeyValue    as character  no-undo.
define variable cQueryWhere  as character  no-undo.
define variable dtAuditDate  as date       no-undo.
define variable hFieldNew    as handle     no-undo.
define variable hFieldOld    as handle     no-undo.
define variable i            as integer    no-undo.
define variable iAuditTime   as integer    no-undo.
define variable j            as integer    no-undo.
define variable cCallProgram as char no-undo.

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
assign
    cCallProgram = "":u.

def var h as handle no-undo.
def var cDir as char no-undo.

if program-name(3) matches "*t0827*" or program-name(4) matches "*t0827*"
    or program-name(5) matches "*t0827*" then
    return.

if pcTranType = "":u or pcTranType = ? or
    not can-do(this-procedure:internal-entries,"log":u + pcTranType)
    then return "**":u + pcTranType + " of table " + pcTableName + " not supported in " + program-name(1) + ".".
else run value("log":u + pcTranType).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-logDelete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE logDelete Procedure 
PROCEDURE logDelete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  29360 - Make sure that pcLogon is set.
------------------------------------------------------------------------------*/
    define buffer bAuditConfig for auditCfg.
    define buffer bAuditLog for Audit.
    
    /* First find out if this table is audited */
    find bAuditConfig no-lock
         where bAuditConfig.tableName = pcTableName
         no-error.
    
    if not available bAuditConfig 
/*         or bAuditConfig.auditFlag = no */  /* Allow delete only auditing */
        or bAuditConfig.auditDelete = no
        then return.
    
    cKeyValue = ''.
    do i = 1 to num-entries(bAuditConfig.keyFields):
        hFieldNew = phOldBuffer:buffer-field(entry(i,bAuditConfig.keyFields)).
        if valid-handle(hFieldNew) then
            cKeyValue = cKeyValue 
                      + (if i eq 1 then '' else cAuditDelim)
                      + (if hfieldNew:buffer-value ne ? then string(hfieldNew:buffer-value) else "?").
        else
            return "Bad Key Field definition for table:  " + pcTableName.
    end.
    
    assign dtAuditDate = today
           iAuditTime  = time.
    
    if pcLogon = ? or pcLogon = "" then pcLogon = 'Unknown'.
    DO for bAuditLog transaction i = 1 to phOldBuffer:num-fields :
    
      hFieldOld = phOldBuffer:buffer-field(i).
    
      repeat j = (if hFieldOld:extent eq 0 then 0 else 1) to hFieldOld:extent:
    
         create bAuditLog.
         assign bAuditLog.TableName = pcTableName
                bAuditLog.KeyValue  = cKeyValue
                bAuditLog.Logon     = pcLogon
                bAuditLog.unuseddate = dtAuditDate
                bAuditLog.unusedint = iAuditTime
                bAuditLog.AuditDateTime = now
                bAuditLog.UpdField  = hFieldOld:name 
                                    + (if hFieldOld:extent eq 0 then '' 
                                       else '[' + string(j) + ']')
                bAuditLog.unused = "DEL:" + pcCallProgram
                bAuditLog.updValue  = string(hFieldOld:buffer-value(j)).
    
      end.   /* REPEAT j */
    
    End.  /* DO for bAuditLog transaction i = 1 to phOldBuffer:num-fields */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-logWrite) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE logWrite Procedure 
PROCEDURE logWrite :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    
  History:  10/25/05 - jhl - Added message to appserver log when pcLogon is null or blank.   
------------------------------------------------------------------------------*/
    define buffer bAuditConfig for auditCfg.
    define buffer bAuditLog for Audit.
    
    /* First find out if this table is audited */
    find bAuditConfig no-lock
         where bAuditConfig.tableName = pcTableName
         no-error.
    
    if not available bAuditConfig or bAuditConfig.auditFlag = no then
        return.
    
    cKeyValue = ''.
    do i = 1 to num-entries(bAuditConfig.keyFields):
        hFieldNew = phNewBuffer:buffer-field(entry(i,bAuditConfig.keyFields)).
        if valid-handle(hFieldNew) then
            cKeyValue = cKeyValue 
                      + (if i eq 1 then '' else cAuditDelim)
                      + (if hfieldNew:buffer-value ne ? then string(hfieldNew:buffer-value) else "?").
        else
            return "Bad Key Field definition for table:  " + pcTableName.
    end.
    
    assign dtAuditDate = today
           iAuditTime  = time.
    
    /* Compare buffers and Audit if not the same */
    
    
    
    if not phNewBuffer:buffer-compare(phOldBuffer) then
    DO for bAuditLog transaction i = 1 to phOldBuffer:num-fields :
    
      hFieldOld = phOldBuffer:buffer-field(i).
      hFieldNew = phNewBuffer:buffer-field(i).
    
    
    
    
      if can-do(bAuditConfig.auditField,hFieldOld:name) or bAuditConfig.auditField = "*" then
      repeat j = (if hFieldOld:extent eq 0 then 0 else 1) to hFieldOld:extent:
    
    /*       MESSAGE hFieldold:NAME  j " old = " hfieldold:BUFFER-VALUE(j) skip               */
    /*               hFieldNew:NAME  j " new = " hfieldnew:BUFFER-VALUE(j) view-as alert-box. */
    
          if hFieldOld:buffer-value(j) <> hFieldNew:buffer-value(j) then
          do:
    
             if pcLogon = ? or pcLogon = "" then pcLogon = 'Unknown'.
                
             create bAuditLog.
             assign bAuditLog.TableName = pcTableName
                    bAuditLog.KeyValue  = cKeyValue
                    bAuditLog.Logon     = pcLogon
                    bAuditLog.unuseddate = dtAuditDate
                    bAuditLog.unusedint = iAuditTime
                    bAuditLog.AuditDateTime = now
                    bAuditLog.UpdField  = hFieldOld:name 
                                        + (if hFieldOld:extent eq 0 then '' 
                                           else '[' + string(j) + ']')

                    bAuditLog.unused = "WRT:" + pcCallProgram
                    bAuditLog.updValue  = string(hFieldOld:buffer-value(j)).
    
          end.   /* IF hFieldOld:BUFFER-VALUE(j) <> hFieldNew:BUFFER-VALUE(j) */
      end.   /* REPEAT j */
    
    End.  /* DO for bAuditLog transaction i = 1 to phOldBuffer:num-fields */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

