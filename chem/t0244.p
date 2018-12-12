&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : chem/t0244.p
    Purpose     : server side tasks for chemistry data

    Syntax      :

    Description :

    Author(s)   : Bruce Thompson
    Created     : 
    Notes       : ipAction entries:
                  1 = Action to be performed 
                  (NOTE: Must be the name of an internal procedure.
                  2...n = as needed by action IP
   Modifications:                 
   10/18/05 - kmk - Do NOT put inv-id on Chem Copied new ChemMstr.
   11/28/05 - kmk - Do NOT try to find Mechanicals by Heat lot and
                    ne owner-id.
   03/16/06 -  fh - TIWO 21174 Modify to allow find by heat-lot and 
                    owner-id if inv not found in IP chemByHeatLot
                    as needed by Minster Lab
                    
   05/05/06 - cel - TIWO 22580 Remove message when no Mech/Chem exists
                    See getMechTestId getChemTestId.
                    
   11/19/07 - blk - TIWO 38552 - Look for t-indx by prev-inv-id in 
                    getMechTestId.
   12/04/08 - djt - TIWO 45491 - Rework procedure getChemTestID to look
                    by Mill ID.  No longer returns a status '2' (copyable)  
   12/15/10 - nlbu  Tiwo 54685-For a package, show the results against the master coil
                    if there are results to be seen and there are none against the package.
   12/05/11   djt   57829 Chem/Mech Audit Project                       
   02/29/12   djt   59673 - add routine to fetch default uom
   02/26/14   djt   65128 - automotive cert program
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input  parameter ipAction as char no-undo.
define output parameter opRtn as char no-undo.

define buffer bChemMstr for ChemMstr.

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

if ipAction = "":U or 
   ipAction = ?    or
   not can-do(this-procedure:internal-entries,entry(1,ipAction)) then 
    opRtn = "**Invalid action in ":U + program-name(1).
else 
    run value(entry(1,ipAction)).

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CheckForAudits) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckForAudits Procedure 
PROCEDURE CheckForAudits :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var iPlant as int.
assign iPlant = int(dynamic-function('getGlobalVar','plant-id')).

if entry(2, ipAction) = "RcptAudit" then
do:
    find first rcpt no-lock 
        where rcpt.rcpt-id eq int(entry(3, ipAction)) no-error.
    if available rcpt then
    do:
        if can-find (first ChemAudit no-lock
                        where ChemAudit.heat-lot eq rcpt.heat-lot
                          and ChemAudit.plant-id eq rcpt.plant-id) then
            assign opRtn = opRtn + "~nChemistry Audit Found for this Heat".
        if can-find (first MechAudit no-lock
                        where MechAudit.inv-id   eq rcpt.rcpt-id
                          and MechAudit.plant-id eq rcpt.plant-id) then
            assign opRtn = opRtn + "~nMechanical Audit Found for this Rcpt".
    end.
end.

if entry(2, ipAction) = "MechAudit" then
do:
    find first t-indx no-lock 
        where t-indx.test-id eq int(entry(3, ipAction)) no-error.
    if available t-indx then
    do:
        if can-find (first ChemAudit no-lock
                        where ChemAudit.heat-lot eq t-indx.heat-lot
                          and ChemAudit.plant-id eq iPlant) then
            assign opRtn = opRtn + "~nChemistry Audit Found for this Heat Lot".
        if can-find (first RcptAudit no-lock
                        where RcptAudit.inv-id   eq t-indx.inv-id
                          and RcptAudit.plant-id eq iPlant) then
            assign opRtn = opRtn + "~nReceipt Audit Found for this Rcpt".
    end.
end.

if entry(2, ipAction) = "ChemAudit" then
do:
    find first ChemMstr no-lock 
        where chemMstr.ChemTestID eq int(entry(3, ipAction)) no-error.
    if available ChemMstr then
    do:
      for each rcpt no-lock where rcpt.heat-lot eq chemMstr.heat-lot
                              and rcpt.plant-id eq iPlant:
        if can-find (first MechAudit no-lock
                        where MechAudit.inv-id   eq rcpt.rcpt-id
                          and MechAudit.plant-id eq rcpt.plant-id) then
            assign opRtn = opRtn + "~nMechanical Audit Found for this Heat Lot".
        if can-find (first RcptAudit no-lock
                        where RcptAudit.inv-id   eq rcpt.rcpt-id
                          and RcptAudit.plant-id eq rcpt.plant-id) then
            assign opRtn = opRtn + "~nReceipt Audit Found for this Heat Lot".
      end.
    end.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ChemAuditPass) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChemAuditPass Procedure 
PROCEDURE ChemAuditPass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

find first ChemMstr
    where ChemMstr.ChemTestID = int(entry(2, ipAction)) no-error.
if not avail ChemMstr then do:
    assign opRtn = '** Failed to update ChemMstr Record.'.
end.
else do:
    assign ChemMstr.AuditBy = entry(3,ipAction).
    delete from ChemAudit where chemAudit.ChemTestID = ChemMstr.ChemTestID.
    for each ChemMeas
        where ChemMeas.chemtestid = chemmstr.chemtestid
        and ChemMeas.ref-no begins '=':
        ChemMeas.ref-no = replace(ChemMeas.ref-no,"=","").
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-chemByHeatLot) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chemByHeatLot Procedure 
PROCEDURE chemByHeatLot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

run invHeatLot.

/* Reset input (properly formatted) */
if opRtn <> "-1" then do:
    assign 
        ipAction = "," + opRtn
        .
end.
else do:
    if num-entries(ipAction,"|") = 2 then do: /* from client/chem/d0120.w */
        ipAction = "," + entry(2,ipAction,"|").
    end.
end.

/* Reset output */
assign 
    opRtn = "".

run getChemTestID.

return.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ChemByID) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChemByID Procedure 
PROCEDURE ChemByID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable voChemTestId  as character no-undo initial "0,?,?,?,?,?,?,0,O".

  find first chemMstr no-lock
       where chemMstr.chemtestid = int(entry(2,ipAction)) no-error.
  
  if available chemMstr then 
      voChemTestId = substitute("1,&1,&2,&3,&4,&5,&6,&7,&8",
                              string(chemMstr.chemTestId),
                              chemMstr.logon,
                              string(chemMstr.t-date,"99/99/99") + ' ' + string(chemMstr.t-time,"9999"),
                              string(chemMstr.tran-no),
                              chemMstr.heat-lot,
                              string(chemmstr.mill-id),
                              '0',
                              (if chemmstr.AuditBy = ? or chemmstr.auditby = '' then 'O' else 'P')).

  opRtn = voChemTestId.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyChemMstr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyChemMstr Procedure 
PROCEDURE copyChemMstr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  12/04/08 - djt - since chems now stored by mill-id, this should
                           normally no longer be used...     
------------------------------------------------------------------------------*/
  DEFINE BUFFER   bChemMstr FOR chemMstr.
  DEFINE BUFFER   bChemMeas FOR chemMeas.

  DEFINE VARIABLE viChemTestId  AS INT.
  DEFINE VARIABLE viMillid     AS INT.
  DEFINE VARIABLE viHeatLot     AS CHAR.
  DEFINE VARIABLE viInvId       AS INT.
  
  viChemTestId = INT(ENTRY(2,ipAction)).
  viHeatLot    = ENTRY(3,ipAction).
  viMillid    = INT(ENTRY(4,ipAction)).
  viInvId      = INT(ENTRY(5,ipAction)).

  DO TRANSACTION:
    FIND chemMstr WHERE
         chemMstr.chemTestId = viChemTestId NO-LOCK NO-ERROR.
    IF AVAILABLE chemMstr THEN
    DO:
      CREATE bChemMstr.
      BUFFER-COPY chemMstr 
           EXCEPT chemMstr.chemTestId 
                  chemMstr.mill-id 
                  chemMstr.inv-id
               TO bChemMstr.
      ASSIGN 
          bChemMstr.chemTestId = NEXT-VALUE(chemTestID)
          bChemMstr.heat-lot   = viHeatLot
          bChemMstr.mill-id   = viMillid
          .
      FOR EACH chemMeas WHERE chemMeas.chemTestId = chemMstr.chemTestId NO-LOCK:
         CREATE bChemMeas.
         BUFFER-COPY chemMeas EXCEPT chemMeas.chemTestId TO bChemMeas.
         ASSIGN bChemMeas.chemTestId = bChemMstr.chemTestId.
      END.

      opRtn = STRING(bChemMstr.chemTestId).

    END. /* Available chemMstr */
    ELSE 
    DO:
      RETURN ERROR 
          "Unable to find chemMstr record using chemTestId of " + STRING(viChemTestId).
    END. /* Not available chemMstr */
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyTindx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyTindx Procedure 
PROCEDURE copyTindx :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-indx FOR T-Indx.
  DEFINE BUFFER b-meas FOR T-Meas.

  DEFINE VARIABLE viTIndxRowId AS ROWID.
  DEFINE VARIABLE viOwnerId     AS INT.
  DEFINE VARIABLE viHeatLot     AS CHAR.
  DEFINE VARIABLE viInvId       AS INT.
  
  viTIndxRowId = TO-ROWID(ENTRY(2,ipAction)).
  viInvId      = INT(ENTRY(3,ipAction)).
  viHeatLot    = ENTRY(4,ipAction).
  viOwnerId    = INT(ENTRY(5,ipAction)).
  
  DO TRANSACTION:
    FIND FIRST t-indx WHERE ROWID(t-indx) EQ viTIndxRowId NO-LOCK NO-ERROR.
    IF AVAILABLE t-indx THEN
    DO:
      CREATE b-indx.
      BUFFER-COPY t-indx EXCEPT t-indx.test-id t-indx.owner-id t-indx.inv-id 
               TO b-indx.
      b-indx.test-id = DYNAMIC-FUNCTION('getNextIdInt', INPUT 0, INPUT 0, INPUT 130).
      opRtn = STRING(b-indx.test-id).
      ASSIGN
        b-indx.owner-id = viOwnerId
        b-indx.inv-id   = viInvId
        b-indx.heat-lot = viHeatLot.

      FOR EACH t-meas WHERE t-meas.test-id = t-indx.test-id:
        CREATE b-meas.
        BUFFER-COPY t-meas EXCEPT t-meas.test-id TO b-meas.
        b-meas.test-id = b-indx.test-id.
      END.
    END.
    ELSE
    DO:
      /* error returned */
      opRtn = "?".
      RETURN.
    END.
  END. /* transaction */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getChemTestId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getChemTestId Procedure 
PROCEDURE getChemTestId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define variable viHeatLot     as character no-undo.
  define variable viMillid     as integer no-undo.
  define variable cCustName  as character  no-undo.
    /* lead char in voChemTestId is:                           */
    /*  0 - no chemMstr.chemTestId found; returned '?'         */
    /*  1 - found and returned a chemMstr.chemTestId           */
    /*  2 - found heat-lot for different owner; copy possible. */
  define variable voChemTestId  as character no-undo initial "0,?,?,?,?,?,?,0,O".

  assign viHeatLot = entry(2,ipAction)
         viMillid = integer(entry(3,ipAction)).

  /* combined logic found in legacy code e0049.p and l0130.p */
  find first chemMstr no-lock
       where chemMstr.mill-id   eq viMillid
       and   chemMstr.heat-lot   eq viHeatLot 
       and   chemMstr.inv-id     eq 0         
       and  (chemMstr.own-inv-id eq ""
             or chemMstr.own-inv-id eq ?)
       use-index heat-lot no-error.
  find first bChemMstr no-lock where bChemMstr.mill-id ne viMillID
                                 and bChemMstr.heat-lot = viHeatLot no-error.
  if available chemMstr then 
      voChemTestId = substitute("1,&1,&2,&3,&4,&5,&6,&7,&8",
                              string(chemMstr.chemTestId),
                              chemMstr.logon,
                              string(chemMstr.t-date,"99/99/99") + ' ' + string(chemMstr.t-time,"9999"),
                              string(chemMstr.tran-no),
                              chemMstr.heat-lot,
                              string(chemmstr.mill-id),
                              (if avail bChemMstr then string(bChemMstr.mill-id) else '0'),
                              (if chemmstr.AuditBy = ? or chemmstr.auditby = '' then 'O' else 'P')).
  

  /*  tiwo 44999 - chem must match to be valid
  else 
  do:
      /* check if another chemistry record exists for a different owner */
      /* ad if it finds one, sends the chemTestId of that record back   */
      /* to see if user wants to make a copy of it.                     */
    find first chemMstr no-lock
         where chemMstr.mill-id   ne viMillid
         and   chemMstr.heat-lot   eq viHeatLot
         and   chemMstr.inv-id     eq 0         
         and  (chemMstr.own-inv-id eq ""
               or chemMstr.own-inv-id eq ?) no-error.
    if available chemMstr then
    do:
      find cust no-lock
           where cust.cust-id eq chemMstr.owner-id  no-error. 
      cCustName = string(entry(1,cust.name)).
      voChemTestId = "2," +
                     string(chemMstr.chemTestId) + "," + 
                     cCustName + "," + 
                     string(rowid(chemMstr)) + "," +
                     string(viHeatLot) + "," +
                     string(viMillid).
    END.
   
    ELSE
      voChemTestId = "0,?".

  END.
  */
  
  opRtn = voChemTestId.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getMechTestId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMechTestId Procedure 
PROCEDURE getMechTestId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define buffer bCust  for cust.
  define buffer bInv   for inv.
  define buffer bRcpt  for rcpt.
  define buffer bRcpt2 for rcpt.
  define buffer bTIndx for t-indx.

  define variable viInvId   as integer no-undo.
  define variable viHeatLot as character no-undo.
  define variable viOwnerId as integer no-undo.

    /* lead char in voMechTestId is:                           */
    /*  0 - no t-indx.test-id found; returned '?'              */
    /*  1 - found and returned a t-indx.Test-Id                */
    /*  2 - found heat-lot for different owner; copy possible. */  
  define variable voMechTestId as character no-undo initial "?".

  assign viInvId   = integer(entry(2,ipAction))
         viHeatLot = entry(3,ipAction)
         viOwnerId = integer(entry(4,ipAction)).
  find bInv no-lock
       where bInv.inv-id eq viInvID no-error.
  if not available bInv then
  do:
      voMechTestId = "0,Inv ID not found for this mechanical check.".

  end.

  find first bTIndx no-lock   /* finds tests,master or package, by inv-id */
       where bTIndx.inv-id   eq viInvId   no-error.
  if available bTIndx then
  do:
    voMechTestId = "1," + string(bTIndx.test-id).
  end.
  else if available bInv and bInv.rcpt-id <> bInv.inv-id then
  do:    /* No test found by inv-id, tries to find master test */
    find first bTIndx no-lock
    where bTIndx.inv-id eq bInv.rcpt-id no-error.
    if available bTIndx then
    do:
      voMechTestId = "3," + string(bTIndx.test-id).
    end.
  end.
  else
  t-indxBlock:
  do:
    
    if available bInv then
    do:
      find bRcpt where bRcpt.rcpt-id = bInv.rcpt-id no-lock no-error.  
    end.

    find first bTIndx no-lock   /* attempt find by prev-inv-id & diff owner */
         where bTIndx.own-inv-id eq bRcpt.prev-inv-id 
         AND   bTindx.heat-lot EQ bRcpt.heat-lot
         and   bTIndx.owner-id   ne viOwnerId no-error.

      /* POTENTIAL t-indx RECORD TO COPY */
    if available bTIndx then
    do:
      find bCust where bCust.cust-id = bTIndx.owner-id no-lock no-error. 
      voMechTestId = "2," +
                     string(bTIndx.Test-Id) + "," +
                     string(bCust.name) + "," +
                     string(rowid(bTIndx)).  
    end.
    else
        voMechTestId = "0,".

  end. /* t-indxBlock */
    
  opRtn = voMechTestId.

  return.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-invHeatLot) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE invHeatLot Procedure 
PROCEDURE invHeatLot :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viInvId    AS CHAR.
  DEFINE VARIABLE voHeatLot  AS CHAR INIT "?".
  DEFINE VARIABLE voOwnerId  AS CHAR INIT "?".
  DEFINE VARIABLE voUserId   AS CHAR INIT "?".
  DEFINE VARIABLE voMillId   AS CHAR INIT "?".

  assign viInvId = ENTRY(2,entry(1,ipAction,"|"))
         opRtn   = "-1".  /* all set if below fails */

  /* same logic found in legacy code e0049.p */
  FIND inv WHERE inv.inv-id = INT(viInvId) NO-LOCK NO-ERROR.
  IF AVAILABLE inv THEN
  DO:
    FIND partner WHERE 
         partner.partner-id = inv.partner-id NO-LOCK NO-ERROR.
    IF AVAILABLE partner THEN
      ASSIGN
        voUserId  = STRING(partner.user-id)
        voOwnerId = STRING(partner.owner-id).
 
    FIND rcpt WHERE rcpt.rcpt-id = inv.rcpt-id NO-LOCK NO-ERROR.
    IF AVAILABLE rcpt THEN assign
        voHeatLot = rcpt.heat-lot
        voMillId = string(rcpt.mill-id).

    opRtn = voHeatLot + "," + voMillId + "," + voUserId + "," + voOwnerID.
  END.
  /* end of e0049.p logic */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MechAuditPass) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MechAuditPass Procedure 
PROCEDURE MechAuditPass :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first t-Indx
    where t-indx.test-id = int(entry(2, ipAction)) no-error.
if not avail t-Indx then do:
    assign opRtn = '** Failed to update T-Indx Record.'.
end.
else do:
    assign t-Indx.AuditBy = entry(3,ipAction).
    delete from MechAudit where MechAudit.Test-ID = T-Indx.Test-ID.
    for each t-meas where t-meas.test-id = t-indx.test-id:
        assign t-meas.logon = replace(t-meas.logon,"=","").
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PassMechList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PassMechList Procedure 
PROCEDURE PassMechList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var iLoop as int.
def var iTest as int.
define variable cPassList as character no-undo.

find first t-Indx no-lock
    where t-indx.test-id = int(entry(2, ipAction)) no-error.
if not avail t-Indx then do:
    assign opRtn = '** Failed to update T-Indx Record.'.
    return.
end.

cPassList = entry(4, ipAction).
do iLoop = 1 to num-entries(cPassList,'|'):
    iTest = int(entry(iLoop,cPassList,'|')).
    for each t-meas
        where t-meas.test-id = t-indx.test-id
        and t-meas.t-code = iTest:
        t-meas.logon = replace(t-meas.logon,"=","").
    end.
end.

if can-find(first t-meas
            where t-meas.test-id = t-indx.test-id
            and t-meas.logon begins '=') then do:
    assign opRtn = 'All tests accessible by this screen are audited~nbut Audits requiring other tests are needed.~n~nPlease re-enter the audit function to complete those.'.
end.
else do:
    /* all tests audited, complete it */
    find current t-indx exclusive-lock.
    assign t-Indx.AuditBy = entry(3,ipAction).
    delete from MechAudit where MechAudit.Test-ID = T-Indx.Test-ID.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PassMechList2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PassMechList2 Procedure 
PROCEDURE PassMechList2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var iLoop as int.
def var iTest as int.
define variable iLoc as integer no-undo.
define variable cTest_Loc as character no-undo.
define variable cPassList as character no-undo.

find first t-Indx no-lock
    where t-indx.test-id = int(entry(2, ipAction)) no-error.
if not avail t-Indx then do:
    assign opRtn = '** Failed to update T-Indx Record.'.
    return.
end.

cPassList = entry(4, ipAction).
do iLoop = 1 to num-entries(cPassList,'|'):
    cTest_Loc = entry(iLoop,cPassList,'|').
    iTest = integer(entry(1,cTest_Loc,'-')).
    iLoc = integer(entry(2,cTest_Loc,'-')).
    for each t-meas
        where t-meas.test-id = t-indx.test-id
        and t-meas.t-code = iTest
        and t-meas.loc = iLoc:
        t-meas.logon = replace(t-meas.logon,"=","").
    end.
end.

if can-find(first t-meas
            where t-meas.test-id = t-indx.test-id
            and t-meas.logon begins '=') then do:
    assign opRtn = 'All tests accessible by this screen are audited~nbut Audits requiring other tests are needed.~n~nPlease re-enter the audit function to complete those.'.
end.
else do:
    /* all tests audited, complete it */
    find current t-indx exclusive-lock.
    assign t-Indx.AuditBy = entry(3,ipAction).
    delete from MechAudit where MechAudit.Test-ID = T-Indx.Test-ID.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PickUOM) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PickUOM Procedure 
PROCEDURE PickUOM :
/*------------------------------------------------------------------------------
  Purpose:    select default UOM to use for this inv or partner 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*   define buffer bCust  for cust. */
  define buffer bInv   for inv.
/*   define buffer bRcpt  for rcpt. */
/*   define buffer bRcpt2 for rcpt.   */
  define buffer bTIndx for t-indx.
  
  define variable iTestID   as integer no-undo.
  define variable iImperial as integer no-undo.
  define variable iMetric   as integer no-undo.
  define variable viInvId   as integer no-undo.
  define variable viHeatLot as character no-undo.
  define variable viOwnerId as integer no-undo.

  define variable voMechTestId as character no-undo initial "?".

  assign viInvId   = integer(entry(2,ipAction))
         viHeatLot = entry(3,ipAction)
         viOwnerId = integer(entry(4,ipAction))
         iTestID = 0
         iImperial = 0
         iMetric = 0.
  find bInv no-lock
       where bInv.inv-id eq viInvID no-error.
  if not available bInv then
  do:
      opRtn = ?.
  end.
  else do:
     find first bTIndx no-lock   /* finds tests,master or package, by inv-id */
          where bTIndx.inv-id   eq viInvId   no-error.
     if available bTIndx then
     do:
       iTestId = bTIndx.test-id.
     end.
     else if available bInv and bInv.rcpt-id <> bInv.inv-id then
     do:    /* No test found by inv-id, tries to find master test */
       find first bTIndx no-lock
       where bTIndx.inv-id eq bInv.rcpt-id no-error.
       if available bTIndx then
       do:
         iTestID = bTIndx.test-id.
       end.
     end.
     if iTestID ne 0 then do:
         for each t-meas no-lock
             where t-meas.test-id = iTestID
             and (t-meas.t-code = 2 or t-meas.t-code = 6):
             iMetric = iMetric + (if t-meas.uom = 2 then 1 else 0).
             iImperial = iImperial + (if t-meas.uom = 1 then 1 else 0).
         end.
     end.
     if iMetric = iImperial then do:
         /* could not determine clear winner, send partner default */
         find first partner no-lock
             where partner.partner-id = bInv.partner-id no-error.
         iMetric = iMetric + (if partner.met-eng > 0 then 1 else 0).
         iImperial = iImperial + (if partner.met-eng = 0 then 1 else 0).
     end.
    
     opRtn = (if iMetric > iImperial then 'Metric' else 'English').

  end.

  return.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UseExtendedCert) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UseExtendedCert Procedure 
PROCEDURE UseExtendedCert :
/*------------------------------------------------------------------------------
  Purpose: Do audits need done on extended certs.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cRtn as character no-undo.
define variable lExtended as logical no-undo.
define variable lPlain as logical no-undo.
crtn = 'no'.
lExtended = no.
lPlain = no.
run getMechTestID.
if entry(1,opRtn) = '1' or entry(1,opRtn) = '3' then do:
    /* there is a test out there, see if it has extended tests */
    for each t-meas no-lock where t-meas.test-id = int(entry(2,opRtn)):
        if not t-meas.logon begins '=' then next.
        if t-meas.t-code ge 42 and t-meas.t-code le 53 then lExtended = yes.
        if t-meas.t-code = 35 then lExtended = yes.
        if t-meas.t-code lt 42 and t-meas.t-code ne 35 then lPlain = yes.
    end.
end.
opRtn = 'no'.
if lExtended = yes then opRtn = 'yes'.
if lExtended = yes and lPlain = yes then opRtn = 'both'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ViewExtendedCert) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewExtendedCert Procedure 
PROCEDURE ViewExtendedCert :
/*------------------------------------------------------------------------------
  Purpose:  Which Cert format should be viewed   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define variable cRtn as character no-undo.
define variable lExtended as logical no-undo.
define variable lPlain as logical no-undo.
crtn = 'no'.
lExtended = no.
lPlain = no.
run getMechTestID.
if entry(1,opRtn) = '1' or entry(1,opRtn) = '3' then do:
    /* there is a test out there, see if it has extended tests */
    for each t-meas no-lock where t-meas.test-id = int(entry(2,opRtn)):
        if t-meas.t-code ge 42 and t-meas.t-code le 53 then lExtended = yes.
        if t-meas.t-code = 35 then lExtended = yes.
        if t-meas.t-code lt 42 and t-meas.t-code ne 35 then lPlain = yes.
    end.
end.
opRtn = 'no'.
if lExtended = yes then opRtn = 'yes'.
if lExtended = yes and lPlain = yes then opRtn = 'both'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

