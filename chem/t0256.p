&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : chem/t0256.p
    Purpose     :

    Syntax      : RUN chem/t0256.p ON ON h_green (
                          INPUT pcTestId,
                          INPUT-OUTPUT DATASET-HANDLE hdsChanges BY-REFERENCE,
                          OUTPUT invList).
                          
    Description :

    Author(s)   : Bruce Thompson
    Created     : 01/12/05
    Notes       :
    History     : 
    Date   Pgmr Issue Description
  -------- ---- ----- --------------------------------------------------  
  11/01/05 rct  3034  test for t-indx record
  05/10/06 rct  22798 fix ErrorHit to test for t-meas record and adjust
                      error message appropriately
  06/01/06 cel  23485 Adding Code to manually create claims for hold-code 1.
  01/25/07 cel  30071 Isolate ValidateMechResults so it can be called regardless
                      of whether the user saves.
  12/05/11 djt  57829 Audit chem/mech project
  06/22/17 djt  75231 Exclude certain users from mechanical audits.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{chem/ttmechdefs.i}
{chem/dsmechdefs.i}
{chem/srcmechdefs.i}

define input        parameter pcTestId  as char.
define input-output parameter dataset for dsMechSet.
define output parameter pcUsrMsg as character  no-undo.

DEFINE VARIABLE hDSChanges AS HANDLE  NO-UNDO.
DEFINE VARIABLE vLog       AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-inv-id   AS INT     NO-UNDO.
DEFINE VARIABLE v-Test-id  AS INT     NO-UNDO.
define variable v-msg            as character  no-undo.
define variable iPlantID   as integer no-undo.
define variable lMechAudit as logical no-undo.
define variable cUserID as character no-undo.
DEFINE STREAM listStream.

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
         HEIGHT             = 8.81
         WIDTH              = 55.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

   /* sneakAPeek allows you to see the contents of the Temp-Tables   */
   /* that were passed back in the DataSet. They should only contain */
   /* records that were added, changed, or deleted                   */
/* RUN sneakAPeek. */



/* Attach DataSource to ttIndx */
BUFFER ttIndx:ATTACH-DATA-SOURCE(DATA-SOURCE srcIndx:HANDLE, ?, ?).

/* Attach DataSource to ttMeas */
BUFFER ttMeas:ATTACH-DATA-SOURCE(DATA-SOURCE srcTMeas:HANDLE, ?, ?).

hDSChanges = DATASET dsMechSet:HANDLE.
iPlantID = {fnarg getGlobalVar 'PlantID'}.
find first AppAttr no-lock
    where appattr.AttrKey = 'MechAudit'
    and AppAttr.AppLog = yes.
if avail appattr then lMechAudit = yes.
cUserID = {fnarg getglobalvar 'userID'}.
if lMechAudit and can-do(entry(2,appattr.appstr,'|'),cUserID)
    then lMechAudit = no.
tranBlock:

DO TRANSACTION
   on error undo, leave:
  FOR EACH ttIndxBefore:
    IF BUFFER ttIndxBefore:ROW-STATE = ROW-CREATED THEN
    DO:
      BUFFER ttIndx:FIND-BY-ROWID(BUFFER ttIndxBefore:AFTER-ROWID).
      CREATE t-Indx.
      BUFFER-COPY ttIndx TO t-Indx NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
        BUFFER ttIndxBefore:ERROR = TRUE.
        BUFFER ttIndxBefore:ERROR-STRING =
          "ERROR IN ttIndxBefore: " + ERROR-STATUS:GET-MESSAGE(1).
        RETURN.
      END.
    END.
    find first ttmeas 
        where ttMeas.test-id = int(pcTestID)
        and ttMeas.logon begins '=' no-error.
    if can-find(first ttIndx where ttIndx.test-id = int(pcTestID))
        and avail ttMeas
        and lMechAudit then do:
        find first t-indx where t-indx.test-id = int(pcTestID) no-error.
        if avail t-Indx then do:
            assign
                t-indx.AuditBy = ''
                t-indx.logon = entry(1,t-indx.Logon,'-') + '-' + cUserID.
            find first MechAudit where MechAudit.test-id = t-indx.test-id no-error.
            if not avail MechAudit then do:
                create MechAudit.
                assign
                    MechAudit.test-id = t-indx.test-id
                    MechAudit.CreateDate = today
                    MechAudit.inv-id = t-indx.inv-id
                    MechAudit.plant-id = iPlantID.
            end.
        end.
    end.
  END.

  find t-indx no-lock
       where t-indx.test-id eq integer(pcTestId) no-error.
  /* Issue 3034 */
  if not available t-indx then
  do:
      pcUsrMsg = "Could not find Test: " + pctestId.
      undo tranBlock, return. 
  end.

  assign v-inv-id  = t-indx.inv-id
         v-test-id = t-indx.test-id .

  FOR EACH ttMeasBefore:
    CASE BUFFER ttMeasBefore:ROW-STATE:
      WHEN ROW-DELETED THEN
      DO:
        BUFFER t-meas:FIND-BY-ROWID(ttMeasBefore.oRowId,EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
        IF AVAILABLE t-meas THEN
          DELETE t-meas NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
          DO:
            BUFFER ttMeasBefore:ERROR = TRUE.
            BUFFER ttMeasBefore:ERROR-STRING = "Error in Delete".
          END.
        ELSE
        DO:
          BUFFER ttMeasBefore:ERROR = TRUE.
        END.
      END.
      WHEN ROW-MODIFIED THEN
      DO:
        BUFFER t-meas:FIND-BY-ROWID(ttMeasBefore.oRowId,EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
        BUFFER ttMeas:FIND-BY-ROWID(BUFFER ttMeasBefore:AFTER-ROWID).
        IF AVAILABLE t-meas THEN
        DO:
          BUFFER-COPY ttMeas TO t-meas NO-ERROR.
        END.
        ELSE
        DO:
          BUFFER ttMeasBefore:ERROR = TRUE.
          BUFFER ttMeasBefore:ERROR-STRING = "Error in modify".
        END.
      END.
      WHEN ROW-CREATED THEN
      DO:
        BUFFER ttMeas:FIND-BY-ROWID(BUFFER ttMeasBefore:AFTER-ROWID) NO-ERROR.
        FIND t-meas WHERE ROWID(t-meas) EQ ttMeas.oRowId NO-ERROR.
        IF NOT AVAILABLE t-meas THEN
        DO:
          CREATE t-Meas.
          BUFFER-COPY ttMeas EXCEPT oRowId TO t-Meas.
        END.
      END.
    END CASE.

  END. /* FOR EACH ttMeasBefore */ 
END. /* Transaction */




/* aftermath is a dump procedure for debugging purposes. */
/* RUN afterMath. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-afterMath) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterMath Procedure 
PROCEDURE afterMath :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

OUTPUT STREAM listStream TO aftermath.txt PAGED.

FOR EACH t-indx WHERE 
         t-indx.test-id = INT(pcTestId) NO-LOCK:
  DISPLAY STREAM listStream 
    t-indx 
    WITH FRAME F1a NO-BOX 2 COL SIDE-LABELS STREAM-IO.
  DISPLAY STREAM listStream SKIP(1) WITH FRAME F1a.

  FOR EACH t-meas WHERE
           t-meas.test-id = t-indx.test-id NO-LOCK
    BY t-meas.t-code BY t-meas.side:

    DISPLAY STREAM listStream
      STRING(ROWID(t-meas)) FORMAT "x(15)" LABEL "O-Rowid"
      t-meas.test-id
      t-meas.t-code
      t-meas.side
      t-meas.loc
      t-meas.uom
      t-meas.exact-meas[1]
      t-meas.min-meas
      t-meas.max-meas
      t-meas.ref-no[1]
      WITH FRAME f2a NO-BOX 2 COL SIDE-LABELS DOWN STREAM-IO.
    DISPLAY STREAM listStream 
      SKIP(2) WITH FRAME f2a.
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sneakAPeek) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sneakAPeek Procedure 
PROCEDURE sneakAPeek :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Attach DataSource to ttIndx */
BUFFER ttIndx:ATTACH-DATA-SOURCE(DATA-SOURCE srcIndx:HANDLE, ?, ?).

/* Attach DataSource to ttMeas */
BUFFER ttMeas:ATTACH-DATA-SOURCE(DATA-SOURCE srcTMeas:HANDLE, ?, ?).

hDSChanges = DATASET dsMechSet:HANDLE.

OUTPUT STREAM listStream TO sneakpeek.txt PAGED.

DO:
  FIND FIRST ttIndxBefore NO-ERROR.
  IF AVAILABLE ttIndxBefore THEN
  DO:
    DISPLAY STREAM listStream 
      "ROW-STATE: " BUFFER ttIndxBefore:ROW-STATE
      WITH FRAME f1 NO-BOX NO-LABELS STREAM-IO.
    DISPLAY STREAM listStream 
      ttIndxBefore.test-id
      ttIndxBefore.owner-id
      ttIndxBefore.heat-lot
      ttIndxBefore.inv-id
      STRING(ttIndxBefore.oRowId) FORMAT "x(12)"
      WITH FRAME F2 NO-BOX 2 COL SIDE-LABELS STREAM-IO.
    DOWN STREAM listStream 1 WITH FRAME f2.
  END.

  FOR EACH ttMeasBefore BY ttMeasBefore.test-id BY ttMeasBefore.cut-no:
    IF BUFFER ttMeasBefore:ROW-STATE <> 1 THEN
       BUFFER ttMeas:FIND-BY-ROWID(BUFFER ttMeasBefore:AFTER-ROWID).
    DISPLAY STREAM listStream "-------- BEFORE RECORD ---------"
      WITH FRAME x1 NO-BOX NO-LABELS STREAM-IO.
    DISPLAY STREAM listStream
      "ROW-STATE: " BUFFER ttMeasBefore:ROW-STATE SKIP(1)
      WITH FRAME f3 NO-BOX NO-LABELS STREAM-IO.
    DISPLAY STREAM listStream 
      STRING(BUFFER ttmeasbefore:ORIGIN-ROWID) FORMAT "x(15)" LABEL "O-Rowid"
      ttmeasbefore.test-id
      ttmeasbefore.t-code
      ttmeasbefore.side
      ttmeasbefore.loc
      ttmeasbefore.uom
      ttmeasbefore.exact-meas[1]
      ttmeasbefore.min-meas
      ttmeasbefore.max-meas
      ttMeasbefore.ref-no[1]
      WITH FRAME f4 NO-BOX 2 COL SIDE-LABELS DOWN STREAM-IO.
    IF BUFFER ttMeasBefore:ROW-STATE <> 1 THEN
    DO:
      DISPLAY STREAM listStream "-------- AFTER RECORD ---------"
        WITH FRAME x2 NO-BOX NO-LABELS STREAM-IO.  
      DISPLAY STREAM listStream
        STRING(BUFFER ttmeas:ORIGIN-ROWID) FORMAT "x(15)" LABEL "O-Rowid"
        ttmeas.test-id
        ttmeas.t-code
        ttmeas.side
        ttmeas.loc
        ttmeas.uom
        ttmeas.exact-meas[1]
        ttmeas.min-meas
        ttmeas.max-meas
        ttmeas.ref-no[1]
        WITH FRAME f5 NO-BOX 2 COL SIDE-LABELS DOWN STREAM-IO.
    END.
  END.
END.

/* RELEASE ttIndxBefore. */
/* RELEASE ttMeasBefore. */
/* RELEASE ttIndx.       */
/* RELEASE ttMeas.       */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

