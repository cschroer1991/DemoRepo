&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : chem/t0243.p
    Purpose     : Accepts a Progress DataSet and saves changes to the
                  chemMeas table to the database.

    Syntax      : RUN chem/t0243.p ON h_green 
                       (INPUT-OUTPUT DATASET-HANDLE hdsChemSet BY-REFERENCE).

    Description :

    Author(s)   : Bruce Thompson
    Created     : 01/04/05
    Notes       :
    History     :
  Date     Pgmr TIWO   Desc
  -------- ---- ------ -------------------------------------------------
  06/08/10 bmf  501898 Coils not going on hold for new chems, only update
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{chem/ttchemdefs.i}
{chem/dschemdefs.i}
{chem/srcchemdefs.i}

DEFINE INPUT  PARAMETER pcChemTestId  AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsChemSet.
DEFINE OUTPUT PARAMETER pcList AS CHAR.

DEFINE VARIABLE hDSChanges AS HANDLE  NO-UNDO.
DEFINE VARIABLE vLog       AS LOGICAL NO-UNDO.
define variable iAuditChem as integer no-undo.
define variable iPlantID as integer no-undo.
define variable cUserID as character no-undo.

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
         HEIGHT             = 4.71
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
    assign  iAuditChem  = 0
            iPlantID    = {fnarg getGlobalVar 'PlantId'}
            cUserID     = {fnarg getglobalvar 'UserID'}.
    
    /* Attach Data-Source to ttChemMstr */
    BUFFER ttChemMstr:ATTACH-DATA-SOURCE(DATA-SOURCE srcCMstr:HANDLE, ?, ?).
    
    /* Attach Data-Source to ttChemMeas */
    BUFFER ttChemMeas:ATTACH-DATA-SOURCE(DATA-SOURCE srcCMeas:HANDLE, ?, ?).
    
    hDSChanges = DATASET dsChemSet:HANDLE.
    
    ON CREATE OF chemMstr
        DO:
            BUFFER  ttChemMstr:FIND-BY-ROWID(BUFFER ttChemMstrBefore:AFTER-ROWID).
            assign  ttChemMstr.chemTestId   = NEXT-VALUE(chemTestID)
                    pcChemTestId            = string(ttChemMstr.chemTestId)
                    iAuditChem              = ttChemMstr.chemTestId.
        END.
    ON CREATE OF chemMeas
        DO:
            BUFFER  ttChemMeas:FIND-BY-ROWID(BUFFER ttChemMeasBefore:AFTER-ROWID).
            assign  ttChemMeas.chemTestId   = INT(pcChemTestId)
                    ttChemMeas.ref-no       = '='
                    iAuditChem              = INT(pcChemTestId).
        END.
    
    DO TRANSACTION:  
        FIND FIRST ttChemMstrBefore NO-ERROR.
        IF AVAILABLE ttChemMstrBefore THEN
        DO:
            IF BUFFER ttChemMstrBefore:ROW-STATE = ROW-CREATED THEN
            DO:
                BUFFER ttChemMstrBefore:SAVE-ROW-CHANGES() NO-ERROR.
            END.
        END.
    
        FOR EACH ttChemMeasBefore:
            CASE BUFFER ttChemMeasBefore:ROW-STATE:
                WHEN ROW-DELETED THEN
                    DO:  /* delete logic */
                        FIND chemMeas WHERE
                            chemMeas.chemTestId = ttChemMeasBefore.chemTestId AND
                            chemMeas.chem-id    = ttChemMeasBefore.chem-id    AND
                            chemMeas.loc        = ttChemMeasBefore.loc        AND
                            chemMeas.side       = ttChemMeasBefore.side 
                                NO-ERROR.
                        IF AVAILABLE chemMeas THEN
                            DO:
                                DELETE chemMeas NO-ERROR.
                                IF ERROR-STATUS:ERROR THEN
                                DO:
                                    ASSIGN
                                        hDSChanges:error                        = TRUE
                                        BUFFER ttChemMeasBefore:ERROR           = TRUE
                                        BUFFER ttChemMeasBefore:ERROR-STRING    = RETURN-VALUE.
                                END.
                                iAuditChem  = ttChemMeasBefore.chemTestId.
                            END. /* if available */
                        ELSE
                            DO:
                                ASSIGN
                                    hDSChanges:ERROR                        = TRUE
                                    BUFFER ttChemMeasBefore:ERROR           = TRUE
                                    BUFFER ttChemMeasBefore:error-string    = "Unable to obtain chemMeas record for deletion".
                            END. /* if not available */
                    END. /* delete logic */
                WHEN ROW-MODIFIED OR WHEN ROW-CREATED THEN
                    DO:
                        /* SAVE-ROW-CHANGES() method does the default handling of the create */
                        /* and modify on the current buffer.                                 */
                        BUFFER ttChemMeasBefore:SAVE-ROW-CHANGES() NO-ERROR.
                        IF BUFFER ttChemMeasBefore:ERROR THEN
                            DO:
                                BUFFER ttChemMeasBefore:ERROR-STRING = ERROR-STATUS:GET-MESSAGE(1).
                            END.
                        else
                            do:
                                if ttChemMeasBefore.chemTestID > 0 then assign iAuditChem = ttchemMeasBefore.chemTestID.
                            end.    
                    END. /* create and modify logic */    
            END CASE.
    
            /*     RUN chem/t0226.p (INPUT ttChemMeasBefore.chemTestId,  */
            /*                       INPUT ttChemMeasBefore.chem-id,     */
            /*                       INPUT ttChemMeasBefore.side,        */
            /*                       OUTPUT pcList).                     */
        END.    /* For Each ttChemMeasBefore */
    END.    /*  DO Transaction  */
    /* Create Audit Record If Needed */
    if iAuditChem > 0 and can-find(AppAttr where AppAttr.AttrKey = 'ChemAudit'
                                             and AppAttr.AppLog = yes) then
    do:
        find first ChemMstr where chemMstr.ChemTestID = iAuditChem no-error.
        if avail ChemMstr then do:
            assign
                ChemMstr.AuditBy    = ''
                ChemMstr.Logon      = entry(1,ChemMstr.Logon,'-') + '-' + cUserID.
        end.
        if not can-find(ChemAudit where chemaudit.chemtestid = iAuditChem) then
            do:
                create ChemAudit.
                assign
                    ChemAudit.ChemTestId = iAuditChem
                    ChemAudit.plant-id   = iPlantID
                    ChemAudit.createdate = today
                    ChemAudit.heat-lot   = ChemMstr.heat-lot.
            end.
    end.
    
    if not can-find(first ChemMeas where ChemMeas.chemTestId = int(pcChemTestId)) then
        do:
            find first chemmstr where chemmstr.chemtestid = int(pcChemTestID) no-error.
            if avail chemmstr then do:
                delete ChemMstr.
            end.
        end.
    else
        do:
            /* 50198 */
            FOR EACH ttChemMeas:
                RUN chem/t0226.p (INPUT ttChemMeas.chemTestId,
                                  INPUT ttChemMeas.chem-id,
                                  INPUT ttChemMeas.side,
                                  OUTPUT pcList).
            end.
        end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


