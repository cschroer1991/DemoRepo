&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0651.p
    Purpose     : Mass Spec Update Server-side Utilities 

    Syntax      :

    Description : 
    
   Date     By   Issue  Remark
 ========  ===  ======  ================================================                 
 01/18/11  DJT   54893  New
 01/27/11  DJT   55149  Add more information to problem listing from validation
 10/15/12 nlbu   60326  Check for edge protectors in Validate Changes. Currently
                        has duplicate types. 
 10/16/12 nlbu   60326  More improvements. 
 06/20/13  cjs   56765  Add ODMin and ODMax fields. 
 12/03/13  cjs   64857  Calculate Mult weights when OD's are changed   
 09/23/14  cjs   66190  Added length range to search spec's by
 10/03/14 zakh   67109  Now blank procitem.reviewby for lube changes
 02/12/15  lcs   67489  Blank procitem.reviewby for exposed changes
 06/23/17  Mas   74438  Changed to update a New Schema Field (UnReviewReason) and sets it
                        to 0.  A "0" will not allow the same CSR user to Review their
                        own work orders.
 02/14/18  Mas   76553  Added 2 Additional Input Fiels and 14 Additional Output Fields to
                        run Common/t0611 in Procedure CommitChanges. Also Added 14 Additional
                        Input Fields to Run Common/t0078 in Procedure CommitChanges. 

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define temp-table gttSpecChange like ttSpecChange.
define temp-table ttModSpec like spec.

define input parameter pcAction as char.
define input parameter pcOptionList as char.
define input parameter table for gttSpecChange.
define output parameter pcReport as char.
define output parameter piWorkOrders as int.
define output parameter piProcOrders as int.
define output parameter pcRtn as char.

define variable iIdx       as int no-undo.
define variable iPlantID   as int no-undo.
define variable iPartner   as int no-undo.
define variable iOwner     as int no-undo.
define variable iUser      as int no-undo.
define variable iProcCode  as int no-undo.
define variable eMinGauge  as dec no-undo.
define variable eMaxgauge  as dec no-undo.
define variable eMinWidth  as dec no-undo.
define variable eMaxWidth  as dec no-undo.
define variable eMinLength  as dec no-undo.
define variable eMaxLength  as dec no-undo.
define variable iMetalType as int no-undo.
define variable iSpecCount as int no-undo.
define variable iChangeCount as int no-undo.
define variable hXprocHand as handle.
define variable hTempHand as handle.
define variable hTempHand2 as handle.
define variable cSubTitle as char no-undo.

DEFINE VARIABLE iMaxMultWt AS integer     NO-UNDO.
DEFINE VARIABLE iMinMultWt AS integer     NO-UNDO.
DEFINE VARIABLE dMaxOD     AS DECIMAL  format '->>>,>>9.99'   NO-UNDO.
DEFINE VARIABLE dMinOD     AS DECIMAL  format '->>>,>>9.99'   NO-UNDO.
DEFINE VARIABLE dId        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dDensity   like type.density no-undo.
DEFINE VARIABLE iMandrel1  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iMandrel2  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iMandrel3  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPI4       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cSize1     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDbFieldList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lChangeList AS LOGICAL initial yes    NO-UNDO.


/* define variable cTxt         as character  no-undo. */

define temp-table ttChangeList
    field iPartner as integer column-label 'Partner' help 'filter'
    field iPart as integer column-label 'Part'
    field cSize as char column-label 'Size'
    field iSpecID as int column-label 'Spec ID'
    field iVersion as int column-label 'Version'
    field iMetaltype as int column-label 'Metal Type'
    field cField as char column-label 'Field Changed' help 'filter'
    field cDbField as char column-label '<SKIP>'
    field cOldValue as char column-label 'Old Value'
    field cNewValue as char column-label 'New Value'
    field cError as char column-label 'Error Condition'.

define temp-table ttSpecs
    field iPartner as int column-label 'Partner ID'
    field iPart as int column-label 'Part ID'
    field cSize as char column-label 'Size'
    field iSpecID as int column-label 'Previous Spec ID'
    field iVersion as int column-label 'Previous Version'
    field cLocked as char column-label 'Was Locked'
    field iNewSpec as int column-label 'New Spec ID'
    field iNewVersion as int column-label '<SKIP>'
    field cRemark as char column-label 'Note'.

define temp-table ttProblems
    field iPartner as int column-label 'Partner ID'
    field iSpecID as int column-label 'Spec ID'
    field iPartID as int column-label 'Part ID'
    field cSize as char column-label 'Size'
    field cError as char column-label 'Error condition'
    field cField as char column-label 'Data Field'.

define buffer bttSpecs for ttSpecs.

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
         HEIGHT             = 9.19
         WIDTH              = 49.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
if pcAction = "":u 
or pcAction = ? 
or not can-do(this-procedure:internal-entries,pcAction) then do: 
  pcRtn = "**Procedure " + pcAction + ' Not available in ' + program-name(1).
end.
else do:
  run ParseOpts.
  run value(pcAction).
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AddToSpecChangeTbl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddToSpecChangeTbl Procedure 
PROCEDURE AddToSpecChangeTbl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cProp as char no-undo.
                                      
do iIdx = 1 to num-entries(cDbFieldList,"|"):
  cProp = entry(iIdx,cDbFieldList,"|").
     
    if cProp = "min-mult-wt" then do:
        create gttSpecChange.
        assign gttSpecChange.cTab = "Process 2"
               gttSpecChange.cDbField = "ODMin"
               gttSpecChange.cPrompt = "O.D. Min"
               gttSpecChange.iOldInteger = 1
               gttSpecChange.cOldDescr = "1"
               gttSpecChange.iNewInteger = 1
               gttSpecChange.cNewDescr = "Change"
               gttSpecChange.cOldChar = ""
               gttSpecChange.cNewChar = ""
               gttSpecChange.lOldLogical = no
               gttSpecChange.lNewLogical = no
               gttSpecChange.cFieldType = "Decimal"
               gttSpecChange.eOldDecimal = 1
               gttSpecChange.eNewDecimal = dMinOD.
    end.
    if cProp = "max-mult-wt" then do:
       create gttSpecChange.
       assign gttSpecChange.cTab = "Process 2"
               gttSpecChange.cDbField = "ODMax"
               gttSpecChange.cPrompt = "O.D. Max"
               gttSpecChange.iOldInteger = 1
               gttSpecChange.cOldDescr = "1"
               gttSpecChange.iNewInteger = 1
               gttSpecChange.cNewDescr = "Change"
               gttSpecChange.cOldChar = ""
               gttSpecChange.cNewChar = ""
               gttSpecChange.lOldLogical = no
               gttSpecChange.lNewLogical = no
               gttSpecChange.cFieldType = "Decimal"
               gttSpecChange.eOldDecimal = 1
               gttSpecChange.eNewDecimal = dMaxOD.
    end.
    if cProp = "ODMin" then do:
       create gttSpecChange.
       assign gttSpecChange.cTab = "Process 2"
               gttSpecChange.cDbField = "min-mult-wt"
               gttSpecChange.cPrompt = "Mult Weight Min"
               gttSpecChange.iOldInteger = 1
               gttSpecChange.cOldDescr = "1"
               gttSpecChange.iNewInteger = iMinMultWt
               gttSpecChange.cNewDescr = "Change"
               gttSpecChange.cOldChar = ""
               gttSpecChange.cNewChar = ""
               gttSpecChange.lOldLogical = no
               gttSpecChange.lNewLogical = no
               gttSpecChange.cFieldType = "Integer"
               gttSpecChange.eOldDecimal = 1
               gttSpecChange.eNewDecimal = 1.
    end.
    if cProp = "ODMax" then do:
        create gttSpecChange.
        assign gttSpecChange.cTab = "Process 2"
               gttSpecChange.cDbField = "max-mult-wt"
               gttSpecChange.cPrompt = "Mult Wt Max"
               gttSpecChange.iOldInteger = 1
               gttSpecChange.cOldDescr = "1"
               gttSpecChange.iNewInteger = iMaxMultWt
               gttSpecChange.cNewDescr = "Change"
               gttSpecChange.cOldChar = ""
               gttSpecChange.cNewChar = ""
               gttSpecChange.lOldLogical = no
               gttSpecChange.lNewLogical = no
               gttSpecChange.cFieldType = "Integer"
               gttSpecChange.eOldDecimal = 1
               gttSpecChange.eNewDecimal = 1.
    end.


end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-addwarning) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addwarning Procedure 
PROCEDURE addwarning :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piSpecID as int.
define input parameter pcError as char.
define input parameter pcField as char.


find first bttSpecs
    where bttSpecs.iSpecID = piSpecID
    no-lock no-error.
create ttProblems.
assign
    ttProblems.iSpecID = piSpecID
    ttProblems.iPartID = (if avail bttSpecs then bttSpecs.iPart else 0)
    ttProblems.cError = pcError
    ttProblems.cField = pcField
    ttProblems.iPartner = (if avail bttSpecs then bttSpecs.iPartner else 0)
    ttProblems.cSize = (if avail bttSpecs then bttSpecs.cSize else '?').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ApplyChange) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApplyChange Procedure 
PROCEDURE ApplyChange :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piSpecID as int no-undo.
define output parameter pcRtn as char no-undo.


define variable hBufferHdl as handle.
define variable hFieldHdl as handle.
define variable hFieldHD2 as handle.
define variable lChange as logical no-undo.
define variable iCurrent as int no-undo.
define variable lCurrent as log no-undo.
define variable eCurrent as dec no-undo.
define variable cCurrent as char no-undo.

    find first spec exclusive-lock
        where spec.spec-id = piSpecID.
    if not avail spec then do:
        assign pcRtn = '** New Spec ' + string(piSpecID) + ' could not be modified!'.
    end.

    hBufferHdl = buffer spec:handle.
    
/* See if this spec has anything changeable */
    for each gttSpecChange:
        assign
            lChange = no
            hFieldHdl = hBufferHdl:buffer-field(gttSpecChange.cDbField).
        case gttSpecChange.cFieldtype:
            when 'integer' then do:
            if gttSpecChange.cNewDescr = "Change" then do:
                find first ttChangeList no-lock 
                    where ttChangeList.iSpecID = ttSpecs.iSpecID 
                    and   ttChangeList.cDbField = gttSpecChange.cDbField no-error.
                if avail ttChangeList then do:
                    assign gttSpecChange.iNewInteger = integer(ttChangeList.cNewValue)
                           gttSpecChange.iOldInteger = integer(ttChangeList.cOldValue).
                end.
            end.
                iCurrent = hFieldHdl:buffer-value.
                if iCurrent ne gttSpecChange.iNewInteger then lChange = yes.
                if gttSpecChange.iOldInteger ne ?
                    and iCurrent ne gttSpecChange.iOldInteger then lChange = no.
                IF iCurrent = ? AND gttSpecChange.iOldInteger = 0 THEN lChange = YES.
                if iCurrent = gttSpecChange.iNewInteger then lChange = no.
                if lChange then assign
                    hFieldHdl:buffer-value = gttSpecChange.iNewInteger.
/*                 if lChange and gttSpecChange.cDbField = "min-mult-wt" then do: */
/*                     hFieldHd2 = hBufferHdl:buffer-field("ODMin").              */
/*                     hFieldHd2:buffer-value = decimal(ttChangeList.cNewValue).  */
/*                 end.                                                           */
/*                 if lChange and gttSpecChange.cDbField = "max-mult-wt" then do: */
/*                     hFieldHd2 = hBufferHdl:buffer-field("ODMax").              */
/*                     hFieldHd2:buffer-value = decimal(ttChangeList.cNewValue).  */
/*                 end.                                                           */
                
            end.
            when 'logical' then do:
                lCurrent = hFieldHdl:buffer-value.
                if lCurrent ne gttSpecChange.lNewLogical then lChange = yes.
                if gttSpecChange.lOldLogical ne ?
                    and lCurrent ne gttSpecChange.lOldLogical then lChange = no.
                if lCurrent = gttSpecChange.lNewLogical then lChange = no.
                if lChange then assign
                    hFieldHdl:buffer-value = gttSpecChange.lNewLogical.
            end.
            when 'decimal' then do:
            if gttSpecChange.cNewDescr = "Change" then do:
                find first ttChangeList no-lock
                    where ttChangeList.iSpecID = ttSpecs.iSpecID
                    and   ttChangeList.cDbField = gttSpecChange.cDbField no-error.
                if avail ttChangeList then do:
                    assign gttSpecChange.eNewDecimal = decimal(ttChangeList.cNewValue)
                           gttSpecChange.eOldDecimal = decimal(ttChangeList.cOldValue).
                end.
            end.
                eCurrent = hFieldHdl:buffer-value.
                if eCurrent ne gttSpecChange.eNewdecimal then lChange = yes.
                if gttSpecChange.eOldDecimal ne ?
                    and eCurrent ne gttSpecChange.eOldDecimal then lChange = no.
                if eCurrent = gttSpecChange.eNewDecimal then lChange = no.
                if lChange then assign
                    hFieldHdl:buffer-value = gttSpecChange.eNewDecimal.
/*                 if lChange and gttSpecChange.cDbField = "ODMin" then do:      */
/*                     hFieldHd2 = hBufferHdl:buffer-field("min-mult-wt").       */
/*                     hFieldHd2:buffer-value = integer(ttChangeList.cNewValue). */
/*                 end.                                                          */
/*                 if lChange and gttSpecChange.cDbField = "ODMax" then do:      */
/*                     hFieldHd2 = hBufferHdl:buffer-field("max-mult-wt").       */
/*                     hFieldHd2:buffer-value = integer(ttChangeList.cNewValue). */
/*                 end.                                                          */
            end.
            when 'character' then do:
                if gttSpecChange.cDbField = 'mandrel' then do:
                    do iIdx = 1 to 3:
                        assign spec.mandrel[iIdx] = (if entry(iIdx, gttSpecChange.cNewChar) = '?'
                                                     then ?
                                                     else int(entry(iIdx, gttSpecChange.cNewChar))).
                    end.
                end.
                else do:
                   cCurrent = hFieldHdl:buffer-value.
                   if cCurrent ne gttSpecChange.cNewChar then lChange = yes.
                   if gttSpecChange.cOldChar ne '?'
                       and cCurrent ne gttSpecChange.cOldChar then lChange = no.
                   if cCurrent = gttSpecChange.cNewChar then lChange = no.
                   if lChange then 
                      assign hFieldHdl:buffer-value = gttSpecChange.cNewChar.
                end.
            end.
        end case.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildChangeList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildChangeList Procedure 
PROCEDURE BuildChangeList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cQuery as char no-undo.
define variable hBufferHdl as handle.
define variable hFieldHdl as handle.
define variable cSize as char no-undo.
define variable lChange as log no-undo.
define variable iCurrent as int no-undo.
define variable lCurrent as log no-undo.
define variable eCurrent as dec no-undo.
define variable cCurrent as char no-undo.

define query qSpec for partner, part, spec.
define variable hQuery as handle no-undo.

if iPlantID eq ? then do:
    assign pcRtn = '** Could not determine what plant to work in. (program/t0651, Routine ReviewChanges)'.
    return.
end.
assign
    hQuery = query qSpec:handle
    iSpecCount = 0
    iChangeCount = 0.

cQuery = 'for each partner no-lock where partner.plant-id eq ' + string(iPlantID)
    + (if iPartner ne ? then ' and partner.partner-id eq ' + string(iPartner) else '')
    + (if iOwner ne ? then ' and partner.owner-id eq ' + string(iOwner) else '')
    + (if iUser ne ? then ' and partner.user-id eq ' + string(iUser) else '')
    + ', each part no-lock where part.partner-id = partner.partner-id'
    + (if eMinGauge ne ? then ' and part.gauge ge ' + string(eMingauge) else '')
    + (if eMaxGauge ne ? then ' and part.gauge le ' + string(eMaxgauge) else '')
    + (if eMinWidth ne ? then ' and part.wdth ge ' + string(eMinWidth) else '')
    + (if eMaxWidth ne ? then ' and part.wdth le ' + string(eMaxWidth) else '')
    + (if eMinLength ne ? then ' and part.lngth ge ' + string(eMinLength) else '')
    + (if eMaxLength ne ? then ' and part.lngth le ' + string(eMaxLength) else '')
    + (if iMetalType ne ? then ' and part.type-id eq ' + string(iMetalType) else '')
    + ', each spec no-lock where spec.part-id eq part.part-id'
    + ' and not spec.obsolete and spec.order-id eq 0'
    + (if iProcCode ne ? then ' and spec.proc-code eq ' + string(iProcCode) else '').

empty temp-table ttChangeList.
empty temp-table ttSpecs.

hQuery:query-prepare(cQuery).
hQuery:query-open().

QryBlk: Repeat:
    hQuery:get-next.
    if hQuery:query-off-end then leave QryBlk.

    hBufferHdl = buffer spec:handle.
    cSize = string(part.gauge, "9.9999") + " x " + trim(string(part.wdth,">>9.9999"))
                   + (if part.lngth gt 0 then " x " + trim(string(part.lngth,">>9.9999")) else "")
                   + (if part.lngth2 gt 0 then " x " + trim(string(part.lngth2,">>9.9999")) else "").

    /* Load into list of specs */
    find first ttSpecs
      where ttSpecs.iSpecID = spec.spec-id no-error.
    if not avail ttSpecs then do:
       create ttSpecs.
       assign
         ttSpecs.iPartner = partner.partner-id
         ttSpecs.iPart = part.part-id
         ttspecs.cSize = cSize
         ttSpecs.iSpecID = spec.Spec-ID
         ttSpecs.iversion = spec.Version
         ttSpecs.iPartner = partner.partner-id
         ttSpecs.cLocked = (if spec.lockdate = ? then 'No' else 'Yes')
         ttSpecs.iNewSpec = ?
         ttSpecs.iNewVersion = ?.
    end.

    /* See if this spec has anything changeable */
    for each gttSpecChange:
        assign
            lChange = no
            hFieldHdl = hBufferHdl:buffer-field(gttSpecChange.cDbField).
        case gttSpecChange.cFieldtype:
            when 'integer' then do:
                iCurrent = hFieldHdl:buffer-value.
                if iCurrent ne gttSpecChange.iNewInteger then lChange = yes.
                if gttSpecChange.iOldInteger ne ?
                    and iCurrent ne gttSpecChange.iOldInteger then lChange = no.
                IF iCurrent = ? AND gttSpecChange.iOldInteger = 0 THEN lChange = YES.
                if iCurrent = gttSpecChange.iNewInteger then lChange = no.
/*                 if session:debug-alert then                                                                       */
/*                 MESSAGE substitute("Part: &1~nSpec: &2~nField: &3~nCurrent Val: &4~nNew Val: &5~nChanging?: &6",  */
/*                                    string(part.part-id),                                                          */
/*                                    string(spec.spec-id),                                                          */
/*                                    gttSpecChange.cDbField,                                                        */
/*                                    string(iCurrent),                                                              */
/*                                    string(gttSpecChange.iNewInteger),                                             */
/*                                    string(lChange))                                                               */
/*                                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                            */
                if lChange then do:
                    iChangeCount = iChangeCount + 1.
                    create ttChangeList.
                    assign
                        ttChangeList.iPartner = partner.partner-id
                        ttChangeList.iPart = part.part-id
                        ttChangeList.cSize = cSize
                        ttChangeList.iSpecID = spec.spec-id
                        ttChangelist.iVersion = spec.version
                        ttChangeList.iMetalType = part.type-id
                        ttChangeList.cField = gttSpecChange.cPrompt
                        ttChangeList.cDbField = gttSpecChange.cDbField
                        ttChangeList.cOldValue = string(hFieldHdl:buffer-value)
                        ttChangeList.cNewValue = string(gttSpecChange.iNewInteger).
                
                    if gttSpecChange.cDbField = "min-mult-wt" then do:
                        run CalcODMin.
                    end.
                    if gttSpecChange.cDbField = "max-mult-wt" then do:
                        run CalcODMax.   
                    end.
                end.
            end.
            when 'logical' then do:
                lCurrent = hFieldHdl:buffer-value.
                if lCurrent ne gttSpecChange.lNewLogical then lChange = yes.
                if gttSpecChange.lOldLogical ne ?
                    and lCurrent ne gttSpecChange.lOldLogical then lChange = no.
                if lCurrent = gttSpecChange.lNewLogical then lChange = no.
                if lChange then do:
                    iChangeCount = iChangeCount + 1.
                    create ttChangeList.
                    assign
                        ttChangeList.iPartner = partner.partner-id
                        ttChangeList.iPart = part.part-id
                        ttChangeList.cSize = cSize
                        ttChangeList.iSpecID = spec.spec-id
                        ttChangelist.iVersion = spec.version
                        ttChangeList.iMetalType = part.type-id
                        ttChangeList.cField = gttSpecChange.cPrompt
                        ttChangeList.cDbField = gttSpecChange.cDbField
                        ttChangeList.cOldValue = string(hFieldHdl:buffer-value)
                        ttChangeList.cNewValue = string(gttSpecChange.lNewLogical).
                end.
            end.
            when 'decimal' then do:
                eCurrent = hFieldHdl:buffer-value.
                if eCurrent ne gttSpecChange.eNewdecimal then lChange = yes.
                if gttSpecChange.eOldDecimal ne ?
                    and eCurrent ne gttSpecChange.eOldDecimal then lChange = no.
                if eCurrent = gttSpecChange.eNewDecimal then lChange = no.
                if lChange then do:
                    iChangeCount = iChangeCount + 1.
                    create ttChangeList.
                    assign
                        ttChangeList.iPartner = partner.partner-id
                        ttChangeList.iPart = part.part-id
                        ttChangeList.cSize = cSize
                        ttChangeList.iSpecID = spec.spec-id
                        ttChangelist.iVersion = spec.version
                        ttChangeList.iMetalType = part.type-id
                        ttChangeList.cField = gttSpecChange.cPrompt
                        ttChangeList.cDbField = gttSpecChange.cDbField
                        ttChangeList.cOldValue = string(hFieldHdl:buffer-value)
                        ttChangeList.cNewValue = string(gttSpecChange.eNewDecimal).
                
                    if gttSpecChange.cDbField = "ODMin" then do:
                        run CalcMinMultWt.
                    end.
                    if gttSpecChange.cDbField = "ODMax" then do:
                        run CalcMaxMultWt.   
                    end.
                end.
            end.
            when 'character' then do:
                cCurrent = (if gttSpecChange.cDbField = 'mandrel'
                            then substitute("&1,&2,&3",
                                             (if spec.mandrel[1] = ? then '?' else string(spec.mandrel[1])),
                                             (if spec.mandrel[2] = ? then '?' else string(spec.mandrel[2])),
                                             (if spec.mandrel[3] = ? then '?' else string(spec.mandrel[3])))
                            else hFieldHdl:buffer-value).
                if cCurrent ne gttSpecChange.cNewChar then lChange = yes.
                if gttSpecChange.cOldChar ne '?'
                    and cCurrent ne gttSpecChange.cOldChar then lChange = no.
                if cCurrent = gttSpecChange.cNewChar then lChange = no.
                if lChange then do:
                    iChangeCount = iChangeCount + 1.
                    create ttChangeList.
                    assign
                        ttChangeList.iPartner = partner.partner-id
                        ttChangeList.iPart = part.part-id
                        ttChangeList.cSize = cSize
                        ttChangeList.iSpecID = spec.spec-id
                        ttChangelist.iVersion = spec.version
                        ttChangeList.iMetalType = part.type-id
                        ttChangeList.cField = gttSpecChange.cPrompt
                        ttChangeList.cDbField = gttSpecChange.cDbField
                        ttChangeList.cOldValue = string(hFieldHdl:buffer-value)
                        ttChangeList.cNewValue = string(gttSpecChange.cNewChar).
                end.
            end.
        end case.
    end.
end.

  hQuery:query-close().
  if valid-handle(hQuery) then delete object hQuery.
  if valid-handle(hBufferHdl) then delete object hBufferHdl.
  if valid-handle(hFieldHdl) then delete object hFieldHdl.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CalcMaxMultWt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcMaxMultWt Procedure 
PROCEDURE CalcMaxMultWt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/                                        
                           
assign dPI4 = 3.1415926535897932384626433832795 / 4.    
                                                                                
  if part.wdth eq 0  then next.                                         
  run common/t0090 (input spec.part-id, part.type-id, output dDensity). 
                                                                        
 if spec.mandrel[1] = 0 or spec.mandrel[1] = ? then iMandrel1 = 99.     
 else iMandrel1 = spec.mandrel[1].                                      
 if spec.mandrel[2] = 0 or spec.mandrel[2] = ? then iMandrel2 = 99.     
 else iMandrel2 = spec.mandrel[2].                                      
 if spec.mandrel[3] = 0 or spec.mandrel[3] = ? then iMandrel3 = 99.             
 else iMandrel3 = spec.mandrel[3].                                              
 did = min(iMandrel1, iMandrel2, iMandrel3).                                    
                                                                                
  find choice where choice.field-no = 39 and choice.val = spec.core no-lock    
   no-error.                                                                    
    if  avail choice and choice.misc3 ne '' then                                
      dId = did + decimal(choice.misc3) * 2.                                    
                                                                                                                
assign iMaxMultWt = ((gttSpecChange.eNewDecimal * gttSpecChange.eNewDecimal) * dPI4 * dDensity * part.wdth)   
 - ((did * diD) * dPI4 * dDensity * part.wdth).  

cSize1 = ttChangeList.cSize.
                                                                                
if lChangeList = yes then do:
    create ttChangeList.
            assign
                ttChangeList.iPartner = partner.partner-id
                ttChangeList.iPart = part.part-id
                ttChangeList.cSize = cSize1
                ttChangeList.iSpecID = spec.spec-id
                ttChangelist.iVersion = spec.version
                ttChangeList.iMetalType = part.type-id
                ttChangeList.cField = "Mult Wt Max"
                ttChangeList.cDbField = "max-mult-wt"
                ttChangeList.cOldValue = string(spec.max-mult-wt)
                ttChangeList.cNewValue = string(iMaxMultWt).
end.
                                                                                                                                                               


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CalcMinMultWt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcMinMultWt Procedure 
PROCEDURE CalcMinMultWt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/                                      
                           
assign dPI4 = 3.1415926535897932384626433832795 / 4.      
                                                                                
  if part.wdth eq 0  then next.                                         
  run common/t0090 (input spec.part-id, part.type-id, output dDensity). 
                                                                        
 if spec.mandrel[1] = 0 or spec.mandrel[1] = ? then iMandrel1 = 99.     
 else iMandrel1 = spec.mandrel[1].                                      
 if spec.mandrel[2] = 0 or spec.mandrel[2] = ? then iMandrel2 = 99.     
 else iMandrel2 = spec.mandrel[2].                                      
 if spec.mandrel[3] = 0 or spec.mandrel[3] = ? then iMandrel3 = 99.             
 else iMandrel3 = spec.mandrel[3].                                              
 did = min(iMandrel1, iMandrel2, iMandrel3).                                    
                                                                                
  find choice where choice.field-no = 39 and choice.val = spec.core no-lock    
   no-error.                                                                    
    if  avail choice and choice.misc3 ne '' then                                
      dId = did + decimal(choice.misc3) * 2.                                    
                                                                                                                
assign iMinMultWt = ((gttSpecChange.eNewDecimal * gttSpecChange.eNewDecimal) * dPI4 * dDensity * part.wdth)   
 - ((did * diD) * dPI4 * dDensity * part.wdth).  

cSize1 = ttChangeList.cSize.

if lChangeList = yes then do:                                                                                
    create ttChangeList.
            assign
                ttChangeList.iPartner = partner.partner-id
                ttChangeList.iPart = part.part-id
                ttChangeList.cSize = cSize1
                ttChangeList.iSpecID = spec.spec-id
                ttChangelist.iVersion = spec.version
                ttChangeList.iMetalType = part.type-id
                ttChangeList.cField = "Mult Weight Min"
                ttChangeList.cDbField = "min-mult-wt"
                ttChangeList.cOldValue = string(spec.min-mult-wt)
                ttChangeList.cNewValue = string(iMinMultWt).
end.
                                                                                                                                                               


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CalcODMax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcODMax Procedure 
PROCEDURE CalcODMax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/                                        
assign dPI4 = 3.1415926535897932384626433832795 / 4.    
                                                                                
  if part.wdth eq 0  then next.                                         
  run common/t0090 (input 0, part.type-id, output dDensity).            
                                                                        
 if spec.mandrel[1] = 0 or spec.mandrel[1] = ? then iMandrel1 = 99.     
 else iMandrel1 = spec.mandrel[1].                                      
 if spec.mandrel[2] = 0 or spec.mandrel[2] = ? then iMandrel2 = 99.     
 else iMandrel2 = spec.mandrel[2].  
 if spec.mandrel[3] = 0 or spec.mandrel[3] = ? then iMandrel3 = 99.
 else iMandrel3 = spec.mandrel[3].                                          
 did = min(iMandrel1, iMandrel2, iMandrel3).                                
                                                                            
 find choice where choice.field-no = 39 and choice.val = spec.core no-lock  
  no-error.                                                                 
    if  avail choice and choice.misc3 ne '' then                            
      dId = did + decimal(choice.misc3) * 2.                                                                                                                          
                                                                            
 assign dMaxOD = sqrt(((gttSpecChange.iNewInteger / 1) + ((dID * dID) * dPI4 *       
 dDensity * part.wdth)) / (dPI4 * dDensity * part.wdth)).    

 cSize1 = ttChangeList.cSize.

create ttChangeList.
    assign
        ttChangeList.iPartner = partner.partner-id
        ttChangeList.iPart = part.part-id
        ttChangeList.cSize = cSize1
        ttChangeList.iSpecID = spec.spec-id
        ttChangelist.iVersion = spec.version
        ttChangeList.iMetalType = part.type-id
        ttChangeList.cField = "O.D. Max"
        ttChangeList.cDbField = "ODMax"
        ttChangeList.cOldValue = string(spec.ODMax)
        ttChangeList.cNewValue = string(dMaxOD).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CalcODMin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CalcODMin Procedure 
PROCEDURE CalcODMin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/                                             
assign dPI4 = 3.1415926535897932384626433832795 / 4.  
                                                                                
  if part.wdth eq 0  then next.                                         
  run common/t0090 (input 0, part.type-id, output dDensity).            
                                                                        
 if spec.mandrel[1] = 0 or spec.mandrel[1] = ? then iMandrel1 = 99.     
 else iMandrel1 = spec.mandrel[1].                                      
 if spec.mandrel[2] = 0 or spec.mandrel[2] = ? then iMandrel2 = 99.     
 else iMandrel2 = spec.mandrel[2].  
 if spec.mandrel[3] = 0 or spec.mandrel[3] = ? then iMandrel3 = 99.
 else iMandrel3 = spec.mandrel[3].                                          
 did = min(iMandrel1, iMandrel2, iMandrel3).                                
                                                                            
 find choice where choice.field-no = 39 and choice.val = spec.core no-lock  
  no-error.                                                                 
    if  avail choice and choice.misc3 ne '' then                            
      dId = did + decimal(choice.misc3) * 2.                                                                                                                          
                                                                            
 assign dMinOD = sqrt(((gttSpecChange.iNewInteger / 1) + ((dID * dID) * dPI4 *       
 dDensity * part.wdth)) / (dPI4 * dDensity * part.wdth)).    

 cSize1 = ttChangeList.cSize.

if lChangeList = yes then do:
    create ttChangeList.
        assign
            ttChangeList.iPartner = partner.partner-id
            ttChangeList.iPart = part.part-id
            ttChangeList.cSize = cSize1
            ttChangeList.iSpecID = spec.spec-id
            ttChangelist.iVersion = spec.version
            ttChangeList.iMetalType = part.type-id
            ttChangeList.cField = "O.D. Min"
            ttChangeList.cDbField = "ODMin"
            ttChangeList.cOldValue = string(spec.ODMin)
            ttChangeList.cNewValue = string(dMinOD).
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CommitChanges) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CommitChanges Procedure 
PROCEDURE CommitChanges :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cFieldList as char no-undo.
define variable lReview as logical no-undo.
define variable iNewSpecID as int no-undo.
define variable cSetupText as char no-undo.
define variable cProcessText as char no-undo.
define variable cWrapText as char no-undo.
define variable cPckgText as char no-undo.
define variable cShipText as char no-undo.
define variable cErr as char no-undo.
define variable cPigList as char no-undo.
define variable cOrderList as char no-undo.
define variable cPSIPOList as char no-undo.
define variable iPOCount as int no-undo.

define variable eGauge-Neg    as decimal   no-undo.
define variable eGauge-Pos    as decimal   no-undo.
define variable eWdth-Neg     as decimal   no-undo.
define variable eWdth-Pos     as decimal   no-undo.
define variable eLngth-Neg    as decimal   no-undo.
define variable eLngth-Pos    as decimal   no-undo.
define variable eLngth2-Neg   as decimal   no-undo.
define variable eLngth2-Pos   as decimal   no-undo.
define variable iMandrel1     as integer   no-undo.
define variable iMandrel2     as integer   no-undo.
define variable iMandrel3     as integer   no-undo.
define variable iStencilType  as integer   no-undo.
define variable iInterleaving as integer   no-undo.
define variable cPckgCode     as character no-undo.

empty temp-table ttProblems.

find first gttSpecChange no-lock
    where gttSpecChange.cDbField = "min-mult-wt" no-error.
            if avail gttSpecChange and cDbFieldList = "" then
                assign cDbFieldList = gttSpecChange.cDbField.
            else if avail gttSpecChange and cDbFieldList ne "" then 
                assign cDbFieldList = cDbFieldList + "|" + gttSpecChange.cDbField.
find first gttSpecChange no-lock
    where gttSpecChange.cDbField = "max-mult-wt" no-error.
            if avail gttSpecChange and cDbFieldList = "" then
                assign cDbFieldList = gttSpecChange.cDbField.
            else if avail gttSpecChange and cDbFieldList ne "" then 
                assign cDbFieldList = cDbFieldList + "|" + gttSpecChange.cDbField.
find first gttSpecChange no-lock
    where gttSpecChange.cDbField = "ODMin" no-error.
            if avail gttSpecChange and cDbFieldList = "" then
                assign cDbFieldList = gttSpecChange.cDbField.
            else if avail gttSpecChange and cDbFieldList ne "" then 
                assign cDbFieldList = cDbFieldList + "|" + gttSpecChange.cDbField.
find first gttSpecChange no-lock
    where gttSpecChange.cDbField = "ODMax" no-error.
            if avail gttSpecChange and cDbFieldList = "" then
                assign cDbFieldList = gttSpecChange.cDbField.
            else if avail gttSpecChange and cDbFieldList ne "" then 
                assign cDbFieldList = cDbFieldList + "|" + gttSpecChange.cDbField.
run AddToSpecChangeTbl.
    


run BuildChangeList.  /* Temp-table of detailed changes */

if iChangeCount = 0 then do:
    pcRtn = 'No Changes Required for the parameters specified.'.
end.
else do:
    cFieldList = "Mandrel,min-mult-wt,max-mult-wt,ODMin,ODMax,gauge-neg,gauge-pos,wdth-neg,wdth-pos,lngth-neg,lngth-pos,lngth2-neg,lngth2-pos,min-pckg-wt,max-pckg-wt,min-pc-ct,max-pc-ct,lube,lube-wt,lube-min,lube-max,exposed".
    for each ttSpecs:
        if ttSpecs.cLocked = 'Yes' then do:
            /* This is a locked spec, make a new version */
            assign
                cPigList = ''
                cOrderList = ''
                cPSIPOList = ''.

            /* 1) is re-Review needed? */
            assign
                lReview = no.
            do iIdx = 1 to num-entries(cFieldList):
                if can-find(first ttChangeList
                            where ttChangeList.iSpecID = ttSpecs.iSpecID
                            and ttChangelist.cDbField = entry(iIdx,cFieldList)) then
                    assign lReview = yes.
            end.

            /* 2) Get list of affected Proc-items, work-items, PIGs */
            for each pig no-lock where pig.spec-id = ttSpecs.iSpecID:
                assign cPigList = cPigList + (if cPigList = '' then '' else ',') + string(rowid(pig)).
            end.
            for each prod-sched no-lock
                 where prod-sched.plant-id = iPlantID,
               each work-item no-lock
                  where work-item.order-id = prod-sched.order-id
                  and work-item.spec-id = ttSpecs.iSpecID:
               assign cOrderList = cOrderList + (if cOrderList = '' then '' else ',') + string(rowid(work-item)) .
            end.
            for each ProcItem no-lock
               where ProcItem.spec-id = ttSpecs.iSpecID:
               assign cPSIPOList = cPSIPOList + (if cPsiPOList = '' then '' else '~n') + string(rowid(ProcItem)).
            end.
            
            /* 3) Gather up all the notes on the old version */              
            run common/t0611.p (input 'FetchNotes',
                                  input 'SpecID=' + string(ttSpecs.iSpecID),
                                  input 0,
                                  input 0,
                                  output cSetupText,
                                  output cProcesstext,
                                  output cWrapText,
                                  output cPckgText,
                                  output cShipText,
                                  output eGauge-Neg,
                                  output eGauge-Pos,
                                  output eWdth-Neg,
                                  output eWdth-Pos,
                                  output eLngth-Neg,
                                  output eLngth-Pos,
                                  output eLngth2-Neg,
                                  output eLngth2-Pos,
                                  output iMandrel1,
                                  output iMandrel2,
                                  output iMandrel3,
                                  output iStencilType,
                                  output iInterleaving,
                                  output cPckgCode,
                                  output cErr).
              if cErr ne '' then assign ttSpecs.cRemark = cErr.
 
             /* 4) Copy the spec */

              run common/t0078.p
                          ( input  ttSpecs.iPartner,
                            input  0,
                            input  '',
                            input  0,
                            input  ttSpecs.iSpecID,
                            input  yes,
                            input  0,
                            input cSetupText,
                            input cProcessText,
                            input cWrapText,
                            input cPckgText,
                            input cShipText,
                            input   eGauge-Neg,
                            input   eGauge-Pos,
                            input   eWdth-Neg,
                            input   eWdth-Pos,
                            input   eLngth-Neg,
                            input   eLngth-Pos,
                            input   eLngth2-Neg,
                            input   eLngth2-Pos,
                            input   iMandrel1,
                            input   iMandrel2,
                            input   iMandrel3,
                            input   iStencilType,
                            input   iInterleaving,
                            input   cPckgCode,
                            output iNewSpecID).

               if return-value <> '' then do:
                   assign ttSpecs.cRemark = ttSpecs.cRemark + ' - ' + return-value.
               end.
               ttSpecs.iNewSpec = iNewSpecID.

               /* 5) Apply to Procitems, Orders, PIGs */
               run common/t0076.p
                    (input ttSpecs.iSpecID,  /* Old Spec ID */
                     input iNewSpecId,
                     input cPSIPOList,
                     input cOrderList,
                     input cPIGList,
                     output cErr,
                     output iPOCount).
               /* 6) Apply Changes to the newly created spec */
               run ApplyChange (input ttSpecs.iNewSpec,
                                output cErr).
               if cErr ne '' then assign ttSpecs.cRemark = ttSpecs.cRemark + ' ' + cErr.

               if lReview then do:   /* Un-Review any affected Proc Items */
                   for each procItem exclusive-lock
                       where ProcItem.spec-id = ttSpecs.iNewSpec:
                       assign
                           ProcItem.ReviewBy = ''
                           ProcItem.UnReviewReason = 0
                           ttSpecs.cRemark = ttSpecs.cRemark + ' / This Process Item now requires review.'
                           .
                   end.
               end.
        end.
        else do:
            /* Just make changes to the current spec */
            run ApplyChange (input ttSpecs.iSpecID,
                             output cErr).
             if cErr ne '' then assign ttSpecs.cRemark = ttSpecs.cRemark + ' ' + cErr.
        end.
    end.
    /* make a report of this transaction */

    run lib/getosfilename(output pcReport). 
    assign
        pcReport = substring(pcReport, 1, (length(pcReport) - 4)) + '.XLS'
        hTemphand = temp-table ttChangeList:handle   
        hTemphand2 = temp-table ttSpecs:handle   
        cSubTitle = "Completed Spec Field Changes" 
        piWorkOrders = 0
        piProcOrders = 0.                                                          
                                                                                 
    RUN lib/t0476.p PERSISTENT SET hXProcHand.                  
                                                                                                               
    run OpenWorkBook in hXProcHand(
            input pcReport,       /* Name of File to Create   */
            input yes,              /* Define Header Styles     */
            input yes,              /* Will there be a Title    */
            input yes).             /* Will there be a SubTitle */

    run AddTab in hXProcHand(
            input table-handle hTempHand2,   /* Handle to Data Table */
            input 'Specs Created',    /* Tab Name             */
            input yes,                       /* Create Header        */
            input 'Precision Strip, Inc.',   /* Title                */
            input cSubTitle).                /* SubTitle Line        */

    run AddTab in hXProcHand(
            input table-handle hTempHand,    /* Handle to Data Table */
            input 'Field Change Detail',     /* Tab Name             */
            input yes,                       /* Create Header        */
            input 'Precision Strip, Inc.',   /* Title                */
            input cSubTitle).                /* SubTitle Line        */

    run CloseWorkBook in hXProcHand.

    delete procedure hXprochand.
    empty temp-table ttSpecs.
    empty temp-table ttChangeList.
   
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ParseOpts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ParseOpts Procedure 
PROCEDURE ParseOpts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cProp as char no-undo.
define variable cVar as char no-undo.
define variable cVal as char no-undo.

assign
   iPlantID = ?
   iPartner = ?
   iOwner = ?
   iUser = ?
   iProcCode = ?
   eMinGauge = ?
   eMaxgauge = ?
   eMinWidth = ?
   eMaxWidth = ?
   eMinLength = ?
   eMaxLength = ?
   iMetalType = ?.

do iIdx = 1 to num-entries(pcOptionList,"|"):
  cProp = entry(iIdx,pcOptionList,"|").
  if num-entries(cProp,"=") gt 1 then do:
    assign cVar = entry(1,cProp,"=")
           cVal  = entry(2,cProp,"=").

     if cVar = 'PlantID'   then assign iPlantID   = integer(cVal).
     if cVar = 'Partner'   then assign iPartner   = integer(cVal).
     if cVar = 'Owner'     then assign iOwner     = integer(cVal).
     if cVar = 'User'      then assign iUser      = integer(cVal).
     if cVar = 'ProcCode'  then assign iProcCode  = integer(cVal).
     if cVar = 'MinGauge'  then assign eMinGauge  = decimal(cVal).
     if cVar = 'MaxGauge'  then assign eMaxGauge  = decimal(cVal).
     if cVar = 'MinWidth'  then assign eMinWidth  = decimal(cVal).
     if cVar = 'MaxWidth'  then assign eMaxWidth  = decimal(cVal).
     if cVar = 'MinLength' then assign eMinLength = decimal(cVal).
     if cVar = 'MaxLength' then assign eMaxLength = decimal(cVal).
     if cVar = 'MetalType' then assign iMetalType = integer(cVal).
     end.
     
  else
    next.

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReviewChanges) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReviewChanges Procedure 
PROCEDURE ReviewChanges :
empty temp-table ttProblems.

run BuildChangeList.

if iChangeCount = 0 then do:
    assign pcRtn = 'No Changes Required for the parameters specified.'.
end.
else do:
    run ValidateChanges.
    run lib/getosfilename(output pcReport). 
    assign
        pcReport = substring(pcReport, 1, (length(pcReport) - 4)) + '.XLS'
        hTemphand = temp-table ttChangeList:handle   
        cSubTitle = "Proposed Spec Field Changes" 
        piWorkOrders = 0
        piProcOrders = 0.        

    if can-find(first ttProblems) then assign
        pcRtn = '** Validation problems exist with proposed changes.~nDetails are in the report...'
        hTemphand = temp-table ttProblems:handle   
        cSubTitle = "Validation Issues" 
        piWorkOrders = 0
        piProcOrders = 0.        

                                                                                 
    RUN lib/t0476.p PERSISTENT SET hXProcHand.                  
                                                                                                               
    run makeDocument in hXProcHand (                                             
       table-handle hTemphand,   /* handle to temp-table */                      
       true,                      /* Include a header? */                         
       "Spec Changes",       /* The name of the worksheet */                 
       "Precision Strip, Inc.",   /* title line */                                
       cSubTitle,                /* Sub-Title Line */                            
       pcReport).                   /* An x-document handle created in this procedure */              
                                              
    DELETE PROCEDURE hXProcHand.    

    for each prod-sched no-lock
        where prod-sched.plant-id = iPlantID,
      each work-item no-lock
        where work-item.order-id = prod-sched.order-id,
      first ttSpecs no-lock
        where ttSpecs.iSpecID = work-item.spec-id:
        assign piWorkOrders = piWorkOrders + 1.
    end.

    for each ttSpecs,
      each ProcItem no-lock
        where ProcItem.spec-id = ttSpecs.iSpecID:
        assign piProcOrders = piProcOrders + 1.
    end.
end.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValidateChanges) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateChanges Procedure 
PROCEDURE ValidateChanges :
/*------------------------------------------------------------------------------
  Purpose:    Validate pending changes 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define variable hBufferHdl as handle.
define variable hFieldHdl as handle.
define variable lChange as logical no-undo.
define variable iCurrent as int no-undo.
define variable lCurrent as log no-undo.
define variable eCurrent as dec no-undo.
define variable cCurrent as char no-undo.

/* place spec record in a temp table,
   apply changes to it and run same tests as spec-lp PreTransValidate*/

for each ttSpecs:
    find first spec no-lock
        where spec.spec-id = ttSpecs.iSpecID no-error.
    if not avail spec then next.
    create ttModSpec.
    buffer-copy spec to ttModSpec.

    find first part no-lock where part.part-id = ttSpecs.iPart.

    hBufferHdl = buffer ttModSpec:handle.

/* See if this spec has anything changeable */
    for each gttSpecChange:
        if gttSpecChange.cDbField = 'mandrel' then next.  /* no validations, complicated, so skip for now */
        assign
            lChange = no
            hFieldHdl = hBufferHdl:buffer-field(gttSpecChange.cDbField).
        case gttSpecChange.cFieldtype:
            when 'integer' then do:
                iCurrent = hFieldHdl:buffer-value.
                if iCurrent ne gttSpecChange.iNewInteger then lChange = yes.
                if gttSpecChange.iOldInteger ne ?
                    and iCurrent ne gttSpecChange.iOldInteger then lChange = no.
                IF iCurrent = ? AND gttSpecChange.iOldInteger = 0 THEN lChange = YES.
                if iCurrent = gttSpecChange.iNewInteger then lChange = no.
                if lChange then assign
                    hFieldHdl:buffer-value = gttSpecChange.iNewInteger.
            end.
            when 'logical' then do:
                lCurrent = hFieldHdl:buffer-value.
                if lCurrent ne gttSpecChange.lNewLogical then lChange = yes.
                if gttSpecChange.lOldLogical ne ?
                    and lCurrent ne gttSpecChange.lOldLogical then lChange = no.
                if lCurrent = gttSpecChange.lNewLogical then lChange = no.
                if lChange then assign
                    hFieldHdl:buffer-value = gttSpecChange.lNewLogical.
            end.
            when 'decimal' then do:
                eCurrent = hFieldHdl:buffer-value.
                if eCurrent ne gttSpecChange.eNewdecimal then lChange = yes.
                if gttSpecChange.eOldDecimal ne ?
                    and eCurrent ne gttSpecChange.eOldDecimal then lChange = no.
                if eCurrent = gttSpecChange.eNewDecimal then lChange = no.
                if lChange then assign
                    hFieldHdl:buffer-value = gttSpecChange.eNewDecimal.
            end.
            when 'character' then do:
                cCurrent = hFieldHdl:buffer-value.
                if cCurrent ne gttSpecChange.cNewChar then lChange = yes.
                if gttSpecChange.cOldChar ne '?'
                    and cCurrent ne gttSpecChange.cOldChar then lChange = no.
                if cCurrent = gttSpecChange.cNewChar then lChange = no.
                if lChange then 
                    assign hFieldHdl:buffer-value = gttSpecChange.cNewChar.
            end.
        end case.
    end.
   /* temp-table version has changes applied, run the tests */
    if  Part.gauge + ttModSpec.gauge-neg lt 0 then
      run addwarning(input ttSpecs.iSpecID, input "Invalid value for Spec Gauge Minimum. Value is too large for the part gauge and would calculate to be less than 0.",input "gauge-neg").

    if ttModSpec.gauge-neg gt ttModSpec.gauge-pos then
      run addwarning(input ttSpecs.iSpecID, input "Min Gauge Cannot be > Max Gauge Tolerance",input "gauge-neg").

    if ttModSpec.wdth-neg gt ttModSpec.wdth-pos then
      run addwarning(input ttSpecs.iSpecID, input "Min Width Cannot be > Max Width Tolerance",input "wdth-neg").

    if ttModSpec.lngth-neg gt ttModSpec.lngth-pos then
      run addwarning(input ttSpecs.iSpecID, input "Min Length Cannot be > Max Length Tolerance",input "lngth-neg").

    if ttModSpec.lngth2-neg gt ttModSpec.lngth2-pos then
      run addwarning(input ttSpecs.iSpecID, input "Min Length2 Cannot be > Max Length2 Tolerance",input "lngth2-neg").

    if ttModSpec.pref-wdth-neg gt ttModSpec.pref-wdth-pos then
      run addwarning(input ttSpecs.iSpecID, input "Min preffered width Cannot be > Max preffered width Tolerance",input "pref-wdth-neg").

    if (ttModSpec.core eq 1 or ttModSpec.core eq 2) then
      run addwarning(input ttSpecs.iSpecID, input "Split Fiber Core and Solid Fiber Core are Obsolete Please Pick Exact Size and Type of Core.",input "core").

    if ttModSpec.lube-qual gt 0 
    and ttModSpec.lube-wt eq ?  then
      run addwarning(input ttSpecs.iSpecID, input "Weight required with qualifier",input "lube-wt").

    if ttModSpec.ODMin gt ttModSpec.ODMax then
      run addwarning(input ttSpecs.iSpecID, input "Min OD is greater than Max OD",input "ODMin").

    if ttModSpec.min-mult-wt gt ttModSpec.max-mult-wt then
      run addwarning(input ttSpecs.iSpecID, input "Min mult weight is greater than Max mult weight",input "min-mult-wt").

    if ttModSpec.min-pc-ct gt ttModSpec.max-pc-ct then
      run addwarning(input ttSpecs.iSpecID, input "Min piece count is greater than Max piece count",input "min-pc-ct").  

    if ttModSpec.min-pckg-wt gt ttModSpec.max-pckg-wt then
      run addwarning(input ttSpecs.iSpecID, input "Min Package Weight Can Not be Greater than Max Package Weight",input "max-pckg-wt").

    if ttModSpec.min-pckg-ht gt ttModSpec.max-pckg-ht then
      run addwarning(input ttSpecs.iSpecID, input "Min Package Height Can Not be Greater than Max Package Height",input "max-pckg-ht").

    if ttModSpec.min-mult-cnt gt ttModSpec.max-mult-cnt then
      run addwarning(input ttSpecs.iSpecID, input "Min Mult/Package Count Can Not be Greater than Max Mult/Package Count",input "max-mult-cnt").

    if ((ttModSpec.max-pckg-wt ge 0 and ttModSpec.max-mult-wt ge 0) 
        and (ttModSpec.max-mult-wt gt ttModSpec.max-pckg-wt))
    and ttModSpec.max-pckg-wt ne 99999 
    and ttModSpec.max-mult-wt ne 99999  then
      run addwarning(input ttSpecs.iSpecID, input "Max Package Weight Can Not be Less than Max Mult Weight",input "max-pckg-wt").

      find choice no-lock 
           where choice.field-no eq 387
           and   choice.val      eq ttModSpec.tagloc no-error.
        if available choice then
        do:
          if (ttModSpec.numtag gt 0 or ttModSpec.tagloc gt 0) 
          and choice.misc2 ne '0' 
          and choice.misc2 ne string(ttModSpec.numtag) then
            run addwarning(input ttSpecs.iSpecID,
                           input substitute("Tag location '&1' is not valid for the number of tags = &2",
                                            choice.val, string(ttModSpec.numtag))
                           ,input "numtag").
        end.
        else
          run addwarning(input ttSpecs.iSpecID, input "Invalid Tag location","tagloc").
     
    if ttModSpec.od-bnd-type eq ? 
    and ttModSpec.od-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "OD Band(s) Specified.  Must have valid OD Band Type",input "od-bnd-no").

    if ttModSpec.od-bnd-loc eq ? 
    and ttModSpec.od-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "OD Band(s) Specified.  Must have valid OD Location",input "od-bnd-no").

    if ttModSpec.od-bnd-ep eq ? 
    and ttModSpec.od-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "OD Band(s) Specified.  Must have valid OD Band Edge Protector",input "od-bnd-no").

    if ttModSpec.core-bnd-type eq ? 
    and ttModSpec.core-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Core Tie(s) Specified.  Must have valid Core Tie Type",input "core-bnd-no").

    if ttModSpec.core-bnd-loc eq ? 
    and ttModSpec.core-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Core Tie(s) Specified.  Must have valid Core Tie Location",input "core-bnd-no  ").

    if ttModSpec.core-bnd-ep eq ? 
    and ttModSpec.core-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Core Tie(s) Specified.  Must have valid Core Tie Edge Protector",input "core-bnd-no").            

    if ttModSpec.grp-bnd-type eq ? 
    and ttModSpec.grp-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Group Band(s) specified. Must have valid Group Band Type",input "grp-bnd-no").            

    if ttModSpec.grp-bnd-loc eq ? 
    and ttModSpec.grp-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Group Band(s) specified. Must have valid Group Band Location",input "grp-bnd-no").            

    if ttModSpec.grp-bnd-ep eq ? 
    and ttModSpec.grp-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Group Band(s) specified. Must have valid Group Band Edge Protector",input "grp-bnd-no").            

    if ttModSpec.bel-bnd-type eq ? 
    and ttModSpec.bel-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Belly Band(s) specified. Must have valid Belly Band Type",input "bel-bnd-no").            

    if ttModSpec.bel-bnd-ep eq ? 
    and ttModSpec.bel-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Belly Band(s) specified. Must have valid Belly Band Edge Protector",input "bel-bnd-no").            

    if ttModSpec.skid-bnd-type eq ? 
    and ttModSpec.skid-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Skid Tie(s) specified. Must have valid Skid Tie Type",input "skid-bnd-no").                   

    if ttModSpec.skid-bnd-loc eq ? 
    and ttModSpec.skid-bnd-no gt 0   then
      run addwarning(input ttSpecs.iSpecID, input "Skid Tie(s) specified. Must have valid Skid Tie Location",input "skid-bnd-no").                   

    if ttModSpec.skid-bnd-ep eq ? 
    and ttModSpec.skid-bnd-no gt 0   then
      run addwarning(input ttSpecs.iSpecID, input "Skid Tie(s) specified. Must have valid Skid Tie Edge",input "skid-bnd-no").                   

    if ttModSpec.post-bnd-type eq ? 
    and ttModSpec.post-bnd-no gt 0   then
      run addwarning(input ttSpecs.iSpecID, input "Post Tie(s) specified. Must have valid Post Tie Type",input "post-bnd-no").                   

    if ttModSpec.post-bnd-ep eq ? 
    and ttModSpec.post-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Post Tie(s) specified. Must have valid Post Tie Edge",input "post-bnd-no").                   

    if ttModSpec.cntr-bnd-type eq ? 
    and ttModSpec.cntr-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "Center Tie(s) specified.  Must have valid Center Tie Type",input "cntr-bnd-no").                   

    if ttModSpec.cntr-bnd-ep eq ? 
    and ttModSpec.cntr-bnd-no gt 0   then
      run addwarning(input ttSpecs.iSpecID, input "Center Tie(s) specified. Must have valid Center Tie Edge",input "cntr-bnd-no").                   

    if ttModSpec.lw-bnd-type eq ? 
    and ttModSpec.lw-bnd-no gt 0 then
      run addwarning(input ttSpecs.iSpecID, input "LW Band(s) specified. Must have valid Lengthwise Band Type",input "lw-bnd-no").                   

    if ttModSpec.lw-bnd-ep eq ? 
    and ttModSpec.lw-bnd-no gt 0   then
      run addwarning(input ttSpecs.iSpecID, input "LW Band(s) specified. Must have valid Lengthwise Band Edge",input "lw-bnd-no").                   

    if ttModSpec.cw-bnd-type eq ? 
    and ttModSpec.cw-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "CW Band(s) specified. Must have valid Crosswise Band Type",input "cw-bnd-no").                   

    if ttModSpec.cw-bnd-ep eq ? 
    and ttModSpec.cw-bnd-no gt 0  then
      run addwarning(input ttSpecs.iSpecID, input "CW Band(s) specified. Must have valid Crosswise Band Edge",input "cw-bnd-no").                   

end.   /* each ttspecs loop */
end.  /* procedure*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

