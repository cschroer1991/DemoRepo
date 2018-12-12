&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0765.p
    Purpose     : This is the procedure file of procedures for the CareGo 
                  automated crane system.
    Author(s)   : Zak Hurley    
    Created     : 5/19/15
    Notes       :
10/02/15 zakh 69805 Fixed two issues with asns and tripped packages.
06/08/16 zakh 71634 new functionality for offset due to linked bunks
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{includes/i0001.i}
{includes/i0207.i} /*ttParams for CareGo definition*/
{includes/xpath.i}

define input  parameter pcAction as character no-undo.
define input  parameter pcLogon as character no-undo.
define input  parameter table for ttParams.
define output parameter pcMsg as character no-undo.

define variable cParams     as character    no-undo.

define variable xDoc        as handle       no-undo.
define variable xResp       as handle       no-undo.
define variable hRoot       as handle       no-undo.
define variable hWebSocket  as handle       no-undo.
define variable lcXML       as longchar     no-undo.
define variable lcResponse  as longchar     no-undo.
define variable cResponse   as character    no-undo.
define variable cNodeRoot   as character    no-undo.
define variable cWSURL      as character    no-undo.
define variable hWebService as handle       no-undo.
define variable hWSPort     as handle       no-undo.
define variable cPriority   as character    no-undo.
define variable cJobName    as character    no-undo.

define temp-table tCoils
    field iInvID as integer
    field iSBMSID as integer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getTripID) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTripID Procedure 
FUNCTION getTripID RETURNS INTEGER
  ( input piInv as integer,
    input piPrev as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ParseMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ParseMsg Procedure 
FUNCTION ParseMsg RETURNS CHARACTER
  (input pcString as character )  FORWARD.

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

if pcAction = "" or pcAction = ? then do:
    pcMsg = "Invalid procedure name passed to t0765.p".
    return.
end. /*if pcAction*/

for each ttParams:
    cParams = substitute("&1,&2|&3", cParams, ttParams.cName, ttParams.cValue).
    cParams = trim(cParams, ",").
end. /*for each ttParams*/

if pcAction <> "getShipment" and pcAction <> "CheckShipment" then do:
    create sbms.
    assign
        sbms.created = now
        sbms.logon = pcLogon
        sbms.plant-id = 9
        sbms.procedure = pcAction
        sbms.transID = next-value(TransID)
        sbms.params = cParams
        sbms.stat = 0.
end.

run value(pcAction).

finally:
    empty temp-table ttParams.
    if valid-handle(xResp) then
        delete object xResp no-error.
    if valid-handle(xDoc) then
        delete object xDoc no-error.
    if valid-handle(hRoot) then
        delete object hRoot no-error.
    if valid-handle(hWSPort) then
        delete object hWSPort no-error.
    if valid-handle(hWebService) then do:
        if hWebService:connected() then
            hWebService:disconnect() no-error.
        delete object hWebService no-error.
    end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AvailSpots) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvailSpots Procedure 
PROCEDURE AvailSpots :
define variable rASNRec as rowid no-undo.
define variable lAlum as logical no-undo.
define variable dWidth as decimal no-undo.
define variable dOD as decimal no-undo.
define variable dWt as decimal no-undo.

run createDoc.

cNodeRoot = "/DocumentElement/COIL_SPOTS_FREE_BY_ZONE/".
xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/",string(sbms.TransID)).

find first ttParams no-lock where ttParams.cName = "RowID" no-error.
if avail ttParams then do:
    rASNRec = to-rowid(ttParams.cValue).
    
    find first asn no-lock where rowid(asn) = rASNRec.
    
    find first metalXRef no-lock where metalXref.descr = asn.descr no-error.
    if avail metalXref then do:
        find first type no-lock where type.type-id = metalXref.type-id.
        find first metal-cat no-lock where metal-cat.cat-id = type.cat-id.
        lAlum = metal-cat.alum.
    end. /*if avail metalXref*/
    
    xPathSetValue(xDoc,cNodeRoot + "COIL_ID/",string(asn.inv-id)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_CUSTOMER_LIFTID/",asn.own-inv-id).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WIDTH/",string(asn.wdth)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WIDTH_UOM/","IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_INSIDE_DIAMETER/",string(asn.i-d)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_INSIDE_DIAMETER_UOM/","IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_OUTSIDE_DIAMETER/",string(asn.o-d)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_OUTSIDE_DIAMETER_UOM/","IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_WEIGHT/",string(asn.net-wt)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WEIGHT_UOM/","LB").
    xPathSetValue(xDoc,cNodeRoot + "COIL_CUSTOMER/","PSI").
    if lAlum then
        xPathSetValue(xDoc,cNodeRoot + "COIL_MATERIAL_TYPE/", "ALDRYCOIL").
    else 
        xPathSetValue(xDoc,cNodeRoot + "COIL_MATERIAL_TYPE/", "STDRYCOIL").
end. /*if avail ttParams*/
else do:
    find first ttParams no-lock where ttParams.cName = "Width" no-error.
    if not avail ttParams then do:
        assign
            pcMsg = "Incorrect Params"
            sbms.stat = 3.
        return.
    end. /*if not avail ttParams*/
    dWidth = decimal(ttParams.cValue).
    
    find first ttParams no-lock where ttParams.cName = "OD" no-error.
    if not avail ttParams then do:
        assign
            pcMsg = "Incorrect Params"
            sbms.stat = 3.
        return.
    end. /*if not avail ttParams*/
    dOD = decimal(ttParams.cValue).
    
    find first ttParams no-lock where ttParams.cName = "Weight" no-error.
    if not avail ttParams then do:
        assign
            pcMsg = "Incorrect Params"
            sbms.stat = 3.
        return.
    end. /*if not avail ttParams*/
    dWt = decimal(ttParams.cValue).
    
    xPathSetValue(xDoc,cNodeRoot + "COIL_ID/","").
    xPathSetValue(xDoc,cNodeRoot + "COIL_CUSTOMER_LIFTID/","").
    xPathSetValue(xDoc,cNodeRoot + "COIL_WIDTH/",string(dWidth)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WIDTH_UOM/","IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_INSIDE_DIAMETER/","0").
    xPathSetValue(xDoc,cNodeRoot + "COIL_INSIDE_DIAMETER_UOM/","IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_OUTSIDE_DIAMETER/",string(dOD)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_OUTSIDE_DIAMETER_UOM/","IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_WEIGHT/",string(dWt)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WEIGHT_UOM/","LB").
    xPathSetValue(xDoc,cNodeRoot + "COIL_CUSTOMER/","PSI").
    xPathSetValue(xDoc,cNodeRoot + "COIL_MATERIAL_TYPE/", "STDRYCOIL").
    
end.


xDoc:save("LONGCHAR",lcXML).

run WSConnect.

if pcMsg > "" then do:
    pcMsg = ParseMsg(pcMsg).
    return.
end.

RUN CoilSpotsFree IN hWSPort(INPUT lcXML,
                             OUTPUT lcResponse).

xResp:load('longchar', lcResponse, false).
if xPathGetValue(xResp,"/DocumentElement/RESPONSE/RESULT/") = "FAIL" then do:
    assign
        pcMsg = xPathGetValue(xResp,"/DocumentElement/RESPONSE/INFORMATION/")
        sbms.stat = 4
        sbms.comments = lcXML + chr(10) + pcMsg.
    return.
end.

pcMsg = xPathGetValue(xResp,"/DocumentElement/RESPONSE/BAY_3_SPOTS_FREE/").
pcMsg = pcMsg + "|" + xPathGetValue(xResp,"/DocumentElement/RESPONSE/BAY_4_SPOTS_FREE/").

assign 
    sbms.comments = pcMsg
    sbms.stat = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BunkOffset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BunkOffset Procedure 
PROCEDURE BunkOffset :
define variable cBay as character no-undo.
define variable cRow as character no-undo.
define variable cPos as character no-undo.
define variable cOffset as character no-undo.

find first ttParams no-lock where ttParams.cName = "Bay" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Bay Number|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cBay = ttParams.cValue.

find first ttParams no-lock where ttParams.cName = "Row" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Row Number|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cRow = ttParams.cValue.

find first ttParams no-lock where ttParams.cName = "Position" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Position Number|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cPos = ttParams.cValue.

find first ttParams no-lock where ttParams.cName = "Offset" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Offset|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cOffset = ttParams.cValue.

run createDoc.

cNodeRoot = "/DocumentElement/BUNK_OFFSET/".

xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/",string(sbms.TransID)).
xPathSetValue(xDoc,cNodeRoot + "BAY/",cBay).
xPathSetValue(xDoc,cNodeRoot + "ROW/",cRow).
xPathSetValue(xDoc,cNodeRoot + "BUNK_POSITION/",cPos).
xPathSetValue(xDoc,cNodeRoot + "OFFSET/",cOffset).
xPathSetValue(xDoc,cNodeRoot + "OFFSET_UOM/","IN").

xDoc:save("LONGCHAR",lcXML).

run WSConnect.

if pcMsg > "" then do:
    pcMsg = ParseMsg(pcMsg).
    return.
end.

RUN BunkOffset IN hWSPort(INPUT lcXML, 
                        OUTPUT lcResponse).

xResp:load('longchar', lcResponse, false).

pcMsg = ParseMsg(xPathGetValue(xResp,"/DocumentElement/RESPONSE/INFORMATION/")).

if xPathGetValue(xResp,"/DocumentElement/RESPONSE/RESULT/") = "FAIL" then do:
    assign
        sbms.stat = 4
        sbms.comments = lcXML + chr(10) + pcMsg.
    return.
end.
sbms.stat = 1.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckShipment) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckShipment Procedure 
PROCEDURE CheckShipment :
define variable iLoad as integer.

find first ttParams no-lock where ttParams.cName = "Load" no-error.
if not avail ttParams or ttParams.cValue = "" then do:
    pcMsg = "no".
    return.
end. /*if not avail ttParams*/
assign
    iLoad = int(ttParams.cValue).
    
find first s-load no-lock where s-load.load-id = iLoad no-error.
if not avail s-load then do:
    pcMsg = "no".
    return.
end. /*if not avail s-load*/

for each s-rlse no-lock where s-rlse.load-id = iLoad,
    each s-req no-lock where s-req.schd-id = s-rlse.schd-id,
    each s-pick no-lock where s-pick.schd-id = s-req.schd-id 
        and (s-pick.ReqLoadId = 0 or s-pick.ReqLoadID = s-rlse.load-id),
    first inv no-lock of s-pick,
    first loc no-lock where loc.loc = inv.loc 
        and loc.plant-id = inv.plant-id
        and (loc.area = 13 or loc.area = 14):
    if not can-find(first sbms where sbms.inv-id = s-pick.inv-id 
                    and sbms.procedure = "GetShipment") then do:
        pcMsg = "yes".
        return.
    end. /*if can-find(first sbms)*/
end. /*for each s-rlse*/

pcMsg = "no".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createDoc Procedure 
PROCEDURE createDoc :
create x-document xDoc.
create x-noderef hRoot.
create x-document xResp.

xDoc:create-node(hRoot,"DocumentElement","element").
xDoc:append-child(hRoot).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCoil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCoil Procedure 
PROCEDURE GetCoil :
define variable cInvs as character.
define variable iOrder as integer.
define variable cLine as character.
define variable cArea as character.
define variable cEnd as character.

find first ttParams no-lock where ttParams.cName = "Order" no-error.
if not avail ttParams or ttParams.cValue = "" then do:
    assign
        pcMsg = "Order ID is required. No Transaction Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
assign
    iOrder = int(ttParams.cValue)
    sbms.order-id = iOrder.

find first ttParams no-lock where ttParams.cName = "Line" no-error.
if not avail ttParams or ttParams.cValue = "" then do:
    assign
        pcMsg = "Line ID is required. No Transaction Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cLine = ttParams.cValue.

find first choice no-lock where choice.field-no = 716
    and choice.descr = "LINE " + cLine no-error.

if not avail choice then do:
    assign
        pcMsg = "Line not defined in choice 716. Contact IT Dept."
        sbms.stat = 3.
    return.
end.

assign
    cArea = choice.misc1
    cEnd = choice.misc2.

for each picklist no-lock where picklist.order-id = iOrder,
    first inv no-lock where inv.inv-id = picklist.master-id,
    first loc no-lock where loc.loc = inv.loc 
    and loc.plant-id = inv.plant-id
    and (loc.area = 13 or loc.area = 14):
    if can-find(first sbms no-lock
                    where sbms.inv-id = picklist.master-id
                    and sbms.procedure = "GetCoil"
                    and sbms.stat < 2) then next.
    create tCoils.
    assign
        tCoils.iInvID = picklist.master-id
        tCoils.iSBMSID = getTripID(input inv.inv-id, input inv.prev-inv-id).
    
    if tCoils.iSBMSID = 0 then do:
        assign
            pcMsg = "Coil " + string(inv.inv-id) + " does not have a store activity. No Trans Sent."
            sbms.stat = 3.
        return.
    end. /*if tCoils.iSBMSID*/
end. /*for each picklist*/

for each tCoils:
    cInvs = cInvs + "," + string(tCoils.iSBMSID).
end.
cInvs = trim(cInvs, ",").

if cInvs = "" then do:
    assign
        pcMsg = "No Coils for this work order need to be requested.".
        sbms.stat = 3.
    return.
end. 

find first choice no-lock where choice.field-no = 719
    and choice.descr = "GetCoil" no-error.
if avail choice then 
    cPriority = choice.misc1.
else
    cPriority = "10".

run createDoc.

cNodeRoot = "/DocumentElement/MOVE_COIL/".

xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/",string(sbms.TransID)).
xPathSetValue(xDoc,cNodeRoot + "JOB_NAME/","Get Coil for Line " + cLine).
xPathSetValue(xDoc,cNodeRoot + "JOB_PRIORITY/",cPriority).
xPathSetValue(xDoc,cNodeRoot + "DESTINATION_BAY/","").
xPathSetValue(xDoc,cNodeRoot + "DESTINATION_AREA/",cArea).
xPathSetValue(xDoc,cNodeRoot + "DESTINATION_END_POINT/",cEnd).
xPathSetValue(xDoc,cNodeRoot + "STORAGE_METHOD/","0").
xPathSetValue(xDoc,cNodeRoot + "COIL_ID/",cInvs).

xDoc:save("LONGCHAR",lcXML).

run WSConnect.

if pcMsg > "" then
    return.

RUN MoveCoil IN hWSPort(INPUT lcXML, 
                                OUTPUT lcResponse).
xResp:load('longchar', lcResponse, false).
if xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/RESULT/") = "FAIL" then do:
    assign
        pcMsg = xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/INFORMATION/")
        sbms.stat = 4
        sbms.comments = pcMsg + chr(10) + chr(10) + lcXML.
    return.
end.
sbms.stat = 1.

cInvs = xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/COIL_ID/").
if cInvs > "" then
    sbms.inv-id = int(cInvs).

sbms.comments = xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/INFORMATION/").

if pcMsg = "" then
    pcMsg = cInvs + " has been requested.".

finally:
    empty temp-table tCoils.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCoilRF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCoilRF Procedure 
PROCEDURE GetCoilRF :
define variable cDest as character.
define variable cEnd as character.
define variable cJob as character.
define variable cPriority as character.
define variable cInv as character.

sbms.srcLoc = 'SBMS'.

find first ttParams no-lock where ttParams.cName = "InvID" no-error.
if not avail ttParams or ttParams.cValue = "" then do:
    assign
        pcMsg = "Inv ID is|required. No|Transaction Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
assign
    cInv = ttParams.cValue
    sbms.inv-id = integer(cInv).

find first ttParams no-lock where ttParams.cName = "JobName" no-error.
if not avail ttParams or ttParams.cValue = "" then do:
    assign
        pcMsg = "Job Name is|required. No|Transaction Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
assign
    cJob = ttParams.cValue.

find first ttParams no-lock where ttParams.cName = "Priority" no-error.
if not avail ttParams or ttParams.cValue = "" then do:
    assign
        pcMsg = "Priority is|required. No|Transaction Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cPriority = ttParams.cValue.

find first ttParams no-lock where ttParams.cName = "Dest" no-error.
if avail ttParams then
    assign
        cDest = ttParams.cValue.
else
    cDest = "1".

find first ttParams no-lock where ttParams.cName = "Final" no-error.
if avail ttParams then
    assign
        cEnd = ttParams.cValue
        sbms.destLoc = cEnd.
else 
    sbms.destLoc = "SBMS".

run createDoc.

cNodeRoot = "/DocumentElement/MOVE_COIL/".

xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/",string(sbms.TransID)).
xPathSetValue(xDoc,cNodeRoot + "JOB_NAME/", cJob).
xPathSetValue(xDoc,cNodeRoot + "JOB_PRIORITY/", cPriority).
xPathSetValue(xDoc,cNodeRoot + "DESTINATION_BAY/","").
xPathSetValue(xDoc,cNodeRoot + "DESTINATION_AREA/",cDest).
xPathSetValue(xDoc,cNodeRoot + "DESTINATION_END_POINT/",cEnd).
xPathSetValue(xDoc,cNodeRoot + "STORAGE_METHOD/","0").
xPathSetValue(xDoc,cNodeRoot + "COIL_ID/",cInv).

xDoc:save("LONGCHAR",lcXML).

run WSConnect.

if pcMsg > "" then
    return.

RUN MoveCoil IN hWSPort(INPUT lcXML, 
                        OUTPUT lcResponse).
xResp:load('longchar', lcResponse, false).
if xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/RESULT/") = "FAIL" then do:
    assign
        pcMsg = ParseMsg(xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/INFORMATION/"))
        sbms.stat = 4
        sbms.comments = pcMsg + chr(10) + chr(10) + lcXML.
    return.
end.
sbms.stat = 1.

sbms.comments = xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/INFORMATION/").

if pcMsg = "" then
    pcMsg = cInv + " has|been requested.".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetShipment) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetShipment Procedure 
PROCEDURE GetShipment :
define variable iLoad as integer.
define variable iBay as integer.
define variable cInvList as character.
define variable cDest as character.

find first ttParams no-lock where ttParams.cName = "Load" no-error.
if not avail ttParams or ttParams.cValue = "" then do:
    assign
        pcMsg = "Load ID is required. No Transaction Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
assign
    iLoad = int(ttParams.cValue).
    
find first s-load no-lock where s-load.load-id = iLoad no-error.
if not avail s-load then do:
    assign
        pcMsg = "Invalid Load ID. No Transaction Sent."
        sbms.stat = 3.
    return.
end. /*if not avail s-load*/

find first choice no-lock where choice.field-no = 719
    and choice.descr = "GetShipment" no-error.
if avail choice then 
    cPriority = choice.misc1.
else
    cPriority = "10".

for each s-rlse no-lock where s-rlse.load-id = iLoad,
    each s-req no-lock where s-req.schd-id = s-rlse.schd-id,
    each s-pick no-lock where s-pick.schd-id = s-req.schd-id 
        and (s-pick.ReqLoadId = 0 or s-pick.ReqLoadID = s-rlse.load-id),
    first inv no-lock of s-pick,
    first loc no-lock where loc.loc = inv.loc 
        and loc.plant-id = inv.plant-id
        and (loc.area = 13 or loc.area = 14):

    if loc.loc begins "X3" or loc.loc begins "3" then
        cDest = "3".
    else 
        cDest = "6".

    create sbms.
    assign
        sbms.created = now
        sbms.logon = pcLogon
        sbms.plant-id = 9
        sbms.procedure = pcAction
        sbms.transID = next-value(TransID)
        sbms.params = cParams
        sbms.stat = 0
        sbms.inv-id = s-pick.inv-id.
    
    run createDoc.

    cNodeRoot = "/DocumentElement/MOVE_COIL/".
    
    xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/",string(sbms.TransID)).
    xPathSetValue(xDoc,cNodeRoot + "JOB_NAME/","Retrieve coil " + string(s-pick.inv-id) + " for load " + string(iLoad)).
    xPathSetValue(xDoc,cNodeRoot + "JOB_PRIORITY/",cPriority).
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_BAY/","").
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_AREA/", cDest).
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_END_POINT/","").
    xPathSetValue(xDoc,cNodeRoot + "STORAGE_METHOD/", "0").
    xPathSetValue(xDoc,cNodeRoot + "COIL_ID/", string(s-pick.inv-id)).
    
    xDoc:save("LONGCHAR",lcXML).
    
    run WSConnect.
    
    if pcMsg > "" then
        return.

    RUN MoveCoil IN hWSPort(INPUT lcXML, 
                            OUTPUT lcResponse).
    xResp:load('longchar', lcResponse, false).
    if xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/RESULT/") = "FAIL" then do:
        assign
            pcMsg = pcMsg + string(s-pick.inv-id) + " - " +
                xPathGetValue(xResp,"/DocumentElement/MOVE_RESPONSE/INFORMATION/") + chr(10)
            sbms.stat = 4
            sbms.comments = pcMsg + chr(10) + chr(10) + lcXML.
    end.
    else 
        assign
            sbms.stat = 1
            cInvList = cInvList + chr(10) + string(s-pick.inv-id).
    
    if valid-handle(xDoc)
        then delete object xDoc no-error.
    if valid-handle(hRoot)
        then delete object hRoot no-error.
    if valid-handle(xResp)
        then delete object xResp no-error.
end. /*for each s-rlse*/

if pcMsg = "" and cInvList = "" then
    pcMsg = "No Coils in Automation for this load.".
if pcMsg = "" then
    pcMsg = "Coils requested from storage:" + cInvList.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetZoneName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetZoneName Procedure 
PROCEDURE GetZoneName :
define variable dWidth as decimal no-undo.
define variable dOD as decimal no-undo.

find first ttParams no-lock where cName = "Width" no-error.
if not avail ttParams then do:
    pcMsg = "N/A".
    return.
end.
dWidth = decimal(ttParams.cValue).

find first ttParams no-lock where cName = "OD" no-error.
if not avail ttParams then do:
    pcMsg = "N/A".
    return.
end.
dOD = decimal(ttParams.cValue).

find first choice no-lock where choice.field-no = 725
    and choice.misc3 = "Width"
    and dWidth >= decimal(choice.misc1)
    and dWidth <= decimal(choice.misc2) no-error.

if not avail choice then do:
    pcMsg = "N/A".
    return.
end.

pcMsg = choice.descr.

find first choice no-lock where choice.field-no = 725
    and choice.misc3 = "OD"
    and dOD >= decimal(choice.misc1)
    and dOD <= decimal(choice.misc2) no-error.

if not avail choice then do:
    pcMsg = "N/A".
    return.
end.

pcMsg = pcMsg + choice.descr.







END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rebunk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rebunk Procedure 
PROCEDURE Rebunk :
define variable cBay as character no-undo.
define variable cRow as character no-undo.
define variable cPos as character no-undo.

find first ttParams no-lock where ttParams.cName = "Bay" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Bay Number|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cBay = ttParams.cValue.

find first ttParams no-lock where ttParams.cName = "Row" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Row Number|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cRow = ttParams.cValue.

find first ttParams no-lock where ttParams.cName = "Position" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Position Number|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
cPos = ttParams.cValue.

run createDoc.

cNodeRoot = "/DocumentElement/BUNK/".

xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/",string(sbms.TransID)).
xPathSetValue(xDoc,cNodeRoot + "BAY/",cBay).
xPathSetValue(xDoc,cNodeRoot + "ROW/",cRow).
xPathSetValue(xDoc,cNodeRoot + "BUNK_POSITION/",cPos).
xPathSetValue(xDoc,cNodeRoot + "CRADLE_ID/", "").

xDoc:save("LONGCHAR",lcXML).

run WSConnect.

if pcMsg > "" then do:
    pcMsg = ParseMsg(pcMsg).
    return.
end.

RUN BunkCoil IN hWSPort(INPUT lcXML, 
                        OUTPUT lcResponse).

xResp:load('longchar', lcResponse, false).

pcMsg = ParseMsg(xPathGetValue(xResp,"/DocumentElement/BUNK_RESPONSE/INFORMATION/")).

if xPathGetValue(xResp,"/DocumentElement/BUNK_RESPONSE/RESULT/") = "FAIL" then do:
    assign
        sbms.stat = 4
        sbms.comments = lcXML + chr(10) + pcMsg.
    return.
end.
else do:
    pcMsg = xPathGetValue(xResp,"/DocumentElement/BUNK_RESPONSE/IS_OFFSET_REQUIRED/").
    if pcMsg = "1" then do:
        assign
            pcMsg = xPathGetValue(xResp,"/DocumentElement/BUNK_RESPONSE/OFFSET_MARGIN/")
            pcMsg = "OFFSET," + pcMsg.
    end.
end.
sbms.stat = 1.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RequestTCar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RequestTCar Procedure 
PROCEDURE RequestTCar :
define variable cLoc as character no-undo.
define variable cCar as character no-undo.
define variable cDesc as character no-undo.
define variable cBay as character no-undo.

find first ttParams no-lock where ttParams.cName = "Loc" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Location|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
assign
    cLoc = ttParams.cValue
    sbms.destLoc = cLoc.
    
find first ttParams no-lock where ttParams.cName = "TCar" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "TCar|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
assign
    cCar = ttParams.cValue.

find first ttParams no-lock where ttParams.cName = "Bay" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "Bay|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
assign
    cBay = ttParams.cValue + substring(cLoc, 2).

find first ttParams no-lock where ttParams.cName = "Description" no-error.
if avail ttParams then
    assign cDesc = ttParams.cValue.
    
find first choice no-lock where choice.field-no = 719
    and choice.descr = "RequestTCAR" no-error.
if avail choice then 
    cPriority = choice.misc1.
else
    cPriority = "10".

run createDoc.

cNodeRoot = "/DocumentElement/STOP_CAR/".

xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/",string(sbms.TransID)).
xPathSetValue(xDoc,cNodeRoot + "JOB_NAME",cDesc).
xPathSetValue(xDoc,cNodeRoot + "JOB_PRIORITY", cPriority).
xPathSetValue(xDoc,cNodeRoot + "DESTINATION_AREA",cCar).
xPathSetValue(xDoc,cNodeRoot + "START_END_POINT",cLoc).
xPathSetValue(xDoc,cNodeRoot + "DESTINATION_END_POINT",cBay).

xDoc:save("LONGCHAR",lcXML).

run WSConnect.

if pcMsg > "" then do:
    pcMsg = ParseMsg(pcMsg).
    return.
end.

RUN StopCar IN hWSPort(INPUT lcXML, 
                       OUTPUT lcResponse).

xResp:load('longchar', lcResponse, false).
if xPathGetValue(xResp,"/DocumentElement/RESPONSE/RESULT/") = "FAIL" then do:
    assign
        pcMsg = ParseMsg(xPathGetValue(xResp,"/DocumentElement/RESPONSE/INFORMATION/"))
        sbms.stat = 4
        sbms.comments = lcXML + chr(10) + pcMsg.
    return.
end.
sbms.stat = 1.

if pcMsg = "" then
    pcMsg = "Car Now Requested.".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StoreCoil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StoreCoil Procedure 
PROCEDURE StoreCoil :
define variable iInv as integer no-undo.
define variable cLoc as character init ? no-undo.
define variable cDest as character init ? no-undo.
define variable cFinal as character init ? no-undo.
define variable lAlum as logical init ? no-undo.
define variable iNextID as integer no-undo.
define variable cCust as character no-undo.
define variable cWt as character no-undo.
define variable dWdth like part.wdth no-undo.

find first ttParams no-lock where ttParams.cName = "InvID" no-error.
if not avail ttParams then do:
    assign
        pcMsg = "PSI ID|is required.|No Trans Sent."
        sbms.stat = 3.
    return.
end. /*if not avail ttParams*/
assign
    iInv = int(ttParams.cValue)
    sbms.inv-id = iInv.

find first ttParams no-lock where ttParams.cName = "Loc" no-error.
if avail ttParams then
    assign
        cLoc = ttParams.cValue
        sbms.srcLoc = cLoc.
else
    sbms.srcLoc = "SBMS".
    
find first ttParams no-lock where ttParams.cName = "Dest" no-error.
if avail ttParams then
    assign
        cDest = ttParams.cValue.
else
    cDest = "1".

find first ttParams no-lock where ttParams.cName = "Final" no-error.
if avail ttParams then
    assign
        cFinal = ttParams.cValue
        sbms.destLoc = cFinal.
else 
    sbms.destLoc = "SBMS".
    
find first ttParams no-lock where ttParams.cName = "Alum" no-error.
if avail ttParams then
    assign
        lAlum = logical(ttParams.cValue).

find first ttParams no-lock where ttParams.cName = "Priority" no-error.
if avail ttParams then
    cPriority = ttParams.cValue.
else do:
    find first choice no-lock where choice.field-no = 719
        and choice.descr = "StoreCoil" no-error.
    if avail choice then 
        cPriority = choice.misc1.
    else 
        cPriority = "10".
end.

find first ttParams no-lock where ttParams.cName = "JobName" no-error.
if avail ttParams then
    cJobName = ttParams.cValue.
else if cFinal > "" then 
    cJobName = "Send Coil " + string(iInv) + " to " + cFinal.
else 
    cJobName = "Store Coil " + string(iInv) + " From " + cLoc.

find first inv where inv.inv-id = iInv no-error.
if not avail inv then 
    find first asn where asn.inv-id = iInv no-error.
if not avail inv and not avail asn then do:
    assign
        pcMsg = "Invalid Inv ID|" + string(iInv)
        sbms.stat = 3.
    return.
end.

if avail inv then do:
    if inv.i-d = 0 or inv.i-d = ? then do:
        assign
            pcMsg = "Coil Requires I-D|To Be Set.|No Trans Sent."
            sbms.stat = 3.
        return.
    end. /*if inv.i-d*/
   
    if inv.o-d = 0 or inv.o-d = ? then do:
        find first part no-lock where part.part-id = inv.part-id.
        inv.o-d = sqrt((15.6 * part.gauge * (inv.linear-ft / inv.pieces)) + (inv.i-d * inv.i-d)).
    end. /*if inv.o-d*/
    find first part no-lock where part.part-id = inv.part-id.
    find first rcpt no-lock where rcpt.rcpt-id = inv.rcpt-id.
    find first partner no-lock where partner.partner-id = inv.partner-id no-error.

    if inv.rcpt-id = inv.inv-id and rcpt.act-width > 0 then
        dWdth = rcpt.act-width.
    else 
        dWdth = part.wdth.

    if avail partner then
        cCust = string(partner.owner-id).
    else 
        cCust = "PSI".
    
    run createDoc.

    cNodeRoot = "/DocumentElement/STORE_COIL/".
    
    xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/", string(sbms.TransID)).
    xPathSetValue(xDoc,cNodeRoot + "JOB_NAME/", cJobName).
    xPathSetValue(xDoc,cNodeRoot + "JOB_PRIORITY/", cPriority).
    xPathSetValue(xDoc,cNodeRoot + "SOURCE_CRADLE_ID/", cLoc).
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_BAY/", "").
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_AREA/", cDest).
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_END_POINT/", cFinal).
    xPathSetValue(xDoc,cNodeRoot + "STORAGE_METHOD/", "0").
    xPathSetValue(xDoc,cNodeRoot + "COIL_ID/", string(iInv)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_CUSTOMER_LIFTID/", rcpt.own-inv-id).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WIDTH/", string(dWdth)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WIDTH_UOM/", "IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_INSIDE_DIAMETER/", string(inv.i-d)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_INSIDE_DIAMETER_UOM/", "IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_OUTSIDE_DIAMETER/", string(inv.o-d)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_OUTSIDE_DIAMETER_UOM/", "IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_WEIGHT/", string(inv.net-wt)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WEIGHT_UOM/", "LB").
    xPathSetValue(xDoc,cNodeRoot + "COIL_CUSTOMER/", cCust).
    if lAlum then
        xPathSetValue(xDoc,cNodeRoot + "COIL_MATERIAL_TYPE/", "ALDRYCOIL").
    else 
        xPathSetValue(xDoc,cNodeRoot + "COIL_MATERIAL_TYPE/", "STDRYCOIL").
    xDoc:save("LONGCHAR",lcXML).
end. /*if avail inv*/

if avail asn then do:
    if asn.act-width = 0 or asn.act-width = ? then do:
        assign
            pcMsg = "ASN needs width|measurement.|Trans not Sent."
            sbms.stat = 3.
        return.
    end. /*if asn.act-wdth*/
    if asn.o-d = 0 or asn.o-d = ? then do:
        assign
            pcMsg = "ASN needs O-D|measurement.|Trans not Sent."
            sbms.stat = 3.
        return.
    end. /*if asn.act-wdth*/
    if asn.i-d = 0 or asn.i-d = ? then do:
        assign
            pcMsg = "ASN needs I-D|measurement.|Trans not Sent."
            sbms.stat = 3.
        return.
    end. /*if asn.act-wdth*/

    if asn.owner-id <> 0 and asn.owner-id <> ? then
        cCust = string(asn.owner-id).
    else 
        cCust = "PSI".
    
    if asn.net-wt > 0 then
        cWt = string(asn.net-wt).
    else
        cWt = string(asn.scale-wt).

    run createDoc.

    cNodeRoot = "/DocumentElement/STORE_COIL/".
    
    xPathSetValue(xDoc,cNodeRoot + "TRANSMISSION_ID/", string(sbms.TransID)).
    xPathSetValue(xDoc,cNodeRoot + "JOB_NAME/", cJobName).
    xPathSetValue(xDoc,cNodeRoot + "JOB_PRIORITY/", cPriority).
    xPathSetValue(xDoc,cNodeRoot + "SOURCE_CRADLE_ID/", cLoc).
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_BAY/", "").
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_AREA/", cDest).
    xPathSetValue(xDoc,cNodeRoot + "DESTINATION_END_POINT/", cFinal).
    xPathSetValue(xDoc,cNodeRoot + "STORAGE_METHOD/", "0").
    xPathSetValue(xDoc,cNodeRoot + "COIL_ID/", string(iInv)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_CUSTOMER_LIFTID/", asn.own-inv-id).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WIDTH/", string(asn.act-width)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WIDTH_UOM/", "IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_INSIDE_DIAMETER/", string(asn.i-d)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_INSIDE_DIAMETER_UOM/", "IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_OUTSIDE_DIAMETER/", string(asn.o-d)).
    xPathSetValue(xDoc,cNodeRoot + "COIL_OUTSIDE_DIAMETER_UOM/", "IN").
    xPathSetValue(xDoc,cNodeRoot + "COIL_WEIGHT/", cWt).
    xPathSetValue(xDoc,cNodeRoot + "COIL_WEIGHT_UOM/", "LB").
    xPathSetValue(xDoc,cNodeRoot + "COIL_CUSTOMER/", cCust).
    if lAlum then
        xPathSetValue(xDoc,cNodeRoot + "COIL_MATERIAL_TYPE/", "ALDRYCOIL").
    else 
        xPathSetValue(xDoc,cNodeRoot + "COIL_MATERIAL_TYPE/", "STDRYCOIL").
    xDoc:save("LONGCHAR",lcXML).
end. /*if avail asn*/

run WSConnect.

if pcMsg > "" then do:
    pcMsg = ParseMsg(pcMsg).
    return.
end.

RUN StoreCoil IN hWSPort(INPUT lcXML, 
                        OUTPUT lcResponse).
xResp:load('longchar', lcResponse, false).

if xPathGetValue(xResp,"/DocumentElement/RESPONSE/RESULT/") = "FAIL" then do:
    assign
        pcMsg = ParseMsg(xPathGetValue(xResp,"/DocumentElement/RESPONSE/INFORMATION/"))
        sbms.stat = 4
        sbms.comments = pcMsg + chr(10) + chr(10) + lcXML.
    return.
end.
sbms.stat = 1.


if pcMsg = "" then
    pcMsg = "COIL HAS|BEEN QUEUED.".

if cDest = "1" then do:
    iNextID = dynamic-function("getNextIDInt",0,0,45).
    create actv.
    assign  
        actv.logon       = pcLogon
        actv.actv-code   = 42
        actv.seq-no      = 1
        actv.tran-date   = today
        actv.tran-no     = iNextid
        actv.tran-time   = integer(replace(string(time,"HH:MM"),":",""))
        actv.plant-id    = g-plant-id.
    if avail inv then
        assign
            actv.owner-id    = partner.owner-id
            actv.user-id     = partner.user-id
            actv.inv-id      = inv.inv-id
            actv.loc         = inv.loc
            actv.plant-id    = inv.plant-id
            actv.rcpt-id     = inv.rcpt-id
            actv.part-id     = inv.part-id
            actv.partner-id  = inv.partner-id.
    else do:
        find first cntl no-lock where cntl.plant-id = g-plant-id.
        find first partner no-lock where partner.owner-id = cntl.cust-id
            and partner.user-id = cntl.cust-id.
        assign
            actv.owner-id    = (if asn.owner-id = ? then cntl.cust-id else asn.owner-id)
            actv.user-id     = (if asn.user-id = ? then cntl.cust-id else asn.owner-id)
            actv.inv-id      = asn.inv-id
            actv.loc         = asn.loc
            actv.plant-id    = asn.plant-id
            actv.rcpt-id     = asn.inv-id
            actv.part-id     = (if asn.part-id = ? then 0 else asn.part-id)
            actv.partner-id  = (if asn.partner-id = ? then partner.partner-id else asn.partner-id).
    end. /*else do*/
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WSConnect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WSConnect Procedure 
PROCEDURE WSConnect :
find first AppAttr no-lock where AppAttr.AttrKey = "CareGoWebService" no-error.
if not avail AppAttr then do:
    pcMsg = "Web Service URL not found.".
    return.
end. /*if not avail AppAttr*/

create server hWebService.
hWebService:connect(AppAttr.AppStr).
if not hWebService:connected() then do:
    pcMsg = "Web Server is not connected".
    return.
end. /*if not hWebService:connected()*/

run IImsPSIService set hWSPort on hWebService.
if not valid-handle(hWSPort) then
    pcMsg = "Port for Web Service is not valid".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getTripID) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTripID Procedure 
FUNCTION getTripID RETURNS INTEGER
  ( input piInv as integer,
    input piPrev as integer ) :
    define buffer bInv for inv.
    
    find first actv no-lock where actv.inv-id = piInv 
        and actv.actv-code = 42 no-error.
    if avail actv then 
        return piInv.
    if piPrev = 0 then
        return 0.
    find first bInv no-lock where bInv.inv-id = piPrev.
    if bInv.stat <> 7 then 
        return 0.
    return getTripID(input piPrev, input bInv.prev-inv-id).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ParseMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ParseMsg Procedure 
FUNCTION ParseMsg RETURNS CHARACTER
  (input pcString as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var iLimit as int.
def var iCount as int.
def var c1 as char no-undo.
def var c2 as char no-undo.

iLimit = 19.
pcString = trim(pcString).
if length(pcString) <= iLimit then
    return pcString.

do iCount = 0 to iLimit - 1:
    if substring(pcString, iLimit - iCount, 1) = " " then do:
        c1 = substring(pcString, 1, iLimit - iCount - 1).
        pcString = substring(pcString, iLimit - iCount + 1).
        leave.
    end.
end.

if c1 = "" then do:
    do iCount = 0 to length(pcString) - iLimit - 1:
        if substring(pcString, iLimit + iCount, 1) = " " then do:
            c1 = substring(pcString, 1, iLimit + iCount - 1).
            pcString = substring(pcString, iLimit + iCount + 1).
            leave.
        end.
    end.
end.

if length(pcString) <= iLimit then
    return c1 + "|" + pcString.

do iCount = 0 to iLimit - 1:
    if substring(pcString, iLimit - iCount, 1) = " " then do:
        c2 = substring(pcString, 1, iLimit - iCount - 1).
        pcString = substring(pcString, iLimit - iCount + 1).
        leave.
    end.
end.
return c1 + "|" + c2 + "|" + pcString.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

