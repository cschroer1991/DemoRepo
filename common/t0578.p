&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0578.p
    Purpose     : misc server side utilities

    Syntax      :
   Date     By    TIWO  Description
 ========  ===  ====== =================================================
 06/18/09  djt   40691  new
 07/31/12  djt   59279  add procedures 'GetPartnerSlip2','DefaultSlipConfig'
 07/09/13  djt   63943  add procedure LineList
 05/07/14  djt   65753  offer to take rewrap mults off hold. (if it is 
                        the primary hold applied to coil)
 09/05/14  djt   66933  change validation process of outbound destination
 09/05/14  djt   66815  Inspection app mods for scrapping mults
 11/14/14  djt   67488  Do not allow check-in of deleted appointment
 11/26/14  cjs   66625  Delete holds and inspections instead of release
 12/11/14  djt   67650  do not allow check in of auto completed appts.
 03/19/15  djt   68127  Schema change to add field ShowToCust
 07/14/15  cjs   68471  Delete UFD procedure.
 09/15/15  cjs   68837  Check Mech/Chem audits before moving to real line. 
 03/30/16  cjs   70985  Find all loads when check-in no matter the scac code. 
 04/15/16  bmf   71043  Vonore outbound checkin changes
 06/29/16  jhl   72149  Okay for Middletown loads to be Built status  (4) and arrive.
 07/07/16  djt   72219  Do not display destination when owner 55 / Plant 4
 10/03/16  djt   72548  pre-emptive mult hold project.
 10/21/16  cjs   72986  Can not delete ufd if not in pending status.
 12/16/16  cjs   72794  When scheduling coil to line check restraints for that line.
 01/05/17  djt   71509  Favorites menu project
 01/13/17  djt   73869  Shipping EDI Project
 09/13/17 zakh   74788  Added procedure CheckTripReq
 09/21/17  djt   76142  Add procedure CreateException
 10.23/17  djt   76387  Secondary reapply enhancements
 11/15/17  mec   76599  Allow use of BOL number for EDI created TruckArrivals/ Remove mill coil check
 12/12/17  djt   76799  Appserver error addressed
 01/31/18 mattco 77204  For EDI inbound appts allow rescheduled appts to check in at kiosk
 04/13/18  cjs   77432  Add Dynamic Message When Checking Load In 
 06/26/18  djt   77711  isMaintUser procedure added
 10/16/18  tam   77766  Add checkShowCust procedure
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
define input  parameter pcAction     as character  no-undo.
define input  parameter pcOptionList as character  no-undo.
define output parameter pcRtn as char no-undo.
define output parameter pcErr as char no-undo.

define variable iLoadID as int no-undo.
define variable cLoadID as char no-undo.
define variable cVehicleID as char no-undo.
define variable iVehicleID as integer no-undo.
define variable iVehicleType as integer no-undo.
define variable iPartnerID as integer no-undo.
define variable iPartID as integer no-undo.
define variable iMetric as integer no-undo.
define variable iPlantID as integer no-undo.
define variable iLineID as integer no-undo.
define variable iFootage as integer no-undo.
define variable iScrapOption as integer no-undo.
define variable iDamageCode as integer no-undo.
define variable iDamageLoc as integer no-undo.
define variable cMultList as character no-undo.
define variable lRemoveHold as logical no-undo.
define variable iIdx as integer no-undo.
define variable iCoilID as integer no-undo.
define variable iArrivalID as integer no-undo.
define variable iEarlyMinutes as integer no-undo.
define variable iLateMinutes as integer no-undo.
define variable cFindLoadNo as character no-undo.
define variable v-dock as integer no-undo.
define variable lMultiStripASN as logical no-undo.
 define variable v-bol as character no-undo.
 define variable v-truck-no as character no-undo.
 define variable v-num-coils as integer no-undo.
 define variable v-appt-no as character no-undo.
 define variable cCoilNo as character no-undo.
 define variable lPrintRpt as logical no-undo.
 define variable lAutodock as logical no-undo.
 define variable v-scac as character no-undo.
 define variable v-NoCoils as integer no-undo.
 define variable iReason as integer no-undo.
 define variable lASNMill as logical no-undo.
 define variable cNewBOL as character no-undo.
 define variable cUser as character no-undo.
 define variable cRtn as character no-undo.
 define variable iBreak as integer no-undo.
 define variable cBreakList as character no-undo.
 define variable lPrintAtDock as logical no-undo.
 define variable cComment as character no-undo.
 define variable iJobID as integer no-undo.
 define variable cUFDKey as decimal no-undo.
 define variable cCoil as character no-undo.
 define variable cOrderList as character no-undo.
 define variable cFaveList as character no-undo.
 define variable cErr as char no-undo.
 define variable iSOID as int no-undo.
 define variable iType as int no-undo.
 define variable cMessageList as character no-undo.
 DEFINE VARIABLE cUserID AS CHARACTER   NO-UNDO.
def buffer bPart for part.
def buffer bta for TruckArrivals.
def buffer bASN for ASN.
DEFINE VARIABLE cInvList AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetNextDockSeq) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetNextDockSeq Procedure 
FUNCTION GetNextDockSeq RETURNS INTEGER
  ( piDockNo as int, piArrivalID as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StripLoadNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StripLoadNum Procedure 
FUNCTION StripLoadNum RETURNS CHARACTER
    ( pcLoad as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StripLoadNum2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StripLoadNum2 Procedure 
FUNCTION StripLoadNum2 RETURNS CHARACTER
  ( pcLoad as char ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TestForWait) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TestForWait Procedure 
FUNCTION TestForWait RETURNS logical
  ( piSeq as int, piDoor as int, piArrivalID as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TruckCount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TruckCount Procedure 
FUNCTION TruckCount RETURNS INTEGER
  ( piDock as integer, piArrivalID as integer )  FORWARD.

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
         HEIGHT             = 28.24
         WIDTH              = 48.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
if pcAction ne "DeleteMultHolds" then
run parseOpts.

if pcAction = "":u 
or pcAction = ? 
or not can-do(this-procedure:internal-entries,pcAction) then 
  pcErr = "**Procedure " + pcAction + ' Not available in ' + program-name(1).
else 
  run value(pcAction).

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AddMetal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddMetal Procedure 
PROCEDURE AddMetal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iOwnerID AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMinWt AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxWt AS INTEGER     NO-UNDO.
pcErr = ''.
if iCoilID ne ? and iCoilID > 0 then do:
    find first inv no-lock where inv.inv-id = iCoilID no-error.
    if avail inv then
        select type-id into iType from part where part.part-id = inv.part-id.
    if not avail inv then pcErr = 'Failed to add!~nCould not find type from the inv-id entered.'.
    if iType ne ? and iType > 0 then pcRtn = string(iType).
end.
if pcErr > '' then return.

find first sotype no-lock
    where sotype.soid = iSOID
    and sotype.type-id = iType no-error.
if avail sotype then do:
    pcErr = 'Metal Type Already in the List for This Order'.
    return.
end.
find first SecondaryOrder no-lock
    where SecondaryOrder.SOID = iSOID no-error.
if not avail SecondaryOrder then do:
    pcErr = 'Select or Define a secondary order before Assigning Metal Types'.
    return.
end.
assign
   iPlantID = SecondaryOrder.plant-id
   iOwnerID = SecondaryOrder.owner-id
   iMinWt = SecondaryOrder.min-wt
   iMaxWt = SecondaryOrder.max-wt.


for each SecondaryOrder no-lock
    where SecondaryOrder.plant-id = iPlantID
    and SecondaryOrder.owner-id = iOwnerID
    and SecondaryOrder.enddate ge today:

    if SecondaryOrder.SOID = iSOID then next.
    if SecondaryOrder.min-wt ne iMinWt
        and SecondaryOrder.max-wt ne iMaxWt
        then next.
    find first sotype no-lock
        where sotype.SOID = SecondaryOrder.SOID
        and sotype.type-id = iType
        no-error.
    if avail sotype then pcErr = 'Active Secondary Order Exists for the same owner and metal type.'.
end.
if pcErr = '' then do:
    create sotype.
    assign
        sotype.SOID = iSOID
        sotype.type-id = iType.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ApptCheckin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ApptCheckin Procedure 
PROCEDURE ApptCheckin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iGap as integer no-undo init ?.
define variable lOutboundCreate as logical no-undo init no.
define variable lOutboundApptLive as logical no-undo init no.

find first truckarrivals no-lock
    where truckarrivals.arrivalID = iArrivalID 
      or (truckarrivals.plant-id = iPlantID
      and truckarrivals.loadid = string(iArrivalID) 
      and truckarrivals.completedate eq ?
      and truckarrivals.CreateBy begins "EDI") no-error.

if avail truckarrivals then
  assign
    iArrivalID = truckarrivals.arrivalID
    iGap = interval(now, truckarrivals.ApptDate, 'minutes')
    pcErr = ''.

assign lOutboundCreate = dynamic-function('getAppAttribute',
              string(substitute('Plant&1CreateOutboundAppt',iPlantID)),'log')
       lOutboundApptLive =  dynamic-function('getAppAttribute',
              string(substitute('Plant&1OutboundApptLive',iPlantID)),'log').
case true:
    when not avail truckArrivals then do:
        /* If this is a PRSP outbound load, create appointment if it does not exist */
        /* 67576 - apply PRSP logic to customer pickups */
        find first s-load no-lock where s-load.load-id = iArrivalID no-error.
        if avail s-load and not lOutboundApptLive then do:
            pcErr = substitute('&1|&2|&3|&4|',
                           'This Kiosk Is For Inbound',
                           'Appointments Only.',
                           'Please Go To Shipping Office.',
                           'Press RESET to start over.').
            return.
        end.
        if avail s-load
            and (s-load.scac-code = 'PRSP'
             or  s-load.scac-code = 'CCCC'
             or  s-load.scac-code = 'CPUP'
             or  s-load.scac-code = 'CUST'
             or  lOutboundCreate)
            and can-find(first s-scslip where s-scslip.load-id = s-load.load-id) then do:
            /* If there is an appointment, use it else create one for right now */
            if s-load.arrivalid ne ? and s-load.arrivalid gt 0 then do:
                find first truckarrivals no-lock
                    where truckarrivals.arrivalid = s-load.arrivalid no-error.
                iArrivalID = s-load.arrivalid.
            end.
            if s-load.arrivalid eq ? or s-load.arrivalid = 0 then do:
                run ship/t0279.p (input(substitute('SaveLoadAppt,&1,&2,&3,&4'
                                                   ,iPlantID
                                                   ,s-load.load-id
                                                   ,string(today)
                                                   ,replace(string(time, "HH:MM"), ':',''))),
                                  output cRtn).
               find current s-load no-lock.
               assign iArrivalID = s-load.arrivalid.
               find first truckarrivals no-lock
                   where truckarrivals.arrivalid = iArrivalid no-error.
            end.
        end.
        /* need to figure out best place for this */
        if not avail truckarrivals then 
            pcErr = substitute('&1|&2|&3|&4|',
                           'Appointment Entered is not valid.',
                           'Call Your Dispatch to get Appointment',
                           'or re-try entering it.',
                           'Press RESET to start over.').
    end.
    when avail truckArrival and Truckarrival.reason = 29 then do:
         pcErr = substitute('&1|&2|&3|&4|',
                        'Appointment Entered is not valid.',
                        'Call Your Dispatch to get Appointment',
                        'or re-try entering it.',
                        'Press RESET to start over.').
    end.
    when avail truckarrivals and truckarrivals.plant-id ne iPlantID and pcErr = '' then do:
        find first cntl no-lock where cntl.plant-id = truckarrivals.plant-id no-error.
        assign
        pcErr = substitute('&1 &5|&2|&3|&4|',
                           'Appt Entered is for',
                           'Please call Your Dispatcher for Appt. No.',
                           '',
                           'Press RESET to start over.',
                           (if avail cntl then 'PSI' + ' ' + cntl.city else 'a different PSI plant')).
    end.

    when avail truckarrivals and truckarrivals.completedate <> ? then do:
        assign
        pcErr = substitute('&1|&2|&3|&4|',
                           'Appointment Entered',
                           'has already been completed',
                           'Please call Your Dispatcher for Appt. No.',
                           'Press RESET to start over.').

    end.

    when avail truckarrivals and truckarrivals.checkinDate <> ?
        and truckarrivals.reason ne 20
        and truckarrivals.reason ne 21 and pcErr = '' then do:
        assign
        pcErr = substitute('&1|&2|&3|&4|',
                           'Appointment Entered',
                           'has already been checked in',
                           'Please call Your Dispatcher for Appt. No.',
                           'Press RESET to start over.').

    end.

    /* allow late appointments for outbound loads */
   when iGap ne ? and (iGap lt iEarlyMinutes 
                       or (truckarrivals.loadType ne 2 and iGap gt iLateMinutes)) and pcErr = '' then do:
        assign
        pcErr = substitute('You are &1 Min. &2 for your appointment|Which was scheduled at &3|&4|&5|',
                           (absolute(iGap) - absolute(if iGap > 0 then iLateMinutes else iEarlyMinutes)),
                           if iGap > 0 then 'Late' else 'Early',
                           string (TruckArrivals.ApptDate, '99/99/99 HH:MM'),
                           if iGap > 0 then 'See PSI Dispatcher' else 'Try again later.',
                           'Press RESET to start over.').
        find bta exclusive-lock
                 where bta.arrivalid = truckarrivals.arrivalid.
         assign bta.reason = 20
                bta.dock   = 0
                bta.checkinDate = now.
         find current bta no-lock no-error. 
   end.
end case.

if pcErr ne '' then return.
/* Data Looks OK so far, see if we can link this Arrival to a Load or ASN */
if truckarrivals.Loadtype = 2 then run OutboundCheckIn.
if truckarrivals.loadType ne 2 then run InboundCheckin.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BackoutRewrapMults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BackoutRewrapMults Procedure 
PROCEDURE BackoutRewrapMults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iLoop as integer no-undo.

do iLoop = 1 to num-entries(cMultList):
    find first mult exclusive-lock where rowid(mult) = to-rowid(entry(iLoop,cMultList)) no-error.
    if not avail mult then next.

    find first multwork exclusive-lock
        where multwork.line-id = iLineID
        and multwork.mult-no = 'Split' + mult.mult-no no-error.
    if avail multwork then do:
        /* rejoin mult weight and footage back into multwork table */
        assign
            multwork.calc-wt = multwork.calc-wt + mult.calc-wt
            multwork.linear-ft = multwork.linear-ft + mult.linear-ft
            multwork.mult-no = replace(multwork.mult-no,'Split','')
            .
    end.
    else do:
        /* throw mult back into multwork table */
        create multwork.
        buffer-copy mult to multwork
            assign multwork.line-id = iLineID.
    end.
    if mult.inv-id = ? or mult.inv-id = 0 then delete mult.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckInboundDynamicMsgs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckInboundDynamicMsgs Procedure 
PROCEDURE CheckInboundDynamicMsgs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piPlant as integer no-undo.
define input parameter piOwner as integer no-undo.
define output parameter pcMessageList as character no-undo.

if piOwner > 0 then do:
    for each Messages no-lock
        where Messages.TriggerAction = 8
        and   Messages.owner-id = piOwner
        and   Messages.plant-id = piPlant
        and   (Messages.ExpireDate > now or Messages.ExpireDate = ?):
        if pcMessageList = "" then do:
            assign pcMessageList = messages.msg.
        end.
        else do:
            assign pcMessageList = pcMessageList + chr(13) + chr(13) + messages.msg.
        end.
    end.
end.

for each Messages no-lock
    where Messages.TriggerAction = 8
    and   Messages.plant-id = piPlant
    and   (Messages.line-id = ? or Messages.line-id = 0)
    and   (Messages.load-id = ? or Messages.load-id = 0)
    and   (Messages.order-id = ? or Messages.order-id = 0)
    and   (Messages.owner-id = ? or Messages.owner-id = 0)
    and   (Messages.user-id = ? or Messages.user-id = 0)
    and   (Messages.partner-id = ? or Messages.partner-id = 0)
    and   (Messages.part-id = ? or Messages.part-id = 0)
    and   (Messages.ExpireDate > now or Messages.ExpireDate = ?):
    if pcMessageList = "" then do:
        assign pcMessageList = messages.msg.
    end.
    else do:
        assign pcMessageList = pcMessageList + chr(13) + chr(13) + messages.msg.
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckMoveChemMech) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckMoveChemMech Procedure 
PROCEDURE CheckMoveChemMech :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iLoop as integer no-undo.
DEFINE VARIABLE iOrderID AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxLineWt AS INTEGER    init 0   NO-UNDO.
DEFINE VARIABLE iMaxLineWdth AS INTEGER  init 0  NO-UNDO.
DEFINE VARIABLE eMaxLineGauge AS decimal init 0   NO-UNDO.
DEFINE VARIABLE iMinLineMan AS INTEGER   init 0  NO-UNDO.
DEFINE VARIABLE iMaxLineMan AS INTEGER   init 0  NO-UNDO.
DEFINE VARIABLE iSpec AS INTEGER     NO-UNDO.
DEFINE VARIABLE lPass AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cManList AS CHARACTER   NO-UNDO.

find first equip no-lock
    where equip.line-id = iLineID
    and   equip.Equiptype = 44 no-error.
if available equip then do:
    find first EquipConstraints no-lock
        where EquipConstraints.EquipID = equip.EquipID
        and   EquipConstraints.fieldname = 45 no-error.
    if available EquipConstraints then do:
        assign iMaxLineWt = EquipConstraints.MaxVal.
    end.
    find first EquipConstraints no-lock
        where EquipConstraints.EquipID = equip.EquipID
        and   EquipConstraints.fieldname = 46 no-error.
    if available EquipConstraints then do:
        assign iMaxLineWdth = EquipConstraints.MaxVal.
    end.
    find first EquipConstraints no-lock
        where EquipConstraints.EquipID = equip.EquipID
        and   EquipConstraints.fieldname = 47 no-error.
    if available EquipConstraints then do:
        assign eMaxLineGauge = EquipConstraints.MaxVal.
    end.
    find first EquipConstraints no-lock
        where EquipConstraints.EquipID = equip.EquipID
        and   EquipConstraints.fieldname = 4 no-error.
    if available EquipConstraints then do:
        assign iMinLineMan = EquipConstraints.MinVal
               iMaxLineMan = EquipConstraints.MaxVal.
    end.
end.

do iLoop = 1 to num-entries(cOrderList):
    assign iOrderID = int(entry(iLoop,cOrderList,",")).
    find first work-order no-lock
        where work-order.order-id = iOrderID no-error.
    if available work-order then do:
        for each pickList no-lock
               where picklist.order-id = work-order.order-id
               and pickList.master-id ne ?,
            first inv no-lock 
               where inv.inv-id = picklist.master-id,
            first part no-lock
               where part.part-id = inv.part-id:
               if can-find(first rcptaudit where RcptAudit.inv-id = inv.rcpt-id) then
                   pcErr = substitute('&1&2Coil &3 on order &4 Requires Receiving Audit',
                                      pcErr,
                                      (if pcErr = '' then '' else '~n'),
                                      pickList.master-id,
                                      work-order.order-id).
              if {fnarg HasChem inv.inv-id} = 1 then
                   pcErr = substitute('&1&2Coil &3 on order &4 Requires Chemistries Audit',
                                      pcErr,
                                      (if pcErr = '' then '' else '~n'),
                                      pickList.master-id,
                                      work-order.order-id).
              if {fnarg HasMech inv.inv-id} = 1 then
                   pcErr = substitute('&1&2Coil &3 on order &4 Requires Mechanicals Audit',
                                      pcErr,
                                      (if pcErr = '' then '' else '~n'),
                                      pickList.master-id,
                                      work-order.order-id).
               if inv.net-wt > iMaxLineWt and iMaxLineWt > 0 then
                   pcErr = substitute('&1&2Coil &3 on order &4 has a net weight greater than the allowed max weight on line &5',
                                      pcErr,
                                      (if pcErr = '' then '' else '~n'),
                                      pickList.master-id,
                                      work-order.order-id,
                                      iLineID).
               if part.wdth > iMaxLineWdth and iMaxLineWdth > 0 then
                   pcErr = substitute('&1&2Coil &3 on order &4 has a width greater than the allowed max width on line &5',
                                      pcErr,
                                      (if pcErr = '' then '' else '~n'),
                                      pickList.master-id,
                                      work-order.order-id,
                                      iLineID).
               if part.gauge > eMaxLineGauge and eMaxLineGauge > 0 then
                   pcErr = substitute('&1&2Coil &3 on order &4 has a gauge greater than the allowed max gauge on line &5',
                                      pcErr,
                                      (if pcErr = '' then '' else '~n'),
                                      pickList.master-id,
                                      work-order.order-id,
                                      iLineID).
               if iMinLineMan > 0 or iMaxLineMan > 0 then do:
                   assign lPass = no.
                   for each work-item no-lock
                       where work-item.order-id = iOrderID:

                       find first spec no-lock
                           where spec.spec-id = work-item.spec-id no-error.
                           
                       if avail spec and spec.mandrel[1] >= iMinLineMan and spec.mandrel[1] <= iMaxLineMan then do:
                           assign lPass = yes.
                       end.
                       if avail spec and spec.mandrel[2] >= iMinLineMan and spec.mandrel[2] <= iMaxLineMan then do:
                           assign lPass = yes.
                       end.
                       if avail spec and spec.mandrel[3] >= iMinLineMan and spec.mandrel[3] <= iMaxLineMan then do:
                           assign lPass = yes.
                       end. 
                       if lPass then leave.
                   end.
                   if not lPass then
                       pcErr = substitute('&1&2Coil &3 on order &4 has a mandrel that is not allowed on line &5',
                                      pcErr,
                                      (if pcErr = '' then '' else '~n'),
                                      pickList.master-id,
                                      work-order.order-id,
                                      iLineID).
               end.
        end.
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckOutboundDynamicMsgs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckOutboundDynamicMsgs Procedure 
PROCEDURE CheckOutboundDynamicMsgs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piPlant as integer no-undo.
define input parameter piLoadID as integer no-undo.
define output parameter pcMessageList as character no-undo.

define variable cMessageIDList as character no-undo.
define variable i as integer no-undo.
define buffer bs-load for s-load.

find first bs-load no-lock
    where bs-load.load-id = piLoadID no-error.
if not avail bs-load then do:
    return no-apply.
end.

for each Messages no-lock
    where Messages.TriggerAction = 9
    and   Messages.plant-id = piPlant
    and   (Messages.line-id = ? or Messages.line-id = 0)
    and   (Messages.load-id = ? or Messages.load-id = 0)
    and   (Messages.order-id = ? or Messages.order-id = 0)
    and   (Messages.owner-id = ? or Messages.owner-id = 0)
    and   (Messages.user-id = ? or Messages.user-id = 0)
    and   (Messages.partner-id = ? or Messages.partner-id = 0)
    and   (Messages.part-id = ? or Messages.part-id = 0)
    and   (Messages.ExpireDate > now or Messages.ExpireDate = ?):
    if index(cMessageIDList, string(messages.messageID,'999999')) = 0
       then assign cMessageIDList =  cMessageIDList + string(messages.messageID,'999999') + ','.
end.

for each Messages no-lock
    where Messages.TriggerAction = 9
    and   Messages.plant-id = piPlant
    and   Messages.load-id = piLoadID
    and   (Messages.ExpireDate > now or Messages.ExpireDate = ?):
    if index(cMessageIDList, string(messages.messageID,'999999')) = 0
       then assign cMessageIDList =  cMessageIDList + string(messages.messageID,'999999') + ','.
end.

for each s-slip no-lock
        where s-slip.load-id = s-load.load-id,
    first partner no-lock
        where partner.partner-id = s-slip.partner-id,
     each Messages no-lock
        where Messages.TriggerAction = 9
        and   Messages.plant-id = piPlant
        and (messages.partner-id = partner.partner-id
            or messages.owner-id = partner.owner-id
            or messages.user-id = partner.user-id)
        and (messages.line-id = iLineID
            or messages.line-id = 0
            or messages.line-id = ?)
        and (Messages.ExpireDate > now or Messages.ExpireDate = ?):

    if index(cMessageIDList, string(messages.messageID,'999999')) = 0
       then assign cMessageIDList =  cMessageIDList + string(messages.messageID,'999999') + ','.

end.

do i = 1 to num-entries(cMessageIDList,","):
    find first Messages no-lock
        where Messages.MessageID = int(entry(i,cMessageIDList,",")) no-error.
    if avail Messages then do:
        if pcMessageList = "" then do:
            assign pcMessageList = messages.msg.
        end.
        else do:
            assign pcMessageList = pcMessageList + chr(13) + chr(13) + messages.msg.
        end.
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckPlantDynamicMsgs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckPlantDynamicMsgs Procedure 
PROCEDURE CheckPlantDynamicMsgs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piPlant as integer no-undo.
define input parameter piTriggerAction as integer no-undo.
define output parameter pcMessageList as character no-undo.

for each Messages no-lock
    where Messages.TriggerAction = piTriggerAction
    and   Messages.plant-id = piPlant
    and   (Messages.line-id = ? or Messages.line-id = 0)
    and   (Messages.load-id = ? or Messages.load-id = 0)
    and   (Messages.order-id = ? or Messages.order-id = 0)
    and   (Messages.owner-id = ? or Messages.owner-id = 0)
    and   (Messages.user-id = ? or Messages.user-id = 0)
    and   (Messages.partner-id = ? or Messages.partner-id = 0)
    and   (Messages.part-id = ? or Messages.part-id = 0)
    and   (Messages.ExpireDate > now or Messages.ExpireDate = ?):
    if pcMessageList = "" then do:
        assign pcMessageList = messages.msg.
    end.
    else do:
        assign pcMessageList = pcMessageList + chr(13) + chr(13) + messages.msg.
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckQueueLimit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckQueueLimit Procedure 
PROCEDURE CheckQueueLimit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iCnt as integer no-undo.
define variable lResult as logical no-undo.
iCnt = truckCount(v-Dock, truckArrivals.arrivalID). 
/* Have to set the limit to '>=' because current load has not been assigned yet.
So if you want to send alert if more than 4 you have to use >= 4 */
if iPlantID = 2 and iCnt >= 4 then do:
    run Alert(input 129,
              input (string(v-Dock) + chr(3) + string(iCnt)),
              input '',
              input '',
              input 0,
              input '',
              output lResult).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckRequireChangeOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckRequireChangeOrder Procedure 
PROCEDURE CheckRequireChangeOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

find first partner no-lock
    where partner.partner-id = iPartnerID no-error.
if not avail partner then do:
    assign pcErr = "No Partner available".
end.

find first choice no-lock
    where choice.field-no = 839
    and   choice.misc1 matches "*Partner*"
    and   choice.misc2 = string(partner.partner-id) no-error.

if not avail choice then 
    find first choice no-lock
        where choice.field-no = 839
        and   choice.misc1 matches "*Owner*"
        and   choice.misc2 = string(partner.owner-id)
        and   choice.plant-id = partner.plant-id no-error.

if not avail choice then 
    find first choice no-lock
        where choice.field-no = 839
        and   choice.misc1 matches "*Owner*"
        and   choice.misc2 = string(partner.owner-id)
        and   (choice.plant-id = 0 or choice.plant-id = ?) no-error.

if avail choice then do:
    assign pcRtn = 'Warning: Typically this customer requires Change Order! If you havent ' +
          'already, please change the order on this inventory before tripping.'.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-checkShowCust) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkShowCust Procedure 
PROCEDURE checkShowCust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

pcRtn = "INV with hold(s) marked as Show to Cust".
pcErr = "0".
do iIdx = 1 to num-entries(cInvList):
    find first inspect no-lock
        where inspect.inv-id = int(entry(iIdx,cInvList))
        and   inspect.ShowToCust = yes no-error.
    if avail(inspect) then do:
        pcRtn = substitute("&1~n&2",pcRtn,inspect.inv-id).
        pcErr = "1".
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckTripReq) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckTripReq Procedure 
PROCEDURE CheckTripReq :
define variable iIdx as integer no-undo.
define variable cParts as character no-undo.

do iIdx = 1 to num-entries(cMultList):
    find first inv no-lock where inv.inv-id = integer(entry(iIdx,cMultList)) no-error.
    if avail inv and not can-do(cParts, string(inv.part-id)) then do:
        cParts = cParts + "," + string(inv.part-id).
    end. /*if avail inv*/
end. /*do iIdx = 1*/
cParts = trim(cParts, ",").

do iIdx = 1 to num-entries(cParts):
    if can-find(first TripReq where TripReq.FromPartID = int(entry(iIdx,cParts))
                and TripReq.stat = 2 and 
                (TripReq.ToPartnerID = 0 or TripReq.ToPartnerID = iPartnerID))
        and not can-find(first TripReq where TripReq.FromPartID = int(entry(iIdx,cParts)) 
                         and TripReq.stat <> 2 and 
                         (TripReq.ToPartnerID = 0 or TripReq.ToPartnerID = iPartnerID)) then
        pcRtn = pcRtn + entry(iIdx,cParts) + "~n".                        
end. /*for each ttParts*/
if pcRtn > "" then
    pcRtn = "The following parts have Trip Requests that have been completed and currently have no open Trip Requests. Do you want to continue?~n~n" + pcRtn.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CountCoils) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CountCoils Procedure 
PROCEDURE CountCoils :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter pcbol as character.


v-NoCoils = 0.

find last asn where asn.bill-o-lad = pcBOL no-lock no-error.
  if asn.load-no ne 0 and asn.load-no ne ? then
    for each basn no-lock
       where basn.plant-id = asn.plant-id 
         and basn.load-no = asn.load-no:
      
      v-NoCoils = v-NoCoils + 1.
    end.
  else
    for each basn 
       where basn.plant-id = iPlantID
         and basn.bill-o-lad = pcbol no-lock:
      
      v-NoCoils = v-NoCoils + 1.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateException) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateException Procedure 
PROCEDURE CreateException :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
cComment = substitute('&1:&2&3',
                      (if cLoadID = '' then 'Order' else 'Load'),
                      cOrderList,
                      cLoadID).
find first choice no-lock where choice.field-no = 775 and choice.descr = cComment no-error.
if avail choice then return.

select max(val) into iIdx from choice where choice.field-no = 775.
if iIdx = ? then iIdx = 0.

create choice.
assign
    choice.field-no = 775
    choice.descr = cComment
    choice.val = iIdx + 1
    choice.misc1 = cOrderList + cLoadID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateRewrapMults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateRewrapMults Procedure 
PROCEDURE CreateRewrapMults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iNewBreak as integer no-undo.
define variable iNewFootage as integer no-undo.
define variable iRemWt as integer no-undo.
define variable iRemFt as integer no-undo.
define variable iNewWt as integer no-undo.
define variable iSysTime as integer no-undo.
define variable cUserID as character no-undo.
define variable iInvID as integer no-undo.
define variable iRewrapID as integer no-undo.
define variable iCondCode as integer no-undo.
define variable iEDICode as integer no-undo.
define variable cNewMult as character no-undo.
define variable cRtnMsg as character no-undo.
define variable hMultLib as handle no-undo.
define variable cMultList as character no-undo.
define variable cHoldAvail as character no-undo.

find first multwork no-lock where multwork.line-id = iLineID no-error.
if not avail MultWork then do:
    pcErr = substitute('An Error Occurred creating mults:~nMultWork Record NOT FOUND for line &1',iLineID).
    return.
end.

pcRtn = substitute('&1,&2,&3',multwork.order-id,multwork.coil-id,multwork.break-no).
iRewrapID = multwork.rewrapid.
cHoldAvail = 'no'.
iInvId = MultWork.coil-id.
cMultList = ''.
isystime = {fn getSysTime}.
iNewBreak =  0.

if iScrapOption = 2 then do:
    cUserID = {fnarg getGlobalVar 'userID'}.
    select Damagecode.edicode into iEDICode
        from damagecode
        where damagecode.damagecode = iDamageCode.
    find first rewrap no-lock
        where rewrap.rewrapid = irewrapid no-error.
    iCondCode = (if avail rewrap and rewrap.fault = 2 then 5 else 6).
end.

run lib/l0006.p persistent set hMultLib.
for each MultWork where MultWork.line-id = iLineID:
    if Multwork.mult-no begins 'Split' then do:
        if iNewBreak = 0 then do:
            /* This is a subsequent break, find max used and incrememt it */
            select max(break-no) into iNewBreak from mult where mult.coil-id = multwork.coil-id.
            iNewBreak = iNewBreak + 1.
            find first inv where inv.inv-id = multwork.coil-id no-error.
            if avail inv then assign
            inv.break-no = iNewBreak.
            pcRtn = substitute('&1,&2,&3',multwork.order-id,multwork.coil-id,iNewBreak).
        end.
        /* need to calculate new mult number */
        find first part no-lock where part.part-id = MultWork.part-id.
        run newMultID in hMultLib
            (input MultWork.coil-id,
             input part.partner-id,
             input iNewBreak,
             input MultWork.cut-no,
             output cNewMult,
             output cRtnMsg).
        if cRtnMsg > '' then pcErr = substitute('&1&2&3',pcErr,(if pcErr = '' then '' else '~n'),cRtnMsg).
        if cRtnMsg = '' then assign
            MultWork.mult-no = cNewMult
            MultWork.break-no = iNewBreak.
    end.
    case iScrapOption:
        when 0 then do:   /* footage didn't change, just put the mults back */
            if multwork.inv-id eq ? or multwork.inv-id = 0 then do:
               create Mult.
               buffer-copy MultWork to Mult.
            end.
            else do:
                if can-find(first hold where hold.inv-id = multwork.inv-id
                            and hold.hold-code = 2
                            and hold.active = yes)
                    and can-find(first inv no-lock where inv.inv-id = multwork.inv-id
                                 and inv.hold-code = 2) then cHoldAvail = 'yes'.
                find first mult no-lock
                    where mult.coil-id = multwork.coil-id
                    and mult.break-no = multwork.break-no
                    and mult.slit-no = multwork.slit-no
                    and mult.rcpt-id = multwork.rcpt-id no-error.
            end.
            cMultList = substitute('&1&2&3',
                                   cMultlist,
                                   (if cMultList = '' then '' else '|'),
                                   (if avail mult then string(rowid(mult)) else '?')).
            delete MultWork.
        end.
        when 1 then do:
            /* Footage Changed, will be making another break from remainder             */
            /* This so far assumes the mult has not been weighed (does not have inv-id) */
            /* Will get a lot more complicated if inventory changes would also be needed*/
    
            /* lnhome-v2 is checking whether making less than full break is permitted   */
    
            iNewFootage = min(MultWork.linear-ft, iFootage).
            iNewWt = round(MultWork.calc-wt * (iFootage / multwork.linear-ft), 0).
            iNewWt = min(iNewWt, MultWork.calc-wt).
            iRemWt = max(0, MultWork.calc-wt - iNewWt).
            iRemFt = max(0, MultWork.linear-ft - iFootage).
            if cRtnMsg = '' then do:
                create mult.
                buffer-copy multwork to mult
                    assign
                    mult.calc-wt = iNewWt
                    mult.linear-ft = iNewFootage.
                if iRemWt = 0 then
                    delete MultWork.
                if iRemWt > 0 then assign
                    MultWork.calc-wt = iRemWt
                    MultWork.linear-ft = iRemFt
                    Multwork.mult-no = 'Split' + Multwork.mult-no
                    .
                 cMultList = substitute('&1&2&3',cMultlist,(if cMultList = '' then '' else '|'),rowid(mult)).
            end.
        end.
        when 2 then do:
            /* Replace mults with new footage, scrap the rest */
            iNewFootage = min(MultWork.linear-ft, iFootage).
            iNewWt = round(MultWork.calc-wt * (iFootage / multwork.linear-ft), 0).
            iNewWt = min(iNewWt, MultWork.calc-wt).
            iRemWt = max(0, MultWork.calc-wt - iNewWt).
           
            create Mult.
            buffer-copy MultWork to Mult
                assign
                mult.calc-wt = iNewWt
                mult.linear-ft = iNewFootage
                no-error.
            if not error-status:error then delete MultWork.
            cMultList = substitute('&1&2&3',cMultlist,(if cMultList = '' then '' else '|'),rowid(mult)).
            if iRemWt > 0 then do:
                create mscrap.
                assign mscrap.logon       = cUserID
                       mscrap.inv-id      = iInvID
                       mscrap.scrap-date  = today
                       mscrap.scrap-time  = iSysTime
                       mscrap.damage-code = iDamageCode
                       mscrap.loc-code    = iDamageLoc
                       mscrap.mult-no     = mult.mult-no
                       mscrap.weight      = iRemWt
                       mscrap.cond-code   = icondCode
                       mscrap.edicode     = iEDICode
                       mscrap.mscrapID    = next-value(mscrapID)
                 .
            end.
        end.
    end case.
end.

if not can-find(first multwork where multwork.line-id = iLineID) then do:
    /* close out the rewrap record */
    find first rewrap where rewrap.rewrapid = iRewrapID no-error.
    if avail rewrap then assign
        rewrap.fixeddate = today
        rewrap.fixedtime = isystime
        rewrap.stat = 1
        .
end.
if valid-handle(hMultLib) then delete procedure hMultLib.
pcRtn = substitute('&1,&2,&3',pcRtn, cHoldAvail, cMultList).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-d0408Defaults) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE d0408Defaults Procedure 
PROCEDURE d0408Defaults :
/*------------------------------------------------------------------------------
  Purpose:  Get lists of applicable customers for next ID for load, mult, pkg   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
cRtn = ''.
for each next-id no-lock
    where next-id.field-no = 49
    and next-id.plant-id = iPlantID:
    find first cust no-lock where cust.cust-id = next-id.cust-id no-error.
    if avail cust then cRtn = substitute('&1&2&4 - &3,&4',
                                         cRtn,
                                         (if cRtn = '' then '' else ','),
                                         replace((if cust.abbr = '' then cust.name else cust.abbr),',',''),
                                         cust.cust-id).
end.
pcRtn = cRtn.

cRtn = ''.
for each next-id no-lock
    where next-id.field-no = 59:
    find first cust no-lock where cust.cust-id = next-id.cust-id no-error.
    if avail cust then cRtn = substitute('&1&2&4 - &3,&4',
                                         cRtn,
                                         (if cRtn = '' then '' else ','),
                                         replace((if cust.abbr = '' then cust.name else cust.abbr),',',''),
                                         cust.cust-id).
end.
pcRtn = substitute('&1|&2',pcrtn,cRtn).

cRtn = ''.
for each next-id no-lock
    where next-id.field-no = 58
    and next-id.plant-id = iPlantID:
    find first cust no-lock where cust.cust-id = next-id.cust-id no-error.
    if avail cust then cRtn = substitute('&1&2&4 - &3,&4',
                                         cRtn,
                                         (if cRtn = '' then '' else ','),
                                         replace((if cust.abbr = '' then cust.name else cust.abbr),',',''),
                                         cust.cust-id).
end.

pcRtn = substitute('&1|&2',pcrtn,cRtn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DefaultSlipConfig) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultSlipConfig Procedure 
PROCEDURE DefaultSlipConfig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
pcRtn = '1,2,3,5,7,8,10,14,0,0,21,3,4,0,0,0,0,0,0,0'.
if iMetric ne ? and iMetric > 0 then pcRtn = '1,2,3,5,7,8,10,14,0,0,21,3,4,0,0,9,11,15,0,0'.
find first appattr no-lock where appattr.attrkey = 'DefaultSlipConfig' no-error.

if avail appattr and iMetric = ? or iMetric = 0 then
    assign pcRtn = entry(1,appattr.appstr,'|').
if avail appattr
    and iMetric ne?
    and iMetric > 0
    and num-entries(appattr.appstr,'|') > 1 then
    assign pcRtn = entry(2,appattr.appstr,'|').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteMultHolds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteMultHolds Procedure 
PROCEDURE DeleteMultHolds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iInvID   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cMults   as CHARACTER   NO-UNDO.
DEFINE VARIABLE cMult    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCut     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBreak   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i        AS INTEGER     NO-UNDO.
DEFINE VARIABLE numHolds AS INTEGER     NO-UNDO.

assign iInvID = int(entry(1, pcOptionList,chr(1)))
       cMults  = entry(2, pcOptionList,chr(1)).

do i = 1 to num-entries(cMults,","):
    assign cMult = entry(i, cMults, ",")
           iCut = int(entry(2, cMult,"-"))
           iBreak = int(entry(1, cMult,"-")).

    find first mult 
        where mult.coil-id = iInvID
        and   mult.break-no = iBreak
        and   mult.slit-no = iCut no-error.

    assign numHolds = 0.

    for each hold no-lock
        where hold.coil-id = iInvID
        and   hold.break-no = iBreak
        and   hold.slit-no = iCut:
        assign numHolds = numHolds + 1.
    end.

    if numHolds > 1 then do:
        assign pcRtn = pcRtn + "There is more than 1 hold for mult number " + mult.mult-no + " .  This cannot be deleted.".
        return no-apply.
    end.

    find first hold 
        where hold.coil-id = iInvID
        and   hold.break-no = iBreak
        and   hold.slit-no = iCut no-error.

    if available mult and available hold then do:
        if mult.inv-id ne 0 then do:
            assign pcRtn = pcRtn + "Mult is already weighted in. You can not Delete Hold for mult number " + mult.mult-no.
            return no-apply.
        end.
        if hold.hold-code = 5 then do:
            assign pcRtn = pcRtn + 'Hold Code 5 may not be removed from mult screen'.
            return no-apply.
        end.

        delete hold.

        for each inspect 
            where inspect.coil-id = iInvID
            and   inspect.inv-id = 0
            and   inspect.break-no = iBreak
            and   inspect.slit-no = iCut:
                delete inspect.
        end.
        assign mult.hold-code = 0.
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DeleteUFD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteUFD Procedure 
PROCEDURE DeleteUFD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
for each ufd exclusive-lock
    where ufd.ufdkey = cUFDKey:
    if ufd.ufdStat = 10 then do:
        delete ufd.
    end.
    else do:
        assign pcErr = "You can not delete a UFD that is not in Pending Status".
        return.
    end.
end.
for each UFDSRXref where UFDSRXref.ufdkey = cUFDKey exclusive-lock:
    delete UFDSRXref.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FaultCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FaultCheck Procedure 
PROCEDURE FaultCheck :
/*------------------------------------------------------------------------------
  Purpose:   returns a list of damage codes and information on preInspect
             records that flag mults as scrap  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cBreakList as character no-undo.
define variable cMult as character no-undo.
define variable iWeight as integer no-undo.

pcRtn = ''.
iWeight = 0.
cBreakList = ''.

for each voiceinspect no-lock
    where voiceinspect.coil-id = iCoilID
    and voiceinspect.break-no = iBreak
    and voiceinspect.ScrapMult = yes
    break by voiceinspect.damage-code:
    cMult = substitute('&1-&2',voiceinspect.break-no, voiceinspect.slit-no).
    if not can-do(cBreakList, cMult) then do:
        find first mult no-lock
            where mult.coil-id = iCoilID
            and mult.break-no = iBreak
            and mult.slit-no = voiceinspect.slit-no no-error.

        iWeight = iWeight + (if avail mult then mult.calc-wt else 0).
        cBreakList = substitute('&1&2&3-&4',
                                cBreakList,
                                (if cBreakList = '' then '' else ','),
                                voiceinspect.break-no,
                                voiceinspect.slit-no).
    end.
    if last-of (voiceinspect.damage-code) then do:
        find first damagecode no-lock
            where damagecode.damagecode = voiceinspect.damage-code no-error.
        pcRtn = substitute('&1&2&3^&4^&5^&6^&7^&8^&9',
                           pcRtn,
                           (if pcRtn = '' then '' else '|'),
                           voiceinspect.damage-code,
                           damagecode.descr,
                           damagecode.MillFault,
                           damagecode.Procfault,
                           damageCode.EDICode,
                           iWeight,
                           cBreakList).
         cBreakList = ''.
         iWeight = 0.
    end.
end.
if pcRtn = '' then pcRtn = 'N/A'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindASN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindASN Procedure 
PROCEDURE FindASN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cBOL as character no-undo.

find last asn no-lock 
    where asn.load-no ne 0 and string(asn.load-no) = cFindLoadNo
      and asn.plant-id = iPlantID 
      and not scan-flag no-error.
  
if not available asn then

  find first asn no-lock
      where asn.plant-id = iPlantID
        and asn.bill-o-lad = cFindLoadNo 
        and not asn.scan-flag no-error.
  if not available asn then
  do:
      cBOL = StripLoadNum(input cFindLoadNo).
      find first asn no-lock
          where asn.plant-id = iPlantID
            and asn.bill-o-lad = cBOL 
            and not asn.scan-flag no-error.
      if not available asn then
      do:
        cBOL = StripLoadNum2(input cFindLoadNo).
        find first asn no-lock
            where asn.plant-id = iPlantID
              and asn.bill-o-lad = cBOL 
              and not asn.scan-flag no-error.
      end.
      if not available asn then 
      do:
          assign cBOL = cFindLoadNo.
          for each basn no-lock 
              where basn.plant-id = iPlantID
                and basn.bill-o-lad <> ?
                and not basn.scan-flag:
            if StripLoadNum(basn.bill-o-lad) = cBOL then
            do:
              find first asn no-lock
                  where asn.plant-id   eq basn.plant-id
                    and asn.owner-id   eq basn.owner-id
                    and asn.own-inv-id eq basn.own-inv-id
                    and asn.barcode-id eq basn.barcode-id
                    and asn.inv-id     eq basn.inv-id no-error. 
              leave.
            end.
          end.
          if not available asn then
          do:
              for each basn no-lock 
                  where basn.plant-id = iPlantID
                    and basn.bill-o-lad <> ?
                    and not basn.scan-flag:
                if StripLoadNum2(basn.bill-o-lad) = cBOL then
                do:
                  find first asn no-lock
                      where asn.plant-id   eq basn.plant-id
                        and asn.owner-id   eq basn.owner-id
                        and asn.own-inv-id eq basn.own-inv-id
                        and asn.barcode-id eq basn.barcode-id
                        and asn.inv-id     eq basn.inv-id no-error. 
                  assign lMultiStripASN = true.
                  leave.
                end.
              end.
          end.
          /* 64492 - find asn's with stripped leading 0's on find & asns */
          if not available asn then
          do:
              assign cBOL = stripLoadNum(cFindLoadNo).
              for each basn no-lock 
                  where basn.plant-id = iPlantID
                    and basn.bill-o-lad <> ?
                    and not basn.scan-flag:
                if StripLoadNum(basn.bill-o-lad) = cBOL then
                do:
                  find first asn no-lock
                      where asn.plant-id   eq basn.plant-id
                        and asn.owner-id   eq basn.owner-id
                        and asn.own-inv-id eq basn.own-inv-id
                        and asn.barcode-id eq basn.barcode-id
                        and asn.inv-id     eq basn.inv-id no-error. 
                  leave.
                end.
              end.
          end.
      end.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetPartnerSlip2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPartnerSlip2 Procedure 
PROCEDURE GetPartnerSlip2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iLoop as integer no-undo.
find first partner no-lock
    where partner.partner-id = iPartnerID no-error.

case true:
    when not avail partner then assign
        pcRtn = ''
        pcErr = '** Partner ID requested is invalid.'.
    when partner.slip-cfg2[1] = 0 then assign
        pcRtn = ''
        pcErr = '** Selected partner slip configuration has not been set.'.
    otherwise do:
        do iLoop = 1 to 20:
            pcRtn = substitute('&1&2&3'
                               ,pcRtn
                               ,(if pcRtn = '' then '' else ',')
                               ,partner.slip-cfg2[iLoop]).
        end.
    end.
end case.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetPCGLines) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPCGLines Procedure 
PROCEDURE GetPCGLines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cKey AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLines AS CHARACTER   NO-UNDO.

assign cKey = string(iPartID) + "-".
for each imageDir no-lock
    where imageDir.iKey begins cKey
    and   imageDir.iKeyType eq 46:

    if cLines = "" then do:
        assign cLines = entry(2,imageDir.iKey,"-").
    end.
    else do:
        assign cLines = cLines + "," + entry(2,imageDir.iKey,"-").
    end.
end.

assign pcRtn = cLines.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InboundCheckIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InboundCheckIn Procedure 
PROCEDURE InboundCheckIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
lASNMill = can-find (first choice no-lock
                     where choice.field-no eq 648
                     and choice.val eq TruckArrivals.mill-id).
cFindLoadNo = TruckArrivals.loadid.

assign v-bol       = ?
       v-truck-no  = ?
       v-scac      = ?
       v-num-coils = 0
       v-NoCoils   = 0
       cCoilNo     = ?
       lASNMill    = no
       lPrintRpt   = no
       lMultiStripASN = no
       lAutoDock   = no
       v-dock = 0.

run FindASN.

IF NOT AVAILABLE asn THEN DO:
    assign v-truck-no = truckArrivals.TruckNo
           v-Scac = TruckArrivals.scac-code
           v-num-coils = truckarrivals.NoCoils.
    /* 65506 - check for any plant defaults */
    run ship/t0666.p (input truckArrivals.loadid,
                      input "",             /* coil # */
                      input iPlantID,
                      input truckArrivals.ArrivalID, 
                      output v-dock,
                      output iReason).
    if v-dock <> 0 then do:
      assign lAutoDock = yes.
      run UpdateTruckArrivals.
      run ProcessASNCoil.
      run CheckInboundDynamicMsgs(input iPlantID, input ?, output cMessageList).
      assign lAutoDock = no.
    end.
    else do:   /* Didn't find asn, ask for more info */
        pcRtn = substitute('mode=&1|msg1=&2|Msg2=&3|Msg3=&4|Msg4=&5|TruckNo=&6|Scac=&7|Coils=&8',
                           'MoreInboundInfo',
                           '',
                           '',
                           'Please Provide More Details',
                           'To Help us Route your Delivery.',
                           v-truck-no,
                           v-scac,
                           v-num-coils).
    end.
END.
else do:
  assign
    v-bol = asn.bill-o-lad
    v-truck-no = (if TruckArrivals.TruckNo ne ''
                  and TruckArrivals.TruckNo ne ?
                  then TruckArrivals.TruckNo
                  else asn.vehicle-id)
    v-scac = (if TruckArrivals.scac-code ne ''
              and TruckArrivals.scac-code ne ?
              then TruckArrivals.scac-code
              else asn.scac-code)
    v-num-coils = ( if TruckArrivals.NoCoils ne ?
                    and TruckArrivals.NoCoils ne 0
                    then TruckArrivals.NoCoils
                    else asn.pieces). 

    run countCoils(asn.bill-o-lad).
    if asn.bill-o-lad = "" then
        assign cCoil = asn.own-inv-id.
    else
        assign cCoil = "".
    run ship/t0666.p (input asn.bill-o-lad,
                      input cCoil,             /* coil # */
                      input iPlantID,
                      input truckArrivals.ArrivalID, 
                      output v-dock,
                      output iReason).

    run UpdateTruckArrivals.
    run ProcessASNCoil.
    run CheckInboundDynamicMsgs(input iPlantID, input asn.owner-id, output cMessageList).
end.

if avail truckarrivals and truckarrivals.loadID = '' then 
    assign v-truck-no = truckArrivals.TruckNo
           v-Scac = TruckArrivals.scac-code
           v-num-coils = truckarrivals.NoCoils.

if lPrintRpt and cMessageList = "" then do:
    run PrintArrivalSheet.
end.
else if lPrintRpt and cMessageList ne "" then do:
    pcRtn = substitute('&1|Mode=ShowDynamicMessage|DynamicMsg=&2|PrintArrival=yes|ArrivalID=&3',
                       pcRtn,
                       cMessageList,
                       truckArrivals.ArrivalID).
end.
else if not lPrintRpt and cMessageList ne "" then do:
    pcRtn = substitute('&1|Mode=ShowDynamicMessage|DynamicMsg=&2|PrintArrival=no|ArrivalID=&3',
                       pcRtn,
                       cMessageList,
                       truckArrivals.ArrivalID).
end.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InboundCheckin2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InboundCheckin2 Procedure 
PROCEDURE InboundCheckin2 :
/*------------------------------------------------------------------------------
  Purpose:     retry inbound checkin with more load data provided
  Parameters:  <none>
  Notes:   code cloned from scanner/j0583, main proc, F2 loop    
------------------------------------------------------------------------------*/
find first truckarrivals no-lock
    where truckarrivals.arrivalID = iArrivalID no-error.

lASNMill = can-find (first choice no-lock
                     where choice.field-no eq 648
                     and choice.val eq TruckArrivals.mill-id).
assign                /* passed in from client app */
    cFindLoadNo = cLoadID
    v-truck-no = cVehicleID
    cNewBOL = cLoadID
    v-num-coils = v-NoCoils.
assign
    cCoilNo     = ?.
assign
    lPrintRpt   = no
    lMultiStripASN = no
    lAutoDock   = no
    v-dock = 0.

run findASN.

if not avail asn then do:
    /* try to locate by an owner inv id "WE NO LONGER LOOK FOR COILS "*/
/*     pcRtn = substitute('mode=InboundCoil|Msg1=|Msg2=ASN Not Located yet,|Msg3=Please Enter a Coil Number|Msg4=To Help us Route your Delivery.').  */
  assign iReason = 21
         v-dock = 0.
  run UpdateTruckArrivals.
  run CheckPlantDynamicMsgs(input iPlantID, input 8, output cMessageList).
  if cMessageList ne "" then do:
      pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4|Mode=ShowDynamicMessage|DynamicMsg=&5|PrintArrival=no',
                       'Could not Locate Inbound Appointment,',
                       'Please see PSI Dispatcher',
                       '',
                       ' Press RESET to clear. ',
                       cMessageList).
  end.
  else do:
      pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                       'Could not Locate Inbound Appointment,',
                       'Please see PSI Dispatcher',
                       '',
                       ' Press RESET to clear. ').
  end.
end.
else do:
  assign cNewBOL = asn.bill-o-lad.
  if available truckarrivals then do:
    iArrivalID = truckArrivals.arrivalID.
    v-truck-no = (if TruckArrivals.TruckNo ne ''
                  and TruckArrivals.TruckNo ne ?
                  then TruckArrivals.TruckNo
                  else asn.vehicle-id).
    v-scac = (if TruckArrivals.scac-code ne ''
              and TruckArrivals.scac-code ne ?
              then TruckArrivals.scac-code
              else asn.scac-code).
    v-num-coils = (if TruckArrivals.NoCoils ne ?
                   and TruckArrivals.NoCoils ne 0
                   then TruckArrivals.NoCoils
                   else asn.pieces). 
      
  
  run countCoils(asn.bill-o-lad).

    if asn.bill-o-lad = "" then
        assign cCoil = asn.own-inv-id.
    else
        assign cCoil = "".
    run ship/t0666.p (input asn.bill-o-lad,
                      input cCoil,           /* coil # */
                      input iPlantID,
                      input truckarrivals.arrivalid, 
                      output v-dock,
                      output iReason).
    run UpdateTruckArrivals.
    run ProcessASNCoil.
    run CheckInboundDynamicMsgs(input iPlantID, input asn.owner-id, output cMessageList).
    if lPrintRpt and cMessageList = "" then do:
        run PrintArrivalSheet.
    end.
    else if lPrintRpt and cMessageList ne "" then do:
        pcRtn = substitute('&1|Mode=ShowDynamicMessage|DynamicMsg=&2|PrintArrival=yes|ArrivalID=&3',
                           pcRtn,
                           cMessageList,
                           truckArrivals.ArrivalID).
    end.
    else if not lPrintRpt and cMessageList ne "" then do:
        pcRtn = substitute('&1|Mode=ShowDynamicMessage|DynamicMsg=&2|PrintArrival=no|ArrivalID=&3',
                           pcRtn,
                           cMessageList,
                           truckArrivals.ArrivalID).
    end.
  end.
  else do:
    run CheckPlantDynamicMsgs(input iPlantID, input 8, output cMessageList).
    if cMessageList ne "" then do:
        pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4|Mode=ShowDynamicMessage|DynamicMsg=&5|PrintArrival=no',
                       'Could not Locate Inbound Appointment,',
                       'Please see PSI Dispatcher',
                       '',
                       ' Press RESET to clear. ',
                       cMessageList).
    end.
    else do:
        pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                       'Could not Locate Inbound Appointment,',
                       'Please see PSI Dispatcher',
                       '',
                       ' Press RESET to clear. ').
    end.
  end.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InboundCheckIn3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InboundCheckIn3 Procedure 
PROCEDURE InboundCheckIn3 :
/*------------------------------------------------------------------------------
  Purpose:     retry inbound checkin with coil number provided
  Parameters:  <none>
  Notes:   code cloned from scanner/j0583, main proc, F2 loop    
------------------------------------------------------------------------------*/
/* PER RECEIVING GROUP WE NO LONGER NEED TO OFFER THE COIL OPTION */
/*
find first truckarrivals where truckarrivals.arrivalID = iArrivalID no-error.

lASNMill = can-find (first choice no-lock
                     where choice.field-no eq 648
                     and choice.val eq TruckArrivals.mill-id).
assign                /* passed in from client app */
    cFindLoadNo = cLoadID
    v-truck-no = cVehicleID
    cNewBOL = cLoadID
    v-num-coils = v-NoCoils
    lPrintRpt   = no
    lMultiStripASN = no
    lAutoDock   = no
    v-dock = 0.


find first asn no-lock 
    where asn.plant-id eq iPlantID
      and (asn.own-inv-id  eq cCoilNo 
           or asn.prev-inv-id eq cCoilNo)
      and not asn.scan-flag no-error.
if not available asn then do:
  find first asn no-lock 
      where asn.barcode-id eq cCoilNo
        and asn.plant-id eq iPlantid 
        and not asn.scan-flag no-error.
end.
if not available asn then do:
    assign cCoilNo = replace(cCoilNo,"-","").
    assign cCoilNo = replace(cCoilNo," ","").
    find first asn no-lock 
        where asn.plant-id eq iPlantid
          and (asn.own-inv-id  eq cCoilNo 
               or asn.prev-inv-id eq cCoilNo) 
          and not asn.scan-flag no-error.
    if not available asn then do:
      find first asn no-lock 
          where asn.barcode-id  eq cCoilNo
            and asn.plant-id eq iPlantid 
            and not asn.scan-flag no-error.
    end.
end.

if available asn then
do:
    run countCoils(asn.bill-o-lad).
    if asn.bill-o-lad = "" then
        assign cCoil = asn.own-inv-id.
    else
        assign cCoil = "".
    run ship/t0666.p (input asn.bill-o-lad,
                      input cCoil,         /* coil # */
                      input iPlantID,
                      input iArrivalID, 
                      output v-dock,
                      output iReason).

    run UpdateTruckArrivals.
    run ProcessASNCoil.
    
    if lPrintRpt then run PrintArrivalSheet.

end.
else do:
  assign iReason = 21
         v-dock = 0.
  run UpdateTruckArrivals.
  pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                   'Mill Requires ASN, but one',
                   'Could not be found for your load.',
                   'Please see PSI Dispatcher',
                   '   Press RESET to clear.').
end. */
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InitCheckin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCheckin Procedure 
PROCEDURE InitCheckin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable lApptReqd as logical no-undo.
define variable iEarlyMinutes as integer no-undo init -60.
define variable iLateMinutes as integer no-undo init 60.

assign lApptReqd = can-find(first cntl
                            where cntl.plant-id = iPlantID
                            and cntl.ApptReqd = true).

find first choice no-lock where choice.field-no = 644
    and choice.val = iPlantID no-error.

if avail choice then assign
    iEarlyMinutes = int(entry(1, choice.misc1, ' ')) * -1
    iLateMinutes = int(entry(2, choice.misc1, ' ')).

assign pcRtn = substitute('ApptReqd=&1|EarlyMinutes=&2|LateMinutes=&3',
                         string(lApptReqd),
                         iEarlyMinutes,
                         iLateMinutes).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-isMaintUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE isMaintUser Procedure 
PROCEDURE isMaintUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       returns 'isMaintUser|isMaintMgr|userID|CSV List of plant Maint users'
------------------------------------------------------------------------------*/
DEFINE VARIABLE lMaintUser AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPMMgr AS LOGICAL     NO-UNDO.
pcRtn = 'no'.
iPlantID = 0.
iIdx = 0.
lMaintUser = no.
lPMMgr = no.

find first psi-user no-lock where psi-user.logon = cUserID no-error.
if avail psi-user then
   find first choice no-lock
       where choice.field-no = 702
       and choice.val = psi-user.psiuserid no-error.

assign
    lMaintUser = (if avail choice and choice.misc1 = 'maint' then yes else no)
    iPlantID = (if avail choice then choice.plant-id else 0).

find first choice no-lock where choice.field-no eq 704
                            and choice.plant-id eq iPlantID
                            and choice.misc1 = "G"
                            and choice.misc3 = "pm" no-error.
if available choice 
    and available psi-user then do:
    /* check if user is PM Mgr (can see all jobs) */
    find first AlertGroup no-lock where Alertgroup.groupid eq int(choice.misc2) no-error.
    if can-find (first contactGroupXRef no-lock 
                    where contactGroupXref.GroupID   eq alertGroup.GroupID
                      and contactGroupXRef.contactID eq psi-user.PSIUserID) then
        assign lPMMgr = yes.
end.

pcRtn = substitute('&1|&2|&3|',lMaintUser,lPMMgr,(if avail psi-user then psi-user.psiuserid else 0)).

if iPlantID > 0 then do:
   for each choice no-lock where choice.field-no = 702
       and choice.plant-id = iPlantID
       and (choice.misc1 = 'maint' or (lPMMgr = yes and choice.misc1 = 'all'))
        by choice.descr:
       iidx = iIdx + 1.
       pcRtn = substitute('&1&2&3,&4',
                          pcRtn,
                          (if iidx = 1 then '' else ','),
                          choice.descr,
                          choice.val).
   end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LineList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LineList Procedure 
PROCEDURE LineList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
pcRtn = 'Undefined' + chr(1) + '0'.
for each line-cfg no-lock
    where (line-cfg.plant-id = iplantID or iPlantID = 0):
    if line-cfg.Linetype = 2 then next.  /* scheduling line */
    pcRtn = substitute('&1&2&3,&4&2&5',
                       pcRtn,
                       chr(1),
                       line-cfg.line-id,
                       line-cfg.descr,
                       line-cfg.line-id).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadCheckin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadCheckin Procedure 
PROCEDURE LoadCheckin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iTotWt as integer no-undo init 0.
define variable cWidths as character no-undo init ''.
define variable iCoils as integer no-undo init 0.
define variable iEarlyMinutes as integer no-undo init 99999.
define variable iLateMinutes as integer no-undo init 0.
define variable cAlertVar as character no-undo.
define variable lResult as logical no-undo.
define variable dtAppointment as datetime no-undo.
define variable dtEarliest as datetime no-undo.
define variable dtLatest as datetime no-undo.

pcErr = ''.
if cVehicleID = ? or cVehicleID = '' then
    pcErr = '** Vehicle ID must be entered.|||Press Reset to Start Over.'.
iLoadID = integer(cLoadID) no-error.
if not error-status:error then do:
    find first s-scslip no-lock
        where s-scslip.load-id = iLoadID
        and s-scslip.plant-id = iPlantID no-error.
end.
if not avail s-scslip then do:
    find first s-scslip no-lock
        where s-scslip.own-load-no = cLoadID
        and s-scslip.plant-id = iPlantID no-error.
end.
if avail s-scslip then
    find first s-load no-lock
    where s-load.load-id = s-scslip.load-id no-error.
if avail s-scslip then
    find first partner no-lock
    where partner.partner-id = s-scslip.partner-id no-error.
if not avail s-scslip or not avail s-load then do:
    pcErr = substitute('** Load Number &1 is not valid.|Check with your dispatcher.||Press Reset to Start Over.',cLoadID).
    return.
end.
if s-scslip.stat le 3
    and iPlantID = 4
    and partner.owner-id = 55 then do:
    pcErr = '** AK has not yet finalized this load.|Check with your dispatcher.||Press Reset to Start Over.'.
    return.
end.
if s-scslip.stat lt 3
    and iPlantID = 4
    and partner.owner-id ne 55 then do:
    pcErr = '** Non-AK Load is not Generated.|Check with your dispatcher||Press Reset to Start Over'.
    return.
end.

if iPlantID = 4 then do:
  if s-scslip.stat >= 5 or (int(s-load.arrv-date) > 0
                          and s-load.arrv-time <> ?) then do:
    pcErr = '** Load is already arrived.|Please check in at Office.||Press Reset to Start Over'.
    return.
  end.
end.
else do:
  if s-scslip.stat >= 4 or (int(s-load.arrv-date) > 0
                          and s-load.arrv-time <> ?) then do:
    pcErr = '** Load is already arrived.|Please check in at Office.||Press Reset to Start Over'.
    return.
  end.
end.

if iPlantID = 4 and s-load.appt-date = ? then do:
    pcErr = '** NO Appointment for this load.|Please check in at Office.||Press Reset to Start Over'.
    return.
end.

find first partner no-lock
    where partner.partner-id = s-scslip.partner-id no-error.
if avail partner then find first cust no-lock
    where cust.cust-id = partner.user-id no-error.

/* too early or late ? */
/* Find early and late allowance - TIWO 63368*/
FIND FIRST Choice WHERE choice.field-no = 658
    AND choice.plant-id = partner.plant-id
    AND choice.misc1 = STRING(Partner.owner-id) NO-LOCK NO-ERROR.
IF NOT AVAILABLE(choice) THEN
    FIND FIRST Choice WHERE choice.field-no = 658
    AND choice.plant-id = 0
    AND choice.misc1 = STRING(Partner.owner-id) NO-LOCK NO-ERROR.
IF NOT AVAILABLE(choice) THEN
    FIND FIRST Choice WHERE choice.field-no = 658
    AND choice.plant-id = partner.plant-id
    AND choice.misc1 = "0" NO-LOCK NO-ERROR.
IF NOT AVAILABLE(choice) THEN
    FIND FIRST Choice WHERE choice.field-no = 658
    AND choice.plant-id = 0
    AND choice.misc1 = "0" NO-LOCK NO-ERROR.
IF AVAILABLE(choice) THEN ASSIGN
    iEarlyMinutes = int(choice.misc2)
    iLateMinutes = INT(choice.misc3).

ASSIGN
    dtAppointment = dynamic-function ('getDTvalue', s-load.appt-date, s-load.appt-time)
    dtEarliest = dtAppointment - (iEarlyMinutes * 60000)
    dtLatest = dtAppointment + (iLateMinutes * 60000).

if dtEarliest > now then assign
    pcErr = '** Checking in TOO EARLY.|Call Dispatch to reschedule.||Press RESET to start Over.'.

if dtLatest < now then assign
    pcErr = '** Late or No Appointment|Check in at the window.||Press RESET to start Over.'.

if pcErr ne '' then do:
   assign cAlertVar = substitute('Load arrived &1~nLoad: &2~nCust Load: &3~nAppointment time:&4',
                                  string(now, '99/99/99 HH:MM'),
                                  string(s-load.load-id) + '-' + string(s-scslip.slip-id,"99"),
                                  s-scslip.own-load-no,
                                  string(s-load.appt-date) + " " + string(s-load.appt-time)).
   run Alert (input 128,           /* alert id */
          input cAlertVar,     /* alert text */
          input ?,            /* alert records */
          input ?,           /* contact type */
          input 0,             /* contact id */
          input '',    /* group */
          output lResult).     
   return.
end.

for each inv no-lock
    where inv.load-id = s-scslip.load-id,
    first part no-lock where part.part-id = inv.part-id:
    iTotWt = iTotWt + inv.net-wt.
    iCoils = iCoils + 1.
    if not can-do (cWidths, trim(string(part.wdth,'>>9.999'))) then
        cWidths = substitute('&1&2&3',
                             cWidths,
                             if cWidths = '' then '' else ', ',
                             trim(string(part.wdth,'>>9.999'))).
end.
cWidths = (if num-entries(cWidths) > 1 then 'Widths:' else 'Width:') + ' ' + cWidths.
if avail s-load then
    find first scac no-lock where scac.scac-code = s-load.scac-code no-error.
pcRtn = substitute('Msg1=Load &1- Checkin @ &2|Msg2=Carrier: &3|Msg3=&4 Coils, &5 Lbs, &6|Msg4=&7 &8 &9',
                   cLoadID,
                   string(now, '99/99/99 HH:MM'),
                   (if avail scac then scac.name else 'Unknown'),
                   iCoils,
                   iTotWt,
                   cWidths,
                   (if partner.plant-id eq 4 and partner.owner-id = 55 then '' else 'Dest: ' + cust.name),
                   (if partner.plant-id eq 4 and partner.owner-id = 55 then '' else cust.city),
                   (if partner.plant-id eq 4 and partner.owner-id = 55 then '' else cust.state)).
pcRtn = substitute('&1|LoadID=&2|Mode=VerifyInfo',
                   pcRtn,
                   s-load.load-id).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadCheckinOK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadCheckinOK Procedure 
PROCEDURE LoadCheckinOK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  Finish up trucker Check-In     
------------------------------------------------------------------------------*/
find first s-scslip no-lock where s-scslip.load-id = iLoadID no-error.
find first s-load exclusive-lock where s-load.load-id = iLoadID no-error.

if not avail s-scslip or not avail s-load then do:
    pcErr = 'A Problem occurred processing your check-in|||Please press Reset to try again.'.
end.
else do:
    assign
        s-load.arrv-date = today
        s-load.arrv-time = integer(replace(string(time,"HH:MM"),":",""))
        s-load.vehicle-id = cVehicleID
        s-load.vehicle-type = iVehicleType
        pcRtn = 'Msg1=Check-In successful.|Msg2=Monitor your radio for dock assignment|Msg3=|Msg4=Press RESET to Check in Next Load.'.
        pcErr = ''
        .
     run CheckOutboundDynamicMsgs(input iPlantID, input s-load.load-id, output cMessageList).
     if cMessageList ne "" then do:
         assign pcRtn = substitute('&1|Mode=ShowDynamicMessage|DynamicMsg=&2|PrintArrival=no',
                           pcRtn,
                           cMessageList).
     end.
/*     run edi/j0427(s-scslip.load-id, s-scslip.slip-id, "08"). */
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutboundCheckIn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutboundCheckIn Procedure 
PROCEDURE OutboundCheckIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define variable iTotWt as integer no-undo init 0.
define variable cWidths as character no-undo init ''.
define variable iCoils as integer no-undo init 0.

find first s-scslip no-lock where s-scslip.load-id = TruckArrivals.psiloadid no-error.
find first s-load exclusive-lock where s-load.load-id = TruckArrivals.psiloadid no-error.

if not avail s-scslip or not avail s-load then do:
    pcErr = 'A Problem occurred processing your check-in|||Please press Reset to try again.'.
end.
else do:
    find first partner no-lock where partner.partner-id = s-scslip.partner-id no-error.
    if avail partner then find first cust no-lock where cust.cust-id = partner.user-id no-error.
    for each inv no-lock
        where inv.load-id = s-scslip.load-id,
        first part no-lock where part.part-id = inv.part-id:
        iTotWt = iTotWt + inv.net-wt.
        iCoils = iCoils + 1.
        if not can-do (cWidths, trim(string(part.wdth,'>>9.999'))) then
            cWidths = substitute('&1&2&3',
                                 cWidths,
                                 if cWidths = '' then '' else ', ',
                                 trim(string(part.wdth,'>>9.999'))).
    end.
    cWidths = (if num-entries(cWidths) > 1 then 'Widths:' else 'Width:') + ' ' + cWidths.
    if avail s-load then
        find first scac no-lock where scac.scac-code = s-load.scac-code no-error.
    pcRtn = substitute('Msg1=Load &1- Checkin @ &2|Msg2=Carrier: &3|Msg3=&4 Coils, &5 Lbs, &6|Msg4=&7 &8 &9',
                       cLoadID,
                       string(now, '99/99/99 HH:MM'),
                       (if avail scac then scac.name else 'Unknown'),
                       iCoils,
                       iTotWt,
                       cWidths,
                       (if avail cust and not(partner.plant-id eq 4 and partner.owner-id eq 55) then 'Dest: ' + cust.name else ''),
                       (if avail cust and not(partner.plant-id eq 4 and partner.owner-id eq 55) then cust.city else ''),
                       (if avail cust and not(partner.plant-id eq 4 and partner.owner-id eq 55) then cust.state else '')).
    pcRtn = substitute('&1|LoadID=&2|Mode=VerifyInfo|ArrivalId=&3',
                       pcRtn,
                       s-load.load-id,
                       truckarrivals.arrivalid).

end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OutboundCheckInOK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OutboundCheckInOK Procedure 
PROCEDURE OutboundCheckInOK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def buffer bTA for truckArrivals.
/* Finish up arriving the outbound load */
    find first truckarrivals
        where truckarrivals.arrivalID = iArrivalID no-error.
    find first s-load
        where s-load.load-id = truckarrivals.PSILoadID no-error.
    if avail truckarrivals and avail s-load then do:
        if s-load.arrv-date <> ? then
        do:
           assign pcRtn = ''
                  pcErr = 'Msg1=Load Already Arrived|Msg2=Please Re-Try your check in|Msg3=|Msg4=Press RESET to Start Over'.
           return.
        end.
        assign
            TruckArrivals.CheckinDate = now
            truckArrivals.reason = 0
            truckarrivals.dock = s-load.dock
            s-load.arrv-date = today
            s-load.arrv-time = integer(replace(string(time,"HH:MM"),":",""))
            s-load.confirmed = yes
            pcRtn = substitute('Msg1=Check in Successful for PSI Load &1|Msg2=&2|Msg3=&3|Msg4=&4',
                               s-load.load-id,
                               (if s-load.dock ne ? and s-load.dock > 0 then 'See Printout for Dock Assignment' else 'See PSI Dispatcher for Door Assignment'),
                               (if s-load.dock ne ? and s-load.dock > 0 then '' else '     and Check-In paperwork.'),
                               'Press RESET to check in next Load').
            pcErr = ''
            .
            find last bTA no-lock where bTA.plant-id eq truckArrivals.plant-id
                                    and bTA.dock     eq truckArrivals.dock
                                    and bTA.seqNo    ge 10
                                    and bTA.CompleteDate eq ?
                                    and bTA.ArrivalID ne truckArrivals.arrivalID no-error.
            if available bTA then
                assign truckArrivals.seqNo = bTA.seqNo + 10.
            else
                assign truckArrivals.seqNo = 10.
            find current s-load no-lock.
            find current truckarrivals no-lock.

            for each s-scslip exclusive-lock
                where s-scslip.load-id eq s-load.load-id:
                if s-scslip.stat <= 1 then
                    s-scslip.stat = 1.

                /* If MT, do EDI */
                if s-load.plant-id eq 4 then
                    RUN edi/j0427(s-scslip.load-id, s-scslip.slip-id, "08").

            end.
            run CheckOutboundDynamicMsgs(input iPlantID, input s-load.load-id, output cMessageList).
            if s-load.dock > 0 and cMessageList = "" then do:
                run PrintArrivalSheet.
            end.
            else if s-load.dock > 0 and cMessageList ne "" then do:
                pcRtn = substitute('&1|Mode=ShowDynamicMessage|DynamicMsg=&2|PrintArrival=yes|ArrivalID=&3',
                           pcRtn,
                           cMessageList,
                           truckArrivals.ArrivalID).
            end.
            else if (s-load.dock = 0 or s-load.dock = ?) and cMessageList ne "" then do:
                pcRtn = substitute('&1|Mode=ShowDynamicMessage|DynamicMsg=&2|PrintArrival=no|ArrivalID=&3',
                           pcRtn,
                           cMessageList,
                           truckArrivals.ArrivalID).
            end.

            run ship/t0798.p(input 'ArrivePickup',
                     input substitute('Load=&1',s-load.load-id),
                     output cRtn,
                     output cErr).
    end.
    else do:
        pcRtn = 'Msg1=Check in FAILED|Msg2=Please Re-Try your check in.|Msg3=|Msg4=Press RESET to Start Over.'.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-parseOpts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parseOpts Procedure 
PROCEDURE parseOpts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var cVal as char no-undo.
def var cprop as char no-undo.
assign
   lRemoveHold = no
   pcErr = ''
   pcRtn = ''
   iJobID = ?
   cComment = ''
   cLoadID = ''
   iCoilID = ?
   cUserID = ''
   cOrderList = ''.

do iIdx = 1 to num-entries(pcOptionList,"|"):
    assign
       cVal = ?
       cProp = entry(iIdx,pcOptionList,"|").

  if num-entries(cProp,"=") gt 0 then do:
  
     assign cVal  = entry(2,cProp,"=")
            cProp = entry(1,cProp,"=").
     case cProp:
        when 'LoadNo' then assign cLoadID = cVal.
        when 'LoadID' then assign iLoadID = int(cval).
        when 'VehicleID' then assign cVehicleID = cVal.
        when 'Vehicle' then assign cVehicleID = cVal.
        when 'VehicleType' then assign iVehicleType = int(cVal).
        when 'PartnerID' then assign iPartnerid = int(cVal).
        when 'PartID' then assign iPartID = int(cVal).
        when 'Metric' then assign iMetric = int(cVal).
        when 'PlantID' then assign iPlantID = int(cVal).
        when 'Line' then assign iLineID = int(cVal).
        when 'Footage' then assign iFootage = int(cVal).
        when 'ScrapOption' then assign iScrapOption = int(cVal).
        when 'DamageCode' then assign iDamageCode = int(cVal).
        when 'DamageLoc' then assign iDamageLoc = int(cVal).
        when 'MultRows' then assign cMultList = cVal.
        when 'RemoveHold2' then lRemoveHold = logical(cVal).
        when 'MultList' then assign cMultList = cVal.
        when 'Coil' then assign iCoilID = int(cVal).
        when 'Break' then assign iBreak = int(cVal).
        when 'Appointment' then iArrivalID = int(cVal).
        when 'EarlyMinutes' then iEarlyMinutes = int(cVal).
        when 'LateMinutes' then iLateMinutes = int(cVal).
        when 'ScacCode' then v-scac = cVal.
        when 'CoilQty' then v-NoCoils = int(cVal).
        when 'CoilNo' then cCoilNo = cVal.
        when 'Office' then lPrintAtDock = logical(cVal).
        when 'JobID' then iJobID = int(cVal).
        when 'Comment' then cComment = cVal.
        when 'UFDKey' then cUFDKey = decimal(cVal).
        when 'Orders' then cOrderList = cVal.
        when 'FaveList' then cFaveList = cVal.
        when 'SOID' then iSOID = int(cVal).
        when 'TypeID' then iType = int(cVal).
        when 'UserID' then cUserID = cVal.
        when 'inv-ids' then cInvList = cVal.
     end case.

  end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintArrivalSheet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintArrivalSheet Procedure 
PROCEDURE PrintArrivalSheet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cFileName as character no-undo.
define variable cPrinter  as character no-undo.

       cUser = {fnarg getGlobalVar 'userID'}.
       find first psi-user no-lock where psi-user.logon = cUser no-error.
       if avail psi-user then do:
         if psi-user.system ne ? and psi-user.system ne '' then
           assign cPrinter = psi-user.system.
         /* if printing in office, use dock's default printer if available */
         if lPrintAtDock then
         do:
           find first truckArrivals no-lock where truckArrivals.ArrivalID eq iArrivalID no-error.
           find first dock no-lock where dock.dock eq truckArrivals.dock no-error.
           if available dock and dock.defaultPrinter ne '' then
               assign cPrinter = dock.defaultPrinter.
         end.

         run reports/r0618.p (input string(iArrivalID),
                              output cFileName).
         run lib/l1005.p (input 1,
                          input cPrinter,
                          input "",
                          input 1,
                          input cFileName).
       end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ProcessASNCoil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcessASNCoil Procedure 
PROCEDURE ProcessASNCoil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

if v-dock = 99 then assign
   pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                      '** HOT COIL **',
                      'See PSI Dispatch for',
                      'Door Assignment',
                      '   Press RESET to clear.').
if v-dock = 399 then assign  /*BG Message*/
   pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                      'Take Sheet and see',
                      'Material Handler for',
                      'Bay Assigment',
                      '   Press RESET to clear.') 
    lPrintRpt = yes.

if v-dock = 0 then assign /* see dispatch */
   pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                      'See PSI Dispatch for',
                      'Door Assignment',
                      '',
                      '   Press RESET to clear.').

if v-dock = 899 then assign  /*Talladega PSI truck*/
   pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                      'Please take printout',
                      'and see Traffic',
                      '',
                      '   Press RESET to clear.')
    lPrintRpt = yes.
 
if pcRtn > '' then return.

run checkQueueLimit.
assign lPrintRpt = yes.

if iPlantID = 3 then assign  /* take printout w bay */
   pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                      'Please take printout',
                      'With Bay Assignment.',
                      '',
                      '   Press RESET to clear.').  
else
  assign  /* Take printout w door */
   pcRtn = substitute('Msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4',
                      'Please take printout',
                      'With Door  Assignment.',
                      '',
                      '   Press RESET to clear.').      
return.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveCoilHolds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RemoveCoilHolds Procedure 
PROCEDURE RemoveCoilHolds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define variable cMsg as character no-undo.
find first inv no-lock
      where inv.inv-id = iCoilID no-error.
if avail inv
    and can-find(first hold
                 where hold.inv-id = inv.inv-id
                 and hold.hold-code = 2
                 and hold.active) then
    run inv/t0114.p (input 'release',              /* pcTransType */
                     input string(rowid(Inv)),     /* rowid(s) */
                     input 'inv',                  /* table name */
                     input 2,                      /* hold code */
                     input substitute('Coil rewrapped on &1', today), /* note text */
                     input 0,                      /* damage code */
                     input 1,                      /* cond code = prime */
                     input '',                     /* options */
                     output cMsg).                 /* error message */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveMetal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RemoveMetal Procedure 
PROCEDURE RemoveMetal :
/*------------------------------------------------------------------------------
  Purpose:  Remove metal type from subtable sotype of secondaryorder    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first sotype
    where sotype.soid = iSOID
    and sotype.type-id = iType no-error.

if not avail sotype then pcErr = 'Delete Operation Failed, please try again.'.
if avail sotype then delete sotype.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveMultHolds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RemoveMultHolds Procedure 
PROCEDURE RemoveMultHolds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iCoilID as integer no-undo.
define variable iBreak as integer no-undo.
define variable iSlit as integer no-undo.
define variable cMsg as character no-undo.
cMultList = entry(1,cMultList,chr(3)).

iCoilID = integer(entry(1,cMultList,chr(1))).
cMultList = entry(2,cMultList,chr(1)).

do  iIdx = 1 to num-entries(cMultList)
on error undo, leave:
  assign iBreak = integer(entry(1,entry(iIdx,cMultList),'-'))
         iSlit  = integer(entry(2,entry(iIdx,cMultList),'-')).
  for each Mult no-lock
           where mult.coil-id  eq iCoilID
           and   mult.break-no eq iBreak
           and   mult.slit-no  eq iSlit:
      if mult.inv-id = ? or mult.inv-id = 0 then next.
      find first inv no-lock
          where inv.inv-id = mult.inv-id no-error.
      if avail inv
          and can-find(first hold
                       where hold.inv-id = mult.inv-id
                       and hold.hold-code = 2
                       and hold.active) then
          run inv/t0114.p (input 'release',              /* pcTransType */
                           input string(rowid(Inv)),     /* rowid(s) */
                           input 'inv',                  /* table name */
                           input 2,                      /* hold code */
                           input substitute('Coil rewrapped on &1', today), /* note text */
                           input 0,                      /* damage code */
                           input 1,                      /* cond code = prime */
                           input '',                     /* options */
                           output cMsg).                 /* error message */
  end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SaveFave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveFave Procedure 
PROCEDURE SaveFave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
cUser = dynamic-function("getglobalvar", "userid").

find first psi-user where psi-user.logon = cUser no-error.
if avail psi-user then do:
    assign psi-user.FavoriteList = cFaveList.
end.
else do:
    pcErr = substitute( '** failed to update user &1 record in procedure t0578  **', cUser).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetJobComment) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetJobComment Procedure 
PROCEDURE SetJobComment :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first PMJob
    where PMJob.JobID = iJobID no-error.

if avail PMJob then assign
    PMJob.Comment = cComment
    pcRtn = cComment
    pcErr = ''.

if not avail PMJob then assign
    pcRtn = ''
    pcErr = '** Job Comment Update Failed~n Try again.'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetVehicleID) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVehicleID Procedure 
PROCEDURE SetVehicleID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first s-load where s-load.load-id = iLoadID no-error.
if avail s-load then assign s-load.vehicle-id = cVehicleID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UpdateTruckArrivals) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateTruckArrivals Procedure 
PROCEDURE UpdateTruckArrivals :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable v-seq as integer no-undo.
    /* 64148 - if assigned or Novelis,
               but previously found asn by 'fixing' bol# */
    if (iReason = 0 or iReason = 13) and lMultiStripASN then
      assign iReason = 28.
/*     assign v-Seq = GetNextDockSeq(v-dock).  */
    if can-find (first cntl where cntl.plant-id = iPlantID and cntl.ApptReqd) then
      find truckarrivals exclusive-lock 
        where truckArrivals.arrivalID eq iArrivalID no-error.
    else
    /* for non-appt plants, use asn or trucker entry */
    do:
        create truckarrivals no-error.
        assign truckarrivals.ArrivalID = next-value(ArrivalID)
               truckarrivals.scac-code = if available asn 
                                         then asn.scac-code 
                                         else ''
               truckarrivals.plant-id  = iPlantID.
    end.

    if available truckarrivals then do:
      assign iArrivalID = truckarrivals.ArrivalID.
      assign truckarrivals.CheckinDate = now
             truckarrivals.dock        = v-dock
             truckarrivals.reason      = if iReason <> ? then iReason
                                       else if lASNMill then 21 else 1.
      /* 65506 */
      if not lAutoDock then
      do:
        assign truckarrivals.loadid = (if available asn and 
                                       (asn.load-no = 0 or asn.load-no = ?) then asn.bill-o-lad else truckarrivals.loadid) 
               truckarrivals.NoCoils = (if v-NoCoils > 0 then v-NoCoils else truckarrivals.NoCoils) .
      end.

      if truckarrivals.seqno = ?
      or truckarrivals.seqno = 0 then 
        assign v-Seq = GetNextDockSeq(v-dock, truckarrivals.ArrivalID)
               truckarrivals.seqno = v-seq.
      else
        assign v-seq = truckarrivals.seqno.

      if truckArrivals.truckNo = ?
      or truckArrivals.truckNo = "" then
      do:
        if not lAutoDock then
          assign truckarrivals.truckNo = if available asn and asn.vehicle-id <> "0" and asn.vehicle-id <> ""
                                         then asn.vehicle-id else truckarrivals.truckNo.
      end.
      if truckarrivals.NoCoils = 0 then
        assign truckarrivals.NoCoils = 1.
      if available asn then
        assign truckarrivals.mill-id = asn.mill-id.
    
      if TestForWait(v-Seq, v-dock, iArrivalID) /*if noone else in line*/
      and v-dock <> 0
      and v-dock <> 999 then do:  /* must be valid dock */
        assign 
          truckarrivals.BackInDate = add-interval(now, 5, "minutes").
      end.
    
      find current truckArrivals no-lock no-error.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetNextDockSeq) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetNextDockSeq Procedure 
FUNCTION GetNextDockSeq RETURNS INTEGER
  ( piDockNo as int, piArrivalID as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
find last bTA no-lock 
    where bTA.plant-id = iPlantID 
    and bTA.dock = piDockNo
    and bTA.seqNo >= 10
    and bTA.completeDate = ?
    and bTA.arrivalID <> piArrivalID
    use-index PltDockSeqComp no-error.
if avail bTA then return bTA.seqNo + 10.
  else RETURN 10.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StripLoadNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StripLoadNum Procedure 
FUNCTION StripLoadNum RETURNS CHARACTER
    ( pcLoad as char ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

  pcLoad = left-trim(pcLoad, "0").
  pcLoad = replace(pcLoad, "-", "").
  if substring(pcLoad, 1, 1) = "f" then
      pcLoad = substring(pcLoad, 2, length(pcload) - 1).
  if substring(pcLoad, 1, 1) = "F" then
      pcLoad = substring(pcLoad, 2, length(pcload) - 1).
  if substring(pcLoad, 1, 1) = "k" then
      pcLoad = substring(pcLoad, 2, length(pcload) - 1).
  if substring(pcLoad, 1, 1) = "K" then
      pcLoad = substring(pcLoad, 2, length(pcload) - 1).
  if pcLoad = "" then
      return "NULL".
  return pcLoad.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StripLoadNum2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StripLoadNum2 Procedure 
FUNCTION StripLoadNum2 RETURNS CHARACTER
  ( pcLoad as char ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*find '-' and remove '-' and all to right.*/
def var iDash as int init 0.

assign iDash = index(pcLoad,"-").
if iDash <> 0 then
    assign pcLoad = substring(pcLoad,1,(iDash - 1)).

assign pcLoad = left-trim(pcLoad, "0").

if substring(pcLoad, 1, 1) = "f" then
    pcLoad = substring(pcLoad, 2, length(pcload) - 1).
if substring(pcLoad, 1, 1) = "F" then
    pcLoad = substring(pcLoad, 2, length(pcload) - 1).
if substring(pcLoad, 1, 1) = "k" then
    pcLoad = substring(pcLoad, 2, length(pcload) - 1).
if substring(pcLoad, 1, 1) = "K" then
    pcLoad = substring(pcLoad, 2, length(pcload) - 1).
if pcLoad = "" then
    return "NULL".
return pcLoad.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TestForWait) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TestForWait Procedure 
FUNCTION TestForWait RETURNS logical
  ( piSeq as int, piDoor as int, piArrivalID as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var iCount as int.

if piDoor = 700 or piDoor = 701 then do:
    iCount = TruckCount(701, piArrivalID).
    iCount = iCount + TruckCount(700, piArrivalID).
    if iCount = 0 then return true.
end.
else
do:
    iCount = truckCount(piDoor, piArrivalID).
    if iCount = 0 then return true.
end.

return false.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TruckCount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TruckCount Procedure 
FUNCTION TruckCount RETURNS INTEGER
  ( piDock as integer, piArrivalID as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define variable iCnt as integer no-undo init 0.

for each bTA no-lock where bTA.plant-id = iPlantID
    and bTA.Dock = piDock
    and bTa.completedate = ?
    and bTA.arrivalID <> piArrivalID:
    iCnt = iCnt + 1.
end.

  RETURN icnt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

