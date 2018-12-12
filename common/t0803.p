&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0803.p
    Purpose     : Server Side Utilities for Drivers Checking Themselves
                  Out

    Syntax      :

    Description :

    Author(s)   : Curtis Schroer
    Created     : 07/27/17
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input  parameter pcAction     as character  no-undo.
define input  parameter pcOptionList as character  no-undo.
define output parameter pcRtn as char no-undo.
define output parameter pcErr as char no-undo.

{ship/ttovshp.i}

define temp-table ttCityList no-undo
    field cust-id as integer
    field city as character
    field state as character.

define temp-table ttSelections no-undo
    field answer as character
    field correctAnswer as logical
    field order as integer.

define buffer bttSelections for ttSelections.

define variable iLoadID as int no-undo.
define variable iKronosID as int no-undo.
define variable cVehicleID as char no-undo.
define variable gcDocNameList     as character  no-undo.

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

&IF DEFINED(EXCLUDE-BuildLoad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildLoad Procedure 
PROCEDURE BuildLoad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lSuccess   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iErrLvl    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cWarnMsg   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErrMsg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRtnMsg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOvrrdList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lLeave     AS LOGICAL     NO-UNDO.
define variable dtDepartDT   as datetime   no-undo.
define variable dtDepartDate as date       no-undo.
define variable iDepartTime  as integer    no-undo.
define variable iLateReason  as integer    no-undo.
define variable lWasLate     as logical    no-undo.

find first s-load no-lock
    where s-load.load-id = iLoadID no-error.
if not avail s-load then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   'No load record available',
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
find first s-scslip no-lock
    where s-scslip.load-id = s-load.load-id no-error.
if not avail s-scslip then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load has already been released",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-load.scac-code ne "PRSP" then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "You cannot release a",
                   'non Precision Strip Load',
                   '',
                   '   Press RESET to clear.').
    return.
end.

find first drivers no-lock
    where drivers.division = 0
    and   drivers.APAccount = iKronosID no-error.
if avail drivers then do:
    find current s-load exclusive-lock.
    assign s-load.DriverID = drivers.DriverID
           s-load.driver = drivers.lastname + ", " + drivers.firstname.
    find current s-load no-lock.
end.
if s-load.DriverID eq 0 or s-load.DriverID eq ?  then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "No Driver is selected for this load.",
                   'You cannot check out at this time.',
                   '',
                   '   Press RESET to clear.').
    return.
end.

if s-load.vehicle-id ne cVehicleID then do:
    find current s-load exclusive-lock.
        assign s-load.vehicle-id = cVehicleID.
    find current s-load no-lock.
end.

if s-load.vehicle-id eq "" or s-load.vehicle-id eq ?  then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "No Vehicle is selected for this load.",
                   'You cannot check out at this time.',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-load.confirm-by eq "" then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load Must Be Verified.",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.

assign lLeave = no.
for each s-slip no-lock
        where s-slip.load-id = s-load.load-id,
    first partner no-lock
        where partner.partner-id = s-slip.partner-id:
    if partner.slip-cfg[14] = 4 then do:
        assign lLeave = yes.
    end.
end.
if lLeave then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Slip Needs Certs",
                   'Please Go To Office',
                   '',
                   '   Press RESET to clear.').
    return.
end.

if s-load.PermitNo = "" then do:
      run ship/t0618.p
          (input 'CheckForPermit',
           input substitute('LoadID=&1', string(s-load.load-id)),
           output cRtnMsg,
           output cErrMsg).
      if cErrMsg > "" then do:
          assign pcErr = substitute('&1|&2|&3|&4',
                   "Load is Missing Permit.",
                   "Go to Office.",
                   '',
                   '   Press RESET to clear.').
          return.
      end. /*if cErrMsg > ""*/
end. /*if no permitNo*/

assign dtDepartDT = now.
run parseDateTime(input dtDepartDT, output dtDepartDate, output iDepartTime).
run ship/t0618.p
        (input 'WasLoadLate',
         input substitute('LoadID=&1|LoadDate=&2|LoadTime=&3',
                           string(s-load.load-id),
                           string(dtDepartDate),
                           string(iDeparttime)),
         output cRtnMsg,
         output cErrMsg).

    assign
        iLateReason = ?
        lWasLate = logical(cRtnMsg).

if lWasLate then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load is Late. Go to Office.",
                   "",
                   '',
                   '   Press RESET to clear.').
    return.
end.

/* check if we allow more than 1 owner order on slip, if not = error */
run ship/t0530.p  (input s-load.load-id, 
                   output lSuccess,
                   output cMessage).
if not lSuccess then
do:
  assign pcErr = substitute('&1|&2|&3|&4',
                 cMessage,
                 '',
                 '',
                 '   Press RESET to clear.').
  return.
end.
if cMessage > '' then do:
  assign pcErr = substitute('&1|&2|&3|&4',
         cMessage,
         '',
         '',
         '   Press RESET to clear.').
end.


run ship/t0038.p  /* Does the checking */
      (input s-load.load-id,
       input 'PRINT',
       output iErrLvl,       /* 0 = ok, 1 = over-ridable, 2 = fatal */
       output cWarnmsg,
       output cErrMsg,
       input-output cOvrrdList).


  if iErrLvl > 0 then do:
      assign pcErr = substitute('&1|&2|&3|&4',
                     cErrMsg,
                     '',
                     '',
                     '   Press RESET to clear.').
      return.
  end.

if iErrLvl = 0 then do:
      assign cOvrrdList = 'OK'.             /* skip validations in t0339 */
      run ship/t0339.p /* Build PS & BOL */
          (input s-load.load-id,
           input 'Mode=Build',
           output gcDocNameList,
           output lSuccess,
           output cRtnMsg,
           input-output cOvrrdList).
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CompleteRequest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompleteRequest Procedure 
PROCEDURE CompleteRequest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iNoteID AS INTEGER     NO-UNDO.
if can-find(first ttOvership) then
do:
  for each ttOvership:
      case ttOvership.type:
          when "ConfirmClose" then
          do:
              run ship/t0010.p(
                 ttOvership.schedID,
                 iNoteID,
                 {fnarg getGlobalVar 'userID'}).
          end.
      end case.
      if not ttOvership.applyOver then
          delete ttOvership.
  end.   /* for each ttOvership */

  empty temp-table ttOverShip.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindLoad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindLoad Procedure 
PROCEDURE FindLoad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lSuccess   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cMessage   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErrMsg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRtnMsg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lLeave     AS LOGICAL     NO-UNDO.
define variable dtDepartDT   as datetime   no-undo.
define variable dtDepartDate as date       no-undo.
define variable iDepartTime  as integer    no-undo.
define variable iLateReason  as integer    no-undo.
define variable lWasLate     as logical    no-undo.
define variable iScanLen     as integer    no-undo.

assign iScanLen = length(iLoadID).
/* Means they scanned an inv tag, so we need to find load */
if iScanLen = 9 or iScanLen = 10 then do:
    find first inv no-lock
        where inv.inv-id = iLoadID no-error.
    if not avail inv then do:
        assign pcErr = substitute('&1|&2|&3|&4',
                   'No Load Record Available',
                   'For Inv Tag Scanned',
                   '',
                   '   Press RESET to clear.').
        return.
    end.
    if inv.load-id > 0 then do:
        find first s-load no-lock
            where s-load.load-id = inv.load-id.
        if not avail s-load then do:
            assign pcErr = substitute('&1|&2|&3|&4',
                           'No load record available',
                           '',
                           '',
                           '   Press RESET to clear.').
            return.
        end.
    end.
    else do:
        assign pcErr = substitute('&1|&2|&3|&4',
                   'No Load Assigned',
                   'For Inv Tag Scanned',
                   '',
                   '   Press RESET to clear.').
        return.
    end.
end.
else do:
    find first s-load no-lock
        where s-load.load-id = iLoadID no-error.
    if not avail s-load then do:
        assign pcErr = substitute('&1|&2|&3|&4',
                       'No load record available',
                       '',
                       '',
                       '   Press RESET to clear.').
        return.
    end.
end.

assign pcRtn = string(s-load.load-id).

find first s-scslip no-lock
    where s-scslip.load-id = s-load.load-id no-error.
if not avail s-scslip then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load has already been released",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-load.scac-code ne "PRSP" then do:
    if s-load.scac-code = "" then do:
        find current s-load exclusive-lock.
        assign s-load.scac-code = "PRSP".
        find current s-load no-lock.
    end.
    else do:
        assign pcErr = substitute('&1|&2|&3|&4',
                       "You cannot release a",
                       'non Precision Strip Load',
                       '',
                       '   Press RESET to clear.').
        return.
    end.
end.
if s-load.confirm-by eq "" then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load Must Be Verified.",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.

assign lLeave = no.
for each s-slip no-lock
        where s-slip.load-id = s-load.load-id,
    first partner no-lock
        where partner.partner-id = s-slip.partner-id:
    if partner.slip-cfg[14] = 4 then do:
        assign lLeave = yes.
    end.
end.
if lLeave then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Slip Needs Certs",
                   'Please Go To Office',
                   '',
                   '   Press RESET to clear.').
    return.
end.

if s-load.PermitNo = "" then do:
      run ship/t0618.p
          (input 'CheckForPermit',
           input substitute('LoadID=&1', string(s-load.load-id)),
           output cRtnMsg,
           output cErrMsg).
      if cErrMsg > "" then do:
          assign pcRtn = pcRtn + "|AskForPermit".
/*           assign pcErr = substitute('&1|&2|&3|&4', */
/*                    "Load is Missing Permit.",      */
/*                    "Go to Office.",                */
/*                    '',                             */
/*                    '   Press RESET to clear.').    */
/*           return.                                  */
      end. /*if cErrMsg > ""*/
      else do:
          assign pcRtn = pcRtn + "|PermitOK".
      end.
end. /*if no permitNo*/
else do:
    assign pcRtn = pcRtn + "|PermitOK".
end.

assign dtDepartDT = now.
run parseDateTime(input dtDepartDT, output dtDepartDate, output iDepartTime).
run ship/t0618.p
        (input 'WasLoadLate',
         input substitute('LoadID=&1|LoadDate=&2|LoadTime=&3',
                           string(s-load.load-id),
                           string(dtDepartDate),
                           string(iDeparttime)),
         output cRtnMsg,
         output cErrMsg).

    assign
        iLateReason = ?
        lWasLate = logical(cRtnMsg).

if lWasLate then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load is Late. Go to Office.",
                   "",
                   '',
                   '   Press RESET to clear.').
    return.
end.

/* check if we allow more than 1 owner order on slip, if not = error */
run ship/t0530.p  (input s-load.load-id, 
                   output lSuccess,
                   output cMessage).
if not lSuccess then
do:
  assign pcErr = substitute('&1|&2|&3|&4',
                 cMessage,
                 '',
                 '',
                 '   Press RESET to clear.').
  return.
end.
if cMessage > '' then do:
  assign pcErr = substitute('&1|&2|&3|&4',
         cMessage,
         '',
         '',
         '   Press RESET to clear.').
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetCityList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCityList Procedure 
PROCEDURE GetCityList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iNumCities AS INTEGER     NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLastCustID AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRandomNumber AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCorrectAnswer AS INTEGER     NO-UNDO.
define variable cMsg as character no-undo extent 4.

find first s-load no-lock
    where s-load.load-id = iLoadID no-error.
if not avail s-load then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   'No s-load record available',
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.

assign iNumCities = 0.
for each s-slip no-lock
        where s-slip.load-id = s-load.load-id,
    first partner no-lock
        where partner.partner-id = s-slip.partner-id,
    first cust no-lock
        where cust.cust-id = partner.user-id
        break by cust.cust-id:

    if first-of(cust.cust-id) then do:
        assign iNumCities = iNumCities + 1.

        find first ttSelections no-lock
            where ttSelections.correctAnswer no-error.
        if avail ttSelections then do:
            assign ttSelections.answer = ttSelections.answer + " - " + cust.city + ", " + cust.state.
        end.
        else do:
            create ttSelections.
            assign ttSelections.answer = cust.city + ", " + cust.state
                   ttSelections.correctAnswer = yes.
        end.
    end.
end.

find last cust no-lock no-error.
if avail cust then do:
    assign iLastCustID = cust.cust-id.
end.

do i = 1 to iNumCities * 3:
    repeat:
        assign iRandomNumber = random(1,iLastCustID).
    
        find first cust no-lock
            where cust.cust-id = iRandomNumber no-error.
        if avail cust and cust.city ne "" and cust.state ne "" then do:
            create ttCityList.
            assign ttCityList.cust-id = cust.cust-id
                   ttCityList.city = cust.city
                   ttCityList.state = cust.state.
            leave.
        end.
        else do:
            next.
        end.
    end.
end.

do i = 1 to 3:
    do iCount = 1 to iNumCities:
        find first ttCityList no-error.
        if iCount = 1 then do:
            create ttSelections.
            assign ttSelections.answer = ttCityList.city + ", " + ttCityList.state
                   ttSelections.correctAnswer = no.
        end.
        else do:
            assign ttSelections.answer = ttSelections.answer + " - " + ttCityList.city + ", " + ttCityList.state
                   ttSelections.correctAnswer = no.
        end.
        delete ttCityList.
    end.
    assign iCount = 0.
end.

for each ttSelections:
    repeat:
        assign iRandomNumber = random(1,4).
        find first bttSelections no-lock
            where bttSelections.order = iRandomNumber no-error.
        if avail bttSelections then do:
            next.
        end.
        else do:
            assign ttSelections.order = iRandomNumber.
            leave.
        end.
    end.
end.

find first ttSelections no-lock
    where ttSelections.correctanswer no-error.
if avail ttSelections then do:
    assign iCorrectAnswer = ttSelections.order.
end.

for each ttSelections no-lock
    by ttSelections.order:
    assign cMsg[ttSelections.order] = ttSelections.answer.
end.
assign pcRtn = substitute('msg1=&1|Msg2=&2|Msg3=&3|Msg4=&4|CorrectAnswer=&5',
                           cMsg[1],
                           cMsg[2],
                           cMsg[3],
                           cMsg[4],
                           string(iCorrectAnswer)).


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
def var iIdx as int no-undo.


do iIdx = 1 to num-entries(pcOptionList,"|"):
    assign
       cVal = ?
       cProp = entry(iIdx,pcOptionList,"|").

  if num-entries(cProp,"=") gt 0 then do:
  
     assign cVal  = entry(2,cProp,"=")
            cProp = entry(1,cProp,"=").
     case cProp:
        when 'LoadID' then assign iLoadID = int(cVal).
        when 'KronosID' then assign iKronosID = int(cVal).
        when 'Vehicle' then assign cVehicleID = cVal.
     end case.

  end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PermitOK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PermitOK Procedure 
PROCEDURE PermitOK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

find first s-load
    where s-load.load-id = iLoadID.
if not avail s-load then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   'No load record available',
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
else do:
    if s-load.permitno = "" then do:
        assign s-load.permitno = "Driver Verified".
    end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintDocs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintDocs Procedure 
PROCEDURE PrintDocs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cLaser as character no-undo.
define variable cWarnMsg   as character no-undo.
define variable cErrMsg    as character no-undo.
define variable iErrLvl    as integer no-undo.
define variable cOvrrdList   as character  no-undo.
define variable cOvrRideCheck as character no-undo.
define variable cHasSig      as character no-undo.
define variable lSuccess     as logical    no-undo.
define variable cRtnMsg      as character  no-undo.
define variable cComments    as character  no-undo.
define variable cDocTypeList as character  no-undo.
define variable iSlipIdx     as integer    no-undo.
define variable cOtherDocList as character  no-undo.
define variable cSlipDocList as character  no-undo.
define variable cCertDocList as character  no-undo.
define variable cRtnFormDoc as character no-undo.
define variable iIdx         as integer    no-undo.
DEFINE VARIABLE cScratch AS CHARACTER   NO-UNDO.

find first s-load no-lock
    where s-load.load-id = iLoadID no-error.
if not avail s-load then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   'No load record available',
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
find first s-scslip no-lock
    where s-scslip.load-id = s-load.load-id no-error.
if not avail s-scslip then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load has already been released",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-scslip.stat lt 4 then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Document's Not Built Yet.",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-load.scac-code ne "PRSP" then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "You cannot release a",
                   'non Precision Strip Load',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-load.confirm-by eq "" then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load Must Be Verified.",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.

/*  assign cLaser = {fnarg getUserInfo 'Laser':u}. */
    assign cScratch = 'packslip'.
    run lib/t0201.p ({fnarg getGlobalVar 'userID'},
                     input-output cScratch,
                     output cLaser).
  /* perform validation checks before proceeding */

  run ship/t0038.p  /* Does the checking */
      (input s-load.load-id,
       input 'PRINT',
       output iErrLvl,       /* 0 = ok, 1 = over-ridable, 2 = fatal */
       output cWarnmsg,
       output cErrMsg,
       input-output cOvrrdList).


  if iErrLvl > 0 then do:
      assign pcErr = substitute('&1|&2|&3|&4',
                   cErrMsg,
                   '',
                   '',
                   '   Press RESET to clear.').
      return.
  end.


  if iErrLvl = 0 then do:
     cOvrRideCheck = cOvrrdList.
    /*  Is this load already signed for? */
      run ship/t0286.p
          (input substitute('HasSignature,&1',s-load.load-id),
           output cHasSig).
    
      if cHasSig ne 'Yes' then do:
          /* Get the Signature here and Build the Documents server-side */
/*           run ship/d0144.w(input ?,              */
/*                            input s-load.load-id, */
/*                            input ?).             */
          assign pcErr = substitute('&1|&2|&3|&4',
                   'No signature on record.',
                   'Please Go To Office',
                   '',
                   '   Press RESET to clear.').
      return.
      end.
      assign cOvrrdList = 'OK'.    /* prevent re-checking in t0339 */
      run ship/t0339.p /* Build PS & BOL */
          (input s-load.load-id,
           input 'Mode=Print',
           output gcDocNameList,
           output lSuccess,
           output cRtnMsg,
           input-output cOvrrdList).
      if cRtnMsg ne ? and cRtnMsg gt '' and not lSuccess then do:
          assign pcErr = substitute('&1|&2|&3|&4',
                   cRtnMsg,
                   '',
                   '',
                   '   Press RESET to clear.').
          return.
      end.

        
          /* (Re)Build and Print BOL Addendum, CERT, Export papers as needed */
          cOvrrdList = cOvrRideCheck.
          run ship/t0039.p 
              (input s-load.load-id,
               input ?,
               input "BOLAdd,Cert,Export,PkgRtn":U,
               input cComments,
               input "Print",   /* Flag to print docs automagically */
               output gcDocNameList,
               output lSuccess,
               output cRtnMsg,
               input-output cOvrrdList).


          if num-entries(gcDocNameList,chr(2)) gt 1 then
          do:
            assign cDocTypeList  = entry(2,gcDocNameList,chr(2))
                   gcDocNameList = entry(1,gcDocNameList,chr(2)).
          end.
    
      assign gcDocNameList = trim(gcDocNameList," |")
             iSlipIdx      = 1.
    
    
      do iSlipIdx = 1 to num-entries(gcDocNameList, chr(1)):
          cOtherDocList = ''.
          assign cSlipDocList = trim(entry(iSlipIdx,gcDocNameList,chr(1))," |").
    
          do iIdx = 1 to num-entries(cSlipDocList,"|"):
             if entry(iIdx,entry(iSlipIdx,cDocTypeList,chr(1)),"|") eq "CERT" then
                 assign cCertDocList = cCertDocList
                                      + "|"
                                      + entry(iIdx,cSlipDocList,"|").
             else
             if entry(iIdx,entry(iSlipIdx,cDocTypeList,chr(1)),"|") eq "PkgRtn" then
                 assign cRtnFormDoc = cRtnFormDoc
                                      + "|"
                                      + entry(iIdx,cSlipDocList,"|").
             else
                assign cOtherDocList = cOtherDocList
                                     + "|"
                                     + entry(iIdx,cSlipDocList,"|").
          end.
    
          assign cCertDocList = trim(cCertDocList," |")
                 cOtherDocList = trim(cOtherDocList," |")
                 cRtnFormDoc = trim(cRtnFormDoc," |").
          if cOtherDocList <> "" then
              run lib/l1005.p 
                (input 1,
                 input cLaser,
                 input '',
                 input 1,
                 cOtherDocList).
      end.

      if cRtnFormDoc <> "" then
          run lib/l1005.p
            (input 1,
             input cLaser,
             input '',
             input 1,
             cRtnFormDoc).

      if cCertDocList <> "" then
          run lib/l1005.p
            (input 1,
             input cLaser,
             input '',
             input 1,
             cCertDocList).
    
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReleaseLoad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReleaseLoad Procedure 
PROCEDURE ReleaseLoad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cRtn         as character  no-undo.
define variable dtDepartDT   as datetime   no-undo.
define variable dtDepartDate as date       no-undo.
define variable iDepartTime  as integer    no-undo.
define variable cRtnMsg      as character  no-undo.
define variable cErrMsg      as character  no-undo.
define variable iLateReason  as integer    no-undo.
define variable lWasLate     as logical    no-undo.
define variable lAddNote     as logical    no-undo.
define variable iOrigNoteID  as integer    no-undo.
define variable iType        as integer    no-undo.
define variable lOK          as logical    no-undo.
define variable lEquip       as logical    no-undo.
define variable iSlipID      as integer    no-undo.

find first s-load no-lock
    where s-load.load-id = iLoadID no-error.
if not avail s-load then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   'No load record available',
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
find first s-scslip no-lock
    where s-scslip.load-id = s-load.load-id no-error.
if not avail s-scslip then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load has already been released",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
else do:
    assign iSlipID = s-scslip.slip-id.
end.
if s-scslip.stat lt 5 then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Document's Not Built Yet.",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-load.scac-code ne "PRSP" then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "You cannot release a",
                   ' non Precision Strip Load',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-load.confirm-by eq "" then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load Must Be Verified.",
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.
if s-scslip.BorrowedEquip then do:
    assign lEquip = yes.
end.

run inv/t0427.p
    (input "checkLoad,":U + string(s-load.load-id),
     output cRtn).
if cRtn gt "":U then do:
    if cRtn begins "**":U then do:
      assign pcErr = substitute('&1|&2|&3|&4',
                   replace(cRtn,"**":U,"":U),
                   '',
                   '',
                   '   Press RESET to clear.').
      return.
    end.
    if int(entry(2,cRtn,"|":U)) = 1 then do:
      assign pcErr = substitute('&1|&2|&3|&4',
                   "Cannot release load until all",
                   "master coils have been released!",
                   '',
                   '   Press RESET to clear.').
      return.
    end.
end.

if s-scslip.own-load-no eq "*CANCELLED*" then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "This load was cancelled by AK.",
                   "",
                   '',
                   '   Press RESET to clear.').
    return.
end.

assign dtDepartDT = now.
run parseDateTime(input dtDepartDT, output dtDepartDate, output iDepartTime).

run ship/t0618.p
        (input 'WasLoadLate',
         input substitute('LoadID=&1|LoadDate=&2|LoadTime=&3',
                           string(s-load.load-id),
                           string(dtDepartDate),
                           string(iDeparttime)),
         output cRtnMsg,
         output cErrMsg).

    assign
        iLateReason = ?
        lWasLate = logical(cRtnMsg).

if lWasLate then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Load is Late. Go to Office.",
                   "",
                   '',
                   '   Press RESET to clear.').
    return.
end.

run ship/t0041.p
      (s-load.load-id,
       dtDepartDate,
       iDepartTime,
       s-scslip.slip-id,
       lAddNote,
       iOrigNoteID,
       lWasLate,
       iLateReason,
       iType,
       output lOK,
       output cRtnMsg,
       output table ttOvership).

if cRtnMsg gt '' and not lOK then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   cRtnMsg,
                   "",
                   '',
                   '   Press RESET to clear.').
    return.
end.

if lEquip then do:
    run ship/t0692.p on {fn getAppServer} (input "Release",
                                             input s-load.load-id,
                                             input iSlipID,
                                             input-output cErrMsg).
end.

run CompleteRequest.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VerifyDriver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VerifyDriver Procedure 
PROCEDURE VerifyDriver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first drivers no-lock
    where drivers.division = 0
    and   drivers.APAccount = iKronosID no-error.

if not avail drivers then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   "Driver Not Found",
                   'With This Clock Number',
                   '',
                   '   Press RESET to clear.').
    return.
end.
else do:
    if (Drivers.CDLExpDate < today or Drivers.CDLExpDate = ?) and Drivers.DriverID ne 999 then do:
        assign pcErr = substitute('&1|&2|&3|&4',
                   "Driver CDL Has",
                   'Expired',
                   '',
                   '   Press RESET to clear.').
        return.
    end.
    if (Drivers.MedExpDate < today or Drivers.MedExpDate = ?) and Drivers.DriverID ne 999 then do:
        assign pcErr = substitute('&1|&2|&3|&4',
                   "Driver Medical Expiration",
                   'Date Has Passed.',
                   '',
                   '   Press RESET to clear.').
        return.
    end.
    assign pcRtn = drivers.firstname + " " + drivers.lastname.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VerifyVehicle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VerifyVehicle Procedure 
PROCEDURE VerifyVehicle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first s-load no-lock
    where s-load.load-id = iLoadID no-error.
if not avail s-load then do:
    assign pcErr = substitute('&1|&2|&3|&4',
                   'No load record available',
                   '',
                   '',
                   '   Press RESET to clear.').
    return.
end.


if s-load.vehicle-id eq "" or s-load.vehicle-id eq ?  then do:
    assign pcRtn = "".
end.
else do:
    assign pcRtn = string(s-load.vehicle-id).
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

