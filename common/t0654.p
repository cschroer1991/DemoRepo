&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : commmon/t0654.p
    Purpose     : Server-side mesaging tasks
    
  Date     By   Issue Remark
  ======== ==== ===== ================================================
  02/18/11  djt 53412 New
  03/26/11  djt 56169 certain workorder messages were not plant specific
  11/19/12  djt 62224 schema change - make part-id 7 characters
  11/21/14 zakh 67525 make set watch specific to plant.
  12/06/16 zakh 70511 Added logic for Line ID.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter pcAction as char no-undo.
define input  parameter pcOptionList as char no-undo.
define output parameter pcMessages as char no-undo.
define output parameter pcErr as char no-undo.


define variable iOrder as int no-undo.
define variable iSetup as int no-undo.
define variable iOwner as int no-undo.
define variable iUser as int no-undo.
define variable cPartners as char no-undo.
define variable cParts as char no-undo.
define variable cScratch as char no-undo.
define variable cTitle as char no-undo.
define variable cText as char no-undo.
define variable iPlantID as int no-undo.
define variable iPartID as int no-undo.
define variable iLineID as int no-undo.

define variable cType as character no-undo.
define variable cContact as character no-undo.
define variable cCallback as character no-undo.
define variable lAfterHours as logical no-undo.
define variable cDescr as character no-undo.
define variable cMsg as character no-undo.
define variable lResult as logical no-undo.

define buffer bOwner for Cust.
define stream rpt.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-GetMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetMessage Procedure 
FUNCTION GetMessage RETURNS CHARACTER
  (input piMessageID as int)  FORWARD.

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

&IF DEFINED(EXCLUDE-CloseIssue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CloseIssue Procedure 
PROCEDURE CloseIssue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
find first alert exclusive-lock where alert.alertid = 2 no-error.
if avail alert then assign
    alert.alerttext = ''
    alert.comment = ''.

find first alert exclusive-lock where alert.alertid = 3 no-error.
if avail alert then assign
    alert.alerttext = ''
    alert.comment = ''.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetShiftMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetShiftMsg Procedure 
PROCEDURE GetShiftMsg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var cMessageList as char extent 10 no-undo.
define var iLoop as int no-undo.
define var iMess as int no-undo.
assign pcMessages = ''.

for each messages no-lock
    where messages.plant-id = iPlantID
    and (messages.line-id = iLineID
            or messages.line-id = 0
            or messages.line-id = ?)
    and messages.TriggerAction = 1:
    if index(cMessageList[1], string(messages.messageID,'999999')) = 0
       then assign cMessageList[1] =  cMessageList[1] + string(messages.messageID,'999999') + ','.
end.

if cMessageList[1] ne '' then do:
    do iLoop = 1 to num-entries(cMessageList[1]):
        iMess = int(entry(iLoop,cMessagelist[1])).
        if iMess ne ? and iMess gt 0 then assign pcMessages = pcMessages + GetMessage(iMess).
    end.
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
def var iIdx as int no-undo.
def var cVal as char no-undo.
def var cprop as char no-undo.

do iIdx = 1 to num-entries(pcOptionList,"|"):
    assign
       cVal = ?
       cProp = entry(iIdx,pcOptionList,"|").

  if num-entries(cProp,"=") gt 0 then do:
  
     assign cVal  = entry(2,cProp,"=")
            cProp = entry(1,cProp,"=").
     case cProp:
        when 'Order'   then assign iOrder = int(cVal).
        when 'Setup'   then assign iSetup = int(cVal).
        when 'Plant'   then assign iPlantID = int(cVal).
        when 'Title'   then assign cTitle = cVal.
        when 'Body'    then assign cText = cVal.
        when 'Part'    then assign iPartID = int(cVal).
        when 'Type'    then assign cType = cVal.
        when 'Line'    then assign iLineID = int(cVal).
        when 'By'      then assign cContact = cVal.
        when 'Callback' then assign cCallBack = cVal.
        when 'Afterhours' then assign lAfterHours = logical(cVal).
        when 'Descr'   then assign cDescr = cVal.
     end case.

  end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintThis) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintThis Procedure 
PROCEDURE PrintThis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable cReportFile as char no-undo.
define variable cUser as char no-undo.
define variable iIndex          as integer   no-undo.
define variable cWorkString     as character no-undo.

cUser = dynamic-function('GetGlobalVar', 'UserID').
find first psi-user no-lock where logon = cUser.
if psi-user.laser = ? or psi-user.laser = '' then do:
    assign pcErr = '** Laser printer not defined on your Genesis Account.'.
end.
else do:

   run lib/getosfilename.p (output cReportFile).
   output stream rpt to value (cReportFile).

   put stream rpt cTitle format "x(80)" skip.
   put stream rpt  '-----------------------------------------------------------------------'.
   assign
       cWorkString = cText.
   repeat while trim(cWorkString) ne '':
       assign
           iIndex      = index(cWorkString,chr(10))
           iIndex      = (if iIndex eq 0 then 80 else iIndex).
       if iIndex > 80 then iIndex = 80.
       display stream rpt
          trim(replace(substr(cWorkString,1,iIndex),chr(10),'')) format "X(80)" skip
                   with stream-io no-box no-label width 132.    
       assign
           cWorkString = substr(cWorkString,iIndex + 1) .
   end.
   output stream rpt close.

   run lib/l1005.p (input 1,
                    input psi-user.laser,
                    input "",
                    input 1,
                    cReportFile).

   assign pcErr = 'Report sent to Printer ' + psi-user.laser.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReportIssue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReportIssue Procedure 
PROCEDURE ReportIssue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iAlert as integer no-undo.
define variable iTime as integer no-undo.
define variable dDate as date no-undo.
define variable iTimeShift as integer no-undo.
define variable dtTime as datetime no-undo.

cMsg = substitute('&1&2Callback: &3-&4',
                  cDescr,
                  '~n',
                  cCallBack,
                  cContact).
/* if not from live, note that in the alert */
find first appattr no-lock where appattr.attrkey = 'environment' no-error.
if avail appattr and appattr.appstr ne 'PSI' then assign
    cMsg = substitute('&1~nTHIS IS FROM &2',cMsg,CAPS((if appattr.appstr = 'genesis' then 'test' else appattr.appstr))).

if lAfterhours = yes then do:
   /* if between 7 am and 4:30 pm (in Minster), just submit to IT help */
    iTime = mtime.
    ddate = today.
    iTimeShift = 0 .
    
    assign iPlantID = integer({fnarg getGlobalVar 'plantID'}).
    find first cntl where cntl.plant-id = iPlantID no-lock no-error.
    if avail cntl and cntl.timezone begins 'C' then iTimeShift = 60.
    if avail cntl and cntl.timezone begins 'M' then iTimeShift = 120.
    if avail cntl and cntl.timezone begins 'P' then iTimeShift = 180.
    iTime = iTime + (iTimeShift * 60000).
    dtTime = datetime(dDate, iTime).
    run parseDateTime(input dtTime, output dDate, output iTime).

    if (weekday(dDate) > 1 and weekday(dDate) < 7)
        and (iTime > 659 and iTime < 1630) then do:
        pcMessages = 'Issue has been submitted to the Helpdesk'.
    end.
    else do:
        /* Set up the alert and send a copy to IT Help mailbox */
        iAlert = (if cType = '1' then 2 else 3).
        find first alert exclusive-lock where alert.alertid = iAlert no-error.
        if avail alert then assign
            alert.AlertText = cMsg
            alert.comments = substitute('&1|1|0',string(now, '99/99/99 HH:MM:SS')).
        /* Send a copy of this alert to IT Help so it isn't lost */
        cMsg = cMsg + '~nPlease remember to cancel this alert (from the support list screen) to keep it from escalating.'.
        pcMessages = 'Issue will be sent to support contacts.~n~nIf you do not hear from someone within 30 minutes~nOr this is urgent, Please call the after hours phone number'.
    end.

    run alert(input 53,
              input cMsg,
              input '',
              input 'G',
              input 31,
              input '',
              output lResult).
end.
else do:
    /* Just send a message to IT Help and we're done */
    run alert(input 53,
              input cMsg,
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

&IF DEFINED(EXCLUDE-SetWatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetWatch Procedure 
PROCEDURE SetWatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign pcMessages = 'No'.
if can-find(Messages
            where messages.part-id = iPartID
            and messages.WatchReqd
            and (messages.ExpireDate = ? or messages.expireDate > now))
            then do:
    assign pcMessages = 'Yes'.
end.
else do:
   find first part no-lock
       where part.part-id = iPartID no-error.
   find first partner no-lock
       where partner.partner-id = part.partner-id no-error.
   if avail partner
       and can-find(Messages
            where messages.partner-id = partner.partner-id
            and messages.WatchReqd
            and (messages.ExpireDate = ? or messages.expireDate > now))
            then assign pcMessages = 'Yes'.
   if pcMessages = 'No' then do:
       if avail partner
           and can-find(Messages
            where messages.owner-id = partner.owner-id
            and messages.plant-id = partner.plant-id
            and messages.WatchReqd
            and (messages.ExpireDate = ? or messages.expireDate > now))
            then assign pcmessages = 'Yes'.
       if avail partner
           and can-find(Messages
            where messages.user-id = partner.user-id
            and messages.plant-id = partner.plant-id
            and messages.WatchReqd
            and (messages.ExpireDate = ? or messages.expireDate > now))
            then assign pcmessages = 'Yes'.
   end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WOMessages) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WOMessages Procedure 
PROCEDURE WOMessages :
/*------------------------------------------------------------------------------
  Purpose: gather any messages pertinent to the order/setup passed in     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var cMessageList as char extent 10 no-undo.
define var cBeginMsg as char no-undo.
define var cResumeMsg as char no-undo.
define var cCoilMsg as char no-undo.
define var cMultMsg as char no-undo.
define var cShiftMsg as char no-undo.
define var iLoop as int no-undo.
define var iMess as int no-undo.

assign
    pcMessages = '||||'
    cMessageList = ''
    cBeginMsg = ''
    cResumeMsg = ''
    cCoilMsg = ''
    cMultMsg = ''
    cShiftMsg = ''.

/* generic shift change message */
for each messages no-lock
    where messages.plant-id = iPlantID
    and messages.TriggerAction = 1
    and (messages.line-id = 0 
         or messages.line-id = ? 
         or messages.line-id = iLineID):
    if index(cMessageList[1], string(messages.messageID,'999999')) = 0
       then assign cMessageList[1] =  cMessageList[1] + string(messages.messageID,'999999') + ','.
end.

find first work-order no-lock where work-order.order-id = iOrder no-error.
if avail work-order then do:

   for each messages no-lock
       where messages.order-id = iOrder:
       if index(cMessageList[messages.TriggerAction], string(messages.messageID,'999999')) = 0
          then assign cMessageList[messages.TriggerAction] =  cMessageList[messages.TriggerAction] + string(messages.messageID,'999999') + ','.
   end.

   /* messages attached to Workorder partner? */
   find first partner no-lock
       where partner.partner-id = work-order.partner-id no-error.
   for each messages no-lock
       where messages.plant-id = iPlantID
       and (messages.partner-id = partner.partner-id
            or messages.owner-id = partner.owner-id
            or messages.user-id = partner.user-id)
       and (messages.line-id = iLineID
            or messages.line-id = 0
            or messages.line-id = ?)
       and messages.TriggerAction ge 2
       and messages.TriggerAction le 5:

      if index(cMessageList[messages.TriggerAction], string(messages.messageID,'999999')) = 0
          then assign cMessageList[messages.TriggerAction] = cMessageList[messages.TriggerAction] + string(messages.messageID,'999999') + ','.
   end.

   for each wosetupdetail no-lock
       where wosetupdetail.order-id = iOrder
       and wosetupdetail.setupNum = iSetup,
       first partner no-lock
         where partner.partner-id = wosetupdetail.partner-id:

       /* message attached to setup parts? */
       for each messages no-lock
           where messages.part-id = wosetupdetail.part-id
           and (messages.line-id = iLineID
                or messages.line-id = 0
                or messages.line-id = ?):
           if index(cMessageList[messages.TriggerAction], string(messages.messageID,'999999')) = 0
               then assign cMessageList[messages.TriggerAction] = cMessageList[messages.TriggerAction] + string(messages.messageID,'999999') + ','.
       end.

       /* messages attached to setup partner? */
       for each messages
           where messages.plant-id = iPlantID
           and (messages.partner-id = partner.partner-id
                or messages.owner-id = partner.owner-id
                or messages.user-id = partner.user-id)
           and (messages.line-id = iLineID
                or messages.line-id = 0
                or messages.line-id = ?)
           and messages.TriggerAction ge 2
           and messages.TriggerAction le 5:
       
          if index(cMessageList[messages.TriggerAction], string(messages.messageID,'999999')) = 0
              then assign cMessageList[messages.TriggerAction] = cMessageList[messages.TriggerAction] + string(messages.messageID,'999999') + ','.
       end.
   end.
end.  /* work order avail */

if cMessageList[1] ne '' then do:
    do iLoop = 1 to num-entries(cMessageList[1]):
        iMess = int(entry(iLoop,cMessagelist[1])).
        if iMess ne ? and iMess gt 0 then assign cShiftMsg = cShiftMsg + GetMessage(iMess).
    end.
end.

if cMessageList[2] ne '' or cMessageList[3] ne '' then do:
    do iLoop = 1 to num-entries(cMessageList[2]):
        iMess = int(entry(iLoop,cMessagelist[2])).
        if iMess ne ? and iMess gt 0 then assign cBeginMsg = cBeginMsg + GetMessage(iMess).
    end.
    do iLoop = 1 to num-entries(cMessageList[3]):
        iMess = int(entry(iLoop,cMessageList[3])).
        if index(cMessageList[2], string(iMess,'999999')) > 0 then next.   /* this message already aboard */
        if iMess ne ? and iMess gt 0 then assign cBeginMsg = cBeginMsg + GetMessage(iMess).
    end.
end.

if cMessageList[3] ne '' then do:
    do iLoop = 1 to num-entries(cMessageList[3]):
        iMess = int(entry(iLoop,cMessagelist[3])).
        if iMess ne ? and iMess gt 0 then assign cResumeMsg = cResumeMsg + GetMessage(iMess).
    end.
end.

if cMessageList[4] ne '' then do:
    do iLoop = 1 to num-entries(cMessageList[4]):
        iMess = int(entry(iLoop,cMessagelist[4])).
        if iMess ne ? and iMess gt 0 then assign cCoilMsg = cCoilMsg + GetMessage(iMess).
    end.
end.

if cMessageList[5] ne '' then do:
    do iLoop = 1 to num-entries(cMessageList[5]):
        iMess = int(entry(iLoop,cMessagelist[5])).
        if iMess ne ? and iMess gt 0 then assign cMultMsg = cMultMsg + GetMessage(iMess).
    end.
end.

assign pcMessages = substitute("&1|&2|&3|&4|&5",
                               cBeginMsg,
                               cResumeMsg,
                               cCoilMsg,
                               cMultMsg,
                               cShiftMsg).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetMessage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetMessage Procedure 
FUNCTION GetMessage RETURNS CHARACTER
  (input piMessageID as int) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
find first messages no-lock
    where Messages.messageID = piMessageID.
if not avail Messages then return ''.
if Messages.ExpireDate < now or Messages.msg = '' then return ''.

/* this is a good message */
assign cScratch = ''.
find first psi-user no-lock
    where psi-user.psiuserid = messages.CreateBy no-error.
assign cScratch = substitute("Message ID: &1        Added By: &2          &3 &4~n&5~n~n",
                             string(Messages.MessageID),
                             (if avail psi-user then psi-user.name else ''),
                             (if Messages.ExpireDate ne ? then 'Expires:' else ''),
                             (if Messages.ExpireDate = ? then '' else string(Messages.ExpireDate,"99/99/99 HH:MM")),
                             Messages.msg). 
return cScratch.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

