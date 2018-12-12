&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input  parameter piOrderID        as integer    no-undo.
define output parameter poAvgMinPerCoil  as integer    no-undo init 0.
define output parameter poAvgMinPerSetup as integer    no-undo init 0.
define output parameter poOrderCnt       as integer    no-undo.

define variable iNumItemsThisOrder as integer    no-undo.
define variable iMatchThisProcCode as integer    no-undo.
define variable iMustMatch         as integer    no-undo.
define variable iMatch             as integer    no-undo init 0.
define variable iTargetOrderSizes  as integer    no-undo.
define variable iOrderCnt          as integer    no-undo.
define variable iCoilMinutes       as integer    no-undo.
define variable iSetupMinutes      as integer    no-undo.
define variable iCoilCnt           as integer    no-undo.
/* Toggle below to yes to output stats msgs */
define variable lDebugMsgs         as logical    no-undo initial no.

define temp-table ttPart
    field part-id like part.part-id
    field OriginalPart like part.part-id
    index pkPart is primary part-id OriginalPart.

define temp-table ttOrder
    field order-id like work-order.order-id
    field part-id like part.part-id
    field OriginalPart like part.part-id column-label "Original"
    field cuts like work-item.cuts
    field item-id like work-item.item-id
    index pkOrder is primary order-id.

define buffer bpart    for part.
define buffer btype    for type.
define buffer bttOrder for ttOrder.

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
define variable iInnerCnt as integer    no-undo.
define variable iOuterCnt as integer    no-undo.
define variable iPartCount as integer    no-undo.
define variable deGaugeMin as decimal    no-undo.
define variable deGaugeMax as decimal    no-undo.
define variable deWidthMin as decimal    no-undo.
define variable deWidthMax as decimal    no-undo.

find work-order no-lock 
     where work-order.order-id eq piOrderId no-error.
if not available work-order then return.

iMatchThisProcCode = work-order.proc-code.

find partner no-lock 
     where partner.partner-id eq work-order.partner-id no-error.
find cust no-lock 
     where cust.cust-id eq partner.owner-id no-error.

/* This collects information about the order we are trying to find matches for */
for each work-item no-lock 
    where work-item.order-id eq piOrderid, 
    first part no-lock 
          where part.part-id eq work-item.part-id:
  assign deGaugeMin = (part.gauge * .9)
         deGaugeMax = (part.gauge * 1.1)
         deWidthMin = (part.wdth * .9)
         deWidthMax = (part.wdth * 1.1)
         iPartCount = 0.
  PartLoop:
  for each partner no-lock
      where partner.plant-id eq work-order.plant-id 
      and   partner.owner-id eq cust.cust-id,
      each bpart no-lock 
           where bpart.partner-id eq partner.partner-id 
           and   (bpart.gauge ge deGaugeMin  
                  and bpart.gauge le deGaugeMax) 
           and   (bpart.wdth ge  deWidthMin 
                  and bpart.wdth le deWidthMax) 
           and bpart.obsolete eq no:
    iInnerCnt = iInnerCnt + 1.
    if not can-find(first ttPart
                    where ttPart.part-id eq bpart.part-id
                    and   ttPart.originalPart eq work-item.part-id) then
    do:
      create ttPart.
      assign
          ttPart.part-id = bpart.part-id
          ttPart.originalPart = work-item.part-id
          iPartCount = iPartCount + 1.
    end.    /* if not can-find(first ttPart) */
    /* TIWO 27863 - 10 parts */
    if iPartCount ge 10 then
      leave PartLoop.
  end.   /* for each partner */
  iNumItemsThisOrder = iNumItemsThisOrder + 1.

end.   /* for each work-item */

if lDebugMsgs then
  message "outer Loops =" iNumItemsThisOrder "inner loops/ttParts =" iInnerCnt
    view-as alert-box info buttons ok.

if iNumItemsThisOrder <= 5 then 
  iMustMatch = 2. 
else 
  iMustMatch = 3.

run FindMatchingOrders.

run GetMinutes.

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FindMatchingOrders) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindMatchingOrders Procedure 
PROCEDURE FindMatchingOrders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable iOuterCnt as integer    no-undo.

/* This is a list of all parts that match the part or parts we are evaluating */
PartLoop:
for each ttPart,
    each work-item  no-lock
         where work-item.part-id eq ttPart.part-id,
    each work-order no-lock 
         where work-order.order-id eq work-item.order-id 
         and   work-order.stat gt 4 
         and   work-order.proc-date ge (today - 365) 
         and   work-order.proc-code eq iMatchThisProcCode 
    break by work-order.order-id:   
  iOuterCnt = iOuterCnt + 1.
  if first-of(work-order.order-id) then
    iOrderCnt = iOrderCnt + 1.

  create ttOrder.                      
  assign
    ttOrder.order-id     = work-item.order-id
    ttOrder.part-id      = work-item.part-id                                       
    ttOrder.OriginalPart = ttPart.OriginalPart
    ttOrder.cuts         = work-item.cuts
    ttOrder.item-id      = work-item.item-id.     
    /* If you find 100 matching orders, that's good enough of a comparison.
       don't waste processing time */
  /* TIWO 27863 - Only need 10 */
  if iOrderCnt > 10 then 
    leave PartLoop.

end.    

if lDebugMsgs then
  message program-name(1) "#1 outer loop =" iOuterCnt "orders = " iOrderCnt
    view-as alert-box info buttons ok.

iOuterCnt = 0.
/* if there is one produced size, make sure that the target order only has one size */
if iNumItemsThisOrder = 1 then
for each ttOrder no-lock 
    break by ttOrder.order-id:
  iOuterCnt = iOuterCnt + 1.
  if last-of(ttOrder.order-id) then 
  do:
    find last work-item no-lock 
         where work-item.order-id eq ttOrder.Order-id no-error.
    if available work-item then 
    do:
      if work-item.item-id ne 1 
      or work-item.cuts ne ttOrder.cuts then 
      do:
        delete ttOrder.
        iOrderCnt = iOrderCnt - 1.
      end.
    end.
  end.
end.

if lDebugMsgs then
  message program-name(1) "#2 outer loop =" iOuterCnt "orders = " iOrderCnt
    view-as alert-box info buttons ok.

iOuterCnt = 0.

/* If there are two work items on the original order make sure there are two workitems on 
the target order we are comparing to that match exactly the number of cuts*/
if iNumItemsThisOrder eq 2 then 
for each ttOrder no-lock 
    break by ttOrder.order-id: 
  iOuterCnt = iOuterCnt + 1.
  /* counts the number of sizes there are for for the target order.  Used in last-of phrase */
  if first-of(ttOrder.order-id) then 
    iTargetOrderSizes = 1.
  else 
    iTargetOrderSizes = iTargetOrderSizes + 1.

  /* find the work-item from the source order for this part and make sure the number of cuts are the same */
  find work-item no-lock 
       where work-item.order-id eq piOrderId 
       and   work-item.part-id  eq ttOrder.OriginalPart 
       and   work-item.cuts     eq ttOrder.cuts no-error.

  /* Get rid of this ttOrder and any other ttOrder records that may not be checked yet */
  if not available work-item then
  for each bttOrder 
      where bttOrder.order-id = ttOrder.order-id:
    delete bttOrder.
    iOrderCnt = iOrderCnt - 1.
  end.

  /* Check that each size produced on the like order has exactly the same number of sizes*/
  /* Has to check if avail because previous checks might have been on first size of ttOrder 
     and the cuts didn't match, so it deleted all ttOrders for this order-id */
  if  avail ttOrder 
  and last-of (ttOrder.order-id) 
  and iTargetOrderSizes <> 2 then
    /* There needs to be only 2 of this size */
    for each bttOrder 
        where bttOrder.order-id = ttOrder.order-id:
      delete bttOrder.
      iOrderCnt = iOrderCnt - 1.
    end.
end.

if lDebugMsgs then
  message program-name(1) "#3 outer loop =" iOuterCnt "orders = " iOrderCnt
    view-as alert-box info buttons ok.
iOuterCnt = 0.

if iNumItemsThisOrder > 2 then 
for each ttOrder no-lock 
    break by ttOrder.order-id:
  iOuterCnt = iOuterCnt + 1.
  find work-item no-lock 
       where work-item.order-id eq piOrderid 
       and   work-item.part-id  eq ttOrder.OriginalPart no-error.
  if available work-item then 
    iMatch = iMatch + 1.
  if last-of(ttOrder.order-id) then 
  do:
    if iMatch < iMustMatch then 
    for each bttOrder 
        where bttOrder.order-id eq ttOrder.order-id:
      delete bttOrder.
      assign iOrderCnt = iOrderCnt - 1
             iMatch = 0.
    end.
  end.
end.

if lDebugMsgs then
  message program-name(1) "#4 outer loop =" iOuterCnt "orders = " iOrderCnt
    view-as alert-box info buttons ok.
iOuterCnt = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetMinutes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMinutes Procedure 
PROCEDURE GetMinutes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable dtEndDate as date       no-undo.
define variable dtLogSt   as datetime   no-undo.
define variable dtLogEnd  as datetime   no-undo.
define variable iEndTime  as integer    no-undo.
define variable iLLCnt    as integer    no-undo.

for each ttOrder no-lock:
  poOrderCnt = poOrderCnt + 1.
  for each line-log no-lock 
      where line-log.order-id eq ttOrder.order-id 
      break by line-log.coil-id
            by line-log.line-id
            by line-log.start-date descending
            by line-log.start-time descending
            by line-log.start-sec  descending:
    iLLCnt = iLLCnt + 1.
    if first-of(line-log.coil-id) 
    or first-of(line-log.line-id) then
      /* set default date/time */
      run parseDateTime (input ?, output dtEndDate, output iEndTime).

      /* If LL record has valid end date/time, reset to this record */
      assign dtEndDate = line-log.end-date when line-log.end-date ne ?
             iEndTime  = line-log.end-time when line-log.end-time gt 0.
      /* Calc time interval in minutes */
      assign dtLogSt  = dynamic-function("getDTValue",line-log.start-date,line-log.start-time)
             dtLogEnd = dynamic-function("getDTValue",dtEndDate,iEndTime).

      /* We can't calc a time if one of them is unknown */
      if  dtLogEnd ne ? 
      and dtLogSt  ne ? then
      do:
        if line-log.descr = 'Coil' then
        do:
          iCoilMinutes = iCoilMinutes + interval(dtLogEnd,dtLogSt,"minutes").
        end.
        else
        if line-log.descr = "SETUP" then 
        do: 
          iSetupMinutes = iSetupMinutes + interval(dtLogEnd,dtLogSt,"minutes").
        end.
      end.
      /* This is outside of the if above so as to    */
      /* not miss the last-of if the dates are wrong */
      if  last-of(line-log.coil-id) 
      and line-log.descr = 'Coil' then 
        iCoilCnt = iCoilCnt + 1.
      
      assign dtEndDate = line-log.start-date
             iEndTime  = line-log.end-time.

  end.

end.        

assign poAvgMinPerCoil = (iCoilMinutes / iCoilCnt)
       poAvgMinPerSetup = iSetupMinutes / iOrderCnt.

if lDebugMsgs then
    message program-name(1) "LL =" iLLCnt "POOrders = " poOrderCnt
            "poAvgMinPerCoil =" poAvgMinPerCoil  "iCoilMinutes =" iCoilMinutes "iCoilCnt =" iCoilCnt
            "poAvgMinPerSetup =" poAvgMinPerSetup  "iSetupMinutes =" iSetupMinutes "iOrderCnt =" iOrderCnt
        view-as alert-box info buttons ok.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

