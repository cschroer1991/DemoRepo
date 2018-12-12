&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common\t0076.p
    Purpose     : Update user selected process/work order item records
                  with the new versioned spec id

    04/03/06 - cel - TIWO 21519 Turn of the logic that forces a process order and associated
                     item to be review after a versioned spec has been applied. If
                     the spec changes the logic in the spec-lp will catch it and
                     force the process item to be reviewed.
    09/07/06 - cel - TIWO 26491 Added cal to common/t0301.p (update the schedule) when a new version of a spec is
                     applied to a work order.
    08/05/09 - djt - 48725 - return number of PO's changed in parameters.
                     
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter piOldSpecID   as integer    no-undo.
define input  parameter piNewSpecID   as integer    no-undo.
define input  parameter pcPSIPO as character  no-undo.
define input  parameter pcOrder as character  no-undo.
define input  parameter pcPIG as character  no-undo.
define output parameter pcRtnMsg as character  no-undo.
define output parameter piPOsChanged as int no-undo.

define variable iCount as integer    no-undo.
define variable lAlert as logical    no-undo.
define variable cChanged as character  no-undo.
define variable cFieldList as character  no-undo.
define variable cString as character  no-undo.
define variable i as integer    no-undo.
define variable lResult as logical no-undo.  

define buffer bOldSpec for spec.
define buffer bNewSpec for spec.

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
define variable iPSIPO as integer init 0  no-undo.
define variable iOrder as integer    no-undo.
define variable iPIG as integer    no-undo.
define variable lSpecLocked as logical    no-undo.

/* update process order records */
do iCount = 1 to num-entries(pcPSIPO):
  find first procitem where rowid(procitem) = to-rowid(entry(icount,pcPSIPO)) exclusive-lock no-error.
  
  if available procitem then do:
  
    /* find procorder where procorder.PSIPO = procitem.PSIPO exclusive-lock no-error. */
    /* if avail procorder then */
      assign /* procorder.stat = 5 */
             procitem.spec-id = piNewSpecID
             /* Turned off Issue 21519   */
             /* procitem.SpecsOK = false */
             /* procitem.ReviewBy = ""   */
             iPSIPO = iPSIPO + 1.
  end.
end. /* do for num-entries */
assign piPOsChanged = iPSIPO.

/* update work order records */
do iCount = 1 to num-entries(pcOrder):
  find first work-item where rowid(work-item) = to-rowid(entry(icount,pcOrder)) exclusive-lock no-error.
  if available work-item then do:
    assign work-item.spec-id = piNewSpecID.
    run common/t0301.p (input work-item.order-id).
  end.
           iOrder = iOrder + 1.
  
  if iCount = 1 then do:
    /* need to lock the spec only if used on a work order */
    find first spec where spec.spec-id = piNewSpecId exclusive-lock no-error.
    assign spec.lockdate = today
           lSpecLocked = yes.
  end.
end. /* do for num-entries */

/* update pig records */ 
do iCount = 1 to num-entries(pcPIG):
  find first pig where rowid(pig) = to-rowid(entry(icount,pcPIG)) exclusive-lock no-error.
  if available pig then
    assign pig.spec-id = piNewSpecID
           iPig = iPig + 1.
end. /* do for num-entries */

find first bOldSpec no-lock
    where bOldSpec.spec-id = piOldSpecID no-error.

find first bNewSpec no-lock
    where bNewSpec.spec-id = piNewSpecID no-error.

if avail bOldSpec and avail bNewSpec then do:
      assign cFieldList = "Mandrel1,Mandrel2,Mandrel3,min-mult-wt,max-mult-wt,ODMin,ODMax,gauge-neg,gauge-pos,wdth-neg,wdth-pos,lngth-neg,lngth-pos,lngth2-neg,lngth2-pos,min-pckg-wt,max-pckg-wt,min-pc-ct,max-pc-ct,minbundlewt,maxbundlewt,lube,lube-wt,lube-min,lube-max,exposed".
      buffer-compare bNewSpec to bOldSpec save result in cChanged.
      do i = 1 to num-entries(cFieldList):
        if lookup(entry(i,cFieldList),cChanged) gt 0 then 
        do:
          assign lAlert = yes.
          leave.
        end.
      end. /* loop thorugh watched fields */
    
      if lAlert then 
      do:
        for each ProcItem exclusive-lock 
            where ProcItem.spec-id eq bNewSpec.spec-id:
          find first ProcOrder no-lock 
               where ProcOrder.PSIPO = ProcItem.PSIPO no-error.
          if not available ProcOrder then
            next.
          /* find full name of who changed the spec */
          find psi-user no-lock 
               where psi-user.logon eq bNewSpec.logon no-error.
          /* Send Alert Notification */
          assign cString = psi-user.name + chr(3) 
                           + string(bNewSpec.spec-id) + chr(3) 
                           + string(ProcItem.psipo) + chr(3) 
                           + string(ProcOrder.Stat).
          /* find id of csr to send alert too */
          find psi-user no-lock 
               where psi-user.logon = ProcItem.Rep no-error.
          run Alert(7,cString,'','U',psi-User.psiUserID,'',output lResult) no-error.
          
          assign ProcItem.ReviewBy = ''
                 ProcItem.UnReviewReason = 0
                 ProcItem.SpecsOK = no.
                 
         
        end. /* for each procitem */
      end. /* Alert needed */
    end.


assign pcRtnMsg = substitute('Records updated with new Spec Id:~nProcess Orders: &1~nWork Order: &2~nPigs: &3~nNew Spec Locked: &4',
                             string(iPSIPO),
                             string(iOrder),
                             string(iPig),
                             string(lSpecLocked)).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


