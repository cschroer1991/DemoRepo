&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0301
    Purpose     : Recompute Info for production Schedule.

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       : 01/05/05 - kmk - When setting prod-sched.interleave, use
                                   spec.interleav, not spec.interleave.
                                   
                  07/17/06 - cel 24741 Changed Logic in updateProdSched for setting
                                 Prod-sched.stencil.
                  09/11/06 - cel 26491 Changed Logic in UpdateProdSched to inherit values from
                                 spec record.
                  04/07/16   djt 70833 - recalc sched-wt from picklist.
                  04/29/16   djt 71658 - update req-wt as well
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input parameter ipiOrderID as integer no-undo.
define variable iLineID as integer    no-undo.
define variable eEstTime like prod-sched.est-time.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getCOTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCOTime Procedure 
FUNCTION getCOTime RETURNS DECIMAL
( input piLineID as integer,input piFromID as integer,input piToID as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnwindMandrel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getUnwindMandrel Procedure 
FUNCTION getUnwindMandrel RETURNS INTEGER
    (input piLineID as integer, 
     input piID as decimal )  FORWARD.

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
run updateProdSched.
if return-value <> "":U
    then return return-value.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getPickListMandrel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPickListMandrel Procedure 
PROCEDURE getPickListMandrel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input  parameter piOrderID         as integer no-undo.
define input  parameter ipiLineID         as integer no-undo.
define output parameter opiOrderUWMandrel as integer.
define output parameter oplMixed          as logical no-undo.

define buffer bInv        for inv.
define buffer bPickList   for picklist.

def var iUnwindMandrel  as integer no-undo.

assign 
    oplMixed          = no
    opiOrderUWMandrel = 0.

for each bPickList no-lock
    where bPickList.order-id = piOrderID:

    find bInv no-lock
        where bInv.inv-id eq bPickList.master-id no-error.
    if available bInv then
    do:
        assign
            iUnwindMandrel = dynamic-function('GetUnwindMandrel',ipiLineID, bInv.i-d).
        if opiOrderUWMandrel eq 0 then 
          assign opiOrderUWMandrel = iUnwindMandrel.
        if opiOrderUWMandrel ne iUnwindMandrel then 
          assign oplMixed = yes.
    end.
    else 
        assign
            iUnwindMandrel = 0
            oplMixed = yes.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateProdSched) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateProdSched Procedure 
PROCEDURE updateProdSched :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define buffer bprodSched  for prod-sched.
define buffer b2prodSched for prod-sched.
define buffer bInv        for inv.
define buffer bPickList   for picklist.
define buffer bWorkItem   for work-item.
define buffer bWorkOrder  for work-order.

def var iCoilCount         as integer init 0 no-undo.
def var iNRCoils           as integer init 0 no-undo.
def var lMixed             as logical no-undo.
def var lPrevMixed         as logical no-undo.
def var iPrevUnwindMandrel as integer no-undo.
def var iOrderUWMandrel    as integer no-undo.
def var iSchedWt           as integer no-undo.

assign 
    lMixed          = no
    iOrderUWMandrel = 0
    iSchedWt        = 0.
find first bprodSched where
    bprodSched.order-id = ipiOrderID 
    exclusive-lock no-error.
if not available bprodsched
    then return "**Cannot find order " + string(ipiOrderID) + " in prod-sched table.".


find bWorkOrder where
    bWorkOrder.order-id = ipiOrderID
    no-lock no-error.
if not available bWorkOrder
    then return "**Cannot find order " + string(ipiOrderID) + " in work-order table.".
for each bWorkItem 
    where bWorkItem.order-id = ipiOrderID no-lock,
    each spec where spec.spec-id = bWorkItem.spec-id no-lock:

    if spec.lube = 8 then
        bprodSched.dry = true.
    else do:
        bprodSched.dry = false.
        bprodSched.oil = spec.lube.
    end.

    if bWorkOrder.plant-id = 1 then do:
       if spec.stenciler = true then 
           bprodSched.stencil = true.
       else
           bprodSched.stencil = false.
    end.
    else do:
        if spec.stencil <> ? and spec.stencil <> "0" and spec.stencil <> "" then 
            bprodSched.stencil = true.
        else
            bprodSched.stencil = false.
    end.
    
    assign bprodSched.exit-id = spec.mandrel[1].

    bprodSched.interleave = spec.interleav.
    bprodSched.level = spec.level.

    if spec.exposed <> 0 and spec.exposed <> 2 then 
        bprodSched.exposed = spec.exposed. 
    else
        bprodSched.exposed = 0.
end.

for each bPickList no-lock
    where bPickList.order-id = ipiOrderID:

    if bPickList.master-id = 0 or bPickList.master-id = ? then assign
           iSchedWt = iSchedWt + bpicklist.EstWt
           iNRCoils = iNRCoils + 1.
    else do:
        find bInv where
            bInv.inv-id = bPickList.master-id
            no-lock no-error.
        if available bInv and (bInv.stat le 2 or bInv.stat = 4) /* status = available or in-process */
            then assign
            iSchedWt = iSchedWt + bInv.Net-wt.
        if avail bInv and bInv.stat le 1 then
            iCoilCount = iCoilCount + 1.
    end.
end.

/* See if this order uses more than one mandrel */
run getPickListMandrel
    (input ipiOrderID,
     input bprodSched.line-id,
     output iOrderUWMandrel, 
     output lMixed).

/* If this order uses more than one mandrel then
   we don't care about the previous prod-sched since COTime can't
   be calculated */
if not lMixed then do: 
    find b2prodSched where
        b2prodSched.line-id = bprodSched.line-id and
        b2prodSched.seq-no = bprodSched.seq-no
        no-lock no-error.

    find prev b2ProdSched where b2ProdSched.line-id = bprodSched.Line-id no-lock no-error.
    if available b2prodSched then do:

        /* get the mandrel for the previous prod-sched order */
        run getPickListMandrel
            (input b2prodSched.order-id,
             input b2prodSched.line-id, 
             output iPrevUnwindMandrel, 
             output lPrevMixed).

        /* Once again if more than one mandrel we can't calculate COTime */
        if lPrevMixed 
            then lMixed = true.
    end.
    else lMixed = true. /* If we're first then we can't calculate COTime */
end.


run lib/l0110.p (input ipiOrderId, output iLineId, output eEstTime).
if eEstTime = ? then eEstTime = 0.

assign
    bprodSched.CoilCnt  = iCoilCount
    bprodSched.NRcoils  = iNRCoils
    bprodSched.est-time = eEstTime
    bprodSched.COTime   = (if not lMixed 
                           then dynamic-function('GetCOTime',bprodSched.line-id,iPrevUnwindMandrel,iOrderUWMandrel)
                           else ?)
    bprodSched.plant-id = bWorkOrder.plant-id
    bprodsched.SchedWt = iSchedWt.

if bWorkOrder.schedule-wt ne iSchedWt or bworkOrder.req-weight ne iSchedWt then do:
    find bWorkOrder where bWorkOrder.order-id = ipiOrderID no-error.
    if avail bWorkOrder then do:
        assign
            bWorkOrder.schedule-wt = iSchedWt
            bworkOrder.req-weight = iSchedWt.
        release bWorkOrder.
        find bWorkOrder where bWorkOrder.order-id = ipiOrderID no-lock no-error.
    end.
end.

/* find work-order where work-order.order-id = ipiOrderID no-error.  */
if iLineID > 0 and bWorkOrder.line-id = (bWorkOrder.plant-ID * 100) then do:
    find bWorkOrder where bWorkOrder.order-id = ipiOrderID no-error.
    bWorkOrder.line-id = iLineId.
end.

return.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getCOTime) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCOTime Procedure 
FUNCTION getCOTime RETURNS DECIMAL
( input piLineID as integer,input piFromID as integer,input piToID as integer ) :
/*------------------------------------------------------------------------------
Purpose:  
  Notes:  
------------------------------------------------------------------------------*/
define buffer bMandrelMatrix for psi.MandrelMatrix.

if piFromID eq piToID 
    then return 0.0.

find bMandrelMatrix no-lock
    where bMandrelMatrix.Line-ID     eq piLineID 
    and   bMandrelMatrix.FromEquipID eq piFromID
    and   bMandrelMatrix.ToEquipID   eq piToID no-error.

if available bMandrelMatrix 
    then return bMandrelMatrix.COTime.
else return ?.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getUnwindMandrel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getUnwindMandrel Procedure 
FUNCTION getUnwindMandrel RETURNS INTEGER
    (input piLineID as integer, 
     input piID as decimal ) :
/*------------------------------------------------------------------------------
Purpose:  
  Notes:  
------------------------------------------------------------------------------*/
define buffer bEquip     for psi.Equip. 
define buffer bLineEquip for psi.LineEquip.

def var iPlantID    as integer no-undo.
def var iMandrelID  as integer no-undo.

assign
  iPlantID = integer(dynamic-function('GetGlobalVar','plantid')).

for each bEquip no-lock
  where bEquip.Plant-ID  eq iPlantID
  and   bEquip.mandrel   eq piID
  and   bEquip.EquipType eq 1: /* Unwind mandrel */

/* indexing is messed up anyway, 
   so let the db engine do the work above */
/*   if bEquip.mandrel ne piID */
/*       then next.            */

  if not can-find(bLineEquip no-lock
      where bLineEquip.EquipID eq bEquip.EquipID
      and   bLineEquip.Line-ID eq piLineID) 
      then next.

  assign
      iMandrelID = bEquip.EquipID.

  /* Once we have it, bail */
  leave.

end.

return iMandrelID.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

