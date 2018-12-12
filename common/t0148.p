&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0148.p
    Purpose     : Fetch Part/Spec Info

    Syntax      :

    Description :

    Author(s)   : Rick Terrell, Bravepoint, Inc.
    Created     : 10/07/04
    History     :
      Date   Pgmr Issue Notes/comments
    -------- ---- ----- -------------------------------------------------
    11/08/05 rct  3157  Handle multiple parts on a single setup  
    08/06/09 djt 47111  Determine if 2 length measurements needed
    08/10/09 djt 48900  Always 2 CTL measurements at plant 8
    12/12/11 zakh 56743 Added logic for Switch Rotate/Tolerance
    06/22/17 cjs  74809 Added New CTL MeasType field
    10/20/18 Mas  77941 Added Input parameters coming from Ctl-Meas-V1.w and
                        some logic for Trapezoid
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter piInvID         as integer      no-undo.
define input  parameter piSetUpNum      as integer      no-undo.
define input  parameter piPartID        as integer      no-undo.
define input  parameter plTrapezoid     as logical      no-undo.
define input  parameter piMeasureNum    as integer      no-undo.
define input  parameter pcLogon1        as character    no-undo.
define input  parameter piSheetsNumber  as integer      no-undo.
define output parameter pcValueList     as character    no-undo.
define output parameter piLengthMeas    as integer      no-undo.
define output parameter pcFirstMeasBy   as character    no-undo.
define output parameter pcRtnMsg        as character    no-undo.

define buffer bCtl-Meas for ctl-Meas.

&scoped-define C1 + chr(1) +

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-decToStr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD decToStr Procedure 
FUNCTION decToStr RETURNS CHARACTER
  ( input pdeValue as decimal )  FORWARD.

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
assign pcValueList = ?
       pcRtnMsg    = ''
       piLengthMeas = 1.

find inv no-lock
     where inv.inv-id eq piInvID no-error.
if not available inv then
do:
    pcRtnMsg = substitute("Inv ID &1 not found.",piInvID).
    return.
end.

/* Find WOSetupDetail for order/setup that is not scrap */
/* if we didn't pass in part-id  */
if piPartID le 0 or piPartID eq ? then
do:
    find first WOSetupDetail no-lock
        where WOSetupDetail.order-id eq inv.morder-id
        and   WOSetupDetail.part-id  gt 0
        and   WOSetupDetail.setupNum eq piSetUpNum no-error.
    if not available WOSetupDetail then
    do:
        pcRtnMsg = substitute("WO Setup detail not found for Order ID &1 and Setup &2.",
                            inv.morder-id,piSetUpNum).
        return.
    end.
   
    piPartID = WOSetupDetail.part-id.
end.

find part no-lock
    where part.part-id eq piPartID no-error.
if not available part then
do:
    pcRtnMsg = substitute("Part ID &1 not found.",inv.part-id).
    return.
end.

find work-item no-lock
     where work-item.order-id eq inv.morder-id
     and   work-item.part-id  eq part.part-id no-error.
if not available work-item then
do:
    pcRtnMsg = substitute("WorkItem not found for Order ID &1 and Part ID &2.",
                          inv.morder-id,part.part-id).
    return.
end.

find spec no-lock
     where spec.spec-id eq work-item.spec-id no-error.
if not available spec then
do:
    pcRtnMsg = substitute("Spec ID &1 not found.",work-item.spec-id).
    return.
end.

find choice no-lock
     where choice.field-no eq 242
     and   choice.val      eq spec.flatness no-error.


pcValueList = decToStr(if work-item.SwitchRotate 
                       then part.lngth else part.wdth) {&C1}
              decToStr(if work-item.SwitchTolerance
                       then spec.lngth-pos else spec.wdth-pos) {&C1}
              decToStr(if work-item.SwitchTolerance
                       then spec.lngth-neg else spec.wdth-neg) {&C1}
              decToStr(if work-item.SwitchRotate 
                       then part.wdth else part.lngth) {&C1}
              decToStr(if work-item.SwitchTolerance
                       then spec.wdth-pos else spec.lngth-pos) {&C1}
              decToStr(if work-item.SwitchTolerance
                       then spec.wdth-neg else spec.lngth-neg) {&C1}
              decToStr(part.lngth2)     {&C1}
              decToStr(spec.lngth2-pos) {&C1}
              decToStr(spec.lngth2-neg) {&C1}
              decToStr(spec.square).

if available choice then
    pcValueList = pcValueList  {&C1}
                  (if choice.misc1 eq ? then "?" else choice.misc1) {&C1}
                  (if choice.misc3 eq ? then "?" else choice.misc3).
else
    pcValueList = pcValueList {&C1} chr(1).

/* 47111 - See if two length measurements are required, first meas by */
    if part.lngth2 ne ? and part.lngth2 ne 0 then do:
        assign piLengthMeas = 2 .  /* Trapezoid part */
    end.
    else do:
        find first partner no-lock
            where partner.partner-id = part.partner-id no-error.
        if avail partner and
            (can-find(first choice no-lock
                     where choice.field-no = 545
                     and choice.plant-id = partner.plant-id
                     and choice.misc1 = string(partner.user-id))
             or partner.plant-id = 8) then
            assign piLengthMeas = 2.
    end.

    if plTrapezoid <> yes then
    do:
        find first ctl-meas no-lock
            where ctl-meas.inv-id = piinvID
            and ctl-meas.part-id = piPartID
            and ctl-meas.MeasType ne 4 no-error.
        assign pcFirstMeasBy = (if avail ctl-meas then ctl-meas.logon1 else '').
    end.
    
return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-decToStr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION decToStr Procedure 
FUNCTION decToStr RETURNS CHARACTER
  ( input pdeValue as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN (if pdeValue eq ? then "?" else string(pdeValue)).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

