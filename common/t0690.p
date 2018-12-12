&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0690.p
    Purpose     : Function for checking if values are in tolerance used
                  for CTL-Meas

    Syntax      :
   Date     By    TIWO  Description
  08/02/12 jmm  49226   New File - for checking CTL measurements and which are 
                        out of tolerance.
 ========  ===  ====== =================================================
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
define input  parameter iPartID    as int no-undo.
define input  parameter iInvID      as int no-undo.
define input  parameter dLength1Ave as decimal no-undo.
define input  parameter dLength2Ave as decimal no-undo.
define input  parameter dLength1Std as decimal no-undo.
define input  parameter dLength2Std as decimal no-undo.
define input  parameter dWidth      as decimal no-undo.
define input  parameter dDiag1      as decimal no-undo.
define input  parameter dDiag2      as decimal no-undo.
define output parameter cTolerance  as character no-undo.

define variable out-length   as logical no-undo.
define variable out-length2  as logical no-undo.
define variable out-width    as logical no-undo.
define variable out-square   as logical no-undo.
define variable out-flatness as logical no-undo.
define variable iLengthMeas  as integer no-undo.
define variable dWdth        as decimal no-undo.
define variable dWdthPos     as decimal no-undo.
define variable dWdthNeg     as decimal no-undo.
define variable dLngth       as decimal no-undo.
define variable dLngthPos    as decimal no-undo.
define variable dLngthNeg    as decimal no-undo.
define variable p-field-list as character no-undo.

define buffer bChoice   for psi.choice.
define buffer bInv      for psi.inv.
define buffer bPart     for psi.part.
define buffer bSpec     for psi.spec.
define buffer bWorkItem for work-item.
define buffer bPartner  for psi.partner.

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
         HEIGHT             = 4.24
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
find Part where Part.part-id = iPartID no-lock no-error.
find Inv where Inv.inv-id = iInvID no-lock no-error.
find Work-Item no-lock
     where Work-Item.order-id = Inv.morder-id
     and   Work-Item.part-id =iPartID  no-error.
find Spec no-lock
     where Spec.spec-id = Work-Item.spec-id no-error.
if available Spec then
do:
  find Choice no-lock
       where field-no = 242 and val = Spec.flatness no-error.

  /*Account for SwitchRotate and SwitchTolerance*/
  if Work-Item.SwitchRotate then
      assign
        dWdth = Part.Lngth
        dLngth = Part.Wdth.
  else
      assign
        dWdth = Part.Wdth
        dLngth = Part.Lngth.

  if Work-Item.SwitchTolerance then
      assign
        dWdthPos = Spec.lngth-pos
        dWdthNeg = Spec.lngth-neg
        dLngthPos = Spec.wdth-pos
        dLngthNeg = Spec.wdth-neg.
  else
      assign
        dWdthPos = Spec.wdth-pos
        dWdthNeg = Spec.wdth-neg
        dLngthPos = Spec.lngth-pos
        dLngthNeg = Spec.lngth-neg.

  assign out-length   = ((dLength1Ave <
                          (dLngth + dLngthNeg)) and dLngthNeg <> ?)
                        or ((dLength1Ave >
                             (dLngth + dLngthPos)) and dLngthPos <> ?)

         out-length2  = if dLength2Ave = 0 and
                             Part.lngth2 > 0 then false
                        else if dLength2Ave = 0 then true
                        else if Part.lngth2 > 0 then
                              (((dLength2Ave <
                                (Part.lngth2 + Spec.lngth2-neg )) and
                                Spec.lngth2-neg <> ?)  or
                               ((dLength2Ave >
                                (Part.lngth2 + Spec.lngth2-pos ))  and
                                Spec.lngth2-pos <> ?))
                        else ((dLength2Ave <
                              (Part.lngth + Spec.lngth-neg ))  and
                                Spec.lngth-neg <> ?) or
                              ((dLength2Ave >
                               (Part.lngth + Spec.lngth-pos ))  and
                               Spec.lngth-pos <> ?)

         out-width      = (( dWidth < (dWdth + dWdthNeg))
                            AND dWdthNeg <> ?) OR
                          (( dWidth >  (dWdth + dWdthPos))
                            AND dWdthPos <> ?)

         out-square     = (ABSOLUTE (dDiag1 - dDiag2) /
                                     2) > Spec.square
         /*    I units = 2.5 * squared(pi * H / L * 100)    */
         out-flatness   = if Choice.misc3 > "" then
                          (2.5 * ((3.14159 * dLength1Std /
                                             dLength2Std * 100) *
                                  (3.14159 * dLength1Std /
                                             dLength2Std * 100))) >
                          dec(Choice.misc3)
                          else dLength1Std > dec(Choice.misc1).
   /* 47111 - See if two length measurements are required */
   /* 48900 - Always measure length twice for talladega plant */
    iLengthMeas = 1.
    if Part.lngth2 ne ? and Part.lngth2 ne 0 then do:
        assign iLengthMeas = 2 .  /* Trapezoid part */
    end.
    else do:
        find first Partner no-lock
            where Partner.partner-id = Part.partner-id no-error.
        if avail Partner and
            (can-find(first Choice no-lock
                     where Choice.field-no = 545
                     and Choice.plant-id = Partner.plant-id
                     and Choice.misc1 = string(Partner.user-id))
             or Partner.plant-id = 8) then
            assign iLengthMeas = 2.
    end.
    if iLengthMeas = 1
        and dLength2Ave = 0
        and out-length2 then out-length2 = false.
end.

         ASSIGN
         cTolerance  = (if out-length  then "length1-ave"  else "")
                       + (if out-length2 then ",length2-ave" else "")
                       + (if out-width   then ",width-ave"   else "")
                       + (if out-square  then ",diag1-ave,diag2-ave" else "")
                       + (if out-flatness and Choice.misc3 <> "" then ",length1-std,length2-std"
                          else if out-flatness then ",length1-std"
                               else "").

         cTolerance = TRIM(cTolerance," ,").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


