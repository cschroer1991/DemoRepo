&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0110.p
    Purpose     : Change Unit of Measure on a Part

    Syntax      :

    Description :

    Author(s)   : Karen Kauffman
    Created     : 07/26/04
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input  parameter piPartID as integer  no-undo.
define input  parameter piOldUOM as integer  no-undo.
define input  parameter pdLbsPerUnit as decimal    no-undo.
define input  parameter piNewUOM as integer  no-undo.
define variable iNewAmt as integer    no-undo.


define variable eRatio   as decimal  decimals 8 no-undo.
define variable eMinStock as decimal decimals 8 no-undo.
define variable eMaxStock as decimal decimals 8 no-undo.
define variable iMinStock as integer no-undo.
define variable iMaxStock as integer no-undo.

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
find part no-lock 
     where part.part-id = piPartID no-error.
if not available part then 
do:
  dynamic-function("setMessage",input "Invalid Part ID: " + string(piPartID)).

end.
else 
do:
  find first s-screq where s-screq.part-id = piPartID no-lock no-error.
  if available s-screq then 
  do:
      dynamic-function("setMessage",
                      input "At least one shipping request exists for this part. " +
                      "You cannot change the UOM unless there are no active requests").
  end.
  else 
  do:
    if part.ProjShipAmt > 0 then 
    do:
      dynamic-function("setMessage",
          input "There is projected ship amt for this part, even though there " +
          "is no shipping request registered.  Contact IT to correct").
    end.
  end.
  if part.PlanAmt > 0 then 
  do:
    dynamic-function("setMessage",
        input "There is planned weight for this part.  You can only change the " +
        "unit of measure if there is no planned amount for the part ").
  end.
  
  if (piOldUOM = piNewUOM)
  and (part.LbsPerUnit = pdLbsPerUnit) then 
  do:
    dynamic-function("setMessage",
        input "Attempting to make old and new uom and Lbs per unit the same!").
  end.
end.

if dynamic-function('chkMsg':u in target-procedure) then 
do:
  return {fn getUserMsg}.  
end.
else 
if part.AvailAmt > 0 then 
do:
  for each inv no-lock 
      where inv.part-id eq piPartID 
      and   inv.stat eq 0:
    case piNewUOM:
      when 1 then   /* Pieces */
        iNewAmt = iNewAmt + inv.pieces.
      when 0 then  /* Weight */ 
        iNewAmt = iNewAmt + inv.net-wt.
      when 2 then /* Package */
        iNewAmt = iNewAmt + 1.
    end case.
  end.
end.

find part exclusive-lock 
     where part.part-id = piPartID no-error.
if available part then 
do:
  if iNewAmt gt 0 then 
  do:
    part.AvailAmt = iNewAmt.
  end.
  
  if part.UOM eq 0 then
  assign
      eMinStock = Part.MinStock / pdLbsPerUnit
      eMaxStock = Part.MaxStock / pdLbsPerUnit.
  else 
  do:
    if piNewUOM eq 0 then
      assign
          eMinStock = Part.MinStock * Part.LbsPerUnit 
          eMaxStock = Part.MaxStock * Part.LbsPerUnit.
    else 
      assign
          eRatio    = part.LbsPerUnit / pdLbsPerUnit
          eMinStock = Part.MinStock * eRatio 
          eMaxStock = Part.MaxStock * eRatio.
  end.

  if trunc(eMinStock,0) ne eMinStock then
    assign iMinStock = int(trunc(eMinStock,0)) + 1.
  else
    assign iMinStock = int(eMinStock).

  if trunc(eMaxStock,0) ne eMaxStock then
    assign iMaxStock = int(trunc(eMaxStock,0)) + 1.
  else
    assign iMaxStock = int(eMaxStock).    

  assign
      part.MinStock = (if part.MinStock ne ? then iMinStock else ?)
      part.MaxStock = (if part.MaxStock ne ? then iMaxStock else ?)
      part.uom      = piNewUOM.
      
  if part.uom eq 0 then
    part.LbsPerUnit = 1.
  else
    part.LbsPerUnit = pdLbsPerUnit.

  /* run Sync part weigh buckets, in case these changes affect that */
  run lib/t0144 (part.part-id).
  
  return.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


