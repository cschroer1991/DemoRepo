&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0668.p
    Purpose     : Dynamic function for returning picklist exception notes.

    Syntax      :
   Date     By    TIWO  Description
 ========  ===  ====== =================================================
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 
define input  parameter piOrderID  as integer   no-undo.
define input  parameter piCoilID   as integer   no-undo.
define input  parameter pcMode     as character no-undo.
define output parameter pcNote     as character no-undo.

define query qPickExcp for picklist.

define var hQuery as handle no-undo.
define var cQueryString as character no-undo.
define var hSpecTableHdl as handle no-undo.
define var hSpecFieldHdl as handle no-undo.

define var iSpecID     as integer   no-undo.
define var iNoteLength as integer   no-undo.
define var cStr        as character no-undo.
define var cTxt        as character no-undo.
define var cComment    as character no-undo.

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
hQuery = query qPickExcp:handle.

if (piOrderID = 0 or piOrderID = ?) 
and (piCoilID = 0 or piCoilID = ?) then
do:
    assign pcNote = "".
    return.
end.

if piOrderID > 0 then
  assign cQueryString = "for each picklist no-lock where picklist.order-id eq " + string(piOrderID).

if piCoilID > 0 
and piOrderID <= 0 then
    assign cQueryString = "for each picklist no-lock where picklist.master-id eq " + string(piCoilID).
else
if piCoilID > 0 then
    assign cQueryString = cQueryString + " and picklist.master-id eq " + string(piCoilID).   

hQuery:query-prepare(cQueryString).
hQuery:query-open().

repeat:
    hQuery:get-next().
    if hQuery:query-off-end then leave.

    if picklist.PlanscrapFt  > 0
    or picklist.ExpBreaks    > 0
    or picklist.ExpNetWt     > 0
    or picklist.CompLinearFt > 0
    or picklist.CSRLogon     > ""
    or picklist.CustApproval > "" then
    do:
        assign pcNote = "COIL EXCEPTIONS~n" 
                      + string(picklist.master-id)
                      + "~n  Plan Scrap Ftg: " + string(picklist.PlanScrapFt)
                      + "~n  Expected Breaks: " + string(picklist.ExpBreaks)
                      + "~n  Expected Net Wt: " + string(picklist.ExpNetWt)
                      + "~n  Comp Linear Ftg: " + string(CompLinearFt)
                      + "~n  Approvals:"
                      + "~n    CSR:  " + trim(CSRLogon)
                      + "~n    Cust: " + trim(CustApproval).
    end.
    
    if picklist.PickInstructions > "" then
    do:
        if pcMode = "coil release" then iNoteLength = 60. 
                                   else iNoteLength = 30.
        assign pcNote = pcNote + "~n~n  Instructions: "
               cTxt = picklist.PickInstructions.

        repeat:
          run lib/l0092.p (iNoteLength, input-output cTxt, output cComment).
          if cTxt = ? then leave.
          assign pcNote = pcNote + "~n    " + cComment.
        end.

    end.

    for each PicklistExcp no-lock 
            where PickListExcp.order-id eq Picklist.order-id
              and PicklistExcp.coil-id eq picklist.master-id,
        each part no-lock where part.part-id eq PicklistExcp.part-id
        break by PicklistExcp.part-id:

        if first-of (PicklistExcp.part-id) then
        do:
            assign pcNote = pcNote + "~n~n  SPEC EXCEPTIONS:"
                          + "~n     Part " + string(PicklistExcp.part-id)
                          + " (" + string(part.wdth,">9.9999") + ")".
        end.
        assign pcNote = pcNote 
                      + "~n       " + trim(PicklistExcp.FieldLabel) + ": "
                      + string(PicklistExcp.FieldValue).
    
        if pcMode = "coil release" then
        do:
            find work-order no-lock 
                where work-order.order-id eq PicklistExcp.order-id no-error.
            find work-item no-lock
                where work-item.order-id eq work-order.order-id
                  and work-item.part-id  eq PicklistExcp.part-id no-error.
            iSpecID = dynamic-function('GetSpec', 
                                       input work-order.order-id,
                                       input PicklistExcp.part-id,
                                       input PicklistExcp.coil-id).
            find spec no-lock where spec.spec-id eq iSpecID no-error.
            if available spec then
            do:
               assign hSpecTableHdl = buffer spec:handle.
               assign hSpecFieldHdl = hSpecTableHdl:buffer-field(PickListExcp.FieldLabel).
               assign pcNote = pcNote 
                             + "(Spec " + string(spec.spec-id)
                             + ", Original = " 
                             + trim(string(hSpecFieldHdl:buffer-value)) + ")".
            end.
        end.
    end.
end.


return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


