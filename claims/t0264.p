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
    
    
     Date     Coder Issue Notes
    -------- ----- ----- -------------------------------------------------------    
    11/14/05  ptg  3132  Somehow the no-error option on the find statement for buffer 
                         bCust in  the CanMakeScrapClain procedure was causing the find 
                         to fail. Removing the no-error option allows the program to work 
                         correctly, but of course in the extreme situation where the owner 
                         record is missing, would cause the program to terminate with extreme 
                         prejudice. I adduce that the problem lies somewhere in the compiler 
                         and relates to the use of the dynamic-function call. Moved the dynamic 
                         function call to an  assign to a variable and the results appear to 
                         be correct. Using the variable as part of the selection returns the 
                         proper customer
    11/15/05  ptg 3132   Changed to allow entry of claim items that are mults of the master                         
    11/16/05  ptg 3132   Changed to utilize both coil-id and inv-id   
    11/21/05  ptg 3132   Changed to  find coil information differently for mults
                         and inv records.
    11/29/05  ptg 3132   Added patch to update defect-amt to be the rounded
                         inspect defectSize until such time as the schema is
                         updated
    01/09/06  ptg 3380   Modified to take break and slit into account for claim item
                         for mults         
    01/31/06  cel 3426   Modified to allow an unlimited number of claims for a Mult, regardless of the 
                         damage code. Modified Logic for setting iCoilID to Parent.  
    02/06/06  cel 3684   Modified logic that handles when piInvID is passed as 0.   
    06/19/06  cel 23281  Set the positionCode = 0 when claim is for scrap.           
    09/07/07  djt 36585  populate new field claim-item.seq-no when creating new claim item
    11/11/09 nlbu 25698 Auto Holds for gauge out of spec - AK not producing claims.
    05/09/16  djt 71686  When any inspected damage gets flagged to hold, create claim-item
                         for any like damage that may not have got flagged to hold on the inspection app.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input parameter piCoilID       as integer    no-undo.
define input parameter piDamageCode   as integer    no-undo.
define input parameter piLocationCode as integer    no-undo.
define input parameter piWeight       as integer    no-undo.
define input parameter pcComment      as character  no-undo.
define input parameter plMillScrap    as logical    no-undo.
define input parameter prInspection   as rowid      no-undo.
define input parameter piInvID        as integer    no-undo.
define input parameter piBreakNo      as integer    no-undo.
define input parameter piSlitNo       as integer    no-undo.

define variable giPlantID as integer    no-undo.
define variable gcUser    as character  no-undo.

define variable iCoilID     as integer no-undo.
define variable iInvID      as integer no-undo.
define variable iClaimID    as integer no-undo.
define variable rClaimItem  as rowid   no-undo.
define variable iOrderID    as integer no-undo.

define buffer bInv for Inv.
define buffer bPrevInv for Inv.
define buffer bClaim for claim.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CanMakeScrapClaim) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CanMakeScrapClaim Procedure 
FUNCTION CanMakeScrapClaim RETURNS LOGICAL
  (input piInvID as integer,
   input piPartnerID as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateClaim) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateClaim Procedure 
FUNCTION CreateClaim RETURNS INTEGER
  ( input piCoilID      as integer,
    input piOrderID     as integer,
    input piPartnerID   as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateClaimItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateClaimItem Procedure 
FUNCTION CreateClaimItem RETURNS ROWID
    (input piClaimID as integer,
    input piCoilID  as integer,
    input piInvID   as integer,
    input piBreakNo as integer,
    input piSlitNo  as integer)  FORWARD.

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
         HEIGHT             = 9.67
         WIDTH              = 52.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


assign
    giPlantID = {fnarg getGlobalVar 'PlantID'}
    gcUser    = {fnarg getGlobalVar 'UserID'}.




do for bInv, bPrevInv, bClaim:

    find bInv no-lock
        where bInv.inv-id eq piCoilID no-error.

    if plMillScrap eq yes and CanMakeScrapClaim(piCoilID,bInv.Partner-ID) eq no then
    leave.

    
    if piInvID > 0 then
    do:
        find bInv no-lock where bInv.inv-id eq piInvID no-error.
        if avail bInv then do:
            if bInv.Inv-ID eq bInv.Rcpt-ID then
                find bPrevInv no-lock where bPrevInv.Inv-ID eq bInv.Inv-ID no-error.
            else if bInv.prev-inv-id gt 0 then
                find bPrevInv no-lock where bPrevInv.inv-id eq bInv.prev-inv-id no-error.
            if not available bPrevInv or bPrevInv.stat eq 7 then
                find bPrevInv no-lock where bPrevInv.inv-id eq bInv.rcpt-id no-error.
                
            assign  iInvID  = bInv.Inv-ID
                    iCoilID = bPrevInv.Inv-ID.    
                            
            if bPrevInv.morder-id eq 0 or 
               bPrevInv.morder-id eq ? then
            assign
                iOrderID = bPrevInv.order-id.
            else
            assign
                iOrderID = bPrevInv.morder-id.    
        end. /*  avail bInv */
    end.
    else do: /* piInvID = 0 */   
    
        find bInv no-lock where bInv.inv-id eq piCoilID no-error.
        if avail bInv then
          assign
          iCoilID = piCoilID
          iInvID  = 0
          iOrderID = bInv.Order-ID.
    end.


    if iCoilID = 0 or giPlantID = ? then do:
       message program-name(1) "Could not create claim for: "skip
            program-name(1) "piCoilID: " piCoilID skip
            program-name(1) "piInvID: " piInvID skip
            program-name(1) "piBreakNo: " piBreakNo skip
            program-name(1) "piSlitNo: " piSlitNo skip
            program-name(1) "PlantID: " giPlantID skip
            program-name(1) "Called from: " source-procedure:file-name
       view-as alert-box.
               
       return.
    end.


               


   

    find first bClaim no-lock
      where bClaim.coil-id eq iCoilID
      and   bClaim.stat    eq 0 no-error.
      
    

    if not available bClaim then
    assign
        iClaimID = DYNAMIC-FUNCTION('CreateClaim', iCoilID, iOrderID, bInv.Partner-ID).
    else
    assign
        iClaimID = bClaim.Claim-ID.

    assign
        rClaimItem = DYNAMIC-FUNCTION('CreateClaimItem':U,input iClaimID,
                                                          input iCoilID, 
                                                          input piInvID,
                                                          input piBreakNo, 
                                                          input piSlitNo).

    
                                                                                  
    if prInspection ne ? then
    run CopyInspection(input rClaimItem, input prInspection).
 
end.

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CopyInspection) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyInspection Procedure 
PROCEDURE CopyInspection :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    define input parameter prClaimItem  as rowid no-undo.
    define input parameter prInspection as rowid no-undo.

    define variable iSeq as int no-undo.
    DEFINE VARIABLE iInspectID AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iBreak AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSlit AS INTEGER     NO-UNDO.
    define buffer bInspection for psi.inspect.
    define buffer bInspect2   for psi.inspect.
    define buffer bClaimItem  for psi.Claim-Item.
    define buffer bClaimItem2 for psi.Claim-Item.

    do for bInspection, bClaimItem transaction:
        
        find bClaimItem exclusive-lock
            where rowid(bClaimItem) eq prClaimItem no-error.

        find bInspection no-lock
            where rowid(bInspection) eq prInspection no-error.

        iInspectID = (if avail bInspection then bInspection.inspectid else ?).
        iBreak = (if avail bInspection then bInspection.break-no else ?).
        iSlit = (if avail bInspection then bInspection.slit-no else ?).
       
        if available bInspection and available bClaimItem then
            buffer-copy bInspection 
               except  coil-id break-no logon slit-no unusedIng
               to bClaimItem.

          
    end.
    /* 71686 If other defects were found in the inspection app but not flagged to hold,
             place those defects in the list as well. */

/*         MESSAGE                                                                                               */
/*        substitute('t0264 Looking for previous item at coil: &1 mult: &2-&3 DC: &4 inspectID other than: &5',  */
/*                   piCoilID, iBreak, iSlit, pidamageCode, iInspectID) view-as alert-box info buttons ok.       */
       for each bInspect2 no-lock
           where bInspect2.coil-id = piCoilID
           and bInspect2.break-no = iBreak
           and bInspect2.slit-no = iSlit
           and bInspect2.damage-code = piDamageCode
           and bInspect2.inspectID ne iInspectID:

           if can-find(bClaimItem2
                       where bClaimItem2.claim-id = iClaimID
                       and bClaimItem2.break-no = iBreak
                       and bClaimItem2.slit-no = iSlit
                       and bClaimItem2.damage-code = piDamageCode
                       and bClaimItem2.start-foot eq bInspect2.start-foot
                       and bClaimItem2.end-foot eq bInspect2.end-foot) then next.
           run claims/t0511.p (input iClaimID,
                                output iSeq).
           create claim-item.
           assign
                claim-item.act-date = today
                claim-item.act-time = {fn getSysTime}
                claim-item.break-no = bInspect2.break-no
                claim-item.claim-id = iClaimID
                claim-item.coil-id = bInspect2.coil-id
                claim-item.comments = bInspect2.comments
                claim-item.damage-code = bInspect2.damage-code
                claim-item.defect-amt = ?
                claim-item.defect-side = bInspect2.defect-side
                claim-item.defectSize = bInspect2.defectSize
                claim-item.defect-uom = bInspect2.defect-uom
                claim-item.end-foot = bInspect2.end-foot
                claim-item.fault = 0
                claim-item.ImageID = bInspect2.ImageID
                claim-item.inv-id = bInspect2.inv-id
                claim-item.loc-code = bInspect2.positioncode
                claim-item.logon = bInspect2.logon
                claim-item.PositionCode = (if piInvID = piCoilID then 15 else 0)
                claim-item.PositionFromEdge = bInspect2.PositionFromEdge
                claim-item.RepeatCode = bInspect2.RepeatCode
                claim-item.repeating = bInspect2.repeating
                claim-item.repeat-uom = bInspect2.repeat-uom
                claim-item.sample = bInspect2.sample
                claim-item.seq-no = iSeq
                claim-item.Severity = bInspect2.severity
                claim-item.slit-no = bInspect2.slit-no
                claim-item.start-foot = bInspect2.start-foot.    
       end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CanMakeScrapClaim) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CanMakeScrapClaim Procedure 
FUNCTION CanMakeScrapClaim RETURNS LOGICAL
  (input piInvID as integer,
   input piPartnerID as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    define buffer bCust for psi.cust.
    
    define variable lCanMakeScrapClaim as logical no-undo.
    define variable iOwnerID           as integer no-undo.
    
    /* if pcComment = 'choice235', then bypass cust config */
    if pcComment = 'choice235' then
        return yes.

    assign
        iOwnerID = dynamic-function('GetPartnerOwner',piPartnerID).
        
    find bCust no-lock
            where bCust.Cust-ID eq iOwnerID no-error.
 
    if available bCust then
    assign
        lCanMakeScrapClaim = bCust.Invc-Cfg[2] eq 1.
    else
    assign
        lCanMakeScrapClaim = no.


    return lCanMakeScrapClaim.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateClaim) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateClaim Procedure 
FUNCTION CreateClaim RETURNS INTEGER
  ( input piCoilID      as integer,
    input piOrderID     as integer,
    input piPartnerID   as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    define buffer bClaim for psi.claim.
    define variable iClaimID as integer no-undo.
    define var cComment as char no-undo.

    cComment = replace(pcComment,'NoInspect','').
    /* 73928 - do not include '235' in claim comment */
    cComment = replace(pcComment,'choice235','').

    do for bClaim Transaction:

        create bClaim.

        assign
            bClaim.Claim-ID     = dynamic-function('getNextIDInt',giPlantID,0,99)
            bClaim.Plant-ID     = giPlantID
            bClaim.Create-Date  = today
            bClaim.Coil-ID      = piCoilID
            bClaim.Stat         = 0
            iClaimID            = bClaim.Claim-ID
            bClaim.descr        = cComment
            bClaim.DeductDate   = ?
            bClaim.cust-code    = ''
            bClaim.Comments     = ''
            bClaim.attention    = ''
            bClaim.author       = ''
            bClaim.CheckNumber  = ''
            bClaim.prep-date    = ?
            bClaim.Reference    = ''
            bClaim.order-id     = piOrderID
            .


        assign
            bClaim.owner-id     = dynamic-function('GetPartnerOwner',piPartnerID).
        
    end.


    return iClaimID.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateClaimItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateClaimItem Procedure 
FUNCTION CreateClaimItem RETURNS ROWID
    (input piClaimID as integer,
    input piCoilID  as integer,
    input piInvID   as integer,
    input piBreakNo as integer,
    input piSlitNo  as integer) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
define buffer bClaimItem for psi.claim-item.

define variable rClaimItem as rowid no-undo.
define variable lCreateNewClaimItem as logical.
define variable cComment as char no-undo.
define variable iSeq as int no-undo.

/* The NoInspect value in pcComments helps us determin the source
   of the call to t0264.p */
if pcComment begins 'NoInspect' then cComment = 'NoInspect'.

do for bClaimItem Transaction:

        if piInvID ne 0 then
        find first bClaimItem exclusive-lock
            where bClaimItem.Claim-ID    eq piClaimID 
            and   bClaimItem.Inv-ID      eq piInvID
            and   bClaimItem.Damage-Code eq piDamageCode no-error.
        /* NoInspect in below line added in tiwo 25698 */ 
        if not available bClaimItem and cComment ne "NoInspect" then
        find first bClaimItem exclusive-lock
            where bClaimItem.Claim-ID    eq piClaimID 
            and   bClaimItem.coil-id     eq piCoilID
            and   bClaimItem.break-no    eq piBreakNo
            and   bClaimItem.slit-no     eq piSlitNo
            and   bClaimItem.Damage-Code eq piDamageCode  no-error.
            
         if not avail bClaimItem then 
              lCreateNewClaimItem = true.
         else do:
           /* Attempt to marry the current claim Item with the current Inspection Record */
           if prInspection <> ? then do:
               if bClaimItem.comments begins 'NoInspect' then
                 lCreateNewClaimItem = false.
               else
                 lCreateNewClaimItem = true.
           end.
           else lCreateNewClaimItem = false.
         end.
    
         if lCreateNewClaimItem = true then do:

            
            run claims/t0511.p (input piClaimID,
                                output iSeq).
            create bClaimItem.
            assign
                bClaimItem.act-time         = {fn getSysTime}
                bClaimItem.Claim-ID         = piClaimID
                bClaimItem.seq-no           = iSeq
                bClaimItem.act-date         = today
                bClaimItem.logon            = gcUser
                bClaimItem.inv-id           = piInvID
                bClaimItem.weight           = piWeight
                bClaimItem.break-no         = piBreakNo
                bClaimItem.comments         = cComment
                bClaimItem.damage-code      = piDamageCode
                bClaimItem.defect-amt       = 0
                bClaimItem.defect-side      = 0
                bClaimItem.defect-uom       = 6
                bClaimItem.end-foot         = 0
                bClaimItem.ImageID          = 0
                bClaimItem.loc-code         = piLocationCode
                bClaimItem.PositionCode     = if piInvID = piCoilID then 15 else 0
                bClaimItem.PositionFromEdge = 0
                bClaimItem.repeat-uom       = 6
                bClaimItem.RepeatCode       = 0
                bClaimItem.repeating        = 0
                bClaimItem.sample           = no
                bClaimItem.Severity         = 0
                bClaimItem.slit-no          = piSlitNo
                bClaimItem.coil-id          = piCoilID
                bClaimItem.start-foot       = 0
                no-error.
         end. /*  if lCreateNewClaimItem = true then do: */

         assign
            rClaimItem = rowid(bClaimItem).



end. /* do for bClaimItem Transaction: */

return rClaimItem.












/***** Turned off by cel 1/31/06 *******************************
    define buffer bClaimItem for psi.claim-item.

    define variable rClaimItem as rowid no-undo.

    do for bClaimItem Transaction:
    
        if piInvID ne 0 then
        find first bClaimItem exclusive-lock
            where bClaimItem.Claim-ID    eq piClaimID 
            and   bClaimItem.Inv-ID      eq piInvID
            and   bClaimItem.Damage-Code eq piDamageCode no-error.
         
        if not available bClaimItem then
        find first bClaimItem exclusive-lock
            where bClaimItem.Claim-ID    eq piClaimID 
            and   bClaimItem.coil-id     eq piCoilID
            and   bClaimItem.break-no    eq piBreakNo
            and   bClaimItem.slit-no     eq piSlitNo
            and   bClaimItem.Damage-Code eq piDamageCode  no-error.
            
        if not available bClaimItem then
        do:
            create bClaimItem.
            assign
                bClaimItem.act-time         = {fn getSysTime}
                bClaimItem.Claim-ID         = piClaimID
                bClaimItem.act-date         = today
                bClaimItem.logon            = gcUser
                bClaimItem.inv-id           = piInvID
                bClaimItem.weight           = piWeight
                bClaimItem.break-no         = piBreakNo
                bClaimItem.comments         = ''
                bClaimItem.damage-code      = piDamageCode
                bClaimItem.defect-amt       = 0
                bClaimItem.defect-side      = 0
                bClaimItem.defect-uom       = 6
                bClaimItem.end-foot         = 0
                bClaimItem.ImageID          = 0
                bClaimItem.loc-code         = piLocationCode
                bClaimItem.PositionCode     = 7
                bClaimItem.PositionFromEdge = 0
                bClaimItem.repeat-uom       = 6
                bClaimItem.RepeatCode       = 0
                bClaimItem.repeating        = 0
                bClaimItem.sample           = no
                bClaimItem.Severity         = 0
                bClaimItem.slit-no          = piSlitNo
                bClaimItem.coil-id          = piCoilID
                bClaimItem.start-foot       = 0
                no-error
                .
        end.

        assign
            rClaimItem = rowid(bClaimItem).
    end.
    
    return rClaimItem.
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

