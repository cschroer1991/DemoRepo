&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server\common\t0086.p
    Purpose     : Return the date that a coil became aged
    Author(s)   : Jenni Lammers
    Created     : 06/16/04

04/20/06 jke  22227 Adjust aged date for Perrysburg and Eldridge to 
              start up date, if the aged date is prior to 3/24.    
10/03/06 jke  27398 Use startup date for Perrysburg & Eldridge if it is 
              after the rcpt date, otherwise, use the rcpt date
09/20/07 jke  35344 For Trial Material, do not recognize trial number of ? as a valid trial-no                 
08/09/16 zakh 71656 Now charging storage when we scrap a coil.
12/18/17 zakh 75813 Looking at storage.AgeDate to determine which start date to use.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

  define input  parameter piInvID    as integer    no-undo.
  define output parameter piDateAged as date       no-undo.

  define buffer bInv        for inv.
  define buffer bPartner    for partner.
  define buffer bCust       for cust.
  define buffer bRcpt       for rcpt.
  define buffer bWork-order for work-order.
  define buffer bChoice     for Choice.
  define buffer bStorage    for storage.

  define variable iPackageDays   as integer    no-undo.
  define variable iMasterDays    as integer    no-undo.
  define variable iGraceDays     as integer    no-undo.
  define variable dBeginDate     as date       no-undo.
  define variable dGoLiveDate    as date       no-undo.
  define variable lTrialMaterial as logical    no-undo.
  define variable iOwner         as integer    no-undo.

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
    
find bInv no-lock where bInv.inv-id = piInvID no-error.
if not avail bInv then
    return ?.

/*  Aged storage only applies to coils and packages */
if bInv.inv-code <> 1 and bInv.inv-code <> 2 and bInv.inv-code <> 4 then
    return '0'.

find bPartner no-lock where bPartner.partner-id = bInv.partner-id no-error.
if not avail bPartner then
    return ?.

find bCust no-lock where bCust.cust-id = bPartner.owner-id no-error.
if not avail bCust then
    return ?.

if bInv.trial-no = "" or bInv.trial-no = ? or bInv.trial-no = "?" then
    lTrialMaterial = no.
else 
    lTrialMaterial = yes.

/* invc-cfg[6] refers to the customer's aged storage classification */
/* If customer is "Primary - Excluding Trial Material" */ 
if bCust.invc-cfg[6] = 3 and lTrialMaterial then
    return '0'.

/* Find the choice table record for the customer's aged storage classification
   so that we know how to define what "Aged" is for that customer's inventory */
find bChoice no-lock
    where  bChoice.field-no = 256 
    and    bChoice.val      = bCust.invc-cfg[6] no-error.
if not avail bChoice then
    assign
        iPackageDays = 180
        iMasterDays  = 180.
else
    assign 
        iPackageDays = int(misc1)
        iMasterDays  = int(misc2).

/* This is the "Not Apply" aged storage classification */
if iPackageDays = 0 and iMasterDays  = 0 then
    return '0'.

/* Unprocessed master coils may use different day mark than packages for
   certain classifications.  The date we start aging the coil differs as well. */
if bInv.inv-id = bInv.rcpt-id then do:
    find first bRcpt no-lock
        where bRcpt.rcpt-id = bInv.rcpt-id no-error.
    if not avail bRcpt then
        return ?.
    
    assign
        dBeginDate = bRcpt.rcpt-date
        iGraceDays = iMasterDays.
end. /*Master Coil*/
else do:
    iOwner = dynamic-function("GetPartnerOwner",bInv.Partner-ID).
    
    find bStorage no-lock
        where bStorage.Plant-ID   = bInv.Plant-ID
        and   bStorage.Cust-ID    = iOwner
        and   bStorage.Partner-ID = bInv.Partner-ID
        and   bStorage.Part-ID    = bInv.Part-ID no-error.
    
    if not avail bStorage then
        find bStorage no-lock
            where bStorage.Plant-ID   = bInv.Plant-ID
            and   bStorage.Cust-ID    = iOwner
            and   bStorage.Partner-ID = bInv.Partner-ID
            and   bStorage.Part-ID    = 0 no-error.
    
    if not avail bStorage then
        find bStorage no-lock
            where bStorage.Plant-ID   = bInv.Plant-ID
            and   bStorage.Cust-ID    = iOwner
            and   bStorage.Partner-ID = 0
            and   bStorage.Part-ID    = 0 no-error.
    
    if not avail bStorage then
        find bStorage no-lock
            where bStorage.Cust-ID    = iOwner
            and   bStorage.Plant-ID   = 0
            and   bStorage.Partner-ID = 0
            and   bStorage.Part-ID    = 0 no-error.
    
    if avail bStorage and bStorage.AgeDate = 1 then do: /*Receipt Date*/
        find first bRcpt no-lock where bRcpt.rcpt-id = bInv.rcpt-id no-error.
        if not avail bRcpt then
            return ?.

        dBeginDate = bRcpt.rcpt-date.
    end. /*Age By Receipt Date*/
    else do:
        find first bWork-order no-lock
            where bWork-order.order-id = bInv.order-id no-error.
        
        dBeginDate = (if avail bWork-order then bWork-order.proc-date 
                      else bInv.create-date).
    end. /*age by Create Date*/

    iGraceDays = iPackageDays.
end. /*Package*/

/* If Perrysburg or Eldridge, don't charge before we took them over 
if bInv.plant-id = 9 or bInv.plant-id = 10 then
    dGoLiveDate = dynamic-function("GetPlantLiveDate", bInv.plant-id).
*/

if dGoLiveDate > dBeginDate then
    dBeginDate = dGoLiveDate.

/* If an unusually large number entered for grace days, that signifies that 
   this inv code does not age */
if iGraceDays > 9000 then
    return '0'.

piDateAged = dBeginDate + iGraceDays.
  
return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


