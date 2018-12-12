&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server\common\t0085.p
    Purpose     : Return the number of days the coil is aged
    Author(s)   : Jenni Lammers
    Created     : 06/16/04
    Notes       :
08/09/16 zakh 71656 Now charging storage when we scrap a coil.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  define input  parameter piInvID       as integer    no-undo.
  define input  parameter pdDate        as date       no-undo.
  define input  parameter pcTimeFrame   as character  no-undo.
  define output parameter piStorageDays as integer    no-undo.

  define buffer bInv for inv.
  define buffer bActv for actv.
  define buffer bPartner for partner.
  define buffer bCust for cust.
  define buffer bChoice for choice.
  define buffer bWork-Order for work-order.
  
  
  define variable lAddInGraceDays    as logical    no-undo init no.
  define variable dAgedDate          as date       no-undo.
  define variable dFirstDayOfMonth   as date       no-undo.

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

  if pdDate = ? then
      pdDate = today.

  assign
      lAddInGraceDays = no.

  find bInv where bInv.inv-id = piInvID no-lock no-error.
  if not avail bInv then do:
      piStorageDays = 0.
      leave.
  end.

  find bPartner where
      bPartner.partner-id = bInv.partner-id no-lock no-error.
  find bCust where
      bCust.cust-id = bPartner.owner-id no-lock no-error.

  /* Check if the customer is part of the Aged Inventory program */
  if avail bCust and bCust.invc-cfg[6] = 1 then do:
      piStorageDays = 0.
      return.
  end.

   /* If coil shipped, tripped or processed, then the coil stopped aging at that
    point and the aged days should reflect accordingly */
  case bInv.stat:
      /* Shipped or tripped */
      when 3 or when 7 then do:
          find bActv where
              bActv.inv-id = bInv.inv-id and
              bActv.actv-code = bInv.stat no-lock no-error.
          if avail bActv then
              pdDate = bActv.tran-date.
          if bInv.inv-id = bInv.rcpt-id then
              lAddInGraceDays = yes.
      end.
      /* Processed */
      when 5 then do:
          find bWork-Order where
              bWork-Order.order-id = bInv.morder-id no-lock no-error.
          if avail bWork-order then
              assign
              pdDate = bWork-Order.proc-date.
      end.
      /* Scrapped */
      when 6 or when 10 then do:
          find first bActv where
              bActv.inv-id = bInv.inv-id and
              bActv.actv-code = 8 no-lock no-error.
          if avail bActv then
              assign
              pdDate = bActv.tran-date.
          if bInv.inv-id = bInv.rcpt-id then
              lAddInGraceDays = yes.
      end.
  end.

  /* Aged storage days equal the difference of the date passsed in, consumed or shipped,
     minus (but including) the date the coil became "Aged".
     This number cannot be less than 0 */  

    run common/t0086.p(input piInvID, output dAgedDate) no-error.
    if RETURN-VALUE eq '0' or dAgedDate = ? then
    do:
        assign
            piStorageDays = 0.
            return.
    end.
        


  /* If the calling procedure wants the aged storage days for the "Month"
     pass it back instead of the total number of storage days */
  if pcTimeFrame = "Month" then do:
      assign
          dFirstDayOfMonth = date(string(month(pdDate)) + "/1/" + string(year(pdDate))).

      if dAgedDate < dFirstDayOfMonth then
          assign 
             piStorageDays = maximum(pdDate - dFirstDayOfMonth + 1,0).
      else
          assign    
             piStorageDays    = maximum(pdDate - dAgedDate + 1,0).
  end.
  else do:
    /* total number of storage days */
    assign    
        piStorageDays    = maximum(pdDate - dAgedDate + 1,0).
  end.

  /* Master Coils shipped, tripped or scrapped will also have grace days added
     back in for the month they shipped */
  if lAddInGraceDays and piStorageDays > 0 then do:
      /* Find the choice table record for the customer's aged storage classification
         so that we know how to define what "Aged" is for that customer's inventory */
      find bPartner where
          bPartner.partner-id = bInv.partner-id no-lock no-error.
      find bCust where
          bCust.cust-id = bPartner.owner-id no-lock no-error.
      find bChoice where 
          bChoice.field-no = 256 and
          bChoice.val = bCust.invc-cfg[6] no-lock no-error.
      /* Misc2 is the grace days for masters, we should only add grace days back in if a master
         ships after it has been aged and we did not perform any processing tasks on it. */
      if avail bChoice then
          piStorageDays = piStorageDays + int(bChoice.misc2).
  end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


