&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : chem/t0471.p
    Purpose     : Validate Mechanical Results
    Author(s)   : C. Longo
    
    Date     Init TIWO  Descr
    04/24/07 nlbu 33351 Added partner.config[5] = 2 check  
                                before display of error msg. Changed logic so
                                material type begins "J" values are checked
                                for all partners.
                                
    07/30/07  cel  35776 Honda Cross App, Added logic that will
                               place material on hold of we are unable to locate
                               mechanicals.
    01/08/08  jke 39321 If putting on hold code 1, must create a claim.  (see TIWO for why)  
    01/19/09 nlbu 44770 Add partner-id to t-limits table.  
    05/06/09 nlbu 47429 partner.config[5] = 6, Add mech check for update mechs. 
    06/11/09 nlbu 48126 Revision for tiwo 47429. 
    06/30/09 nlbu 47667 WOW Mech check - only give warning.  New input params.  
    07/01/08 nlbu 48398 Compare limits to nominal gauge instead of part.gauge.  
                        Code change actually done in above tiwo 47667.         
    06/03/10 bmf  50198 Fix chemistry hold bug.  
    11/10/10 nlbu 53395 chem mech UpGrade - Strip down to only generate out of 
                        tolerance msgs for range of limits.
    12/01/16 zakh 73526 Fixed a bug by adding a 'first' clause
    07/26/18 tjn  78669 Updating the way that mech limits are searched for with the goal of eventually 
                          turning these limits into a hard stop. Also added RoundUp function. 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input parameter piInvID  as integer no-undo.
define input parameter piTestID as integer no-undo.
define input parameter pcHeatLot as char no-undo.
define input parameter piOwnerId as integer no-undo.
define input parameter piTypeID  as integer no-undo.
define input parameter plHoldClaim as logical no-undo.
define output parameter pcUsrMsg as char no-undo.
define output parameter pcInvList as char no-undo.

define variable v-rec-found      as logical    no-undo.
define variable i                as integer    no-undo.
define variable v-mech-list      as character  init "2,6,7" no-undo.
define variable v-mech-desc      as character  init "TENSILE, YIELD, ELONG" no-undo.
define variable cEmailGroup      as character  no-undo.
define variable lResult          as logical    no-undo.
define variable v-nominal-gauge  as decimal    no-undo.
define variable v-msg            as character  no-undo.
define variable gcParmStr        as character  no-undo.
define variable lLastTest        as logical    no-undo.
define variable eMetricPartGauge as decimal    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-RoundUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RoundUp Procedure 
FUNCTION RoundUp RETURNS decimal
  ( input x as dec )  FORWARD.

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
         HEIGHT             = 5.33
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

  find inv no-lock 
       where inv.inv-id = piInvID no-error.
  find part no-lock
       where part.part-id = inv.part-id no-error.
  find partner no-lock
       where partner.partner-id = inv.partner-id  no-error.
  /* need the t-indx for BHI and other checks */
  find first t-indx where t-indx.inv-id = inv.inv-id no-lock no-error.
  if not avail t-indx then do:
      find first t-indx where t-indx.inv-id = inv.rcpt-id no-lock no-error.
  end.
  if avail t-indx then piTestId = t-indx.test-id.
  else if not avail t-indx and partner.MSACrossAppReqd = 1 then do:
      pcUsrMsg = "Mechanical Tests For Honda could not be found for PSI ID: " + string(piInvId) +
          " Putting material on hold for missing Mechs".
      return.
  end.
  
   /*  Most of the time type-id is for inv-id. tiwo 47667 type-id is for work-item */
  if piTypeId = ? then
      find type where type.type-id = part.type-id no-lock no-error.
  else
      find type where type.type-id = piTypeId no-lock no-error.
 
  /* no longer do mechanical checks in here, use t0625*/
  
/*   assign cEmailGroup = "Honda-" + {fnarg getGlobalVar 'PlantId'}. */

  /* TIWO 22821 */
  if not(type.descr begins "J") then  /* Honda non Hes Spec Check */
  do:
    if partner.MSACrossAppReqd = 1 then do:  
      find choice no-lock
         where choice.field-no eq 452
         and   choice.val      eq type.type-id no-error.
      if not available choice then
      do:
        return.
      end.  /* available choice */
    end.   /* partner.config[5] */
    else do:
       return.  /* Not a Honda partner - no checks needed */
    end.
  end.    /* not (type.descr begins 'J")  */
  
  /* Nominal Gauge Check for Honda. */
  /* Nominal Gauge stored in t-limits.lo-limit field with test code = 32 */
  assign
      eMetricPartGauge = roundUp(part.gauge * 25.4) /* Convert the part gauge to metric */
      v-rec-found = no.
  /* Need to find the lower limit from a t-limit record under test-code 32
        where the part.gauge falls in the gauge range. */
  for each  t-limits no-lock 
      where t-limits.owner-id eq 1864     /* HTA */
      and   (t-limits.partner-id = ? or t-limits.partner-id = 0)
      and   t-limits.type-id eq 3606      /* unknown */
      and   t-limits.t-code  eq 32:        /* nominal gauge */
    
    if (t-limits.gauge      = eMetricPartGauge or  
        t-limits.lo-limit   = eMetricPartGauge or 
        (t-limits.lo-gauge <= eMetricPartGauge and 
         t-limits.hi-gauge >= eMetricPartGauge)) then
    do:
      assign 
          v-nominal-gauge = t-limits.lo-limit
          v-rec-found = yes.
      leave.
    end.
  end.   /* for each  t-limits */

  if not v-rec-found then
  do:     /* No t-limits set up for nominal gauge. */
      assign v-msg    = "Nominal Limits not set up for this gauge:"
                      + string(part.gauge)
                      + " This test code: 32 "
                      + " PSI ID: " + string(piInvID)
             pcUsrMsg = pcUsrMsg
                      + (if pcUsrMsg eq "" then "" else "~n")
                      + v-msg.
/*       run Alert (input 49, /* Alert ID# */                          */
/*                  input v-msg, /* alert arg */                       */
/*                  input '',  /* Alert Buffers */                     */
/*                  input 'G', /* Contact Type */                      */
/*                  input 0 ,  /* Contact id */                        */
/*                  input cEmailGroup, /* group name */                */
/*                  output lResult). /* Result */                      */
      v-msg = "".
      return.
  end.

  /* End issues # 2630 & 2632 */

  /*CONVERT GAUGE VALUE TO ENGLISH IF IT IS IN T-LIMITS BEFORE COMPARING TO */
  /*PART*/
  REPEAT-loop:
  repeat i = 1 to num-entries(v-mech-list):
    find first t-meas no-lock
         where t-meas.t-code = integer(entry(i,v-mech-list)) 
         and   t-meas.test-id = piTestID no-error.
    
    /* WHAT TO DO IF NO VALUE ENTERED ??? */
    if not available t-meas then 
      next REPEAT-loop.

    assign lLastTest = no. 
    v-rec-found = no.
    hit-loop:                        /* ONLY CHECKING FOR OWNER-ID = 1864 (Honda) */
    for each t-limits no-lock
        where t-limits.owner-id = 1864 
        and   (t-limits.partner-id = ? or t-limits.partner-id = 0) 
        and   t-limits.type-id  = type.type-id 
        and   t-limits.t-code   = integer(entry(i,v-mech-list))
        break by t-limits.hi-gauge:
        if last(t-limits.hi-gauge) then lLastTest = true. 
        ELSE lLastTest = FALSE.

        if ((t-limits.gauge = v-nominal-gauge and t-limits.gauge <> 0) or  
            (t-limits.lo-gauge <= v-nominal-gauge and t-limits.hi-gauge > v-nominal-gauge)
            or (lLastTest and t-limits.hi-gauge = v-nominal-gauge)) then
            do: 
                v-rec-found = yes.
                if t-limits.t-code = 2 or t-limits.t-code = 6 then 
                    do:
                        if dynamic-function("tompa-ksi", 
                                            input t-meas.exact-meas[1], 
                                            input t-meas.uom, 
                                            input 0, 
                                            input partner.user-id) 
                            lt 
                            dynamic-function("tompa-ksi", 
                                             input t-limits.lo-limit, 
                                             input t-limits.limit-uom, 
                                             input 0, 
                                             input partner.user-id) 
                            or
                            dynamic-function("tompa-ksi", 
                                             input t-meas.exact-meas[1], 
                                             input t-meas.uom,  
                                             input 0, 
                                             input partner.user-id) 
                            gt 
                            dynamic-function("tompa-ksi", 
                                             input t-limits.up-lim,
                                             input t-limits.limit-uom,
                                             input 0, 
                                             input partner.user-id) then
                                do:
                                    /* ERROR */
                                    run ErrorHit(buffer t-meas,
                                                 buffer t-limits,
                                                 input piInvID,
                                                 input entry(i,v-mech-desc)).
                                    pcInvList = string(piInvID).
                                end.   /* If outside limits */
                    end.   /* If t-code = 2 or 6 */
                else 
                    if t-limits.t-code = 7 then 
                        do: 
                            if t-meas.exact-meas[1] < t-limits.lo-limit 
                                or t-meas.exact-meas[1] > t-limits.up-lim then 
                                do:
                                    /* ERROR */
                                    run ErrorHit(buffer t-meas,
                                                 buffer t-limits,
                                                 input piInvID,
                                                 input entry(i,v-mech-desc)).
                                    pcInvList = string(piInvID).
                                end.   /* outside limits */
                            else 
                                do: 
                                end.   /* within limits */
                        end.   /* IF t-limits.t-code = 7 */

            end.   /* IF gauge inside limits */
    end.   /* hitloop:  F/E t-limits */

    if not v-rec-found then 
        do:
            assign 
                v-msg    = "Limits not set up for this gauge:" + string(part.gauge)
                    + " This test type: " 
                    + entry(i,v-mech-desc) + " PSI ID: " 
                    + string(piInvID)
                pcUsrMsg = pcUsrMsg
                    + (if pcUsrMsg eq "" then "" else "~n")
                    + v-msg.
            v-msg = "".
        end.   /* if not v-rec-found */
  end.   /* repeat-loop:  REPEAT i = 1 TO NUM-ENTRIES(v-mech-list) */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ErrorHit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorHit Procedure 
PROCEDURE ErrorHit :
DEFINE PARAMETER BUFFER t-meas   FOR t-meas.
  DEFINE PARAMETER BUFFER t-limits FOR t-limits.
  DEFINE INPUT PARAMETER piInvId   AS INT.
  DEFINE INPUT PARAMETER piTDesc   AS CHAR.
  
  DEF VAR cErrorMsg              AS CHAR   NO-UNDO.
  define variable lResult as logical    no-undo.
  define variable p-msg            as character  no-undo.

  /* TIWO 22798 - Check for existence of t-meas and do hold message appropriately */
  /* If t-meas not avail then v-msg set before calling this IP               */
  if available t-meas then
  do:
    cErrorMsg = "Value falls outside range of acceptable values for this " 
              + substitute("test type: &1",piTDesc)
              + substitute("~n   PSI ID: &1",piInvId) 
              + substitute("~n   VALUE: &1 &2",t-meas.exact-meas[1],TRIM(DYNAMIC-FUNCTION('getdesc',312,t-meas.uom)))
              + substitute("~n   LO LIMIT: &1",
                           (if available t-limits then 
                            string(t-limits.lo-limit) + " " + TRIM(DYNAMIC-FUNCTION('getdesc',312,t-limits.limit-uom))
                            else "Not Available"))
              + substitute("~n   UP LIMIT: &1",
                           (if available t-limits then 
                            STRING(t-limits.up-lim) + " " + TRIM(DYNAMIC-FUNCTION('getdesc',312,t-limits.limit-uom))
                            else "Not Available. (t0471)")).
    /* reset v-msg */
    v-msg = cErrorMsg.
  end.

  pcUsrMsg = pcUsrMsg 
           + (if pcUsrMsg gt "" then "~n" else "")
           + v-msg.
  v-msg = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-RoundUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RoundUp Procedure 
FUNCTION RoundUp RETURNS decimal
  ( input x as dec ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 if x = truncate(x,2) then
     return decimal(x). /* Function return value. */
 else
     return decimal((truncate(x,2)) + .01).  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

