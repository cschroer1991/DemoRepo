&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : TESTSTESTSETSET
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
    History     : 01/19/09-nlbu- Tiwo 44770 Add partner-id to t-limits table.
                  09/29/10-nlbu- Tiwo 53395 Chems/Mechs Upgrades
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER pi-ChemTest AS INT     NO-UNDO.
DEF INPUT  PARAMETER pi-chemId   AS INT     NO-UNDO.
DEF INPUT  PARAMETER pi-side     AS INT     NO-UNDO.
DEF OUTPUT PARAMETER cInvList    AS CHAR    NO-UNDO.
 
DEF BUFFER b-rcpt FOR rcpt.

DEF VARIABLE gc-msg             AS CHAR    NO-UNDO.
DEF VARIABLE cRowList           AS CHAR    NO-UNDO.
DEF VARIABLE p-msg              AS CHAR    NO-UNDO.

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

find chemMstr where chemMstr.chemTestId = pi-ChemTest no-lock no-error.
if not available chemMstr then
do: 
    return.
end.

find chemMeas where 
     chemMeas.chemTestId = pi-ChemTest and
     chemMeas.chem-id    = pi-chemId   and
     chemMeas.loc        = 0           and
     chemMeas.side       = pi-side
    no-lock no-error.

find first rcpt where 
           rcpt.heat-lot = chemMstr.heat-lot and
           rcpt.mill-id = chemMstr.mill-id 
    no-lock no-error.

if not available rcpt then
do:
  return.
end.

find inv where inv.inv-id = rcpt.rcpt-id no-lock no-error.
find part where part.part-id = inv.part-id no-lock no-error.
find partner where partner.partner-id = inv.partner-id no-lock no-error.

IF NOT AVAILABLE partner THEN RETURN.

/* message                                             */
/*     "   partner.owner-id = " partner.owner-id  skip */
/*     "  partner.config[5] = " partner.config[5] SKIP */
/*     "         part.gauge = " part.gauge        skip */
/*     "       part.type-id = " part.type-id      skip */
/*     "   chemmeas.chem-id = " chemmeas.chem-id  skip */
/*     "      chemmeas.side = " chemmeas.side     SKIP */
/*     "chemmeas.exact-meas = " chemmeas.exact-meas    */
/*     view-as alert-box.                              */

if partner.ChemReqd = 4 and partner.ChemTriggers = 1 then   /* Limits Testing- Hold 26 */
do:
  find first t-limits where t-limits.partner-id = partner.partner-id and
    (t-limits.gauge    = part.gauge or t-limits.gauge = ?) and 
    (t-limits.type-id  = part.type-id or t-limits.type-id = ?) and
     t-limits.t-code   = 4 and
     t-limits.chem-id  = chemMeas.chem-id and 
     t-limits.side     = chemMeas.side no-lock no-error.
  if available t-limits then do:
   for each t-limits where   /* partner limits */
     t-limits.partner-id = partner.partner-id and
    (t-limits.gauge    = part.gauge or t-limits.gauge = ?) and 
    (t-limits.type-id  = part.type-id or t-limits.type-id = ?) and
     t-limits.t-code   = 4 and
     t-limits.chem-id  = chemMeas.chem-id and 
     t-limits.side     = chemMeas.side:

     if chemMeas.exact-meas < t-limits.lo-limit or
        chemMeas.exact-meas > t-limits.up-lim then
     do:
        RUN sendEmailMsg.
        RUN setInvHold.
     end.
   end. /* for each */
  end. /* if available */ 
  else do:
   for each t-limits where /* owner limits */
    t-limits.owner-id = partner.owner-id
       and (t-limits.partner-id = ? or t-limits.partner-id = 0) and 
    (t-limits.gauge    = part.gauge or t-limits.gauge = ?) and 
    (t-limits.type-id  = part.type-id or t-limits.type-id = ?) and
     t-limits.t-code   = 4 and
     t-limits.chem-id  = chemMeas.chem-id and 
     t-limits.side     = chemMeas.side:

     if chemMeas.exact-meas < t-limits.lo-limit or
        chemMeas.exact-meas > t-limits.up-lim then
     do:
        RUN sendEmailMsg.
        RUN setInvHold.
     end.
    end.  /* for each */
  end. /* else do */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-sendEmailMsg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendEmailMsg Procedure 
PROCEDURE sendEmailMsg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var cEmailGroup  as char                 no-undo.
  def var mMemPtr      as memptr               no-undo.
  def var cChemMsg     as char format "x(15)"  no-undo.
  def var cLoMsg       as char format "x(15)"  no-undo.
  def var cHiMsg       as char format "x(15)"  no-undo.
  define variable lResult as logical    no-undo.

  cEmailGroup = "Chem" + {fnarg getGlobalVar 'PlantId'}.
  set-size(mMemPtr) = 0.

  cChemMsg = DYNAMIC-FUNCTION('getFieldValue',
                        input 'chemCode',
                        input 'chemId',
                        input t-limits.chem-id,
                        input 'first',
                        input 'chemVal').

  IF cChemMsg <> ? THEN
    cChemMsg = "CHEM TYPE: " + cChemMsg + " ".
  ELSE
    cChemMsg = "".

  cLoMsg = if t-limits.lo-limit <> ? then 
              " LO: " + string(t-limits.lo-limit) 
           else "".
  cHiMsg = if t-limits.up-lim <> ? then 
              " HI: " + string(t-limits.up-lim) 
           else "".

  gc-Msg = " CHEMISTRY FOR: " + partner.name + '~n' +
           cChemMsg + " ON COIL IS NOT WITHIN SPEC: " + '~n~n' +
           " ENTERED VALUE: " + string(chemMeas.exact-meas) + '~n' + 
           cLoMsg + '~n' +
           cHiMsg + '~n' +
           " HEAT LOT: " + string(rcpt.heat-lot) + '~n' +
           " INV ID: " + string(rcpt.rcpt-id).
         
  run Alert (input 13, /* Alert ID# - Chem alert */
             input gc-Msg, /* alert arg */
             input '',  /* Alert Buffers */
             input 'G', /* Contact Type */
             input 0 ,  /* Contact id */
             input cEmailGroup, /* group name */
             output lResult). /* Result */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setInvHold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setInvHold Procedure 
PROCEDURE setInvHold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH b-rcpt WHERE
           b-rcpt.heat-lot = chemMstr.heat-lot and
           b-rcpt.mill-id = chemMstr.mill-id NO-LOCK,
      EACH inv WHERE
           inv.rcpt-id = b-rcpt.rcpt-id AND
           inv.stat = 0 NO-LOCK:
      
      ASSIGN 
         cRowList = cRowList + "," + string(ROWID(inv))
         cInvList = cInvList + "~n" + STRING(inv.inv-id).

  END.
  cRowList = TRIM(cRowList,',').
  cInvList = TRIM(cInvList,',').

  MESSAGE "In t0226.p in the SetInvHold Procedure" skip
      "gc-Msg = " gc-Msg skip
      "p-Msg = " p-Msg skip
      "cRowList = " cRowList skip
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

  IF cRowList <> "" THEN
    RUN inv/t0114.p (INPUT "hold",
                     INPUT cRowList,
                     INPUT 'inv',
                     INPUT 26,
                     INPUT gc-Msg,
                     INPUT 0,
                     INPUT 0,
                     INPUT '',
                     OUTPUT p-msg).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

