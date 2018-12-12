&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0742.p
    Purpose     : Format Novelis QR Feed Forward instructions
    
    Date     By   Issue  Remark
  ========  ===  ======  ===============================================
  06/16/14  jhl   66445  New
  06/19/14  jhl   66445  Rick and Bruce want "." instead of newlines ~n between footages.
  09/02/14  jhl   67040  INput format is completely changing.
  09/18/14  jhl   67040  Remove trailing $ at end of scanned code.
  10/09/14  jhl   67040  Changed logic to invert the numbers from Novelis to coil direction Line Operator sees.
                         Also added input parameter inv-id so that we can create VoiceInspect records.
  10/12/14  jhl   67040  Per Karen,  removed VoiceInspect record creation.  
  10/20/14  jhl   67040  Per Karen,  Spank,  Rick:  they would like it to read with bad and good footages.                     
  11/10/14  jhl   67446  Per Karen,   Novelis will be sending Sample instructions in inbound QR code now.   If 1st 
                         flaw code = 7 then we need to take samples otherwise  we do not.     For know just add text
                         if sampling is required.
  11/12/14  jhl   67474  Not calculating bad footage correctly if first part of coil is good.                       
  11/24/14  jhl   67547  Uncommented logic to say no samples required.  Aso add if no ttQR records  (no flaws and no code = 7).
  12/01/14  jhl   67574  If send 7 flaw code for samples then do NOT add instruction that no samples required later.
  03/19/15  djt   68127  Schema change to add field ShowToCust
  10/14/15  jhl   69879  Changed to handle Alcoa QR code specs.
  11/16/15  jhl   69879  Aloca uses heat-lot so added another parameter 
  01/25/16  jhl   70494  Bruce Thobe would like to change the "Bad" to reflect inspect surface/stain for Alcoa for flaw codes 5 and 6.
  09/23/16  djt   72806  Alcoa Davenport QR project 
  04/19/17  djt   74318  Alcoa Mill defect carry-thru project
  10/16/17  djt   76340  Arconic TN changes to Mill QR on inbound coils
  11/22/17  djt   76623  Novelis incorporating specific damage codes
  12/07/18  djt   76770  Novelis verbiage changed.
  12/08/17  djt   76770  Logic error evaluating different mill conditions
  02/27/18  djt   77221  Novelis is changing their barcode format.
  04/12/18  djt   77820  Novelis change in positioning of flaws.
  05/15/18  djt   78098  Novelis added description for their flaw code 7
  05/24/18  djt   78203  Novelis added format 4, includes disposition code
  06/19/18  djt   78362  Novelis r4 code populated multiple sample instructions
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input parameter pcINQR as character.
define input parameter piInv like inv.inv-id.
define output parameter pcOutQR as character.
define output parameter pcCoilId as character.
define output parameter pUseHeat as logical init no.

def var giCount as int no-undo.
def var i as int no-undo.
def var gcChar as character no-undo.
def var gcDelimiter as character no-undo init "$".
def var giFirstFt as int no-undo.
def var giNextFt as int no-undo.
def var gDecdm2ft as decimal init .32808399.
def var iTotLength as int no-undo.
def var iStart as int no-undo.
def var iPrevHi as int no-undo.
def var lAlcoa as logical no-undo init no.
def var lNovelis as logical no-undo init no.
def var cNovelisSuppID as character no-undo init "D5X8J".
def var cAlcoaSuppId as character no-undo init "CDD0E".
def var cDavenportID as character no-undo init "A247L".
def var cSuppId as character no-undo format "x(5)".
DEFINE VARIABLE iFlawCode AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNovelisFormat AS INTEGER     NO-UNDO.
define variable cDispoCode as char no-undo.
DEFINE VARIABLE cAction AS CHARACTER   NO-UNDO.


def temp-table ttQR
   field LoBad as int
   field HiBad as int
   field FlawDesc as char
   field FlawCode as int
   field cDispoCode as char
   index BYLoBad LoBad.

def var vTime as int.

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
         HEIGHT             = 14.95
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
                                  
vTime = integer(replace(string(time,"HH:MM"),":","")).
iNovelisFormat = 0.
iStart = 0.
pcCoilID = ''.

if pcINQR = ? or pcINQR = "" then do:
    assign pcOUTQR = "".
    return.
end.

assign pcOUTQR = "QRFF=".

/* Brazing will not have traditional QR code */
if index(pcINQR,"@") > 0 then do:
    assign pcCoilId  = entry(1,pcINQR,"@")
           pcOUTQR = pcOUTQR + " NO FF ".
    return.
end.
if num-entries(pcINQR,")") > 1 then
    pcINQR = entry(2, pcINQR, ")").

if substring(pcINQR,23,5) = cDavenportID then do:
    run doDavenport.
    return.
end.
if length(entry(1,pcINQR,gcDelimiter)) = 71
    and (substring(pcInqr,28,3) = 'KGN'
         or substring(pcInqr,28,3) = 'OSW'
         or substring(pcInqr,28,3) = 'GTR') then do:
    iNovelisFormat = int(trim(substring(pcINQR,1,2))).
    lNovelis = yes.
    pcCoilID = trim(substring(pcINQR,31,10)).
    iTotLength = round(int(substring(pcINQR,47,6)) * gDecdm2ft, 0).
    iStart = round(int(substring(pcINQR,41,6)) * gDecdm2ft, 0).
    if substring(pcINQR,70,1) = 'H' then pcOutQR = 'QRFF= Head Samples Required.'.
    if substring(pcINQR,70,1) = 'T' then pcOutQR = 'QRFF= Tail Samples Required.'.
    if substring(pcINQR,70,1) = 'B' then pcOutQR = 'QRFF= Head and Tail Samples Required.'.
    if substring(pcINQR,70,1) = 'N' then pcOutQR = 'QRFF= No Samples Required.'.
    if substring(pcINQR,71,1) = 'U' or substring(pcINQR,71,1) = '9' then pcOutQR = pcOutQR + '  UnderPay.'.
    if substring(pcINQR,71,1) = 'O' or substring(pcINQR,71,1) = '8' then pcOutQR = pcOutQR + '  OverPay.'.
end.
giCount = num-entries(pcINQR,gcDelimiter) - 1.

do i = 1 to giCount:
   if i = 1 and iNovelisFormat > 0 then next.
   gcChar = entry(i,pcINQR,gcDelimiter).
   if i = 1 and iNovelisFormat = 0 then  do:
       assign pcCoilId   = left-trim(substring(gcChar,28,10),"")
                 cSuppID = substring(gcChar,23,5)
              iTotLength = round(int(substring(gcChar,38,5)) * gDecdm2ft,0).

       if cSuppID = cAlcoaSuppID then do:
           assign iStart = iTotLength
                  pUseHeat = yes.
                  lAlcoa = yes.
       end.
       else lAlcoa = no.
       if cSuppID = cNovelisSuppID then lNovelis = yes.
/*        find rcpt where rcpt.rcpt-id = piInv no-lock no-error. */
/*        if available rcpt then do:                             */
/*            if rcpt.own-inv-id <> pcCoilId then return.        */
/*        end.                                                   */
/*        else do:                                               */
/*          find asn where asn.inv-id = piInv no-lock no-error.  */
/*          if not available asn then return.                    */
/*          if asn.own-inv-id <> pcCoilID then return.           */
/*        end.                                                   */
   end.
   else if i = 2 and iNovelisFormat = 0 then do:
       if not lAlcoa then do:
          iStart = round(int(substring(gcChar,1,5)) * gDecdm2ft,0).
       end.
       if substring(gcChar,11,1) = "8" then 
              pcOUTQR = pcOUTQR + " OverPay  ".
       else pcOUTQR = pcOUTQR + " UnderPay ".
   end.
   else do:
       /* TIWO 67446 Samples */
       if substring(gcChar,11,1) = "7" and iNovelisFormat = 0 then 
           assign pcOUTQR = pcOUTQR + " Head and Tail samples required ".
       else do:
           /* TIWO 67446 and 67547 Leave commented out now until we process  all old coils before Novelis turns this on. */
         if i = 3 and iNovelisFormat = 0 then assign pcOUTQR = pcOUTQR + " No samples required ".
         
         giFirstFt = round(int(substring(gcChar,1,5)) * gDecdm2ft,0).
         giNextFt = round(int(substring(gcChar,6,5)) * gDecdm2ft,0).
         iFlawCode = int(substring(gcChar,11,1)).
         cDispoCode = ''.
         if iNovelisFormat > 0 then assign
             giFirstFt = min(round(int(substring(gcChar,1,6)) * gDecdm2ft,0),round(int(substring(gcChar,7,6)) * gDecdm2ft,0))
             iFlawCode = int(substring(gcChar,13,1))
             giNextFt = max(round(int(substring(gcChar,1,6)) * gDecdm2ft,0),round(int(substring(gcChar,7,6)) * gDecdm2ft,0)).
         if iNovelisFormat > 3 then
             cDispoCode = substring(gcChar,14,1).

/*          message gifirstft "  " ginextft. pause.                         */
/*          message iTotLength " " iStart " "  (iStart - giFirstFt). pause. */
         create ttQR.
         assign ttQR.LoBad = maximum(0,iStart - giNextFt)
                ttQR.HiBad = minimum(iTotLength, iStart - giFirstFt)
                ttQR.FlawCode = iFlawCode
                ttQR.cDispoCode = cDispoCode.
         case true:
             when lAlcoa then do:
                case iFlawCode:
                    when 5 then assign ttQR.FlawDesc = "Inspect Surface".
                    when 6 then assign ttQR.FlawDesc = "Inspect Stain".
                    otherwise assign ttQR.FlawDesc = "Bad".
                end case.
             end.
      
             when lNovelis then do:
               cAction = 'Inspect For'.
               if cDispoCode = 'B' then cAction = 'Remove'.
               if cDispoCode = 'X' then cAction = 'Remove'.
               case iFlawCode:
                   when 1 then assign ttQR.FlawDesc = "Bad".
                   when 2 then assign ttQR.FlawDesc = substitute("&1 Scratches", cAction).
                   when 3 then assign ttQR.FlawDesc = substitute("&1 Dents", cAction).
                   when 4 then assign ttQR.FlawDesc = substitute("&1 Laminations", cAction).
                   when 5 then assign ttQR.FlawDesc = substitute("&1 Roll Marks", cAction).
                   when 6 then assign ttQR.FlawDesc = substitute("&1 Staining", cAction).
                   when 7 then assign ttQR.FlawDesc = substitute("&1 Rolled in Debris", cAction).
                   otherwise assign ttQR.FlawDesc = "Bad".
               end case.
             end.
             otherwise do:
                 assign ttQR.FlawDesc = "Bad".
             end.
         end case.
       end.
   end.
end.

/*  If only 2 entries in QR code no ttQR records created,  that means no flaws reported. */


find first ttQR no-lock no-error.
if not available ttQR then do:
    if giCount < 3 and iNovelisformat = 0 then  /* TIWO 67574 only say no sample if no flash and no 7 sent */  
      if not lAlcoa then assign pcOUTQR = pcOUTQR + " No samples required".
    assign pcOUTQR = pcOUTQR + " NO FF ".
    return.
end.

/* For Alcoa Davenport, create MillDefect Records that will carry thru to mults */
if lAlcoa then do:
    /* might be a re-scan, so clear any existing and re-load */
    if piInv ne ? and piInv > 0 then do:
        for each MillDefect
            where MillDefect.own-inv-id = pcCoilID
            and MillDefect.rcpt-id = 0:
            delete MillDefect.
        end.
        for each MillDefect
            where Milldefect.rcpt-id = piInv:
            delete MillDefect.
        end.
    end.
    else do:
        for each Milldefect
            where Milldefect.rcpt-id = 0
            and Milldefect.own-inv-id = pcCoilID:
            delete Milldefect.
        end.
    end.
    for each ttQR by ttQR.LoBad:
        create MillDefect.
        assign
            Milldefect.rcpt-id = (if piInv = ? or piInv = 0 then 0 else piInv)
            Milldefect.own-inv-id = pcCoilID
            Milldefect.DefectID = next-value(DefectID)
            Milldefect.start-foot = ttQR.LoBad
            Milldefect.end-foot = ttQR.HiBad
            Milldefect.damage-code = (if ttQR.flawCode = 6 then 21 else 36).
    end.
end.

/* for each ttqr: */
/*     disp ttqr. */
/* end.           */
iPrevHi = 0.
for each ttQR  break by ttQR.LoBad:          
    if first(ttQR.LoBad) then do:
        if ttQR.LoBad <= 1 then
             pcOUTQR = pcOUTQR + " " + ttQR.FlawDesc + " first " + string(ttQR.HiBad) + " ft.".
        else 
            pcOUTQR = pcOUTQR + " Good first " + string(ttQR.LoBad) + " ft." +  " " + ttQR.FlawDesc + " next " + string(ttQR.HiBad - ttQR.LoBad) + " ft.".
        if last(ttQR.LoBad) and ttQR.HiBad < iTotLength then
            pcOUTQR = pcOUTQR + " Good last " + string (iTotLength - ttQR.HiBad).
    end.
    else do:
        pcOUTQR = pcOUTQR + " Good next " + string(ttQR.LoBad - iPrevHi) + " ft.".
        if last(ttQR.LoBad) then do:
           if ttQR.HIBad < iTotLength then
             pcOUTQR = pcOUTQR + " " + ttQR.FlawDesc + " next " + string(ttQR.HiBad - ttQR.LoBad) + " ft." + " Good last " + string(iTotLength - ttQR.HiBad) + " ft.".
           else pcOUTQR = pcOUTQR + " " + ttQR.FlawDesc + " last " + string(ttQR.HiBad - ttQR.LoBad) + " ft.".
        end.
        else do:
            pcOUTQR = pcOUTQR + " " + ttQR.FlawDesc + " next " + string(ttQR.HiBad - ttQR.LoBad) + " ft.".
        end.
    end.

    iPrevHi = ttQR.HiBad.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-doDavenport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doDavenport Procedure 
PROCEDURE doDavenport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cHeat AS CHARACTER   NO-UNDO.
pcINQR = substr(pcINQR,3).

giCount = num-entries(pcINQR,gcDelimiter) - 1.

do i = 1 to giCount:
   gcChar = entry(i,pcINQR,gcDelimiter).

   if i = 1 then  do:
       assign cHeat = left-trim(substring(gcChar,28,8),"")  /* not per spec, but alcoa davenport is adding a '01' suffix to the heat*/
              cSuppID = substring(gcChar,23,5)
              iStart = 1
              pUseHeat = yes
              iTotLength = round(int(substring(gcChar,38,5)) * gDecdm2ft,0).

       find first rcpt no-lock
           where rcpt.heat-lot = cHeat
           and rcpt.owner-id = 31
           and rcpt.rcpt-date > (today - 350) no-error.

        if avail rcpt then assign
            pUseHeat = no
            pcCoilId = rcpt.own-inv-id.

        if not avail rcpt then do:
            find first asn no-lock
                where asn.owner-id = 31
                and asn.heat-lot = cHeat no-error.
             if avail asn then assign
                 pUseheat = no
                 pcCoilId = asn.own-inv-id.

        end.

        if pcCoilID > '' then assign pcOUTQR = "QRFF= Overpay. Head and Tail Samples required. ".

/*        find rcpt where rcpt.rcpt-id = piInv no-lock no-error. */
/*        if available rcpt then do:                             */
/*            if rcpt.own-inv-id <> pcCoilId then return.        */
/*        end.                                                   */
/*        else do:                                               */
/*          find asn where asn.inv-id = piInv no-lock no-error.  */
/*          if not available asn then return.                    */
/*          if asn.own-inv-id <> pcCoilID then return.           */
/*        end.                                                   */
   end.
   else do:
         /* per Bruce thobe, defects are listed from the OD and are
            in the order we will encounter them                     */

         giFirstFt = round(int(substring(gcChar,1,5)) * gDecdm2ft,0).
         giNextFt = round(int(substring(gcChar,6,5)) * gDecdm2ft,0).

/*          message gifirstft "  " ginextft. pause.                         */
/*          message iTotLength " " iStart " "  (iStart - giFirstFt). pause. */
         create ttQR.
         assign ttQR.LoBad = maximum(0, giFirstFt)
                ttQR.HiBad = minimum(iTotLength, giNextFt).

         case int(substring(gcChar,11,1)):
             when 5 then assign ttQR.FlawDesc = "Inspect Surface".
             when 6 then assign ttQR.FlawDesc = "Inspect Stain".
             otherwise assign ttQR.FlawDesc = "Bad".
             end case.
/* Per Karen,   we do not want to create voiceinpect records at this time  */
/*        find first choice where choice.field-no = 690 */
/*            and choice.misc1 = substring(gcChar,11,1) */
/*            and choice.misc2 <> ""  no-lock no-error. */
/*                                                      */
/*        create VoiceInspect.                                                      */
/*        assign VoiceInspect.InspectId   = next-value(InspectID)                   */
/*               VoiceInspect.damage-code = if avail choice then choice.val else 40 */
/*               VoiceInspect.logon       = "AUTO"                                  */
/*               VoiceInspect.coil-id     = piInv                                   */
/*               VoiceInspect.start-foot  = giFirstFt                               */
/*               VoiceInspect.end-foot    = giNextFt                                */
/*               VoiceInspect.InspectDate = today                                   */
/*               VoiceInspect.InspectTime = vTime.                                  */
   end.
end.

/*  If only 2 entries in QR code no ttQR records created,  that means no flaws reported. */
find first ttQR no-lock no-error.
if not available ttQR then do:
    if giCount = 1 then  
      assign pcOUTQR = pcOUTQR + " NO FF ".
    return.
end.


/* for each ttqr: */
/*     disp ttqr. */
/* end.           */
iPrevHi = 0.
for each ttQR  break by ttQR.LoBad:          
    if first(ttQR.LoBad) then do:
        if ttQR.LoBad <= 1 then
             pcOUTQR = pcOUTQR + " " + ttQR.FlawDesc + " first " + string(ttQR.HiBad) + " ft.".
        else 
            pcOUTQR = pcOUTQR + " Good first " + string(ttQR.LoBad) + " ft." +  " " + ttQR.FlawDesc + " next " + string(ttQR.HiBad - ttQR.LoBad) + " ft.".
        if last(ttQR.LoBad) and ttQR.HiBad < iTotLength then
            pcOUTQR = pcOUTQR + " Good last " + string (iTotLength - ttQR.HiBad).
    end.
    else do:
        pcOUTQR = pcOUTQR + " Good next " + string(ttQR.LoBad - iPrevHi) + " ft.".
        if last(ttQR.LoBad) then do:
           if ttQR.HIBad < iTotLength then
             pcOUTQR = pcOUTQR + " " + ttQR.FlawDesc + " next " + string(ttQR.HiBad - ttQR.LoBad) + " ft." + " Good last " + string(iTotLength - ttQR.HiBad) + " ft.".
           else pcOUTQR = pcOUTQR + " " + ttQR.FlawDesc + " last " + string(ttQR.HiBad - ttQR.LoBad) + " ft.".
        end.
        else do:
            pcOUTQR = pcOUTQR + " " + ttQR.FlawDesc + " next " + string(ttQR.HiBad - ttQR.LoBad) + " ft.".
        end.
    end.

    iPrevHi = ttQR.HiBad.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

