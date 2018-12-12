&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Generate Claim PDF file

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
   Modifications:
   12/14/05   fh  3259 Change ref from BOLServerDir to BOLUnixDir for FTP/OCS issue
   04/26/06  jke 20969 Update prep-date when printing claim  
   06/01/06  cel 23481 Modified to not set g-filename.   
   05/29/13  jke 63410 Do not mark claim printed if emailed from the cron  
   03/21/14  djt 65430 create Claims with PDF Include vs Crystal     
   05/21/14  djt 66324 would break out of comment loop when a blank line inserted.
   08/18/14  djt 66945 Display whether material is exposed on claim report
   08/18/14  djt 57595 shorten up HTA Footer
   03/16/15  lcs 68133 Removed any references to Crystal.
   04/29/15  lcs 68168 Renamed appattr CEBOLUnixDir to BOLUnixDir.
   05/20/16  djt 71686 PFE calculation - not enough digits
   06/03/16  djt 71944 Report is incorrect for mults of remaining master.
   07/08/16  djt 71785 Add min/max weight from spec.
   12/30/16  djt 73744 Print Claims in break/slit/footage order
   10/13/17  djt 76339 Part number not always in the comments.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
{includes/i0156.i}
{includes/ttclaimform.i}
/* ***************************  Definitions  ************************** */
define input  parameter pcClaimRows     as character no-undo.
define input  parameter pcPrintMetric   as character no-undo.
define output parameter pcClaimFile     as character no-undo.
define output parameter pcFormat        as character no-undo.

define buffer bClaim for psi.claim.
define buffer   bGroups for AlertGroup.

define variable iLCV    as integer    no-undo.
define variable cEntry  as character  no-undo.
define variable iClaim  as integer    no-undo.
define variable iCounter         as integer    no-undo.
define variable iStartTime       as integer    no-undo.
define variable cClaimList       as character  no-undo.
define variable lMarkPrinted     as logical    no-undo.

/* stuff used for pdf include claim maker */
define variable iLoop as integer no-undo.
define variable iMetEng as integer no-undo.
define variable lTest as logical no-undo.
define variable iPage as integer no-undo.
define variable eSpaceLeft as decimal no-undo.
define variable ePos as decimal no-undo.
define variable iSeq as integer no-undo init 0.
define variable lHonda as logical no-undo.
define variable eSectionSpace as decimal extent 10 no-undo.
define variable iPkgHdrPage as integer no-undo.
define variable cRtn as character no-undo.
define buffer bttClaimHdr for ttClaimHdr.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CreateBuildMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateBuildMap Procedure 
FUNCTION CreateBuildMap RETURNS LOGICAL
  ( input piClaimID as integer, input piRecType as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateClaimHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateClaimHdr Procedure 
FUNCTION CreateClaimHdr RETURNS LOGICAL
    ( input piClaimID as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateClaimItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateClaimItem Procedure 
FUNCTION CreateClaimItem RETURNS LOGICAL
    ( input piClaimID as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateCoilHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreateCoilHdr Procedure 
FUNCTION CreateCoilHdr RETURNS LOGICAL
    ( input piClaim as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSectionSpace) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetSectionSpace Procedure 
FUNCTION SetSectionSpace returns logical
  (  ) FORWARD.

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
         HEIGHT             = 14.95
         WIDTH              = 50.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* message "in new t0262"                 */
/*     view-as alert-box info buttons ok. */

do  iLCV = 1 to num-entries(pcClaimRows,chr(2)) transaction:
    
  assign
      cEntry = entry(iLCV,pcClaimRows,chr(2))
      iClaim = integer(cEntry) no-error.
  
  if error-status:error then
    dynamic-function("setMessage",'Invalid ClaimID ' + cEntry).
  else 
  do:
    find bClaim exclusive-lock 
         where bClaim.Claim-ID eq iClaim no-wait no-error.
    if not available bClaim then
    do:
      if dynamic-function('GetExistingFileName':U,iClaim) ne ? then
      assign
          pcClaimFile = dynamic-function('GetExistingFileName':U,iClaim)
          pcFormat    = 'TEXT'.
      else
      dynamic-function("setMessage",'Cannot find Claim with Claim ID ' + cEntry).
    end.
    else 
    do:
      if bClaim.Legacy eq true then
        run LegacyClaim(bClaim.Claim-ID).
      else 
      do:
        assign
            cClaimList = cClaimList + string(bClaim.Claim-ID) + '!'
            pcFormat = 'pdf'.
      end.

      /* do not mark claim printed if printed from the extranet or emailed from cron */
      lMarkPrinted = if (program-name(2) = "r0532.r" and bClaim.plant-id = 1) or session:client-type eq 'WEBSPEED' then false 
                     else true.

      if lMarkPrinted then do:
          assign 
          bClaim.stat = 2
          bClaim.prep-date = today.

      end.

    end. /* Found the claim */
  end. /* Valid Rowid */
  release bClaim.
end.

if pcFormat = 'pdf' then do:

  run CreatePDFInclude (input trim(cClaimList,'!'),
                        output dataset dsClaimdata).

  run claims/t0736.p(
      input dataset dsClaimData,
      input cClaimList,
      output pcClaimFile,
      output cRtn).

  hDataset:empty-dataset().
end.

if {fn chkMsg} then
  return error replace({fn getUserMsg},chr(3),"~n").
else
  return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreatePDFInclude) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreatePDFInclude Procedure 
PROCEDURE CreatePDFInclude :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
define input parameter pcClaimList as char no-undo.
define output parameter dataset for dsClaimData.

    hDataset:empty-dataset().
    SetSectionSpace().
    do iLoop = 1 to num-entries(pcClaimList,'!'):
         iClaim = integer(entry(iloop,pcClaimList,'!')).
         if not can-find(first claim where claim.claim-id = iClaim) then next.
         iPage = 0 .
         iPkgHdrPage = 0 .
         CreateClaimHdr(iClaim).
         CreateCoilHdr(iClaim).
         CreateClaimItem(iClaim).
         iSeq = iSeq + 1.
         CreateBuildMap(iClaim,(if lHonda then 7 else 5)).   /* footer section */
         for each ttClaimhdr where ttClaimhdr.iclaimID = iClaim:
             ttClaimHdr.cPage = replace(ttClaimHdr.cPage,'YY',trim(string(iPage,'>9'))).
         end.
    end.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FindBreak) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindBreak Procedure 
PROCEDURE FindBreak :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piLength as int.
define input-output parameter pcText as char.
define output parameter pcLine as char.

define variable iLastSpace as integer no-undo.
define variable iLFeed as integer no-undo.
define variable iLoopy as integer no-undo.

    iLastSpace = 10000.
    pcLine = ?.
    iLFeed = index(pcText, chr(10)).
    if iLFeed = 0 then iLFeed = 10000.

    case true:
        when iLFeed lt piLength then do:
            /*Line feed occurs before length limit */
            pcLine = substring(pcText,1,iLFeed - 1).
            pcText = substring(pcText, iLFeed + 1, 2500).
        end.
        when index(substring(pcText,1,piLength), ' ') = 0 then do:
            /* first X characters does not contain a space, just break it */
            pcLine = substring(pcText,1,piLength).
            pcText = substring(pcText,piLength + 1, 2500).
        end.
        when iLFeed = 10000 and length(pcText) le piLength then do:
            pcLine = pcText.
            pcText = ''.
        end.
        otherwise do:
            do iLoopy = min(piLength, length(pctext)) to 1 by -1:
                if substring(pcText, iLoopy, 1) = ' '
                    and iLastSpace = 10000
                    then iLastSpace = iLoopy.
            end.
            pcLine = substring(pcText,1,iLastSpace - 1).
            pcText = substring(pcText, iLastSpace + 1, 2500).
        end.
    end case.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LegacyClaim) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LegacyClaim Procedure 
PROCEDURE LegacyClaim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
  define input parameter piClaim as integer no-undo.
  define variable cClaimFile as character no-undo.
  
  assign cClaimFile = DYNAMIC-FUNCTION('GetExistingFileName':U,piClaim).

  if cClaimFile eq ? then
    run PrintLegacyForm(piClaim).
  else 
    assign pcClaimFile = cClaimFile.
      
  assign pcFormat = 'TEXT'.          
      
  return.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintLegacyForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintLegacyForm Procedure 
PROCEDURE PrintLegacyForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input parameter piClaimID as integer no-undo.
  
  define buffer bPartner for psi.Partner. 
  define buffer bClaim   for psi.Claim.
  define buffer bInv     for psi.Inv.
  
  define variable lMetric as logical no-undo.

  find bClaim no-lock
       where bClaim.Claim-ID eq piClaimID. /* Already verified existence in main program */
      
  find bInv no-lock
       where bInv.inv-id eq bClaim.coil-id no-error.
      
  if not available bInv then
  do:
    dynamic-function("setMessage",'Could not find PSI bInv : ' + string(bClaim.coil-id)).
    return.
  end.

  find bPartner no-lock
       where bPartner.partner-id eq bInv.partner-id no-error.
  if not available bPartner then
  do:
    dynamic-function("setMessage",'Could not find bPartner : ' + string(bInv.partner-id)).
    return.    
  end.
  
  assign lMetric   = bPartner.Met-Eng gt 0.
      
  run reports/r0196.p(input piClaimID,input lMetric) no-error.

  assign pcClaimFile = dynamic-function('getStreamDest').            
       
  return.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CreateBuildMap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateBuildMap Procedure 
FUNCTION CreateBuildMap RETURNS LOGICAL
  ( input piClaimID as integer, input piRecType as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
def var cTypeList as char init 'Page Header,Coil Header,Pkg Detail,Pkg Note,Claim Footer,Pkg Header,HTA Footer'.
  if eSpaceLeft - eSectionSpace[piRecType] < 0.5 then do:
    CreateClaimHdr(iClaim).
  end.
  if piRecType > 1 then eSpaceLeft = eSpaceLeft - eSectionSpace[piRecType].
  create ttClaimMap.
  assign
      ttClaimMap.iClaimID = piclaimID
      ttClaimMap.iSeq = iSeq
      ttClaimMap.iRecType = piRecType
      ttClaimMap.cDescr = entry(piRecType,cTypeList)
      ttClaimMap.ePosition = (if piRecType = 1 then 8.02 else eSpaceLeft + eSectionSpace[piRecType])
      .
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateClaimHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateClaimHdr Procedure 
FUNCTION CreateClaimHdr RETURNS LOGICAL
    ( input piClaimID as int ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
  iPage = iPage + 1.
  iSeq = iSeq + (if iPage = 1 then 1 else 0).
  eSpaceLeft = 8.5 - eSectionSpace[1] .
  if iPage > 1 then do:
     /* copy the existing header info */
     find first bttClaimHdr where bttClaimHdr.iClaimID = piClaimID.
     create ttClaimHdr.
     buffer-copy bttClaimHdr to ttClaimHdr
         assign
         ttClaimHdr.iSeq = iSeq
         ttClaimHdr.cPage = substitute('Page &1 of YY',iPage).
  end.
  else do:
    find first claim no-lock where claim.claim-id = piClaimID no-error.
    find first cntl no-lock where cntl.plant-id = claim.plant-id.
    find first cust no-lock where cust.cust-id = cntl.cust-id no-error.

    create ttClaimHdr.
    assign
        ttClaimHdr.iClaimId = piClaimID
        ttClaimHdr.iSeq = iSeq
        ttClaimHdr.cPlantName = (if avail cust then cust.name else 'Precision Strip')
        ttClaimHdr.cPlantCity = (if avail cust
                       then substitute('&1, &2',cust.city,cust.state)
                       else '')
        ttClaimHdr.cPlantAddr = cust.addr-1
        ttClaimHdr.cPage = substitute('Page &1 of YY',iPage)
        ttClaimHdr.dClaimDate = today
        ttClaimHdr.cAttn = claim.attention
        .
    find first inv no-lock
        where inv.inv-id = claim.coil-id no-error.
    if avail inv then find first rcpt no-lock
        where rcpt.rcpt-id = inv.rcpt-id no-error.
    if avail rcpt then find first cust no-lock
        where cust.cust-id = rcpt.mill-id no-error.
    if avail cust then assign ttClaimHdr.cMillName = substitute('&1  &2',cust.cust-id, cust.name).
    find first partner no-lock where partner.partner-id = inv.partner-id no-error.
    if avail partner then find first cust no-lock
        where cust.cust-id = partner.owner-id no-error.
    if avail cust then assign ttClaimHdr.cOwnerName = substitute('&1  &2',cust.cust-id, cust.name).
    lHonda = (if cust.entityid = 3 then yes else no).
    if avail partner then find first cust no-lock
        where cust.cust-id = partner.user-id no-error.
    if avail cust then assign ttClaimHdr.cUserName = substitute('&1  &2',cust.cust-id, cust.name).
  end.
  createBuildMap(piClaimID, 1).
  iSeq = iSeq + (if iPage = 1 then 0 else 1).
    RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateClaimItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateClaimItem Procedure 
FUNCTION CreateClaimItem RETURNS LOGICAL
    ( input piClaimID as int ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
  define variable cTemp as character no-undo.
  define variable lFirst as logical no-undo.
  define variable cText as character no-undo.
  define variable cLine as character no-undo.
  define variable iNoteLines as integer no-undo.
  define variable cExposed as character no-undo.
    for each claim-item no-lock
        where claim-item.claim-id = piClaimID
        by claim-item.break-no
        by claim-item.slit-no
        by claim-item.start-foot
        by claim-item.seq:

        find first inv no-lock where inv.inv-id = claim-item.inv-id no-error.
        if avail inv then find first rcpt no-lock where rcpt.rcpt-id = inv.rcpt-id no-error.
        if avail inv then find first part no-lock where part.part-id = inv.part-id no-error.
        if avail part then find first type no-lock where type.type-id = part.type-id no-error.
        if avail inv then find first partner no-lock where partner.partner-id = inv.partner-id no-error.
        find first mult no-lock
            where mult.coil-id = (if claim-item.coil-id ne ?
                                  and claim-item.coil-id > 0
                                  then claim-item.coil-id
                                  else inv.rcpt-id)
            and mult.break-no = claim-item.break-no
            and mult.slit-no = claim-item.slit-no no-error.
        iSeq = iSeq + 1 .
        find first damagecode no-lock where damagecode.damagecode = claim-item.damage-code no-error.
        create ttPkgDetail.
        assign
          ttPkgDetail.iClaimId = piClaimID
          ttPkgDetail.iSeq = iSeq
          ttPkgDetail.iInvID = claim-item.inv-id
          ttPkgDetail.cCaseNo = (IF AVAIL inv AND inv.case-no NE ? THEN inv.case-no ELSE "")
          ttPkgDetail.dClaimDate = claim-item.act-date
          ttPkgDetail.iBrk = claim-item.break-no
          ttPkgDetail.iSlit = claim-item.slit-no
          ttPkgDetail.cDamageDescr = (if avail damagecode then damagecode.descr else '')
          ttPkgDetail.cSize = substitute('&1 X &2&3&4 &5&6&7 &8 &9',
                                       trim(dynamic-function('to-in-mm',part.gauge,'gauge',6,iMeteng)),
                                       trim(dynamic-function('to-in-mm',part.wdth,'wdth',6,iMeteng)),
                                       (if part.lngth gt 0 then ' X ' else ''),
                                       (if part.lngth gt 0 then
                                                           trim(dynamic-function('to-in-mm',part.lngth,'wdth',6,iMeteng))
                                                           else ''),
                                       (if iMeteng = 0 then 'In' else 'mm'),
                                       (if avail mult and mult.linear-ft > 0 then ' X ' else ''),
                                       (if avail mult and mult.linear-ft > 0 then trim(dynamic-function('to-ft-m',mult.linear-ft,iMeteng)) else ''),
                                       (if avail mult and mult.linear-ft > 0 then (if iMeteng = 0 then 'Ft' else 'M') else ''))
                                        
          ttPkgDetail.cPFE = substitute('&1 &2',
                                         trim(string(decimal(dynamic-function('to-in-mm',claim-item.PositionFromEdge,'wdth',6,iMeteng)),'>>>9.99')),
                                         (if iMetEng = 0 then 'In' else 'mm'))
          ttPkgDetail.cOwnerOrder = inv.own-order
          ttPkgDetail.cStartPos = substitute('&1 &2',
                                         dynamic-function('to-ft-m',claim-item.start-foot,iMetEng),
                                         (if iMetEng = 0 then 'Ft' else 'M'))
          ttPkgDetail.cEndPos = substitute('&1 &2',
                                         dynamic-function('to-ft-m',claim-item.end-foot,iMetEng),
                                         (if iMetEng = 0 then 'Ft' else 'M'))
          ttPkgDetail.cSample = (if claim-item.sample = yes then 'Y' else 'N')
          ttPkgDetail.cDefectSize = (if claim-item.defectSize > 0 then string(claim-item.defectSize) else '')
          ttPkgDetail.cAmount = substitute('&1 &2',
                                        dynamic-function('to-lb-kg', claim-item.weight, iMetEng),
                                        (if iMetEng = 0 then 'Lb' else 'Kg'))
                                        
          cExposed = ''
          .
          if claim-item.defect-uom ne 0 and claim-item.defectSize > 0 then do:
              find first uom no-lock where uom.uomid = claim-item.defect-uom no-error.
              if avail uom then ttPkgDetail.cDefectSize = substitute('&1 &2',ttPkgDetail.cDefectSize, uom.abbrev).
          end.
          select cust.name into cTemp
              from cust
              where cust.cust-id = partner.user-id.
          ttPkgDetail.cEndUser = cTemp.
          select descr into cTemp
            from choice
            where choice.field-no = 316 and choice.val = claim-item.severity.
          ttPkgDetail.cSeverity = cTemp.
          select descr into ctemp
            from choice
            where choice.field-no = 414 and choice.val = claim-item.positioncode.
          ttPkgDetail.cPosition = cTemp.
          select descr into cTemp
            from choice
            where choice.field-no = 317 and choice.val = claim-item.defect-side.
          ttPkgDetail.cOrient = cTemp.
          select descr into cTemp
            from choice
            where choice.field-no = 415 and choice.val = claim-item.RepeatCode.
          ttPkgDetail.cRepeat = cTemp.
          if claim-item.repeatCode = 1 then do:
              find first uom where uom.uomid = claim-item.repeat-uom no-lock no-error.
              ttPkgDetail.cRepeat = substitute('&1 &2 &3',
                                               ttPkgDetail.cRepeat,
                                               string(claim-item.repeating),
                                               (if avail uom then uom.abbrev else '')).
          end.
          find first hold no-lock
              where hold.inv-id = claim-item.inv-id
              and hold.hold-date = claim-item.act-date no-error.
          if not avail hold then
              find first hold no-lock where hold.inv-id = claim-item.inv-id no-error.
          ttPkgDetail.iHoldCode = (if avail hold then hold.hold-code else 0).
          find first work-item no-lock
              where work-item.order-id = inv.order-id
              and work-item.part-id = inv.part-id no-error.
          if avail work-item then
              find first spec no-lock
              where spec.spec-id = work-item.spec-id no-error.
          if avail spec then
              find first choice no-lock
              where choice.field-no = 34
              and choice.val = spec.exposed no-error.
          if avail choice then cExposed = (if choice.misc2 = 'E' then 'Exposed Part.' else 'Unexposed Part.').
          if avail spec and spec.min-mult-wt ne ? and spec.max-mult-wt ne ? and spec.max-mult-wt > 0 then
              cExposed = substitute('&1&2Mult Wt: &3 &4(min) - &5 &4(max)',
                                    cExposed,
                                    (if cExposed = '' then '' else '     '),
                                    trim(dynamic-function('to-lb-kg', spec.min-mult-wt, iMetEng)),
                                    (if iMetEng = 0 then 'Lb' else 'Kg'),
                                    trim(dynamic-function('to-lb-kg', spec.max-mult-wt, iMetEng))).
          cExposed = substitute('&1    Part No: &2    MSA #: &3',
                                    cExposed,
                                    (IF part.part-no = ? THEN '' ELSE part.part-no),
                                    (IF part.msa-no = ? THEN '' ELSE part.msa-no)).
          /* How many note lines we going to need? Need to get the note and data all on same page*/
          iNoteLines = 0.
          if cExposed > '' or (claim-item.comments ne ? and claim-item.comments > '') then do:
              cText = substitute('&1&2&3',
                                 cExposed,
                                 (if cExposed > '' and (claim-item.comments ne ? and claim-item.comments > '') then chr(10) else ''),
                                 (if claim-item.comments ne ? then claim-item.comments else '')).
              lFirst = yes.
              repeat:
                  run FindBreak(input 140   /* characters per line*/
                                ,input-output cText
                                ,output cLine).
                  if cLine = ? or (cLine = '' and cText = '') then leave.
                  iNoteLines = iNoteLines + 1.
              end.
          end.

          /* If first inv ID on this page, put a header */
          if iPkgHdrPage ne iPage then do:
              createBuildMap(piClaimID, 6).
              iPkgHdrPage = iPage .
              iSeq = iSeq + 1 .
              ttPkgdetail.iSeq = iSeq .
          end.
          else if (eSpaceLeft - eSectionSpace[3] - (iNoteLines * eSectionSpace[4])) < 0.5  then do:
              /* Needs to roll to the next page */
              CreateClaimHdr(iClaim).
              createBuildMap(piClaimID,1).
              iSeq = iSeq + 1.
              createBuildMap(piclaimId,6).
              iSeq = iSeq + 1.
              ttPkgDetail.iSeq = iSeq.
          end.
          createBuildMap(piClaimID, 3).
          if cExposed > '' or (claim-item.comments ne ? and claim-item.comments > '') then do:
              cText = substitute('&1&2&3',
                                 cExposed,
                                 (if cExposed > '' and (claim-item.comments ne ? and claim-item.comments > '') then chr(10) else ''),
                                 (if claim-item.comments ne ? then claim-item.comments else '')).
              lFirst = yes.
              repeat:
                  run FindBreak(input 140   /* characters per line*/
                                ,input-output cText
                                ,output cLine).
                  if cLine = ? or (cLine = '' and cText = '') then leave.
                  iSeq = iSeq + 1.
                  create ttComment.
                  assign
                      ttComment.iClaimID = piClaimID
                      ttComment.iSeq = iSeq
                      ttComment.cText = cLine
                      ttComment.cLabel = (if lFirst then 'Comments:' else '')
                  .
                  lFirst = no.
                  createBuildMap(piClaimID, 4).
              end.
              eSpaceLeft = eSpaceLeft - 0.1 .
          end.
    end.
    RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateCoilHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreateCoilHdr Procedure 
FUNCTION CreateCoilHdr RETURNS LOGICAL
    ( input piClaim as int ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

    find first claim no-lock where claim.claim-id = piClaim no-error.
    find first inv no-lock where inv.inv-id = claim.coil-id no-error.
    if avail inv then find first rcpt no-lock where rcpt.rcpt-id = inv.rcpt-id no-error.
    if avail inv then find first part no-lock where part.part-id = inv.part-id no-error.
    if avail part then find first type no-lock where type.type-id = part.type-id no-error.
    if avail inv then find first partner no-lock where partner.partner-id = inv.partner-id no-error.
    if avail rcpt then find first scac where scac.scac-code = rcpt.scac-code no-error.
    if not avail inv then return false.
    case pcPrintMetric:
        when 'default' then iMetEng = partner.met-eng.
        when 'Imperial' then iMetEng = 0.
        when 'Metric' then iMetEng = 1.
        otherwise iMetEng = partner.met-eng.
    end case.

    iSeq = iSeq + 1 .
    create ttCoilHdr.
    assign
          ttCoilHdr.iClaimId = piClaim
          ttCoilHdr.iSeq = iSeq
          ttCoilHdr.iMetEng = iMetEng
          ttCoilHdr.cPrepBy = claim.author
          ttCoilHdr.iOrderID = claim.order-id
          ttCoilHdr.cCustCode = claim.cust-code
          ttCoilHdr.cNetWt = substitute('&1 &2',
                                        dynamic-function('to-lb-kg', inv.net-wt, iMet),
                                        (if iMet = 0 then 'Lb' else 'KG')) 
          ttCoilHdr.cTypeDescr = type.descr
          ttCoilHdr.cOwnerOrder = inv.own-order
          ttCoilHdr.cUserOrder = inv.user-order
          ttCoilHdr.cMillOrder = inv.mill-order
          ttCoilHdr.cRef1 = inv.ref-no[1]
          ttCoilHdr.cRef2 = inv.ref-no[2]
          ttCoilHdr.iInvID = inv.inv-id
          ttCoilHdr.cOwnInvID = rcpt.own-inv-id
          ttCoilHdr.cPartNo = part.part-no
          ttCoilHdr.cSize = substitute('&1 X &2&3&4&5&6 &7',
                                       trim(dynamic-function('to-in-mm',part.gauge,'gauge',6,iMet)),
                                       trim(dynamic-function('to-in-mm',part.wdth,'wdth',6,iMet)),
                                       (if part.lngth gt 0 then ' X ' else ''),
                                       (if part.lngth gt 0 then
                                                           trim(dynamic-function('to-in-mm',part.lngth,'wdth',6,iMet))
                                                           else ''),
                                       (if part.lngth2 gt 0 then ' X ' else ''),
                                       (if part.lngth2 gt 0 then
                                                            trim(dynamic-function('to-in-mm',part.lngth2,'wdth',6,iMet))
                                                            else ''),
                                       (if iMet = 0 then 'In' else 'mm'))
          ttCoilHdr.cShipper = rcpt.bill-o-lad
          ttCoilHdr.dRcptDate = rcpt.rcpt-date
          ttCoilHdr.cPrevID = rcpt.prev-inv-id
          ttCoilHdr.cHeatLot = rcpt.heat-lot
          ttCoilHdr.cCaseN  = inv.case-no
          ttCoilHdr.cLength = substitute('&1 &2',
                                         dynamic-function('to-ft-m',inv.linear-ft,iMet),
                                         (if iMet = 0 then 'Ft' else 'M'))
          ttCoilHdr.cCarrier = (if avail scac then scac.name else rcpt.scac-code).

    CreateBuildMap(iClaimID, 2).
    RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetSectionSpace) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetSectionSpace Procedure 
FUNCTION SetSectionSpace returns logical
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
assign
    eSectionSpace[1] = 1.87
    eSectionSpace[2] = 1.10
    eSectionSpace[3] = 0.5
    eSectionSpace[4] = 0.17
    eSectionSpace[5] = 2.4
    eSectionSpace[6] = 0.40
    eSectionSpace[7] = 2.5
    eSectionSpace[8] = 0.0
    esectionSpace[9] = 0.0
    eSectionSpace[10] = 0.0
    .
  RETURN true.   /* Function return value. */


end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

