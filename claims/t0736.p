&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : invoicing/t0736.p
    Purpose     : generate Claim from passed in ProDataSet
                  uses pdfInclude to produce the pdf file.
   History:                           
   Date     By   Issue  Remark
 ========  ===  ======  =================================================
 04/17/14  djt   65430  new
 07/31/14  djt   66816  prevent null fields from reaching PDFInclude
 08/18/14  djt   57595  shortened up HTA Footer section
 ------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{includes/ttclaimform.i}

define input parameter dataset for dsClaimData.
define input parameter pcClaimList as char.
define output parameter pcClaimPath as char.
define output parameter pcRtnMsg as char.

define variable cFilePath as character no-undo.
define variable cLogofile as character no-undo.
define variable iCurrClaim as integer no-undo init 0.
define variable iClaim as integer no-undo.
define variable lTest as logical no-undo.
define variable iMetEng as integer no-undo init 0.
define variable cMetVal as character no-undo.
define variable iLoop as integer no-undo.
define variable cVal as character no-undo.
define variable eVal as decimal no-undo.
define variable eLabelX as decimal no-undo init 2.5.
define variable cNotePrefix as character no-undo init ''.
define variable eLineFrom as decimal no-undo.
define variable eLineTo as decimal no-undo.

define temp-table ttColumns
    field iSeq as int
    field iRow as int
    field eWidth as dec
    field eCellHeight as dec
    field cLabel as char
    field cVar as char
    field eLeftEdge as dec.


{includes/pdf_inc.i "NOT SUPER"}

def stream test.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-MakeBox) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MakeBox Procedure 
FUNCTION MakeBox RETURNS LOGICAL
  ( input peX as decimal,
    input peY as decimal,
    input peWidth as decimal,
    input peHeight as decimal )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PointVal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PointVal Procedure 
FUNCTION PointVal RETURNS INTEGER
    ( input peCol as decimal)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TextPair) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TextPair Procedure 
FUNCTION TextPair RETURNS LOGICAL
  ( input pcLabel as char,
    input pcValue as char,
    input peXPos as decimal,
    input peYPos as decimal )  FORWARD.

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
         HEIGHT             = 14.48
         WIDTH              = 57.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
find first ttClaimMap use-index StdIndex no-error.
if not avail ttClaimMap then do:
    assign pcRtnMsg = '** No data was transfered to PDF generator t0736.p'.
    return.
end.
if opsys = 'win32' then do:
    cfilePath = session:temp-directory.
end.
else do:
    find first appattr no-lock where appattr.attrkey = 'ReportOutputDir' no-error.
    cFilePath = (if avail appattr then appattr.appstr else '').
end.

if cFilePath = '' then do:
    assign pcRtnMsg = '** could not determine file path in t0736 Claim generator.'.
    return.
end.

run SetColumns.
do iClaim = 1 to num-entries(pcClaimList, "!"):
    if iClaim = 1 then run NewFile.
    iCurrClaim = integer(entry(iClaim,pcClaimList,'!')).
    for each ttClaimMap
        where ttClaimMap.iClaimId = iCurrClaim
        use-index stdIndex:

        case true:
        when ttClaimMap.iRecType = 1 then do:
            run BuildDocHeader(input ttClaimMap.iClaimID,
                               input ttClaimMap.iSeq).
        end.
        when ttClaimMap.iRecType = 2 then do:
            RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",9.0).
            run BuildCoilHdr(input ttClaimMap.iClaimID,
                                input ttClaimMap.iSeq,
                                input ttClaimMap.ePosition).
        end.
        when ttClaimMap.iRecType = 3 then do:
            run BuildPkgDetail(input ttClaimMap.iClaimID,
                                input ttClaimMap.iSeq,
                                input ttClaimMap.ePosition).
        end.
        when ttClaimMap.iRecType = 4 then do:
            run doNote(input ttClaimMap.iClaimID,
                                input ttClaimMap.iSeq,
                                input ttClaimMap.ePosition).
        end.
        when ttClaimMap.iRecType = 5 then do:
            RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",9.0).
            RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).
            run BuildPageFooter(input ttClaimMap.iClaimID,
                                input ttClaimMap.iSeq,
                                input ttClaimMap.ePosition).
        end.
        when ttClaimMap.iRecType = 6 then do:
            run BuildPkgHdr(input ttClaimMap.iClaimID,
                                input ttClaimMap.iSeq,
                                input ttClaimMap.ePosition).
        end.
        when ttClaimMap.iRecType = 7 then do:
            run BuildHTAFooter(input ttClaimMap.iClaimID,
                                input ttClaimMap.iSeq,
                                input ttClaimMap.ePosition).
        end.
        end case.
    end.
end.
RUN pdf_close IN h_PDFinc ("Spdf").

IF VALID-HANDLE(h_PDFinc) THEN
  DELETE PROCEDURE h_PDFinc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BuildCoilHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildCoilHdr Procedure 
PROCEDURE BuildCoilHdr :
define input parameter piClaimNum as int no-undo.
define input parameter piSeq as int no-undo.
define input parameter peLoc as dec no-undo.

find first ttCoilHdr no-lock
    where ttCoilHdr.iClaimID = piClaimNum
    and ttCoilHdr.iSeq = piSeq no-error.

if not avail ttCoilHdr then return.
lTest = TextPair('Prepared By:', ttCoilHdr.cPrepBy         , 1.4, peLoc).
lTest = TextPair('Work Order:' , string(ttCoilHdr.iOrderID), 1.4, peLoc - 0.17).
lTest = TextPair('Cust Code:'  , ttCoilHdr.cCustCode       , 1.4, peLoc - 0.34).
lTest = TextPair('Weight:'     , ttCoilHdr.cNetWt          , 1.4, peLoc - 0.51).
lTest = TextPair('Type:'       , ttCoilHdr.cTypeDescr      , 1.4, peLoc - 0.68).
lTest = TextPair('Owner Order:', ttCoilHdr.cOwnerOrder     , 1.4, peLoc - 0.85).
lTest = TextPair('Ref No 1:'   , ttCoilHdr.cRef1           , 1.4, peLoc - 1.02).

lTest = TextPair('PSI Coil ID:', string(ttCoilHdr.iInvID), 4.6, peLoc).
lTest = TextPair('Coil Number:', ttCoilHdr.cOwnInvID     , 4.6, peLoc - 0.17).
lTest = TextPair('Part No:'    , ttCoilHdr.cPartNo       , 4.6, peLoc - 0.34).
lTest = TextPair('Size:'       , ttCoilHdr.cSize         , 4.6, peLoc - 0.51).
lTest = TextPair('Shipper No:' , ttCoilHdr.cShipper      , 4.6, peLoc - 0.68).
lTest = TextPair('User Order:' , ttCoilHdr.cUserOrder    , 4.6, peLoc - 0.85).
lTest = TextPair('Ref No 2:'   , ttCoilHdr.cref2         , 4.6, peLoc - 1.02).


lTest = TextPair('Rcpt Date:'   , string(ttCoilHdr.dRcptDate), 8.6, peLoc).
lTest = TextPair('Rcpt Prev ID:', ttCoilHdr.cPrevID          , 8.6, peLoc - 0.17).
lTest = TextPair('Heat Lot:'    , ttCoilHdr.cHeatLot         , 8.6, peLoc - 0.34).
lTest = TextPair('Case No:'     , ttCoilHdr.cCaseNo          , 8.6, peLoc - 0.51).
lTest = TextPair('Length:'      , ttCoilHdr.cLength          , 8.6, peLoc - 0.68).
lTest = TextPair('Carrier:'     , ttCoilHdr.cCarrier         , 8.6, peLoc - 0.85).
lTest = TextPair('Mill Order:'  , ttCoilHdr.cMillOrder       , 8.6, peLoc - 1.02).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildDocHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildDocHeader Procedure 
PROCEDURE BuildDocHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piClaimNum as int no-undo.
define input parameter piSeq as int no-undo.

find first ttClaimHdr no-lock
    where ttClaimHdr.iClaimID = piClaimNum
    and ttClaimHdr.iSeq = piSeq no-error.

if not avail ttClaimHdr then return.
RUN pdf_new_page IN h_PDFinc ("Spdf").

 /*  Logos and titles */
 RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-Bold",15.0).
 RUN pdf_text_color IN h_PDFinc ("Spdf",.0,.0,.0).
 run pdf_text_center in h_PDFinc ("Spdf",'Metals Complaint Report',396,576).
 RUN pdf_place_image IN h_PDFinc ("Spdf","PSLogo1", PointVal(0.50), PointVal(1.0), 48, 48). 

 RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-Bold",12.0).
 RUN pdf_text_xy IN h_PDFinc ("Spdf",ttClaimHdr.cPlantName ,PointVal(0.50),PointVal(7.3)).
 RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",9.0).
 RUN pdf_text_xy IN h_PDFinc ("Spdf",ttClaimHdr.cPlantAddr ,PointVal(0.50),PointVal(7.15)).
 RUN pdf_text_xy IN h_PDFinc ("Spdf",ttClaimHdr.cPlantCity ,PointVal(0.50),PointVal(7.00)).

 lTest = TextPair('Owner Name:',   ttClaimHdr.cOwnerName,  4.6, 7.55).
 lTest = TextPair('User Name:',    ttClaimHdr.cUserName,   4.6, 7.38).
 lTest = TextPair('Mill Name:',    ttClaimHdr.cMillName,   4.6, 7.21).
 lTest = TextPair('Attention Of:', ttClaimHdr.cAttn,       4.6, 6.87).
 lTest = TextPair('Claim Number:', string(piClaimNum),     8.8, 7.55).
 lTest = TextPair('Date:',         string(ttClaimHdr.dClaimDate,'99/99/99'), 8.8, 7.38).
 lTest = TextPair('Page:',         ttClaimHdr.cPage,       8.8, 7.21).

 lTest = MakeBox(0.5,6.76,10,0.06).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildHTAFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildHTAFooter Procedure 
PROCEDURE BuildHTAFooter :
define input parameter piClaimNum as int no-undo.
define input parameter piSeq as int no-undo.
define input parameter peLoc as dec no-undo.

define variable iLoop2 as integer no-undo.
define variable cCols as character no-undo.

RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",9.0).

/* Claim Calculation Block */
ltest = MakeBox(0.4,peLoc - 2.3, 10.2, 1.5).

eVal = peLoc - 2.3.
do iLoop = 1 to 4:
    eVal = eVal + 0.25 .
    RUN pdf_line IN h_PDFinc ("Spdf",Pointval(0.4),
                                     PointVal(eVal),
                                     PointVal(10.6),
                                     PointVal(eVal),
                                     1).
end.

cCols = '0.8,2.35,5.10,7.70,9.50'.
do iLoop = 1 to 3:
    if iLoop = 1 then assign
        cVal = 'Claim,Purchase,Total,Less Scrap,Total To Be'
        eVal = peLoc - 0.91.
    if iLoop = 2 then assign
        cVal = 'Weight,Price,Claim,Credit,Reimbursed'
        eval = peLoc - 1.08.
    if iLoop = 3 then assign
        eval = peLoc - 1.25
        cVal = '(Kg),($ / Kg),[A],[B],[A - B]'.
    do iLoop2 = 1 to 5:
        RUN pdf_text_center IN h_PDFinc ("Spdf",
                                         entry(iLoop2,cVal),
                                         PointVal(decimal(entry(iLoop2,cCols))),
                                         PointVal(eVal)).
    end.
end.

/* draw dashed lines surrounding 'Remarks' and 'Customer Coments'*/ 
run pdf_stroke_color IN h_PDFinc("Spdf",0.7,0.7,0.7).
run pdf_set_dash IN h_PDFinc("Spdf",2,2).

RUN pdf_line IN h_PDFinc ("Spdf",Pointval(0.4),
                                 PointVal(peLoc - 0.1),
                                 PointVal(4.75),
                                 PointVal(peLoc - 0.1),
                                 1).
RUN pdf_line IN h_PDFinc ("Spdf",Pointval(5.5),
                                 PointVal(peLoc - 0.1),
                                 PointVal(10.6),
                                 PointVal(peLoc - 0.1),
                                 1).
eVal = peLoc - 0.65.

RUN pdf_line IN h_PDFinc ("Spdf",Pointval(0.4),
                                 PointVal(eVal),
                                 PointVal(4.25),
                                 PointVal(eVal),
                                 1).
RUN pdf_line IN h_PDFinc ("Spdf",Pointval(6.0),
                                 PointVal(eVal),
                                 PointVal(10.6),
                                 PointVal(eVal),
                                 1).

run pdf_set_dash IN h_PDFinc("Spdf",1,0).
run pdf_stroke_color IN h_PDFinc("Spdf",0,0,0).

RUN pdf_text_center IN h_PDFinc ("Spdf",
                                 'Remarks',
                                 PointVal(5.14),
                                 PointVal(peLoc - 0.15)).
RUN pdf_text_center IN h_PDFinc ("Spdf",
                                 'Customer Comments',
                                 PointVal(5.2),
                                 PointVal(peLoc - 0.7)).
                               
/* Signature Area */
RUN pdf_line IN h_PDFinc ("Spdf",Pointval(1.2),
                                 PointVal(peLoc - 2.7),
                                 PointVal(2.7),
                                 PointVal(peLoc - 2.7),
                                 1).

RUN pdf_line IN h_PDFinc ("Spdf",Pointval(7.3),
                                 PointVal(peLoc - 2.7),
                                 PointVal(10.3),
                                 PointVal(peLoc - 2.7),
                                 1).

RUN pdf_line IN h_PDFinc ("Spdf",Pointval(1.2),
                                 PointVal(peLoc - 2.5),
                                 PointVal(2.7),
                                 PointVal(peLoc - 2.5),
                                 1).

RUN pdf_line IN h_PDFinc ("Spdf",Pointval(7.3),
                                 PointVal(peLoc - 2.5),
                                 PointVal(10.3),
                                 PointVal(peLoc - 2.5),
                                 1).

RUN pdf_text_xy IN h_PDFinc ("Spdf",
                             'Review By:',
                             PointVal(0.4),
                             PointVal(peLoc - 2.5)).
RUN pdf_text_xy IN h_PDFinc ("Spdf",
                             'Date:',
                             PointVal(0.8),
                             PointVal(peLoc - 2.7)).
RUN pdf_text_xy IN h_PDFinc ("Spdf",
                             'Processor Authorization:',
                             PointVal(5.7),
                             PointVal(peLoc - 2.5)).
RUN pdf_text_xy IN h_PDFinc ("Spdf",
                             'Mill Authorization:',
                             PointVal(5.7),
                             PointVal(peLoc - 2.7)).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildPageFooter Procedure 
PROCEDURE BuildPageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piClaimNum as int no-undo.
define input parameter piSeq as int no-undo.
define input parameter peLoc as dec no-undo.

run pdf_stroke_color IN h_PDFinc("Spdf",0.7,0.7,0.7).
run pdf_set_dash IN h_PDFinc("Spdf",2,2).

RUN pdf_line IN h_PDFinc ("Spdf",Pointval(0.4),
                                 PointVal(peLoc - 0.1),
                                 PointVal(4.75),
                                 PointVal(peLoc - 0.1),
                                 1).
RUN pdf_line IN h_PDFinc ("Spdf",Pointval(5.5),
                                 PointVal(peLoc - 0.1),
                                 PointVal(10.6),
                                 PointVal(peLoc - 0.1),
                                 1).
eVal = (peLoc + 0.5) / 2.

RUN pdf_line IN h_PDFinc ("Spdf",Pointval(0.4),
                                 PointVal(eVal),
                                 PointVal(4.25),
                                 PointVal(eVal),
                                 1).
RUN pdf_line IN h_PDFinc ("Spdf",Pointval(6.0),
                                 PointVal(eVal),
                                 PointVal(10.6),
                                 PointVal(eVal),
                                 1).
                                
run pdf_set_dash IN h_PDFinc("Spdf",1,0).
run pdf_stroke_color IN h_PDFinc("Spdf",0,0,0).

RUN pdf_line IN h_PDFinc ("Spdf",Pointval(6.62),
                                 PointVal(0.5),
                                 PointVal(9.6),
                                 PointVal(0.5),
                                 1).

RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",9.0).
RUN pdf_text_center IN h_PDFinc ("Spdf",
                                 'Remarks',
                                 PointVal(5.14),
                                 PointVal(peLoc - 0.15)).
RUN pdf_text_center IN h_PDFinc ("Spdf",
                                 'Customer Comments',
                                 PointVal(5.2),
                                 PointVal(eVal - .02)).
RUN pdf_text_xy IN h_PDFinc ("Spdf",
                             'Mill Authorization',
                             PointVal(5.5),
                             PointVal(0.5)).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildPkgDetail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildPkgDetail Procedure 
PROCEDURE BuildPkgDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piClaimNum as int no-undo.
define input parameter piSeq as int no-undo.
define input parameter peLoc as dec no-undo.

define variable hBuff as handle.
define variable hField as handle.

find first ttPkgDetail
   where ttPkgDetail.iClaimID = piClaimNum
   and ttPkgDetail.iSeq = piSeq no-error.
   
if not avail ttPkgDetail then return.
hBuff = temp-table ttPkgDetail:default-buffer-handle no-error.
peLoc = peLoc - 0.5 .
lTest = MakeBox(0.4, peLoc, 10.2, 0.50).
run pdf_stroke_color IN h_PDFinc("Spdf",0.7,0.7,0.7).
/* run pdf_set_dash IN h_PDFinc("Spdf",2,2). */
RUN pdf_line IN h_PDFinc ("Spdf",Pointval(eLinefrom),
                                 PointVal(peLoc + 0.2),
                                 PointVal(eLineTo),
                                 PointVal(peLoc + 0.2),
                                 1).
                                 
/* run pdf_set_dash IN h_PDFinc("Spdf",1,0). */
run pdf_stroke_color IN h_PDFinc("Spdf",1,1,1).

for each ttColumns:
    cVal = ''.
    hField = hBuff:buffer-field(ttColumns.cvar) no-error.
    if hField:buffer-value = ? then next.  /* pdfInclude HATES null values */
    if ttColumns.eLeftEdge ne 0.4 then 
    RUN pdf_line IN h_PDFinc ("Spdf",
                              PointVal(ttColumns.eLeftEdge),
                              PointVal(peLoc + (if ttColumns.eLeftEdge le eLinefrom
                                                or ttColumns.eLeftEdge ge eLineTo
                                                then 0 else 0.2)),
                              Pointval(ttColumns.eLeftEdge),
                              PointVal(peLoc + 0.5),
                              1).
    if hField:data-type = 'INTEGER' then
       cVal = trim(string(hField:buffer-value)).
    if hfield:data-type = 'DATE' then
       cVal = string(hField:buffer-value, '99/99/99').
    if hfield:data-type = 'CHARACTER' then
       cVal = hfield:buffer-value.
    if cVal = '' then next.
    RUN pdf_text_center IN h_PDFinc ("Spdf",
                                     cVal,
                                     PointVal(ttColumns.eWidth / 2 + ttColumns.eLeftEdge),
                                     PointVal(peLoc + (if ttColumns.iRow = 1 then 0.31 ELSE IF ttCOlumns.irow = 3 THEN .18 ELSE 0.03))).


end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-BuildPkgHdr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildPkgHdr Procedure 
PROCEDURE BuildPkgHdr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piClaimNum as int no-undo.
define input parameter piSeq as int no-undo.
define input parameter peLoc as dec no-undo.

RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",9.0).
lTest = MakeBox(0.4, peLoc - 0.4, 10.2, 0.40).
for each ttColumns:
    case true:
        when index(ttColumns.cLabel,'|') > 0 then do:
          RUN pdf_text_center IN h_PDFinc ("Spdf",
                                       entry(1, ttColumns.cLabel, '|'),
                                       PointVal(ttColumns.eWidth / 2 + ttColumns.eLeftEdge),
                                       PointVal(peLoc - 0.15)).
          if entry(2, ttColumns.cLabel, '|') > '' then
          RUN pdf_text_center IN h_PDFinc ("Spdf",
                                       entry(2, ttColumns.cLabel, '|'),
                                       PointVal(ttColumns.eWidth / 2 + ttColumns.eLeftEdge),
                                       PointVal(peLoc - 0.34)).
        end.
        when ttColumns.cLabel = 'Owner Order' then do:
          RUN pdf_text_center IN h_PDFinc ("Spdf",
                                       ttColumns.cLabel,
                                       PointVal(ttColumns.eWidth / 2 + ttColumns.eLeftEdge),
                                       PointVal(peLoc - 0.34)).
          RUN pdf_line IN h_PDFinc ("Spdf",
                                    PointVal(ttColumns.eLeftEdge),
                                    PointVal(peLoc - 0.2),
                                    Pointval(ttColumns.eLeftEdge + ttColumns.eWidth),
                                    PointVal(peLoc - 0.2),
                                    1).

        end.
        when ttColumns.cLabel gt '' then do:
          RUN pdf_text_center IN h_PDFinc ("Spdf",
                                       ttColumns.cLabel,
                                       PointVal(ttColumns.eWidth / 2 + ttColumns.eLeftEdge),
                                       PointVal(peLoc - 0.26)).
        end.
    end case.
    if ttColumns.eLeftEdge eq 0.4 then next.
    if ttColumns.iRow = 2 and ttColumns.cLabel eq '' then next.
    RUN pdf_line IN h_PDFinc ("Spdf",
                              PointVal(ttColumns.eLeftEdge),
                              PointVal(if ttColumns.cLabel = 'PFE|' then peLoc - 0.2 else peLoc - 0.4),
                              Pointval(ttColumns.eLeftEdge),
                              PointVal(peLoc),
                              1).
end.
RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",9.0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoNote) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoNote Procedure 
PROCEDURE DoNote :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter piClaimNum as int no-undo.
define input parameter piSeq as int no-undo.
define input parameter peLoc as dec no-undo.

find first ttComment no-lock
    where ttComment.iClaimID = piClaimNum
    and ttComment.iSeq = piSeq no-error.
if not avail ttComment then return.
peLoc = peLoc - 0.14 .    
lTest = TextPair(ttComment.cLabel, ttComment.cText, 1.2, peLoc).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NewFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewFile Procedure 
PROCEDURE NewFile :
define variable cLogoFile1 as character no-undo.
define variable cImgPath as character no-undo.
define variable cTest as character no-undo.
define variable cTest2 as character no-undo.
define variable cLabelFile as character no-undo.

if opsys = 'win32' then
  pcClaimPath = session:temp-directory.
else do:
  find first AppAttr no-lock
       where AppAttr.AttrKey = "ReportOutputDir"
       and appattr.appstr > "" no-error.
  pcClaimPath = (if avail appattr then appattr.appstr else os-getenv("TDIR")).
end.

/* Test our path, if it is bad or not a directory/folder, fall back to
   session:temp-directory, which should always be good  */
file-info:file-name = pcClaimPath.
if file-info:full-pathname eq ? 
   or index(file-info:file-type,"D") eq 0 then   
   pcClaimPath = session:temp-directory.

pcClaimPath = substitute("&1&2Claim&3.pdf",
                        pcClaimPath,
                        (if num-entries(pcClaimList,'!') > 1 then 'M' else ''),
                        entry(1,pcClaimList,'!')).

cImgPath = {fnarg getAppStr '"SDIR"'} + 'bin/'.
cLogoFile1 = substitute('&1PSLogo1.jpg',cImgPath).

/* see if the file is available */
file-info:file-name = cLogoFile1.
cTest = file-info:full-pathname.

if cTest = ? then do:
   /* may be running on devel mode, try looking in p-tools */
   cLogoFile1 = 'p:\tools\PSLogo1.jpg'.
   file-info:file-name = cLogoFile1.
   cTest2 = FILE-INFO:FULL-PATHNAME. 
   if cTest2 ne ? then assign cImgPath = substitute("P:&1tools&1",chr(92)). 
end.

run pdf_new in h_PDFinc ("Spdf", pcClaimPath).

/* Set Document Information */ 
RUN pdf_set_info IN h_PDFinc ("Spdf","Author","Precision Strip, Inc.").
RUN pdf_set_info IN h_PDFinc ("Spdf","Subject","Metals Complaint Report").
RUN pdf_set_info IN h_PDFinc ("Spdf","Title","Precision Strip Claim").
RUN pdf_set_info IN h_PDFinc ("Spdf","Keywords","Claim").
RUN pdf_set_info IN h_PDFinc ("Spdf","Creator","PDFinclude V2").
RUN pdf_set_info IN h_PDFinc ("Spdf","Producer","claims/t0736.p").
run pdf_set_orientation in h_PDFinc ("Spdf","Landscape").


if cTest ne ? or cTest2 ne ? then
      RUN pdf_load_image IN h_PDFinc ("Spdf","PSLogo1",cLogoFile1).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetColumns Procedure 
PROCEDURE SetColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define variable eLeftPos as decimal no-undo init 0.4 .
/* inv id */
create ttColumns.
assign
    ttColumns.iSeq = 1
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.89
    ttColumns.eCellHeight = 0.5
    ttColumns.cLabel = 'PSI Inv ID|(Case No)'
    ttColumns.cVar = 'iInvID'
    ttColumns.eLeftEdge = eLeftPos
    .
/* case no */
create ttColumns.
assign
    ttColumns.iSeq = 3
    ttColumns.iRow = 3
    ttColumns.eWidth = 0.89
    ttColumns.eCellHeight = 0.5
    ttColumns.cLabel = ''
    ttColumns.cVar = 'cCaseNo'
    ttColumns.eLeftEdge = eLeftPos
    .

/* ClaimDate */
create ttColumns.
assign
    ttColumns.iSeq = 2
    ttColumns.iRow = 2
    ttColumns.eWidth = 0.89
    ttColumns.eCellHeight = 0.5
    ttColumns.cLabel = ''
    ttColumns.cVar = 'dClaimDate'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Break */
create ttColumns.
assign
    ttColumns.iSeq = 3
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.36
    ttColumns.eCellHeight = 0.5
    ttColumns.cLabel = 'Brk|No.'
    ttColumns.cVar = 'iBrk'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Slit */
create ttColumns.
assign
    ttColumns.iSeq = 4
    ttColumns.iRow = 1
    ttColumns.eWidth = .25
    ttColumns.eCellHeight = 0.5 
    ttColumns.cLabel = 'Slit|No.'
    ttColumns.cVar = 'iSlit'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Damage Descr */
create ttColumns.
assign
    ttColumns.iSeq = 5
    ttColumns.iRow = 1
    ttColumns.eWidth = 2.0
    ttColumns.eCellHeight = 0.33
    ttColumns.cLabel = 'Description|Gauge X Width X Length'
    ttColumns.cVar = 'cDamageDescr'
    ttColumns.eLeftEdge = eLeftPos
    eLineFrom = eLeftPos
    .
/* Size */
create ttColumns.
assign
    ttColumns.iSeq = 6
    ttColumns.iRow = 2
    ttColumns.eWidth = 3.00
    ttColumns.eCellHeight = 0.17
    ttColumns.cLabel = ''
    ttColumns.cVar = 'cSize'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + 2.00
    .
/* Severity */
create ttColumns.
assign
    ttColumns.iSeq = 7
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.59
    ttColumns.eCellHeight = 0.33
    ttColumns.cLabel = 'Severity'
    ttColumns.cVar = 'cSeverity'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Position */
create ttColumns.
assign
    ttColumns.iSeq = 8
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.67
    ttColumns.eCellHeight = 0.33
    ttColumns.cLabel = 'Position|'
    ttColumns.cVar = 'cPosition'
    ttColumns.eLeftEdge = eLeftPos
    .
/* Owner Order */
create ttColumns.
assign
    ttColumns.iSeq = 9
    ttColumns.iRow = 2
    ttColumns.eWidth = 1.5
    ttColumns.eCellHeight = 0.17
    ttColumns.cLabel = 'Owner Order'
    ttColumns.cVar = 'cOwnerOrder'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + 0.67
    .
/* Position from Edge */
create ttColumns.
assign
    ttColumns.iSeq = 10
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.83
    ttColumns.eCellHeight = 0.33
    ttColumns.cLabel = 'PFE|'
    ttColumns.cVar = 'cPFE'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Orientation */
create ttColumns.
assign
    ttColumns.iSeq = 11
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.79
    ttColumns.eCellHeight = 0.33
    ttColumns.cLabel = 'Orientation'
    ttColumns.cVar = 'cOrient'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* End User */
create ttColumns.
assign
    ttColumns.iSeq = 12
    ttColumns.iRow = 2
    ttColumns.eWidth = 3.03
    ttColumns.eCellHeight = .17
    ttColumns.cLabel = ''
    ttColumns.cVar = 'cEndUser'
    ttColumns.eLeftEdge = eLeftPos - 0.79
    .
/* repeating Defect */
create ttColumns.
assign
    ttColumns.iSeq = 13
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.86
    ttColumns.eCellHeight = 0.33
    ttColumns.cLabel = 'Repeat'
    ttColumns.cVar = 'cRepeat'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Start Position */
create ttColumns.
assign
    ttColumns.iSeq = 14
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.69
    ttColumns.eCellHeight = 0.33 
    ttColumns.cLabel = 'Start|Position'
    ttColumns.cVar = 'cStartPos'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* end position */
create ttColumns.
assign
    ttColumns.iSeq = 15
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.69
    ttColumns.eCellHeight = 0.33 
    ttColumns.cLabel = 'End|Position'
    ttColumns.cVar = 'cEndPos'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* sample */
create ttColumns.
assign
    ttColumns.iSeq = 16
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.17
    ttColumns.eCellHeight = 0.5
    ttColumns.cLabel = 'S'
    ttColumns.cVar = 'cSample'
    ttColumns.eLeftEdge = eLeftPos
    eLineTo = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Hold */
create ttColumns.
assign
    ttColumns.iSeq = 17
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.17
    ttColumns.eCellHeight = 0.5
    ttColumns.cLabel = 'H|C'
    ttColumns.cVar = 'iHoldCode'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Defect Size */
create ttColumns.
assign
    ttColumns.iSeq = 18
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.67
    ttColumns.eCellHeight = 0.5
    ttColumns.cLabel = 'Defect|Size'
    ttColumns.cVar = 'cDefectSize'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/* Weight */
create ttColumns.
assign
    ttColumns.iSeq = 19
    ttColumns.iRow = 1
    ttColumns.eWidth = 0.57
    ttColumns.eCellHeight = 0.5
    ttColumns.cLabel = 'Wt'
    ttColumns.cVar = 'cAmount'
    ttColumns.eLeftEdge = eLeftPos
    eLeftPos = eLeftPos + ttcolumns.eWidth
    .
/*     output stream test to 'c:\_work\60k\65430_PDF_Claims\ttColumns.txt'. */
/*     export stream test delimiter "~011" 'seq' 'row' 'width' 'cell height' 'Label' 'var' 'LeftEdge'. */
/*     for each ttColumns by ttcolumns.iSeq: */
/*     export stream test delimiter "~011" ttcolumns. */
/*     end. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-MakeBox) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MakeBox Procedure 
FUNCTION MakeBox RETURNS LOGICAL
  ( input peX as decimal,
    input peY as decimal,
    input peWidth as decimal,
    input peHeight as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  Draws a box using 4 lines,
  pass in, X-coord, y-coord, width, height (all in inches)
     
------------------------------------------------------------------------------*/

 RUN pdf_line IN h_PDFinc ("Spdf",PointVal(peX),PointVal(peY),Pointval(peX + peWidth),PointVal(peY),1).
 RUN pdf_line IN h_PDFinc ("Spdf",PointVal(peX + peWidth),PointVal(peY),PointVal(peX + peWidth),PointVal(peY + peHeight),1).
 RUN pdf_line IN h_PDFinc ("Spdf",PointVal(peX + peWidth),PointVal(peY + peHeight),PointVal(peX),PointVal(peY + peHeight),1).
 RUN pdf_line IN h_PDFinc ("Spdf",PointVal(peX),PointVal(peY + peHeight),PointVal(peX),PointVal(peY),1).
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PointVal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PointVal Procedure 
FUNCTION PointVal RETURNS INTEGER
    ( input peCol as decimal) :
  /*------------------------------------------------------------------------------
    Purpose:  input the dimension from the left or bottom margin,
              returns the PDF X or Y coordinate to use in points.
              (there are 72 points per inch)
      Notes:  
  ------------------------------------------------------------------------------*/

    RETURN int(peCol * 72).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TextPair) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TextPair Procedure 
FUNCTION TextPair RETURNS LOGICAL
  ( input pcLabel as char,
    input pcValue as char,
    input peXPos as decimal,
    input peYPos as decimal ) :
/*------------------------------------------------------------------------------
  Purpose:  Places a label and a value, 
            label is right justified at XPos, value placed 0.15" to its right (left justified)
     
------------------------------------------------------------------------------*/
if pcLabel ne ? and pcLabel gt '' then do:
    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica-bold",9.0).
    eVal = decimal(pdf_text_width("Spdf",pcLabel)) / 72 .
    RUN pdf_text_xy IN h_PDFinc ("Spdf",
                                 pcLabel,
                                 PointVal(peXPos - eVal),
                                 PointVal(peYPos)).
    RUN pdf_set_font IN h_PDFinc ("Spdf","Helvetica",9.0).
end.
if pcValue ne ? and pcValue > '' then
    run pdf_text_xy in h_PDFInc("Spdf", pcvalue, PointVal(peXPos + 0.15), Pointval(peYPos)).
  RETURN true.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

