&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0689.p
    Purpose     : Update AverageCoilWeight on Inverntory MGMT table

    Syntax      :

    Description :

    Author(s)   : JMM
    Created     : 06/07/2012
    Notes       : 06/07/2012 - jmm - Added to recompute averagecoilweight
                                     each month for the mgmt table. Use as a Cron Job.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{includes/i0001.i}
{includes/i0034.i}

define variable cSubtitle as char no-undo.  /* Populates the second row in the spreadsheet */
define variable hXProcHand AS HANDLE.       /* Handle to the procedure that creates XML file */
define variable hTempHand as handle.        /* Used for Metric label option */
define variable hBuff as HANDLE.            /* Used for Metric label option */

define temp-table ttAverage
    field iTotal as int column-label "Total"
    field iNewAvg as int column-label "New Avg Wt"
    field iPackage as int column-label "Package"
    field iPart as int column-label "Part"
    field cMetalNo as char column-label "Metal"
    field iPressNum as int column-label "Press"
    field iAvailWeight as int column-label "Avail Wt"
    field iAvgWeight as int column-label "Old Avg Wt".

hTempHand = TEMP-TABLE ttAverage:HANDLE.
hBuff = hTempHand:DEFAULT-BUFFER-HANDLE.

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

run UpdateAverageCoilWeight.
run GenerateExcelReport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-GenerateExcelReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenerateExcelReport Procedure 
PROCEDURE GenerateExcelReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


run gettheosfilename(output g-filename).
assign g-filename = substring(g-filename, 1, (length(g-filename) - 4)) + '.XLS'.

cSubTitle = "Update Average Coil Weight"
   + " For Dates: " + string(today - 30, "99/99/99") + " - " + string(today, "99/99/99")
   + " - generated " + string(today,"99/99/99") + "."
    .
run lib/t0476.p PERSISTENT SET hxProcHand.  

run addStyle IN hXProcHand ("BareDec0").
run addStyleElement in hXProcHand ("BareDec0", "NumberFormat").
run addStyleElementAttribute in hXProcHand ("BareDec0", "NumberFormat", "Format", '#,##0;-#,##0;_(* &quot;-&quot;??_)').
run assignStyleToColumn in hxProcHand ("BareDec0", "iTotal").
run assignStyleToColumn in hxProcHand ("BareDec0", "iNewAvg").
run assignStyleToColumn in hxProcHand ("BareDec0", "iAvailWeight").
run assignStyleToColumn in hxProcHand ("BareDec0", "iAvgWeight").

run makeDocument in hxProcHand (                                             
   table-handle hTempHand,   /* handle to temp-table */                      
   true,                      /* Include a header? */                         
   "Averages",              /* The name of the worksheet */                 
   "Precision Strip, Inc.",   /* title line */                                
   cSubTitle,                /* Sub-Title Line */                            
   g-filename).               /* Fileto create */              

delete procedure hxProcHand.

end Procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-gettheosfilename) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gettheosfilename Procedure 
PROCEDURE gettheosfilename :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* server/lib/getosfilename.p */
define output parameter pcFilename as character  no-undo.

if opsys = 'win32' then
  pcFilename = session:temp-directory.
else 
do:
  find AppAttr  no-lock
       where AppAttr.AttrKey = "ReportOutputDir" no-error.
  if avail AppAttr then
  do:
    if AppAttr.AppStr gt "" then
      pcFilename = AppAttr.AppStr.
  end.
  else /* Leave this code in just for redundency. - cel */
    pcFilename = os-getenv("TDIR").
end.

/* Test our path, if it is bad or not a directory/folder, fall back to
   session:temp-directory, which should always be good  */
file-info:file-name = pcFilename.
if file-info:full-pathname eq ? 
or index(file-info:file-type,"D") eq 0 then   
  pcFilename = session:temp-directory.

pcFilename = substitute("&1&2.txt",pcFilename,"updatecoilweight").

return.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UpdateAverageCoilWeight) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateAverageCoilWeight Procedure 
PROCEDURE UpdateAverageCoilWeight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Update to not output to test, actually update the proper field in the mgmt table.
------------------------------------------------------------------------------*/
def var i as int init 0 column-label "PKG" format ">>>>9" .
for each inventory where psipartid ne 0:
find partner where partner.partner-id = inventory.psipartnerid no-lock no-error.

if avail partner and plant-id ne 2 or not avail partner then next.
for each s-part where s-part.ship-date >= today - 60 and s-part.part-id = inventory.psipartid no-lock:
  accumulate s-part.act-wt (total).
  for each inv where inv.load-id = s-part.load-id and inv.slip-id = s-part.slip-id and s-part.part-id = inv.part-id no-lock:
     i = i + 1.
  end.
end.

if (accum total s-part.act-wt) = 0 then do:
  i = 0.
  next.
end.


create ttAverage.
assign ttAverage.iTotal       = accum total s-part.act-wt  
       ttAverage.iNewAvg      = (accum total s-part.act-wt) / i
       ttAverage.iPackage     = i
       ttAverage.iPart        = inventory.psipartid
       ttAverage.cMetalNo     = inventory.metalno
       ttAverage.iPressNum    = inventory.pressnum
       ttAverage.iAvailWeight = inventory.availcoilweight
       ttAverage.iAvgWeight   = inventory.avgcoilweight
       .

/* Update Inventory with new average */
assign inventory.avgcoilweight = ttAverage.iNewAvg.

i = 0.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

