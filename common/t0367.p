&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0367.p
    Purpose     : Read/Write Documents From Server to Client using
                  the Progress AppServer. This functionality is duplicated
                  client-side in common/t0368.p.

   Date     By   Issue   Remark                       
 ========  ===  ======  ================================================
 01/25/11  djt   55107  When e-mailing reports, convert to PDF
 09/18/13  cjs   63576  Recompile for changes in blobtransport.i
 12/01/17  djt   76090  Recompile for change in blobtransport.i
 12/13/17  djt   76819  Claim on pkg has incomplete subject line when emailed
 01/25/18  djt   77057  Now the subject line has too much information for Aleris
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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



{includes/blobtransport.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ReadDoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadDoc Procedure 
PROCEDURE ReadDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

define input parameter pcFileName as character no-undo.

define variable cFileName as character no-undo.

assign cFileName = pcFileDir + '/' + pcFileName
       file-info:file-name = cFileName.

if file-info:full-pathname = ? then 
  return error 'Could not find file.'.

create gttImageLib.
                                  
assign gttImageLib.ikey = pcFileName
       gttImageLib.ikeytype = 99
       gttImageLib.filetype = entry(num-entries(pcFileName, '.'),pcFileName, '.')
       gttImageLib.descr = ''
       gttImageLib.iDate =  now.      
    
gttImageLib.takenby = dynamic-function('getGlobalVar', input 'UserID').
    
copy-lob from file cFileName to gttImageLib.rawimage.
if index(pcFilename, 'MClaim') > 0 then do:
    find first claim
        no-lock where claim.claim-id = int(substring(entry(1,pcFileName,'.'),7,12)) no-error.
    if avail claim then do:
        find first inv no-lock where inv.inv-id = claim.coil-id no-error.
        if avail inv then find first rcpt no-lock where rcpt.rcpt-id = inv.rcpt-id no-error.
        gttImageLib.descr = substitute('MCR: &1 - PSI Inv: &2 - Mill Coil: &3 - HeatLot: &4',
                                       claim.claim-id,
                                       claim.coil-id,
                                       (if avail rcpt then rcpt.own-inv-id else ''),
                                       (if avail rcpt then rcpt.heat-lot else '')).
        if avail rcpt then find first cust no-lock where cust.cust-id = rcpt.mill-id no-error.
        if avail cust and cust.entityid = 9 then
        gttImageLib.descr = substitute('MCR for Coil: &1 - HeatLot: &2',
                                       (if avail rcpt then rcpt.own-inv-id else ''),
                                       (if avail rcpt then rcpt.heat-lot else '')).

    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

