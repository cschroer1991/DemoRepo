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
    Modified    :
    02/15/06 -  fh - Issue 3308 Add update for pieces in CSR
    03/14/06 -  fh - TIWO 20497
    04/04/06 -  fh & nlb - TIWO 21054 Change calc to use actual pkg wt not
                     part wt.
    10/22/09 -  nlbu  tiwo 45112 -  Send Alert if mult.calc-wt is negative   
    04/23/10 - nlbu 50995 Change email for negative mult.calc-wt - Do Not send to phone.             
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter pcMultRowID as character  no-undo.
define input  parameter piLinearFt as integer    no-undo.
define output parameter poCalcWt as integer    no-undo init 0.

def buffer bMultPart for part.
def var cNegMsg as char no-undo.
def var lNegResult as logical no-undo.
def var iCalcWT  like mult.calc-wt no-undo.
def var iNegMsgWT like mult.calc-wt no-undo.

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
   
   find mult where rowid(mult) = to-rowid(pcMultRowID) no-lock no-error.
   if not avail mult 
       then return.

   find work-order where 
       work-order.order-id = mult.order-id
       no-lock no-error.
   find processCode where
       processCode.proc-code = work-order.proc-code
       no-lock no-error.
   find inv where
       inv.inv-id = mult.inv-id
       no-lock no-error.
   find part where
       part.part-id = work-order.part-id
       no-lock no-error.
   find bMultpart where
       bMultpart.part-id = mult.part-id
       no-lock no-error.
   
   /* Note: pilinearft from CSR d0131 is pieces if endtype = 3 */
   if processCode.endtype = 3 /* pieces */
       then poCalcWt = int((pilinearft / inv.pieces) * inv.net-wt).
   else poCalcWt = int((pilinearft / inv.linear-ft) * inv.net-wt).
   
   /* Tiwo 45112 Test for Mult negative weight */
/*      assign iNegMsgWT = poCalcWt.                                                                                                                             */
/*      assign iCalcWt = max(poCalcWt, 1).                                                                                                                       */
/*      if iCalcWt = 1 then                                                                                                                                      */
/*      do:                                                                                                                                                      */
/*          assign poCalcWt = 1.                                                                                                                                 */
/*          cNegMsg = 'Mult: ' + string(mult.break-no) + '-' + string(mult.slit-no) + '  Coil Id: ' + string(mult.coil-id) + '  Mult Wt:  ' + string(iNegMsgWT). */
/*          run Alert(input 120,             /* Alert ID# */                                                                                                     */
/*              input cNegMsg,           /* Alert arg */                                                                                                         */
/*              input "",             /* Alert Buffers */                                                                                                        */
/*              input "",            /* Contact Type */                                                                                                          */
/*              input 0,              /* Contact Id */                                                                                                           */
/*              input "",    /* group name */                                                                                                                    */
/*              output lNegResult) no-error.      /* Result */                                                                                                   */
/*      end.                                                                                                                                                     */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


