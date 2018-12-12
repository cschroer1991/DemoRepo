&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
File        : common\t0078.p
Purpose     : Copy/Version a spec
Notes       : 
         kmk  27177 do not wipe out scrap and scrap paint except for Minster 
                    who are the only ones that use this.
         cel  27337 When a spec is versioned make sure the Images follow the 
                    new version.
         djt  52390 Notes are now pre-edited and passed in
         cjs  56765 Added ODMin and ODMax fields.
         djt  73181 appserver errors copying spec images
02/24/17 zakh 74206 don't copy pckg-codes.
03/17/17 zakh 74480 Now copy pckg-codes if it is a new version.
01/18/18 Mas  76553 Added 14 new Fields to Accomodate the Copy Specs Screen
                    Known As The Visible Screen (d0283.p)
                    
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter piPartnerID     as integer    no-undo.
define input  parameter piPartID        as integer    no-undo.
define input  parameter pcDescription   as character  no-undo.
define input  parameter piProcCode      as integer    no-undo.
define input  parameter piSpecID        as integer    no-undo.
define input  parameter plNewVersion    as logical  no-undo.
define input  parameter piPSIPO         as integer  no-undo.
define input  parameter pcSetupText     as char no-undo.
define input  parameter pcProcessText   as char no-undo.
define input  parameter pcWrapText      as char no-undo.
define input  parameter pcPckgText      as char no-undo.
define input  parameter pcShipText      as char no-undo.
define input  parameter peGauge-Neg     as decimal no-undo.
define input  parameter peGauge-Pos     as decimal no-undo.
define input  parameter peWdth-Neg      as decimal no-undo.
define input  parameter peWdth-Pos      as decimal no-undo.
define input  parameter peLngth-Neg     as decimal no-undo.
define input  parameter peLngth-Pos     as decimal no-undo.
define input  parameter peLngth2-Neg    as decimal no-undo.
define input  parameter peLngth2-Pos    as decimal no-undo.
define input  parameter piMandrel1      as integer no-undo.
define input  parameter piMandrel2      as integer no-undo.
define input  parameter piMandrel3      as integer no-undo.
define input  parameter piStencilType   as integer no-undo.
define input  parameter piInterleaving  as integer no-undo.
define input  parameter pcPckgCode      as Character no-undo.
define output parameter poNewSpecID     as integer    no-undo.

define buffer bNewSpec for spec.
define buffer bChoice771 for Choice.

define variable cUserID as character  no-undo.
define variable iNoteID as int no-undo.

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

/* the record should always exist but just incase leave the copy if it 
 * somehow got deleted between the dialog's sdo loading and the copy */
find first spec exclusive-lock where spec.spec-id = piSpecID no-error.
if not avail spec then
    do:
        dynamic-function("setMessage",
            "The spec-id could not be found please verify the "
            + "correct spec is selected.!").
    end. /* not avail spec */
else
    do: /* Copy Spec */
        /*  First Check And Make Sure That No Minimum Tolerances Are Greater Than The Maximum Tolerances   */
        if peGauge-Neg > peGauge-Pos then
        do:
            dynamic-function("setMessage",
                "This Spec has Not Been Copied." + 
                "~nMinimum Gauge Tolerance Cannot Be > Maximum Gauge Tolerance." + 
                "~nPlease Try Copy Again").
            return {fn getUserMsg}.
        end.
        if peWdth-Neg > peWdth-Pos then
        do:
            dynamic-function("setMessage",
                "This Spec has Not Been Copied." + 
                "~nMinimum Width Tolerance Cannot Be > Maximum Width Tolerance." + 
                "~nPlease Try Copy Again").
            return {fn getUserMsg}.
        end.
        if peLngth-Neg > peLngth-Pos then
        do:
            dynamic-function("setMessage",
                "This Spec has Not Been Copied." + 
                "~nMinimum Length Tolerance Cannot Be > Maximum Length Tolerance." + 
                "~nPlease Try Copy Again").
            return {fn getUserMsg}.
        end.
        if peLngth2-Neg > peLngth2-Pos then
        do:
            dynamic-function("setMessage",
                "This Spec has Not Been Copied." + 
                "~nMinimum Length2 Tolerance Cannot Be > Maximum Length2 Tolerance." + 
                "~nPlease Try Copy Again").
            return {fn getUserMsg}.
        end.

        /*  Second Make Sure That The piStencilType <> 0 If The Spec.Stenciler is yes    */
        if Spec.Stenciler = yes and piStencilType = 0 then
        do:
            dynamic-function("setMessage",
                "This Spec has Not Been Copied." + 
                "~nStenciling is set to required, so you must pick the Stencil Type. " + 
                "~nPlease Try Copy Again").
            return {fn getUserMsg}.
        end.

        /*  Third Make Sure That The Pckg-Code Equals what is set up in the Choice Field    */
        /*  If This Owner-Id Is In The Choice Field                                         */
        find first Partner no-lock where Partner.Partner-Id = piPartnerId no-error.
        if avail partner then do:
            find first choice no-lock where choice.field-no = 780
            and choice.val = partner.owner-id no-error.
            if avail choice then do:
                if pcPckgCode = ? or pcPckgCode = "" then
                    do:
                        dynamic-function("setMessage",
                            "This Spec has Not Been Copied." + 
                            "~nPckg Code Is Required For This Customer." +
                            "~nPlease Try Copy Again").
                        return {fn getUserMsg}.
                    end.
                else if not can-find(first bChoice771 no-lock 
                               where bChoice771.field-no = 771
                               and bChoice771.misc1 = pcPckgCode
                               and bChoice771.misc3 = string(partner.owner-id)) 
                    and not can-find(first bChoice771 no-lock 
                               where bChoice771.field-no = 771
                               and bChoice771.misc1 = pcPckgCode
                               and bChoice771.misc3 = "") then
                    do:
                        dynamic-function("setMessage",
                            "This Spec has Not Been Copied." + 
                            "~nPckg Code Is Not a Valid Value." +
                            "~nPlease Try Copy Again").
                        return {fn getUserMsg}.
                    end.
            end. /*if avail choice*/
        end. /*if avail partner*/

        /*  Final Edits Are Done And Have Passed So Continue On */
        /* get the new id for the spec and assign it to the parameter
        * to be passed back to the window to reposition to the new spec */
        assign poNewSpecID = dynamic-function("getNextIdInt",0,0,32)
            cUserId = dynamic-function('GetGlobalVar','UserID').

        BUFFER-COPY spec EXCEPT 
                /* fields user must enter manually if copy, set below if version */
                spec.edge-prot 
                spec.id-paint
                spec.id-paint-val
                spec.min-mult-wt
                spec.max-mult-wt
                spec.ODMin
                spec.ODMax
                spec.order-id 
                spec.scrap
                spec.scrap-paint
                /* key fields pgm sets */
                spec.obsolete
                spec.lockDate
                spec.version
                spec.partner-id 
                spec.spec-id
                spec.logon
                spec.mod-date 
                spec.part-id 
                spec.descr 
                spec.proc-code
                spec.prevSpecID 
                spec.psipo /* passed in if was spec screen was called from process orders */
                /* set with the new id's of the copied notes */
                spec.note-id 
                spec.ship-note-id
                spec.pckg-note-id 
                spec.proc-note-id 
                spec.setup-note-id 
                spec.wrap-note-id 
                spec.special-tag
                spec.specialsticky
/*                 Spec.Pckg-Code  */
        TO bNewSpec ASSIGN  bNewSpec.spec-id    = poNewSpecID
                            bNewSpec.mod-date   = today
                            bNewSpec.logon      = cUserID
                            bNewSpec.obsolete   = no
                            bNewSpec.Gauge-Neg  = peGauge-Neg
                            bNewSpec.Gauge-Pos  = peGauge-Pos
                            bNewSpec.Wdth-Neg   = peWdth-Neg
                            bNewSpec.Wdth-Pos   = peWdth-Pos
                            bNewSpec.Lngth-Neg  = peLngth-Neg
                            bNewSpec.Lngth-Pos  = peLngth-Pos
                            bNewSpec.Lngth2-Neg = peLngth2-Neg
                            bNewSpec.Lngth2-Pos = peLngth2-Pos
                            bNewSpec.Mandrel[1] = piMandrel1
                            bNewSpec.Mandrel[2] = piMandrel2
                            bNewSpec.Mandrel[3] = piMandrel3
                            bNewSpec.StencilType= piStencilType
                            bNewSpec.Interleav  = piInterleaving
                            bNewSpec.Pckg-Code  = pcPckgCode
                            NO-ERROR. 
        if plNewVersion then
            do:
                assign 
                    bNewSpec.prevSpecId   = spec.spec-id
                    bNewSpec.edge-prot    = spec.edge-prot 
                    bNewSpec.id-paint     = spec.id-paint
                    bNewSpec.id-paint-val = spec.id-paint-val
                    bNewSpec.min-mult-wt  = spec.min-mult-wt
                    bNewSpec.max-mult-wt  = spec.max-mult-wt
                    bNewSpec.ODMin       = spec.ODMin
                    bNewSpec.ODMax       = spec.ODMax
                    bNewSpec.order-id     = spec.order-id 
                    bNewSpec.psipo        = spec.psipo 
                    bNewSpec.scrap        = spec.scrap
                    bNewSpec.scrap-paint  = spec.scrap-paint
                    bNewspec.version      = spec.version + 1
                    bNewSpec.partner-id   = Spec.partner-id
                    bNewSpec.Part-id      = Spec.Part-id
                    bNewSpec.descr        = Spec.descr
                    bNewSpec.proc-code    = Spec.proc-code
/*                     bNewSpec.pckg-code    = spec.pckg-code  */
                    /* obsolete versioned spec */
                    spec.obsolete         = yes.      

                run common/t0457.p (input string(spec.spec-id),
                                      input string(bNewSpec.spec-id),
                                      input ?,
                                      input 'CopySpecImages').

            end.
        else
            do: /* regular copy */
                assign
                    bNewSpec.partner-id = piPartnerId
                    bNewSpec.Part-id    = piPartID
                    bNewSpec.descr      = pcDescription
                    bNewSpec.proc-code  = piProcCode
                    bNewSpec.version    = 1
                    bNewSpec.prevSpecId = 0
                    bNewSpec.psipo      = piPSIPO.
            end.
        /* Assign the scrap and scrap paint values for all other plants
        because only Minster paints their scrap */
        if dynamic-function('GetGlobalVar','PlantID') <> 1 then do:
            assign
                bNewSpec.scrap = spec.scrap
                bNewSpec.scrap-paint = spec.scrap-paint.
        end.
        /* copy notes (now pre-edited and passed in)*/

        if pcSetupText ne ? and pcSetupText ne '' then do:
            assign iNoteID = ?.
            run lib/al0003.p (input-output iNoteID
                              , pcSetupText
                              , 15
                              , string(bNewSpec.spec-id)).
            if iNoteID ne ? and iNoteID > 0 then assign bNewSpec.setup-note-id = iNoteID.
        end.
        if pcProcessText ne ? and pcProcessText ne '' then  do:
            assign iNoteID = ?.
            run lib/al0003.p (input-output iNoteID
                              , pcProcessText
                              , 16
                              , string(bNewSpec.spec-id)).
            if iNoteID ne ? and iNoteID > 0 then assign bNewSpec.proc-note-id = iNoteID.
        end.
        if pcWrapText ne ? and pcWrapText ne '' then  do:
            assign iNoteID = ?.
            run lib/al0003.p (input-output iNoteID
                              , pcWrapText
                              , 17
                              , string(bNewSpec.spec-id)).
            if iNoteID ne ? and iNoteID > 0 then assign bNewSpec.wrap-note-id = iNoteID.
        end.
        if pcPckgText ne ? and pcPckgText ne '' then  do:
            assign iNoteID = ?.
            run lib/al0003.p (input-output iNoteID
                              , pcPckgText
                              , 18
                              , string(bNewSpec.spec-id)).
            if iNoteID ne ? and iNoteID > 0 then assign bNewSpec.pckg-note-id = iNoteID.
        end.
        if pcShipText ne ? and pcShipText ne '' then  do:
            assign iNoteID = ?.
            run lib/al0003.p (input-output iNoteID
                              , pcShipText
                              , 13
                              , string(bNewSpec.spec-id)).
            if iNoteID ne ? and iNoteID > 0 then assign bNewSpec.ship-note-id = iNoteID.
        end.
end. /* copy spec */

return {fn getUserMsg}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


