&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/reports/r0640.p
    Purpose     : Spec Notes Update/Delete 

    Syntax      :

    Description : This program runs through all specs from criteria entered
                  from d0294.w and creates a temp table with spec notes that
                  will be updated/deleted.  After user views report and 
                  hits commit, it will update all note-page records.
                  
    Author(s)   : Brad Feltz    
    Created     : 10/29/10
    Notes       :
    
    History     :
    01/27/11 - bmf - 55072 Add logic for mass spec note add function.
    09/23/14 - cjs - 66190 Add length range to search for specs.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{includes/i0001.i}
{includes/i0187.i}  /* spec note update temp tables */

define input parameter pcProc as char.
define input-output parameter table for ttInput.
define input-output parameter table for ttSpecNotes.
define output parameter piSpecCnt as int.
define output parameter piNoteCnt as int.
define output parameter pcRtnMsg as char.

define variable cTxt         as character  no-undo.
define variable giPlantID    as integer    no-undo.
define variable iNoteType    as integer    no-undo.

define buffer bOwner for cust.
define buffer bUser for cust.

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
         HEIGHT             = 9.19
         WIDTH              = 49.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

assign giPlantID = int({fnarg getGlobalvar 'plantID'}).

if pcProc = "CreateTT" then
do:
  empty temp-table ttSpecNotes.
  assign piNoteCnt = 0
         piSpecCnt = 0.

  run QueryFilter.
end.

if  pcProc = "Commit" then
do:
    run CommitSpecChanges.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ClearSpecNoteID) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ClearSpecNoteID Procedure 
PROCEDURE ClearSpecNoteID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  Note Types:
  
------------------------------------------------------------------------------*/
find spec exclusive-lock where spec.spec-id = ttSpecNotes.ispecID no-error.
if available spec then
do:
    case ttSpecNotes.iNoteType:
        when 8 then
            assign spec.note-id = 0.
        when 13 then
            assign spec.ship-note-id = 0.
        when 15 then
            assign spec.setup-note-id = 0.
        when 16 then
            assign spec.proc-note-id = 0.
        when 17 then
            assign spec.wrap-note-id = 0.
        when 18 then
            assign spec.pckg-note-id = 0.
    end case.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CommitSpecChanges) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CommitSpecChanges Procedure 
PROCEDURE CommitSpecChanges :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var lNotePageDeleted as logical no-undo init no.
  def var iNewNoteID as int.
  def var cCharID as char.
  def buffer bNotePage for note-page.
  def buffer bSpec for spec.

  for each ttSpecNotes:
    /* if iNoteID = 0, then we are adding new notes */
    if ttSpecNotes.iNoteID = 0 then
    do:
        run lib/l0053 (0,0, 10, output iNewNoteID, output cCharID).
        find bSpec no-lock
            where bSpec.spec-id eq ttSpecNotes.iSpecID no-error.
        create note.
        assign note.note-id  = iNewNoteID
               note.mod-date = today
               note.page-cnt = 1
               note.subject  = string(bSpec.spec-id)
               note.type     = ttSpecNotes.iNoteType.
        create note-page.
        assign note-page.logon = {fnarg getGlobalVar 'logon'}
               note-page.mod-date = today
               note-page.note-id = note.note-id
               note-page.page-no = 1
               note-page.txt = ttSpecNotes.cNewTxt.
        find current bSpec exclusive-lock no-error.
        case ttSpecNotes.iNoteType:
            when 8 then
                assign bspec.note-id = iNewNoteID.
            when 13 then
                assign bspec.ship-note-id = iNewNoteID.
            when 15 then
                assign bspec.setup-note-id = iNewNoteID.
            when 16 then
                assign bspec.proc-note-id = iNewNoteID.
            when 17 then
                assign bspec.wrap-note-id = iNewNoteID.
            when 18 then
                assign bspec.pckg-note-id = iNewNoteID.
        end case.
    end.
    else
    /* this will fire if note already exists for spec, adding new note page */
    if ttSpecNotes.iNotePage = 0 then
    do:
        /* add note page, update note - page count */
        find note no-lock where note.note-id = ttSpecNotes.iNoteID.
        create note-page.
        assign note-page.logon = {fnarg getGlobalVar 'logon'}
               note-page.mod-date = today
               note-page.note-id = note.note-id
               note-page.page-no = note.page-cnt + 1
               note-page.txt = ttSpecNotes.cNewTxt.
        find current note exclusive-lock no-error.
        assign note.page-cnt = note.page-cnt + 1.
    end.
    /* this logic will fire if not adding a new note/note-page */
    else
    do:
      find first note-page exclusive-lock
          where note-page.note-id eq ttSpecNotes.iNoteID
            and note-page.page-no eq ttSpecNotes.iNotePage no-error.
      if available note-page then
      do:
          assign iNotePage = note-page.page-no.
          /* if replaced note is blank, delete note-page, 
             renumber all other pages */
          if ttSpecNotes.cNewTxt = "" then
          do:
            delete note-page.
            lNotePageDeleted eq true.
            find note exclusive-lock
                where note.note-id eq ttSpecNotes.iNoteID no-error.
            if available note then
                assign note.page-cnt = note.page-cnt - 1.

            /* delete note if no pages exist */
            if note.page-cnt = 0 then
            do:
              delete note.
              run ClearSpecNoteID.
            end.
            else
            do:
                if lNotePageDeleted then do:
                   for each bNotePage exclusive-lock
                       where bNotePage.note-id eq note.note-id
                         and bNotePage.page-no gt iNotePage:
                       assign bNotePage.page-no = bNotePage.page-no - 1.
                   end.
                end.
            end.
          end.
          else
            /* replace current note with new note - built in CreateTTSpecNotes */
            assign note-page.txt = ttSpecNotes.cNewTxt.
      end.
      else
      do:
        if pcRtnMsg <> "" then assign pcRtnMsg = pcRtnMsg 
                + "~n Verify Results, there were problems with some note updates".
        else
           pcRtnMsg = "Verify Results, there were problems with some note updates".
      end.
    end.  /* ttSpecNotes.iNoteID = 0 (add) */
  end.  /* for each ttSpecNotes */
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateTTAdd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTTAdd Procedure 
PROCEDURE CreateTTAdd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if ttInput.lSpecNotes then
  do:
      find first choice no-lock
            where choice.field-no eq 11
              and choice.val eq 8 no-error.
      find first note no-lock 
           where note.note-id eq spec.note-id no-error.
      run CreateTTSpecNotes.
  end.
  if ttInput.lSetupNotes then
  do:
      find first choice no-lock
            where choice.field-no eq 11
              and choice.val eq 15 no-error.
      find first note no-lock 
           where note.note-id eq spec.setup-note-id no-error.
      run CreateTTSpecNotes.
  end.
  if ttInput.lProcNotes then
  do:
      find first choice no-lock
            where choice.field-no eq 11
              and choice.val eq 16 no-error.
      find first note no-lock 
           where note.note-id eq spec.proc-note-id no-error.
      run CreateTTSpecNotes.
  end.
  if ttInput.lSeqNotes then
  do:
      find first choice no-lock
            where choice.field-no eq 11
              and choice.val eq 17 no-error.
      find first note no-lock 
           where note.note-id eq spec.wrap-note-id no-error.
      run CreateTTSpecNotes.
  end.
  if ttInput.lPckgNotes then
  do:
      find first choice no-lock
            where choice.field-no eq 11
              and choice.val eq 18 no-error.
      find first note no-lock 
           where note.note-id eq spec.pckg-note-id no-error.
      run CreateTTSpecNotes.
  end.
  if ttInput.lShipNotes then
  do:
      find first choice no-lock
            where choice.field-no eq 11
              and choice.val eq 13 no-error.
      find first note no-lock 
            where note.note-id eq spec.ship-note-id no-error.
      run CreateTTSpecNotes.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateTTSpecNotes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTTSpecNotes Procedure 
PROCEDURE CreateTTSpecNotes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  Need to replace old text with new, but after note-page is trimmed
          and carriage return/line feeds removed.       
------------------------------------------------------------------------------*/
    create ttSpecNotes.
    assign 
          ttSpecNotes.iSpecID      = spec.spec-id
          ttSpecNotes.cSpecDesc    = spec.descr
          ttSpecNotes.iPartnerID   = partner.partner-id
          ttSpecNotes.cPartnerName = partner.name
          ttSpecNotes.iOwnerID     = partner.owner-id
          ttSpecNotes.cOwnerName   = bOwner.name
          ttSpecNotes.iUserID      = partner.user-id
          ttSpecNotes.cUserName    = bUser.name
          ttSpecNotes.iPartID      = part.part-id
          ttSpecNotes.cPartNo      = part.part-no
          ttSpecNotes.eGauge       = part.gauge
          ttSpecNotes.eWdth        = part.wdth
          ttSpecNotes.eLength      = part.lngth
          ttSpecNotes.cMetalType   = type.descr
          ttSpecNotes.iNoteID      = (if available note then note.note-id
                                      else 0)
          ttSpecNotes.iNoteType    = choice.val
          ttSpecNotes.iNotePage    = (if ttInput.cOldText = "" then 0
                                      else note-page.page-no)
          ttSpecNotes.cNoteType    = choice.descr
          ttSpecNotes.cOldTxt      = (if ttInput.cOldText = "" then "" 
                                     else note-page.txt)
          ttSpecNotes.cNewTxt      = (if ttInput.cOldText = "" then ttInput.cNewText
                                     else
                    /* Don't get rid of CR and LF or else the note later down 
                       will squish down and possibly be unreadable */
                    replace(note-page.txt,ttInput.cOldText,ttInput.cNewText)).
          .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateTTUpdDlt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateTTUpdDlt Procedure 
PROCEDURE CreateTTUpdDlt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     using trim(replace(replace(note-page.txt,chr(10),""),chr(13),""))
             to remove all carriage return/line feeds to match against text 
             entered.  
------------------------------------------------------------------------------*/

  /* add '*'s around entered text for match statement */
  assign cTxt = "*" + ttInput.cOldText + "*".

  if ttInput.lSpecNotes 
  and spec.note-id <> 0 then
  do:
    for each note-page no-lock where note-page.note-id eq spec.note-id:
      
         if replace(replace(note-page.txt,chr(10),""),chr(13),"") matches cTxt then
        do:
            find first note no-lock 
                  where note.note-id eq note-page.note-id no-error.
            find first choice no-lock
                  where choice.field-no eq 11
                    and choice.val eq note.type no-error.
            run CreateTTSpecNotes.
        end.
    end.
  end.
  if ttInput.lSetupNotes 
  and spec.setup-note-id <> 0 then
  do:
    for each note-page no-lock where note-page.note-id eq spec.setup-note-id:
        if replace(replace(note-page.txt,chr(10),""),chr(13),"") matches cTxt then
        do:
            find first note no-lock 
                  where note.note-id eq note-page.note-id no-error.
            find first choice no-lock
                  where choice.field-no eq 11
                    and choice.val eq note.type no-error.
            run CreateTTSpecNotes.
        end.
    end.
  end.
  if ttInput.lProcNotes
  and spec.proc-note-id <> 0 then
  do:
    for each note-page no-lock where note-page.note-id eq spec.proc-note-id:
        if replace(replace(note-page.txt,chr(10),""),chr(13),"") matches cTxt then
        do:
            find first note no-lock 
                  where note.note-id eq note-page.note-id no-error.
            find first choice no-lock
                  where choice.field-no eq 11
                    and choice.val eq note.type no-error.
            run CreateTTSpecNotes.
        end.
    end.
  end.
  if ttInput.lSeqNotes 
  and spec.wrap-note-id <> 0 then
  do:
    for each note-page no-lock where note-page.note-id eq spec.wrap-note-id:
        if replace(replace(note-page.txt,chr(10),""),chr(13),"") matches cTxt then
        do:
            find first note no-lock 
                  where note.note-id eq note-page.note-id no-error.
            find first choice no-lock
                  where choice.field-no eq 11
                    and choice.val eq note.type no-error.
            run CreateTTSpecNotes.
        end.
    end.
  end.
  if ttInput.lPckgNotes
  and spec.pckg-note-id <> 0 then
  do:
    for each note-page no-lock where note-page.note-id eq spec.pckg-note-id:
       if replace(replace(note-page.txt,chr(10),""),chr(13),"") matches cTxt then
        do:
            find first note no-lock 
                  where note.note-id eq note-page.note-id no-error.
            find first choice no-lock
                  where choice.field-no eq 11
                    and choice.val eq note.type no-error.
            run CreateTTSpecNotes.
        end.
    end.
  end.
  if ttInput.lShipNotes
  and spec.ship-note-id <> 0 then
  do:
    for each note-page no-lock where note-page.note-id eq spec.ship-note-id:
        if replace(replace(note-page.txt,chr(10),""),chr(13),"") matches cTxt then
        do:
            find first note no-lock 
                  where note.note-id eq note-page.note-id no-error.
            find first choice no-lock
                  where choice.field-no eq 11
                    and choice.val eq note.type no-error.
            run CreateTTSpecNotes.
        end.
    end.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-QueryFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryFilter Procedure 
PROCEDURE QueryFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find first ttInput.

  for each partner no-lock
            where partner.plant-id eq giPlantID
              and (partner.partner-id eq ttInput.iPartnerID
               or  ttInput.iPartnerID eq 0)
              and (partner.owner-id eq ttInput.iOwnerID
               or  ttInput.iOwnerID eq 0)
              and (partner.user-id eq ttInput.iUserID
               or  ttInput.iUserID eq 0),
      each part no-lock
            where part.partner-id eq partner.partner-id
              and (part.gauge ge ttInput.eGaugeLo
              and  part.gauge le ttInput.eGaugeHi
               or  (ttInput.eGaugeLo eq 0 and ttInput.eGaugeHi eq 0))
              and (part.wdth ge ttInput.eWdthLo
              and  part.wdth le ttInput.eWdthHi
               or  (ttInput.eWdthLo eq 0 and ttInput.eWdthHi eq 0))
              and (part.lngth ge ttInput.eLengthLo
              and  part.lngth le ttInput.eLengthHi
               or  (ttInput.eLengthLo eq 0 and ttInput.eLengthHi eq 0))
              and  (part.type-id eq ttInput.iTypeID
               or   ttInput.iTypeID eq 0),
      each spec no-lock
            where spec.partner-id eq partner.partner-id
              and spec.part-id    eq part.part-id
              and not spec.obsolete
              and spec.order-id eq 0:
    
      find first bOwner no-lock where bOwner.cust-id eq partner.owner-id no-error.
      find first bUser no-lock where bUser.cust-id eq partner.user-id no-error.
      find first type no-lock where type.type-id eq part.type-id no-error.


      if  ttInput.cOldText = "" then
          run CreateTTAdd.
      else
          run CreateTTUpdDlt.

  end.

  for each ttSpecNotes break by ttSpecNotes.iSpecID:
      assign piNoteCnt = piNoteCnt + 1.
      if first-of(ttSpecNotes.iSpecID) then
          assign piSpecCnt = piSpecCnt + 1.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

