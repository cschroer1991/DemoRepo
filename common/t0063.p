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
    Notes       : 25165 - kmk - 07/26/06 - Removed original note copied
                        from messages.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* User to assign on new note */
define input  parameter pcUser as character  no-undo.
/* Note Id to copy */
define input parameter piOldNoteID as integer    no-undo.
/* New Note Subject */
define input  parameter pcSubject as character  no-undo.
/* Pass back the new Note Id to save in the appropriate note field */
define output parameter piNewNoteID as integer    no-undo.

define buffer bNewNote for note.
define buffer bNewNotePage for note-page.

/* recheck the notecnt and renumber the new pages */
define variable iNoteCnt as integer    no-undo.

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

/* find the note to copy */
find first note no-lock where note.note-id = piOldNoteID no-error.
if not available note then
   leave.

/* get the id for the new note, recount the note pages as a double check */
assign piNewNoteId = dynamic-function("getNextIdInt",0,0,10)
       iNoteCnt = 0.
if not piNewNoteId > 0 then do:
  /* note id not assigned, nothing to copy to */
  return.
end.

/* copy the note header */
create bNewNote.
assign bNewNote.note-id = piNewNoteId
       bNewNote.mod-date = today
       bNewNote.subject = pcSubject
       bNewNote.type = note.type.

/* copy each note page */
for each note-page no-lock where note-page.note-id = note.note-id 
                           by    note-page.page-no:
    create bNewNotePage.
    ASSIGN bNewNotePage.note-id = bNewNote.note-id
           bNewNotePage.mod-date = today
           bNewNotePage.logon = pcUser
           bNewNotePage.txt = note-page.txt.
    assign iNoteCnt = iNoteCnt + 1
           bNewNotePage.page-no = iNoteCnt.
end. /* for each note-page */


assign bNewNote.page-cnt = iNoteCnt.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


