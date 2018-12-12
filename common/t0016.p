&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0016.p 
    Purpose     :

    Syntax      :

    Description : Get Inv and Hold Notes

    Author(s)   :
    Created     :
    Notes       :
    
   Date     By   Issue  Remark 
 ========  ===  ======  =================================================   
 10/29/10  djt   53373  Do not show inactive notes.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

  define temp-table ttNotes
      field hold-inv like inv.inv-id
      field active like hold.active format "Y/N"
      field note-id like note-page.note-id
      field txt like note-page.txt
      field page-no like note-page.page-no
      field tdate as date
      field ttime like inv.create-time
      field holdrecid as recid
      field invrecid as recid
      field logon like note-page.logon
      field note-date like note-page.mod-date
      field tbl as char
      index sort-date tdate ttime.

  define input parameter piInv as integer.
  define output parameter table for ttNotes.

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

find inv where inv.inv-id = piInv no-lock no-error.
if not available inv then
  return.

find rcpt where rcpt.rcpt-id = inv.rcpt-id no-lock no-error.
if inv.note-id = 0 or inv.note-id = ? 
    or (inv.note-id ne ?
        and inv.note-id > 0
        and not can-find(first note-page
                         where note-page.note-id = inv.note-id
                         and note-page.inactive = no)) then do:
    create ttnotes.
    assign ttnotes.hold-inv = inv.inv-id
           ttnotes.active   = ?
           ttnotes.tdate    = rcpt.rcpt-date
           ttnotes.tbl      = "inv"
           ttnotes.txt      = "No Active Notes Available"
           ttnotes.ttime    = 0.
end.
else do:
    for each note-page
        where note-page.note-id = inv.note-id
        and not note-page.inactive no-lock:
        create ttnotes.
        assign ttnotes.hold-inv  = inv.inv-id
               ttnotes.active    = ?
               ttnotes.note-id   = note-page.note-id 
               ttnotes.page-no   = note-page.page-no 
               ttnotes.logon     = note-page.logon
               ttnotes.note-date = note-page.mod-date
               ttnotes.tbl       = "inv"
               ttnotes.txt       = note-page.txt
               ttnotes.tdate     = rcpt.rcpt-date
               ttnotes.ttime     = 0.
    end.
end.
for each hold where hold.inv-id= piInv and hold.note-id > 0 no-lock,
    each note-page
    where note-page.note-id = hold.note-id
    and not note-page.inactive no-lock:
      create ttnotes.
      assign ttnotes.hold-inv = hold.hold-code
             ttnotes.active    = hold.active
             ttnotes.note-id   = note-page.note-id 
             ttnotes.page-no   = note-page.page-no 
             ttnotes.logon     = note-page.logon
             ttnotes.note-date = note-page.mod-date
             ttnotes.tbl       = "hold"
             ttnotes.txt       = note-page.txt
             ttnotes.tdate     = hold.hold-date
             ttnotes.ttime     = hold.hold-time
             ttnotes.holdrecid = recid(hold).
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


