&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0140.p
    Purpose     :

09/07/10 zakh 53412 Now can return Work Order Notes as well.
10/06/10 djt  53373 Don't display inactive note pages
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

  define input  parameter piKeyId        as integer   no-undo.
  define input  parameter pcType         as character no-undo. 
  define output parameter pcEditorString as character no-undo.
 
  define variable i         as integer no-undo.
  define variable rRecId    as recid no-undo.
  define variable v-label   as character no-undo.
  define variable v-date    as date no-undo.
  define variable v-logon   like note-page.logon no-undo.
  define variable v-note-id like inv.note-id no-undo.
  define variable v-page    as character no-undo.
  
  define temp-table t-notes no-undo                                          
      field hold-inv like inv.inv-id format ">>>>>>>>>9"            
      field active like hold.active format "Y/N"                    
      field note-id like note-page.note-id                          
      field txt like note-page.txt                                  
      field page-no like note-page.page-no                          
      field t-date as date                                          
      field t-time like inv.create-time                             
      field hold-recid as recid 
      field inv-recid as recid
      field logon like note-page.logon                              
      field note-date like note-page.mod-date                       
      field tbl as character
      index sort-date t-date t-time.

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
if pcType = 'Inv' then do:
    run CreateInvTT.
    run PopulateInvString.
end.
if pcType = 'Order' then 
    run PopulateOrderString.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreateInvTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateInvTT Procedure 
PROCEDURE CreateInvTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

empty temp-table t-notes.
assign i = 0.
find inv  
     where inv.inv-id = piKeyId no-lock no-error.
if not available inv then
  return.
find rcpt 
     where rcpt.rcpt-id = inv.rcpt-id no-lock no-error.
if inv.note-id = 0 or inv.note-id = ? then 
do:
    create t-notes.
    assign
      t-notes.hold-inv = inv.inv-id
      t-notes.active = ?
      t-notes.t-date = rcpt.rcpt-date
      t-notes.tbl = "inv"
      t-notes.txt = "No Notes Available"
      t-notes.t-time = 0.
end.
else
do:                                                                
  for each note-page 
       where note-page.note-id = inv.note-id 
       and note-page.inactive ne true no-lock:     
     create t-notes.                                                       
     assign  
       i = i + 1
       t-notes.hold-inv = inv.inv-id                                     
       t-notes.active = ?                                                
       t-notes.note-id = note-page.note-id when avail note-page          
       t-notes.page-no = note-page.page-no when avail note-page 
       t-notes.logon = note-page.logon                                   
       t-notes.note-date = note-page.mod-date 
       t-notes.tbl = "inv"                                               
       t-notes.txt = note-page.txt
       t-notes.t-date = rcpt.rcpt-date
       t-notes.t-time = 0.                                      
  end.  
  if i = 0 then do:
      /* there were notes, bet none are active */
      create t-notes.
      assign
        t-notes.hold-inv = inv.inv-id
        t-notes.active = ?
        t-notes.t-date = rcpt.rcpt-date
        t-notes.tbl = "inv"
        t-notes.txt = "No Active Notes Available"
        t-notes.t-time = 0.
  end.
end.                                                                    

for each hold
         where hold.inv-id = piKeyID
         and   hold.note-id > 0 no-lock,     
    each note-page 
         where note-page.note-id = hold.note-id
         and note-page.inactive ne true no-lock:         
     create t-notes.                                                            
     assign                                                                   
       t-notes.hold-inv = hold.hold-code                                      
       t-notes.active = hold.active                                           
       t-notes.note-id = note-page.note-id                                    
       t-notes.page-no = note-page.page-no                                    
       t-notes.t-date = hold.hold-date                                        
       t-notes.t-time = hold.hold-time                                        
       t-notes.hold-recid = RECID(hold)                                       
       t-notes.logon = note-page.logon                                        
       t-notes.note-date = note-page.mod-date                                 
       t-notes.tbl = "hold"                                                   
       t-notes.txt = note-page.txt.                                           
end.                                                                       

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PopulateInvString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateInvString Procedure 
PROCEDURE PopulateInvString :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable v-tab as character no-undo initial "     ".
define variable v-separator as character no-undo.

for each 
    t-notes 
    break by t-date 
          by hold-inv:
    do:
      if first-of(hold-inv) then 
      do:
          if t-notes.tbl = "inv" then 
              v-label = "INVENTORY NOTES:          ".
          else 
          if t-notes.active then
              v-label = "ACTIVE HOLD NOTES:   HC= " + string(t-notes.hold-inv).
          else 
              v-label = "INACTIVE HOLD NOTES: HC= " + string(T-NOTES.hold-inv).
      end. /* first-of */
      else
          v-label = "                          ".

              
      if t-notes.page-no = ? then
        v-page = "0".
      else
        v-page = string(t-notes.page-no).
      
      if t-notes.note-date = ? then
        v-date = t-date.
      else
        v-date = t-notes.note-date.
      
      if t-notes.logon = ? then
        v-logon = "".
      else 
        v-logon = t-notes.logon.
    end. /* do */

    pcEditorString = pcEditorString + chr(10) 
           + v-label + v-tab
           + " ADDED: " + string(v-date) + "  "  
           + " BY: " + v-logon + v-tab 
           + "PAGE: " + v-page + v-tab + chr(10)
           + t-notes.txt + chr(10).
    if last-of(hold-inv) then
        pcEditorString = pcEditorString 
           + "-----------------------------------------------------------------------------".
end. /* for each */
empty temp-table t-notes.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PopulateOrderString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateOrderString Procedure 
PROCEDURE PopulateOrderString :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define variable v-tab as character no-undo initial "     ".
define variable v-separator as character no-undo.

find work-order no-lock  
     where work-order.order-id = piKeyId no-error.
if not available work-order then
    return.

if work-order.note-id = 0 or work-order.note-id = ? then do:
    pcEditorString = "No Notes Available".
    return.
end.
pcEditorString = "WORK ORDER NOTES:".
for each note-page no-lock
    where note-page.note-id = work-order.note-id 
    and note-page.inactive ne true
    by note-page.mod-date:     

    if note-page.page-no = ? then
        v-page = "0".
    else
        v-page = string(note-page.page-no).
    if note-page.mod-date = ? then
        v-date = work-order.CreateDate.
    else
        v-date = note-page.mod-date.
      
    if note-page.logon = ? then
        v-logon = "".
    else 
        v-logon = note-page.logon.

    pcEditorString = pcEditorString + chr(10)
           + "ADDED: " + string(v-date) + "  "  
           + " BY: " + v-logon + v-tab 
           + "PAGE: " + v-page + v-tab + chr(10)
           + note-page.txt + chr(10).
end. /* for each */

pcEditorString = pcEditorString 
      + "-----------------------------------------------------------------------------".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

