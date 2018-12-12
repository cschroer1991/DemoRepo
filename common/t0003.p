/* savemenudata.p */

define temp-table gttmbMenu no-undo like mbMenu.

define input parameter pcProcName as char.
define input parameter pcGroupID as char.
define input parameter table for gttmbMenu.
define output parameter pcResult as char.

do transaction on error undo, retry:

  if retry then do:
      pcResult = "Menu Changes not saved. Database Commit Aborted.".
      leave.
  end.

  for each mbMenu where mbMenu.ProcName = pcProcName and
                       mbMenu.GroupID = pcGroupID exclusive-lock:
    delete mbMenu.
  end.

  for each gttmbMenu:
      create mbMenu.
      buffer-copy gttmbMenu to mbMenu.
  end.

  pcResult = "Menu Changes saved.".
end.
