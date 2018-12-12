/* Gather all Global var values from */
/* sesContext table.  Externalized   */
/* so main program can be run w/o a  */
/* database connection.              */

define output parameter pcVarList as character  no-undo.

pcVarList = ''.

for each sesContext no-lock
     where sesContext.connectID eq session:server-connection-id:
       pcVarList = pcVarList
                + "   "
                + trim(sesContext.attrName) + " = "
                + (if sesContext.attrValue eq ? then '?' else sesContext.attrValue)
                + "~n".
end.
