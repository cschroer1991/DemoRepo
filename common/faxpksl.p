/*************************************************************************  
PROGRAM:  faxpksl.p                                                         
   NAME:  Add labels to Packing slips                                       
 AUTHOR:  Milton L. Volosyn, Higley & Company, Inc.                         
   DATE:  May 15, 1996

PURPOSE:  To replace the C program that adds labels to packing slips    
          for faxing...                                                     
HISTORY:
*************************************************************************/  
DEFINE INPUT PARAMETER p-file1 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-file2 AS CHARACTER NO-UNDO. 

DEFINE STREAM slp.
INPUT STREAM slp FROM VALUE(p-file1).
DEFINE STREAM lbl.
OUTPUT STREAM lbl TO VALUE(p-file2).

DEFINE VARIABLE strchr AS CHARACTER.
DEFINE VARIABLE ln-cnt AS INTEGER.

ASSIGN
  ln-cnt = 0.

REPEAT:

IMPORT STREAM slp  
  UNFORMATTED 
  strchr.
ASSIGN
  ln-cnt = ln-cnt + 1.

IF INDEX(strchr,CHR(12)) > 0  
  AND INDEX(strchr,CHR(47)) > 0 THEN
  ln-cnt = 1.

IF ln-cnt = 1 THEN
  strchr = SUBSTRING(strchr,1,66) + "DATE" + SUBSTRING(strchr,71,15).

IF ln-cnt = 3 THEN
  strchr = SUBSTRING(strchr,1,66) + "PAGE" + SUBSTRING(strchr,71,15).

IF ln-cnt = 4 then
  strchr = "LOAD NUMBER     CARRIER             B/L NUMBER" +
           SUBSTRING(strchr,47,39).

IF INDEX(strchr,"SOLD TO") > 0 THEN
  strchr = SUBSTRING(strchr,1,44) + "SHIP TO" + SUBSTRING(strchr,52,34).

PUT STREAM lbl 
  strchr FORMAT "x(85)" TO 85.
  

END.
