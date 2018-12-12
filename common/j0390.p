/*************************************************************************
PROGRAM: j0390.p
   NAME: Middletown Create ASR Record
 AUTHOR: Jim Lefeld
   DATE: 12/05/96
PURPOSE: This module will create that ASR record with the appropriate fields
         based on the message to be sent.  If message is created successfully
         g-success will be sent to yes. Otherwise it will be set to no.

HISTORY: 
12/15/04 - rct - modified for Genesis from legacy program.
08/09/07 - jhl - Moved to server/common directory.   Programs existed in
                 server/ship and scanner and decided move to server/common.
09/10/07 - jke - 36583 Added asrID field and populate   
09/11/07 - bmf - tiwo 36761, Need to assign g-recid for asr record added.  
01/14/08 - jhl - TIWO 38888 - Add new commands "rfl" to request inventory
                 report from ASRS for discrepency report.
10/10/14 zakh    66991 I adjusted the reqs for weight and OD.
                                     
*************************************************************************/
{includes/i0001.i}

define input  parameter in-mess-id    like asr.mess-id.
define input  parameter in-rf-id      like asr.rf-id.
define input  parameter in-coil-id    like asr.coil-id.
define input  parameter in-weight     like asr.weight.
define input  parameter in-wdth       like asr.wdth.
define input  parameter in-od         like asr.od.
define input  parameter in-src-loc    like asr.src-loc.
define input  parameter in-proc-flag  like asr.proc-flag.
define input  parameter in-wrap-flag  like asr.wrap-flag.
define input  parameter in-priority   like asr.priority.
define input  parameter in-comment    like asr.comment. 
define output parameter pcRtnMsg      as character no-undo.
define output parameter plSuccess     as logical   no-undo.

define buffer bAsr for asr.

define variable mess-lst  as character  no-undo initial "BSR,CMA,CRR,CSR,DRR,CCI,RFL". /* "BSR,CMA,CRR,CSR,CCS,DRR,CCI,RFL". */
define variable cNCMsg    as character  no-undo initial "~nASR not created".
def var cr-date   as date.
def var cr-time   as integer. 

assign cr-date    = today
       cr-time    = integer(replace(string(time,"hh:mm:ss"),":",""))
       plSuccess  = no
       pcRtnMsg   = ''
       in-mess-id = caps(in-mess-id).

if in-priority = ? or in-priority = "" then 
    assign in-priority = "4".

if index(mess-lst,in-mess-id) = 0 then 
do:
  pcRtnMsg = substitute("Invalid code: &1. &2.~n",in-mess-id,cNCMsg).
end.    
if in-rf-id = "" then 
do:
  pcRtnMsg = pcRtnMsg + substitute("RF ID required.  &1.~n",cNCMsg).
end.
if in-coil-id = "" then 
do:
  pcRtnMsg = pcRtnMsg + substitute("Coil ID required.  &1.~n",cNCMsg).
end.        

if (in-mess-id = "BSR" or in-mess-id = "CSR")
and (in-weight < 1 or in-weight > 90000) then 
do:
  pcRtnMsg = pcRtnMsg + substitute("Invalid weight: &1. &2.~n",in-weight,cNCMsg).
end.
if (in-mess-id = "BSR" or in-mess-id = "CSR")
and (in-wdth < 24 or in-wdth > 80) then 
do:
  pcRtnMsg = pcRtnMsg + substitute("Invalid width: &1. &2.~n",in-wdth,cNCMsg).
end.
if (in-mess-id = "BSR" or in-mess-id = "CSR")
and (in-od <  40.0 or in-od > 94.0) then 
do:
  if index("WS",in-wrap-flag) = 0 or in-mess-id = "BSR" then 
  do: 
    pcRtnMsg = pcRtnMsg + substitute("Invalid OD: &1. &2.~n",in-od,cNCMsg).
  end.
  else 
      assign in-od = 40.0.
end. 
if (in-mess-id = "BSR" or in-mess-id = "CSR")
and (index("SPU",in-proc-flag) = 0) then 
do:
  pcRtnMsg = pcRtnMsg + substitute("Invalid proc: &1. &2.~n",in-proc-flag,cNCMsg).
end.
if (in-mess-id = "BSR" or in-mess-id = "CSR")
and (index("WNUS",in-wrap-flag) = 0) then 
do:
  pcRtnMsg = pcRtnMsg + substitute("Invalid wrap: &1. &2.~n",in-wrap-flag,cNCMsg).
end.                    
if (in-mess-id = "CRR" 
    or in-mess-id = "CSR" 
    or in-mess-id = "CCI")
and in-src-loc = "" then 
do:
  pcRtnMsg = pcRtnMsg + substitute("Invalid src: &1. &2.~n",in-src-loc,cNCMsg).
end.
if (in-mess-id = "CRR") 
and index("123456789",in-priority) = 0 then 
do:
  pcRtnMsg = pcRtnMsg + substitute("Invalid priority: &1. &2.~n",in-priority,cNCMsg).
end.                    

if pcRtnMsg gt "" then
    return.

do for bAsr transaction:
    /* Create and assign common fields */
    create bAsr.
    assign bASR.asrID       = next-value(asrID)
           bAsr.mess-id     = caps(in-mess-id)
           bAsr.rf-id       = in-rf-id
           bAsr.coil-id     = caps(in-coil-id)
           bAsr.create-date = cr-date
           bAsr.create-time = cr-time
           bAsr.stat        = 0.
    /* Now add type specific fields (if any) */
    case in-mess-id: 
      when "BSR" then 
      do:
        assign bAsr.src-loc     = caps(in-src-loc)    
               bAsr.weight      = in-weight     
               bAsr.wdth        = in-wdth
               bAsr.od          = in-od
               bAsr.proc-flag   = caps(in-proc-flag)
               bAsr.wrap-flag   = caps(in-wrap-flag)  
               bAsr.comment     = in-comment.
      end.                  
      when "CMA" then 
      do:
          /* Nothing */
      end.
      when "CRR" then 
      do:
        assign bAsr.dest-loc    = caps(in-src-loc)
               bAsr.priority    = in-priority.
      end.                   
      when "CSR" then 
      do: 
        assign bAsr.src-loc     = caps(in-src-loc)
               bAsr.weight      = in-weight     
               bAsr.wdth        = in-wdth
               bAsr.od          = in-od
               bAsr.proc-flag   = caps(in-proc-flag)
               bAsr.wrap-flag   = caps(in-wrap-flag)  
               bAsr.comment     = in-comment.
      end.
      when "DRR" then 
      do:                               
          /* Nothing */
      end.
      when "CCI" then 
      do:
        assign bAsr.src-loc     = caps(in-src-loc)
               bAsr.weight      = in-weight     
               bAsr.wdth        = in-wdth
               bAsr.od          = in-od
               bAsr.proc-flag   = caps(in-proc-flag)
               bAsr.wrap-flag   = caps(in-wrap-flag)  .
      end.
       when "RFL" then 
      do:                               
          /* Nothing */
      end.
    end case.

    assign g-recid   = recid(bAsr)
           plSuccess = yes.

end.
  
return.

