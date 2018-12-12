/*************************************************************************
PROGRAM: j0391.p
   NAME: Middletown Read IP Pipe for ASRS
 AUTHOR: Jim Lefeld
   DATE: 12/05/96
PURPOSE: This module will constantly check to see if any messages exist on 
         the asrread pipe and update the appropriate asr and inv records.

HISTORY: 02/07/97 - jhl - Check time diff is negative.  Before/after midnight.
         02/07/97 - jhl - Use pause instead of Higley's repeat loop for 
                          delay.
         02/10/97 - jhl - Write BSA or BSC to dest-loc instead of src-loc.
         02/21/97 - jhl - Treat stat = 9 like stat = 3.
         03/19/97 - jhl - Added logic to handle updating asn.loc for 
                          non-asn coils that are put in ASRS.
         04/01/97 - jhl - Use progress date and time for ack-date/time
                          instead of date/time that is passed back from
                          AAC transaction.
         04/07/97 - jhl - For BSA find last asr where mess-id = 'BSR'
                          not last.  For INU do not find a 'BSR'.
         04/30/97 - jhl - Changed pause to 1 second instead of 2.
         05/01/97 - jhl - Only allow this program to run in batch mode.
         05/15/97 - jhl - Changed all find first to find last.
         05/29/97 - jhl - Added logic for CCC transaction.
         10/13/98 - jhl - Added error messaging logic
         12/20/00 - nlh - Changed workfile to temp-table.
         01/14/08 - jhl - TIWO 38888 - Added logic for FLR and FLC.
                          changen i0001.i to includes/i0001.i.  Was still using /prod/src
         01/21/08 - jhl - TIWO 38888 - RLR and FLC now had request id (AsrId)
                          in message after the RLR and/or FLC
         09/30/15 - jhl - TIWO 69429 - Per Randy Donisi - When receive CRF, Assign Truck/Rail when pulled out of ASRS and 
                          inv record 3 (shipped) or 9 (plan to ship).  Otherwise it was setting loc = "".                   
*************************************************************************/
{includes/i0001.i "new" "global"}

def var i as int.
def var v-in-time   as character.
def var v-in-hh     as integer.
def var v-in-mm     as integer.
def var v-in-ss     as integer.
def var v-ack-date   as date.
def var v-ack-time   as integer. 
def var rec-in    as character.  
def var val-mess  as character.
def var iAsrID like asr.asrid no-undo.
def var mess-lst  as character 
   initial "BSA,BSC,CRA,CRC,CRF,CSA,CSC,DRC,INU,CCC,FLR,FLC". 
def temp-table w-asr
  field w-ack-date    AS DATE      FORMAT "99/99/9999"
  field w-ack-time    AS INTEGER   FORMAT "999999"
  field w-coil-id     AS CHARACTER FORMAT "X(15)" 
  field w-comment     AS CHARACTER FORMAT "X(40)" 
  field w-create-date AS DATE      FORMAT "99/99/9999"
  field w-create-time AS INTEGER   FORMAT "999999"
  field w-dest-loc    AS CHARACTER FORMAT "X(8)" 
  field w-mess-id     AS CHARACTER FORMAT "X(3)"
  field w-od          AS DECIMAL   DECIMALS 2 FORMAT ">>9.9" 
  field w-priority    AS CHARACTER FORMAT "X"
  field w-proc-flag   AS CHARACTER FORMAT "X" 
  field w-reason-code AS CHARACTER FORMAT "X(2)"
  field w-req-src     AS CHARACTER FORMAT "x" 
  field w-rf-id       AS CHARACTER FORMAT "X(2)"
  field w-src-loc     AS CHARACTER FORMAT "X(8)"
  field w-stat        AS INTEGER   FORMAT "9" 
  field w-status-id   AS CHARACTER FORMAT "X(3)"
  field w-wdth        AS DECIMAL   DECIMALS 2 FORMAT ">9.9999"
  field w-weight      AS DECIMAL   DECIMALS 2 FORMAT ">>,>>9" 
  field w-wrap-flag   AS CHARACTER FORMAT "X".

if not session:batch-mode then do:
  bell. bell. bell.
  message " j0391 CAN ONLY BE RUN IN BATCH MODE !! ".
  return.
end.

main-loop:
repeat: 
  input from /dev/asrread unbuffered no-echo.
  detail-loop:
  repeat:
    assign rec-in = "".
    import rec-in 
      no-error. 
    assign val-mess = substring(rec-in,1,3).
    if val-mess = "" then
      next detail-loop.

    assign v-ack-date = today
       v-in-time = string(time,"hh:mm:ss")
       v-in-hh   = integer(substring(v-in-time,1,2))
       v-in-mm   = integer(substring(v-in-time,4,2))
       v-in-ss   = integer(substring(v-in-time,7,2))
       v-ack-time = (v-in-hh * 10000) +
                 (v-in-mm * 100)   +
                 (v-in-ss).

    case val-mess: 
      when "BSA" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-dest-loc    = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time.
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                          integer(substring(rec-in,37,2)),
                                          integer(substring(rec-in,29,4)))
               w-asr.w-create-time = (integer(substring(rec-in,40,2)) *
                                       10000) +
                                     (integer(substring(rec-in,43,2)) *
                                       100)   +
                                     (integer(substring(rec-in,46,2)))
               */
        find last asr where asr.coil-id   = w-asr.w-coil-id
        and asr.mess-id = 'BSR'
        and asr.stat = 1 no-error.
        if available asr then do:
          assign asr.dest-loc  = w-asr.w-dest-loc
                 asr.ack-date  = w-asr.w-create-date
                 asr.ack-time  = w-asr.w-create-time
                 asr.stat      = 9.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.                                                           
      when "BSC" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-dest-loc    = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                           integer(substring(rec-in,37,2)),
                                           integer(substring(rec-in,29,4)))
               w-asr.w-create-time  = (integer(substring(rec-in,40,2)) * 
                                         10000) +
                                      (integer(substring(rec-in,43,2)) * 
                                         100)   +
                                      (integer(substring(rec-in,46,2)))
               */
               w-asr.w-req-src      = substring(rec-in,48,1)
               w-asr.w-reason-code  = substring(rec-in,49,2)
               w-asr.w-comment      = substring(rec-in,51,40).
        find last asr where asr.coil-id   = w-asr.w-coil-id
        and asr.mess-id = 'BSR'
        and asr.stat = 1 no-error.
        if available asr then do:
          assign asr.reason-code = w-asr.w-reason-code
                 asr.comment     = w-asr.w-comment
                 asr.ack-date    = w-asr.w-create-date
                 asr.ack-time    = w-asr.w-create-time
                 asr.stat        = 8.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.                                                              
      when "CCC" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-reason-code = substring(rec-in,21,2)
               w-asr.w-comment     = substring(rec-in,23,40)
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time.

        find last asr where asr.coil-id   = w-asr.w-coil-id
        and asr.mess-id = 'CCI'
        and asr.stat = 1 no-error.
        if available asr then do:
          assign asr.reason-code = w-asr.w-reason-code
                 asr.comment     = w-asr.w-comment
                 asr.ack-date    = w-asr.w-create-date
                 asr.ack-time    = w-asr.w-create-time
                 asr.stat        = 8.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.
      when "CRA" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-src-loc     = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                          integer(substring(rec-in,37,2)),
                                          integer(substring(rec-in,29,4)))
               w-asr.w-create-time = (integer(substring(rec-in,40,2)) * 
                                        10000) +
                                     (integer(substring(rec-in,43,2)) * 
                                        100)   +
                                     (integer(substring(rec-in,46,2)))
               */
               w-asr.w-req-src     = substring(rec-in,48,1).
        find last asr where asr.coil-id   = w-asr.w-coil-id
        and asr.mess-id = 'CRR'
        and asr.stat = 1 no-error.
        if available asr then do:
          assign asr.dest-loc  = w-asr.w-src-loc
                 asr.ack-date  = w-asr.w-create-date
                 asr.ack-time  = w-asr.w-create-time
                 asr.req-src   = w-asr.w-req-src
                 asr.stat      = 2.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.                                                             
      when "CRC" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-dest-loc    = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                          integer(substring(rec-in,37,2)),
                                          integer(substring(rec-in,29,4)))
               w-asr.w-create-time = (integer(substring(rec-in,40,2)) * 
                                        10000) +
                                     (integer(substring(rec-in,43,2)) * 
                                        100)   +
                                     (integer(substring(rec-in,46,2)))
               */
               w-asr.w-req-src     = substring(rec-in,48,1)
               w-asr.w-reason-code = substring(rec-in,49,2)
               w-asr.w-comment     = substring(rec-in,51,40).
        find last asr where  asr.coil-id   = w-asr.w-coil-id
        and (asr.stat = 1 or asr.stat = 2) no-error.
        if available asr then do:
          assign asr.reason-code = w-asr.w-reason-code
                 asr.comment     = w-asr.w-comment
                 asr.ack-date    = w-asr.w-create-date
                 asr.ack-time    = w-asr.w-create-time
                 asr.stat        = if asr.mess-id = "DRR" then
                                     9
                                   else
                                     8.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.                                                              
      when "CRF" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-src-loc     = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                          integer(substring(rec-in,37,2)),
                                          integer(substring(rec-in,29,4)))
               w-asr.w-create-time = (integer(substring(rec-in,40,2)) * 
                                        10000) +
                                     (integer(substring(rec-in,43,2)) * 
                                        100)   +
                                     (integer(substring(rec-in,46,2)))
               */
               w-asr.w-req-src     = substring(rec-in,48,1).
        find inv where inv.inv-id = integer(w-asr.w-coil-id) no-error.
        if available inv then do:
            assign inv.loc = w-asr.w-src-loc.
        end.
        else do:
          find first asn where asn.inv-id = integer(w-asr.w-coil-id) no-error.
          if available asn then asn.loc = w-asr.w-src-loc.
        end.
        
        find last asr where asr.coil-id   = w-asr.w-coil-id
        and asr.mess-id = "CRR"
        and asr.stat = 2 no-error.
        if available asr then do:
          assign asr.dest-loc  = w-asr.w-src-loc
                 asr.ack-date  = w-asr.w-create-date
                 asr.ack-time  = w-asr.w-create-time
                 asr.req-src   = w-asr.w-req-src
                 asr.stat      = 9.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.                                                             
      when "CSA" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-src-loc     = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                          integer(substring(rec-in,37,2)),
                                          integer(substring(rec-in,29,4)))
               w-asr.w-create-time = (integer(substring(rec-in,40,2)) * 
                                        10000) +
                                     (integer(substring(rec-in,43,2)) * 
                                        100)   +
                                     (integer(substring(rec-in,46,2)))
               */
               w-asr.w-req-src     = substring(rec-in,48,1).
        find last asr where asr.coil-id = w-asr.w-coil-id
        and asr.mess-id = "CSR"
        and asr.stat = 1 no-error.
        if available asr then do:
          assign asr.dest-loc  = w-asr.w-src-loc
                 asr.ack-date  = w-asr.w-create-date
                 asr.ack-time  = w-asr.w-create-time
                 asr.req-src   = w-asr.w-req-src
                 asr.stat      = 2.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.                     
      when "CSC" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-src-loc     = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                          integer(substring(rec-in,37,2)),
                                          integer(substring(rec-in,29,4)))
               w-asr.w-create-time = (integer(substring(rec-in,40,2)) * 
                                        10000) +
                                     (integer(substring(rec-in,43,2)) * 
                                        100)   +
                                     (integer(substring(rec-in,46,2)))
               */
               w-asr.w-req-src     = substring(rec-in,48,1)
               w-asr.w-reason-code = substring(rec-in,49,2)
               w-asr.w-comment     = substring(rec-in,51,40).
        find last asr where asr.coil-id   = w-asr.w-coil-id
        and asr.mess-id = "CSR"
        and asr.stat = 1 no-error.
        if available asr then do:
          assign asr.reason-code = w-asr.w-reason-code
                 asr.comment     = w-asr.w-comment
                 asr.ack-date    = w-asr.w-create-date
                 asr.ack-time    = w-asr.w-create-time
                 asr.stat        = 8.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.                                                              
      when "DRC" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-src-loc     = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                          integer(substring(rec-in,37,2)),
                                          integer(substring(rec-in,29,4)))
               w-asr.w-create-time = (integer(substring(rec-in,40,2)) * 
                                        10000) +
                                     (integer(substring(rec-in,43,2)) * 
                                        100)   +
                                     (integer(substring(rec-in,46,2)))
               */
               w-asr.w-req-src     = substring(rec-in,48,1)
               w-asr.w-reason-code = substring(rec-in,49,2)
               w-asr.w-comment     = substring(rec-in,51,40).
        find last asr where asr.coil-id   = w-asr.w-coil-id
        and asr.mess-id = "DRR"
        and asr.stat = 1 no-error.
        if available asr then do:
          assign asr.reason-code = w-asr.w-reason-code
                 asr.comment     = w-asr.w-comment
                 asr.ack-date    = w-asr.w-create-date
                 asr.ack-time    = w-asr.w-create-time
                 asr.stat        = 8.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.                                                               
      when "INU" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-rf-id       = substring(rec-in,4,2)
               w-asr.w-coil-id     = substring(rec-in,6,15)
               w-asr.w-src-loc     = substring(rec-in,21,8)  
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               /*
               w-asr.w-create-date = date(integer(substring(rec-in,34,2)),
                                          integer(substring(rec-in,37,2)),
                                          integer(substring(rec-in,29,4)))
               w-asr.w-create-time = (integer(substring(rec-in,40,2)) * 
                                        10000) +
                                     (integer(substring(rec-in,43,2)) *
                                        100)   +
                                     (integer(substring(rec-in,46,2)))
               */
               w-asr.w-status-id   = substring(rec-in,48,3).
        if w-asr.w-status-id = 'IN' then do:
          find inv where inv.inv-id = integer(w-asr.w-coil-id) no-error.
          if available inv then do:
            assign inv.loc = 
              substring(w-asr.w-src-loc,1,1) + substring(w-asr.w-src-loc,3,3)
              + substring(w-asr.w-src-loc,7,2).
          end.
          else do:
            find first asn where asn.inv-id = integer(w-asr.w-coil-id) no-error.
            if available asn then do:
              assign asn.loc = 
                substring(w-asr.w-src-loc,1,1) + substring(w-asr.w-src-loc,3,3)
                + substring(w-asr.w-src-loc,7,2).
            end.
          end.
        end.

        find last asr where asr.coil-id = w-asr.w-coil-id
        and (asr.stat = 1 or asr.stat = 2) 
        and asr.mess-id <> "bsr" no-error.
        if available asr then do:
          assign asr.dest-loc  = w-asr.w-src-loc
                 asr.ack-date  = w-asr.w-create-date
                 asr.ack-time  = w-asr.w-create-time
                 asr.status-id = w-asr.w-status-id
                 asr.stat      = if (asr.mess-id = "CSR" or asr.mess-id = "CCI")
                                 then 9
                                 else 2.
          assign w-asr.w-stat  = 9.
        end.
        else
          assign w-asr.w-stat = 9.
      end.  
      when "FLR" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-comment     = substring(rec-in,13,80)
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time.

        assign iAsrId = integer(substring(rec-in,4,9)).

        find last asr where asr.AsrID = iAsrId
        and asr.mess-id = 'RFL'
        and asr.stat = 1 no-error.
        if available asr then do:
          assign asr.comment     = w-asr.w-comment
                 asr.ack-date    = w-asr.w-create-date
                 asr.ack-time    = w-asr.w-create-time
                 asr.stat        = 9.
          delete w-asr.
          next detail-loop.
        end.
        else
          assign w-asr.w-stat = 8.
      end.
      when "FLC" then do:
        create w-asr.
        assign w-asr.w-mess-id     = substring(rec-in,1,3)
               w-asr.w-reason-code = substring(rec-in,13,1)
               w-asr.w-comment     = substring(rec-in,14,80)
               w-asr.w-create-date = v-ack-date
               w-asr.w-create-time = v-ack-time
               w-asr.w-stat        = if w-asr.w-reason-code = "0" 
                                     then 9 
                                     else 8.
      end.
    end case. 

    if available w-asr then do:
      create asr.
      assign asr.asrid       = next-value(asrid)
             asr.mess-id     = w-asr.w-mess-id
             asr.rf-id       = w-asr.w-rf-id
             asr.coil-id     = w-asr.w-coil-id
             asr.weight      = w-asr.w-weight
             asr.wdth        = w-asr.w-wdth
             asr.od          = w-asr.w-od
             asr.src-loc     = w-asr.w-src-loc
             asr.dest-loc    = w-asr.w-dest-loc
             asr.status-id   = w-asr.w-status-id
             asr.req-src     = w-asr.w-req-src
             asr.reason-code = w-asr.w-reason-code
             asr.comment     = w-asr.w-comment
             asr.proc-flag   = w-asr.w-proc-flag
             asr.wrap-flag   = w-asr.w-wrap-flag
             asr.priority    = w-asr.w-priority
             asr.create-date = w-asr.w-create-date
             asr.create-time = w-asr.w-create-time
             asr.ack-date    = w-asr.w-ack-date
             asr.ack-time    = w-asr.w-ack-time
             asr.stat        = w-asr.w-stat.  
      delete w-asr.       
    end.
  end. /* END DETAIL LOOP */                                                      
  pause 1 no-message.     
end.  /* END OF REPEAT */                                       
       
if error-status:error then do:
  output to value(os-getenv("DDIR") + "log/j0391.log") append.
  put skip(1) today string(time,"hh:mm:ss") skip. 
  do i = 1 to error-status:num-messages:
    put error-status:get-number(i) space(2) error-status:get-message(i) skip.
  end.
  output close.
end.

