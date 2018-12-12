/*************************************************************************
PROGRAM: j0392.p
   NAME: Middletown Write to IP Pipe for ASRS
 AUTHOR: Jim Lefeld
   DATE: 12/05/96
PURPOSE: This module will go through all of the asr records with stat = 0 and
         write the appropriate message to the asrwrite pipe.

HISTORY: 02/07/97 - jhl - Check if time-dif > 2 or < 0 for Midnight difference
         02/07/97 - jhl - Use pause instead of Higley's repeat loop for delay.
         04/02/97 - jhl - Pause 1 instead of 2.
         05/01/97 - jhl - Can only run this program in batch mode.
         05/29/97 - jhl - Added CCI transaction\
         01/14/08 - jhl - TIWO 38888 - Added RFL transaction (Request File Load)
                          Changed Legacy i0001.i to use Genesis includes/i0001.i
         01/21/08 - jhl - TIWO 38888 - Send asr.asrID ( 9 digits) after "RFL" message.
*************************************************************************/
{includes/i0001.i "new" "global"}

def var st-time   as integer.
def var cr-time   as integer. 
def var time-dif  as integer.  
def var cnt       as integer.
def var cBlank     as character.
def var v-string as character.

if not session:batch-mode then do:
  bell. bell. bell.
  message " j0392 CAN ONLY BE RUN IN BATCH MODE !! ".
  return.
end.

main-loop:
repeat: 
  for each asr where asr.stat = 0 use-index stat:
    output to /dev/asrwrite.
    case asr.mess-id: 
      when "BSR" then do:
        cBlank = fill(' ',13). 
        put caps(asr.mess-id) format "x(3)"
            asr.rf-id format "x(2)"     
            caps(asr.coil-id) format "x(15)"   
            caps(asr.src-loc) format "x(8)"    
            asr.weight format "99999"    
            asr.wdth  format "99.9999"
            asr.od format "999.9"
            caps(asr.proc-flag) format "x"
            caps(asr.wrap-flag) format "x"
            asr.comment format "x(40)" 
            cBlank format "x(13)".
        assign asr.stat = 1.               
      end.                  
      when "CMA" then do:
        cBlank = fill(' ',80). 
        put caps(asr.mess-id) format "x(3)"
            asr.rf-id format "x(2)"
            caps(asr.coil-id) format "x(15)" 
            cBlank format "x(80)".
        assign asr.stat = 9.
      end.
      when "CRR" then do: 
         cBlank = fill(' ',71).
         put caps(asr.mess-id) format "x(3)"
             asr.rf-id format "x(2)"               
             caps(asr.coil-id) format "x(15)"
             caps(asr.dest-loc) format "x(8)"
             asr.priority format "x" 
             cBlank format "x(71)".
         assign asr.stat = 1.          
      end.                   
      when "CSR" then do:
        cBlank = fill(' ',13).
        put caps(asr.mess-id) format "x(3)"
            asr.rf-id format "x(2)"     
            caps(asr.coil-id) format "x(15)"   
            caps(asr.src-loc) format "x(8)"    
            asr.weight format "99999"    
            asr.wdth format "99.9999"
            asr.od format "999.9"
            caps(asr.proc-flag) format "x"
            caps(asr.wrap-flag) format "x" 
            asr.comment format "x(40)" 
            cBlank format "x(13)".
        assign asr.stat = 1.               
      end.
      when "DRR" then do:
        cBlank = fill(' ',80).
        put caps(asr.mess-id) format "x(3)"   
            asr.rf-id format "x(2)"     
            caps(asr.coil-id) format "x(15)" 
            cBlank format "x(80)".
        assign asr.stat = 1.               
      end.
      when "CCI" then do:
        find inv where inv.inv-id = integer(asr.coil-id) no-lock no-error.
        cBlank = fill(' ',38).

        if inv.stat = 7 then v-string = string(inv.next-inv-id) + 
          fill(' ',15 - length(string(inv.next-inv-id))).
        else  v-string = fill(' ',15).

        if asr.weight = ? then v-string = v-string + fill(' ',5).
        else v-string = v-string + string(asr.weight,"99999").
        if asr.wdth = ? then v-string = v-string + fill(' ',7).
        else v-string = v-string + string(asr.wdth,"99.9999").
        if asr.od = ? then v-string = v-string + fill(' ',5).
        else v-string = v-string + string(asr.od,"999.9").
        if asr.proc-flag = ? then v-string = v-string + " ".
        else v-string = v-string + caps(asr.proc-flag).
        if asr.wrap-flag = ? then v-string = v-string + " ".
        else v-string = v-string + caps(asr.wrap-flag).
           
        put caps(asr.mess-id) format "x(3)"
          asr.rf-id format "x(2)"
          caps(asr.coil-id) format "x(15)"
          caps(asr.src-loc) format "x(8)"
          v-string format "x(34)"            
          cBlank format "x(38)".

        assign asr.stat = 1.
      end.
      when "RFL" then do:
        cBlank = fill(' ',88). 
        put caps(asr.mess-id) format "x(3)"
            string(asr.asrID, "999999999") format "x(9)"
            cBlank format "x(88)".
        assign asr.stat = 1.
      end.
    end case.
    output close.
  end.   /* end for each */
  
  pause 1 no-message.
end.  /* end repeat   */               
         
