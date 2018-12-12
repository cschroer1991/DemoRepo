/*--------------------------------------------------------------------------
    File        : common/t0007.p
    Purpose     : Get the convert (Metric/Imperial) table.

    Syntax      : 

    Modified    : 12/16/03 - clongo@bravepoint.com
  ------------------------------------------------------------------------*/
  
define temp-table gttConvert no-undo like convert.

define output parameter table for gttConvert.

for each convert no-lock:
    create gttConvert.
    buffer-copy convert to gttConvert.
end.
