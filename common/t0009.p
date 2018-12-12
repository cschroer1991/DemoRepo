/*------------------------------------------------------------------------

  File: common/t0009.p

  Description: Delivers the functional area for a register procedure.

  Author: 

  Modification: 

------------------------------------------------------------------------*/

define input parameter pcProgramName as char.
define output parameter pcFuncArea as char.

find first program where program.ProgramName = pcProgramName no-lock no-error.
if avail program then
    pcFuncArea = program.FuncArea.
