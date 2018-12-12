define input parameter  pcReportFile as character no-undo.
define output parameter pcReportName as character no-undo.

define buffer ReportRegister for psi.ReportRegister.

do for ReportRegister:
    find ReportRegister no-lock
        where ReportRegister.ReportName eq pcReportFile no-error.
    if available ReportRegister then
    assign
        pcReportName = ReportRegister.ReportDesc.
    else
    assign
        pcReportName = "Unknown Report".
end.

return.


