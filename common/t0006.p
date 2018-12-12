/*------------------------------------------------------------------------
    File        : t0006.p
    Purpose     : Recieving a file on the AppServer

    Syntax      :

    Description :

    Author(s)   : clongo@bravepoint.com
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
  
define input parameter pmFileFromClient as memptr.
define output parameter pcFileOnAppServer as char.




run lib/getosfilename.p (OUTPUT pcFileOnAppServer).

output to value(pcFileOnAppServer) binary no-convert.
  export pmFileFromClient.
output close.

set-size(pmFileFromClient) = 0.


