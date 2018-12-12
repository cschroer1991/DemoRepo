&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0634.p
    Purpose     : SQL DB Connector

    Syntax      :

    Description :

    Author(s)   : C. Longo
    Created     :
    Notes       :
  ----------------------------------------------------------------------
  04/21/11 jhl  56127  Added connection to Transaction manager sql database (level 2) 
  07/09/13 jhl  63402 Added connection to PeopleNet sql database.
  03/20/14 jhl  65046 Changed port from 5162 --> 5362
  05/02/14 jhl  65046 DB service ports needed to be changed also.   Still using 10.2B database on Kronos server.
  05/06/14 jhl  66172 Retire and remove Comet Tracker
  02/27/15 jhl  68034 Change Host server connection from Kronos to ProgressSQL
  04/03/17 jhl  74622 Changed connections settings for TransMgr.   TransMgrNew instead of TransMgrNew.
  
  */
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define var cErrorMsg as character no-undo.

define input parameter pcConnectProfile as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 14.95
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

case pcConnectProfile:
    when 'Kronos' then do:
      if not connected('kronoshld')  then
         run ConnectKronos.
    end.
    when 'TransMgr' then do:
      if not connected('transmgrnew') then
         run ConnectTransMgr.
    end.
    when 'pnet' then do:
      if not connected('pnethld') then
         run ConnectPnet.
    end.
end case.








catch eSysError as progress.lang.error:

  define var i as int no-undo.
 
  do i = 1 to eSysError:NumMessages:
      cErrorMsg = substitute('&1~nErrorNum: &2 ErrorMsg: &3',
                          cErrorMsg,
                          eSysError:GetMessageNum(i),
                          eSysError:GetMessage(i)).
        
  end.

  message cErrorMsg view-as alert-box.

end. /* catch */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ConnectKronos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectKronos Procedure 
PROCEDURE ConnectKronos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
connect 
'-db kronoshld -H ProgressSQL -S 8100
 -db Kronos
 -ld wfcdb
 -dt MSS
 -H ProgressSQL
 -S 5362
 -DataService mssbroker1
 -Dsrv SVUB,1
 -U tkcsowner
 -P tkcsowner'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConnectPnet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectPnet Procedure 
PROCEDURE ConnectPnet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
connect                              
'-db pnethld -H ProgressSQL -S 8400   
 -db PeopleNet                        
 -ld PeopleNet                        
 -dt MSS                             
 -H ProgressSQL                           
 -S 5362                             
 -DataService mssbroker1             
 -Dsrv SVUB,1                        
 -U pnuser                         
 -P pn12345'.                       
                           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ConnectTransMgr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectTransMgr Procedure 
PROCEDURE ConnectTransMgr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
connect                              
'-db TransMgrNew -H ProgressSQL -S 8500   
 -db TransMgrNew
 -ld newtransmgr                        
 -dt MSS                             
 -H ProgressSQL                           
 -S 5362                             
 -DataService mssbroker1             
 -Dsrv SVUB,1                        
 -U transmgr                         
 -P transmgr'.                       
                                     



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

