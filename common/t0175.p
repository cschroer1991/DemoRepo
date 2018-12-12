&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :   t0175.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
    History     :
    Date       Init  TIWO   Description
    07/20/09 - jhl - 48597 - Reset etime via etime(yes)  so that integer
                             does not get too large to cause Progress error.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
create widget-pool.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

&SCOPED-DEFINE HTTP-NEWLINE CHR(13) + CHR(10)
&SCOPED-DEFINE RESPONSE-TIMEOUT 45

DEFINE INPUT PARAMETER pHOST        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pPORT        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pURL         AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pRESULT     AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pRESPONSE   AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pContent    AS CHAR NO-UNDO.


DEFINE VARIABLE requestString       AS CHAR   NO-UNDO.
DEFINE VARIABLE vSocket             AS HANDLE NO-UNDO.   
DEFINE VARIABLE vBuffer             AS MEMPTR NO-UNDO.
DEFINE VARIABLE vloop               AS LOGICAL NO-UNDO.
DEFINE VARIABLE vPackets            AS INTEGER NO-UNDO.
DEFINE VARIABLE wStatus             AS LOGICAL NO-UNDO.

ASSIGN requestString = "GET " + pURL + " HTTP/1.0" + {&HTTP-NEWLINE} +
          "Accept: */*" + {&HTTP-NEWLINE} + 
          "Host: " + phost + {&HTTP-NEWLINE} + 
          /*"Connection: Keep-Alive" + {&HTTP-NEWLINE} + */
          {&HTTP-NEWLINE}.




/*OPEN THE SOCKET*/
CREATE SOCKET vSocket.
vSocket:SET-READ-RESPONSE-PROCEDURE ("readHandler",THIS-PROCEDURE).
ASSIGN wstatus = vSocket:CONNECT("-H " + phost + " -S " + pport) NO-ERROR.

/*Now make sure the socket is open*/
IF wstatus = NO THEN DO:
    pResult = "0:No Socket".
    DELETE OBJECT vSocket NO-ERROR.
    RETURN.
END.

/*Got socket - Now make HTTP request*/

SET-SIZE(vBuffer) = LENGTH(requestString) + 1.
PUT-STRING(vBuffer,1) = requestString.
vSocket:WRITE(vBuffer, 1, LENGTH(requestString)).
SET-SIZE(vBuffer) = 0.

/*Wait for a response*/
ASSIGN vloop = TRUE.  /*Turns off automatically when request is done*/
DEFINE VAR vstarttime AS INTEGER.
ETIME(YES).
ASSIGN vstarttime = etime.

WAITLOOP: DO WHILE vSocket:CONNECTED():

    WAIT-FOR READ-RESPONSE OF vSocket PAUSE 30.
  
    /* Build in timer in case sending is never set to NO 
       this will terminate the program after 60 seconds
       start-Etime will be reset by WriteData each time there
       is activity on the socket to allow for long transmissions */
    IF vstarttime + ({&RESPONSE-TIMEOUT} * 1000) < ETIME 
     THEN DO:
        
        MESSAGE "timed out at " + string(etime - vstarttime) + " msec".

        vSocket:DISCONNECT().
        
        ASSIGN pResult = "0:Failure".
        
    END. /*No Response, or timed out*/
END.

/*At this point, pResponse should be populated with the result (up to 32K)*/



DELETE OBJECT vSocket NO-ERROR.



/*All Done!*/
IF pResult = "" THEN ASSIGN pResult = "1:Success".

IF index(pResponse, {&HTTP-NEWLINE} + {&HTTP-NEWLINE}) > 0 THEN DO:

ASSIGN 
 pContent = SUBSTRING(pResponse,INDEX(pResponse,{&HTTP-NEWLINE} + {&HTTP-NEWLINE}),-1).
ASSIGN
 pResponse = SUBSTRING(pResponse,1,INDEX(pResponse,{&HTTP-NEWLINE} + {&HTTP-NEWLINE})).

END.


RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-readHandler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE readHandler Procedure 
PROCEDURE readHandler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*Handle the response from the webserver*/

    DEFINE VARIABLE bytesAvail  AS INTEGER  NO-UNDO.
    DEFINE VARIABLE b           AS MEMPTR   NO-UNDO.
    DEFINE VARIABLE lastBytes   AS INTEGER  NO-UNDO.

    IF vSocket:connected() THEN ASSIGN bytesAvail = vSocket:GET-BYTES-AVAILABLE().
       


    IF bytesAvail > 0 THEN DO: /*All Done*/
      
        /*OK, there's something on the wire... Read it in*/    
        SET-SIZE(b) = bytesAvail + 1.
        vSocket:READ(b, 1, bytesAvail, 1).
        ASSIGN pResponse = pResponse + GET-STRING(b,1).
        SET-SIZE(b) = 0.

    END.
    vSocket:DISCONNECT().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

