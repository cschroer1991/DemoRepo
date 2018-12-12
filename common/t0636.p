&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : common/t0636.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : C. Longo
    Created     :
    Notes       : You must be connected to Kronos to compile this 
                  proedure.
                  
                  See run common/t0634.p (input 'Kronos').
    History      :
    Date     Init    TIWO  Purpose
    02/05/16  jhl   70540  New paycodes were added in Kronos for night shift differntial and 
                           now night drivers hours are showing as 0.  Worked with Kronos Tech support
                           and we can use combined paycode table to get paycodes.
                           payCat1mm table using field GrandPayCode.            
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



define temp-table ttDriver no-undo
    field DriverID      as integer 
    field DriverName    as character
    field PersonNum     as character
    field PersonID      as integer
    field PlantID       as integer 
    field IBShipPound   as integer 
    field IBLoadCount   as integer 
    field IBRevenue     as decimal 
    field OBShipPound   as integer  
    field OBLoadCount   as integer  
    field OBRevenue     as decimal
    field FuelSurcharge as decimal
    field HoursWorked   as decimal
    index Driver is primary unique DriverID
    index PersonNum PersonNum
    index Plant PlantID.



define input parameter table for ttDriver.
define input parameter pdFromDate    as date    no-undo.
define input parameter pdThroughDate as date    no-undo.

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

for each PERSON no-lock:
    find ttDriver where ttDriver.PersonNum = PERSON.PERSONNUM no-lock no-error.
    if avail ttDriver then 
        assign ttDriver.PersonID = PERSON.PERSONID.
end.


for each ttDriver:
    
    for each WFCTOTAL no-lock where WFCTOTAL.APPLYDTM >= pdFromDate and 
                                    WFCTOTAL.APPLYDTM <= pdThroughDate and
                                    WFCTOTAL.EMPLOYEEID = ttDriver.PersonID,
        each paycat1mm where paycat1mm.paycodeid = wfctotal.paycodeid no-lock:
                                    
    
       
    
         case true:
             when paycat1mm.GrandPAYCODEID = 801   
             then /* Regular Time */
                 ttDriver.HoursWorked = ttDriver.HoursWorked + WFCTOTAL.DURATIONSECSQTY.
             when paycat1mm.GrandPAYCODEID = 901               
             then /* Overtime */
                 ttDriver.HoursWorked = ttDriver.HoursWorked + (WFCTOTAL.DURATIONSECSQTY * 1.5).
             when paycat1mm.GrandPAYCODEID = 902     
             then  /* Double Time */
                 ttDriver.HoursWorked = ttDriver.HoursWorked + (WFCTOTAL.DURATIONSECSQTY * 2).
         end case.

         
      
    end. /* for each WFCTOTAL */
    
end.

for each ttDriver:
    ttDriver.Hours = (ttDriver.Hours / 60) / 60.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


