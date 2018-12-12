&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0298.p
    Purpose     : Get Scrap Partner

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  Date      By  TIWO    Description
 --------  --- ------  --------------------------------------------------- 
 11/29/07  DJT  38436  Scrap partner may be defined on procitem table now.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input parameter piPSIPO as int.
define input parameter piOwnerID as int.
define input parameter piPartnerID as int.
define output parameter piScrapPartner as int.

define var iScrapMethod as int no-undo.
define var iPlantID as int no-undo.

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

iScrapMethod = ?.
iPlantID  = dynamic-function('getGlobalVar' , 'PlantID').
piScrapPartner = ?.

/* TIWO 38486 - Scrap partner may now be defined on the procitem table,
                if so, use that one first - there could possibly be several
                different items & partners so just grabbing the first avail */
if piPSIPO ne ? and piPSIPO gt 0 then do:
    find first ProcItem no-lock where ProcItem.PSIPO = piPSIPO
        and ProcItem.scrap-partner ne ?
        and ProcItem.scrap-partner gt 0 
        and Procitem.obsolete = no no-error.
    if available ProcItem then assign piScrapPartner = ProcItem.scrap-partner.
end.

if piScrapPartner = ? then do:

   if piOwnerId = ? then do:
       find partner where partner.partner-id = piPartnerId no-lock no-error.
       if avail partner then
         piOwnerId = partner.owner-id.
   end.

   /* By this time the piOwner Id will be set to what is passed in or the 
      owner of the partner passed in */

   find cust where cust.cust-id = piOwnerID no-lock no-error.
   if avail cust then
       iScrapMethod = cust.scrap-meth.
     

   if iPlantID = 1 and piOwnerID = 4686 then do: 
      piScrapPartner = 3125.
   end.
   else if iScrapMethod = 1 or  iScrapMethod = 2 then do:
       find cntl where cntl.plant-id = iPlantID no-lock no-error.
       if avail cntl then do:
      
         find first partner where partner.owner-id = cntl.cust-id and
                                  partner.user-id = cntl.cust-id and 
                                  partner.plant-id = iPlantID no-lock no-error.
         if avail Partner then
              piScrapPartner = partner.partner-id.
        
       end.

   end.
   else do:
       piScrapPartner = piPartnerID. 
   end.
end. /*scrap partner not defined in any of the procitems*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


