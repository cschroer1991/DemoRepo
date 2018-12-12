&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : Returns default values for adding new Export Templates
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
    
    Date     By   Issue  Remark
  ========  ===  ======  ======================================================
  10/02/09  djt   49623  Do NOT pre-populate with PSI plant address as exporter
  08/05/10  zakh  51605  Removed all defaults except for the ones for Vendor, 
                         Cosignee and Originator or for Arcelor Mittal.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table ttExport like ExportTemplate.

define input parameter piPartnerID as int.
define output parameter table for ttExport.

define var giPlantID as int no-undo.

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

define buffer buCust for cust. /* for user id */
define buffer baCust for cust. /* for acct id */
define variable cMsg as character  no-undo.

giPlantID = dynamic-function('getGlobalVar', input 'PlantID').

find partner where partner.partner-id = piPartnerID no-lock no-error.
if not avail partner then do:
    cMsg = "Partner " + string(partner.partner-id) + " Could not be found".
    run AddMessage in target-procedure (cMsg,"VendorName","ExportTemplate").
    return.
end.

find cust where cust.cust-id = partner.owner-id no-lock no-error.
if not avail cust then do:
    cMsg = "Partner Owner/Customer " + string(partner.owner-id) 
            + " Could not be found".
    run AddMessage in target-procedure (cMsg,"VendorName","ExportTemplate").
    return.
end.

find buCust where buCust.cust-id = partner.user-id no-lock no-error.
if not avail buCust then do:
    cMsg = "Partner User/Customer " + string(partner.user-id) 
           + " Could not be found".
    run AddMessage in target-procedure (cMsg,"VendorName","ExportTemplate").
    return.
end.
/*
find first cntl where cntl.plant-id = giPlantID no-lock no-error.
if not avail cntl then do:
    cMsg = "Error finding Current Plant".
    run AddMessage in target-procedure (cMsg,"VendorName","ExportTemplate").
    return.
end.
*/

/**** SET TEMPLATE DEFAULTS ****/

create ttExport.
assign ttExport.VendorName = cust.name
       ttExport.VendorAddr1 = cust.addr-1
       ttExport.VendorAddr2 = cust.addr-2
       ttExport.VendorCity = cust.city
       ttExport.VendorStateCountry = string(cust.state + " " + cust.country) 
       ttExport.VendorPostalCode = cust.zip
       ttExport.ConsigneeName  = buCust.name
       ttExport.ConsigneeAddr1 = buCust.addr-1
       ttExport.ConsigneeAddr2 = buCust.addr-2
       ttExport.ConsigneeCity  = buCust.city 
       ttExport.ConsigneeStateCountry = string(buCust.state + " " + buCust.Country)
       ttExport.ConsigneePostalCode = buCust.zip
       ttExport.OriginatorName = cust.name
       ttExport.OriginatorAddr1 = cust.addr-1
       ttExport.OriginatorAddr2 = cust.addr-2
       ttExport.OriginatorCity = cust.city
       ttExport.OriginatorStateCountry = string(cust.state + " " + cust.country)
       ttExport.OriginatorPostalCode = cust.zip
       .
if partner.owner-id = 16 then do:
  find cust where cust.cust-id = 2319 no-lock no-error.
  assign ttExport.OriginatorName  = "ALCAN ROLLED PRODUCTS"
         ttExport.OriginatorAddr1 = "ATTN: C.J. WHITE  100 ERIEVIEW"
         ttExport.OriginatorCity  = "CLEVELAND"
         ttExport.OriginatorStateCountry = "OH"
         ttExport.OriginatorPostalCode = "44114-1878"
         ttExport.PurchaserName = cust.name
         ttExport.PurchaserAddr1 = "DIV. OF ALCAN ALUMINIUM LIMITED"
         ttExport.PurchaserAddr2 = cust.addr-2
         ttExport.PurchaserCity  = cust.city
         ttExport.PurchaserStateCountry = string(cust.state + " " + cust.country)
         ttExport.PurchaserPostalCode = cust.zip
         ttExport.CountryOfOrigin = "UNITED KINGDOM".
end.
else if partner.owner-id = 86 and partner.user-id = 3339 then do:
  find cust where cust.cust-id = 187 no-lock no-error.
  assign ttExport.VendorName = cust.name
         ttExport.VendorAddr1 = cust.addr-1
         ttExport.VendorAddr2 = cust.addr-2
         ttExport.VendorCity = cust.city 
         ttExport.VendorStateCountry = string(cust.state + " " + cust.country)
         ttExport.VendorPostalCode = cust.zip
         ttExport.OriginatorName = ""
         ttExport.OriginatorAddr1 = ""
         ttExport.OriginatorAddr2 = ""
         ttExport.OriginatorCity = ""
         ttExport.OriginatorStateCountry = ""
         ttExport.OriginatorPostalCode = ""
         .
end.
else if partner.owner-id = 2979 then do:
  assign ttExport.VendorName = "ALLEGHENY LUDLUM STEEL"
         ttExport.VendorAddr1 = "100 RIVER ROAD"
         ttExport.VendorCity = "BRACKENRIDGE"
         ttExport.VendorStateCountry = "PA"
         ttExport.VendorPostalCode = ""
         ttExport.ConditionOrTerms =
                    "THE UNIT PRICE INCLUDES AN AMOUNT FOR SLITTING OF THE TENNECO"
                  + " OWNED MATERIAL"
         ttExport.CommoditySpecifications = 
                    "SLIT MATERIAL DOWN TO USER WIDTH.  MASTER  SLIT  BROKER: ".
end.

if partner.slip-cfg[12] = 3 then do:
 assign ttExport.VendorName  = "" 
        ttExport.VendorAddr1 = "" 
        ttExport.VendorAddr2 = ""
        ttExport.VendorCity  = ""
        ttExport.VendorStateCountry = ""
        ttExport.VendorPostalCode = ""
        ttExport.ConsigneeName  = ""
        ttExport.ConsigneeAddr1 = ""
        ttExport.ConsigneeAddr2 = ""
        ttExport.ConsigneeCity  = ""
        ttExport.ConsigneeStateCountry = ""
        ttExport.ConsigneePostalCode = ""
        ttExport.OriginatorName  = "" 
        ttExport.OriginatorAddr1 = "" 
        ttExport.OriginatorAddr2 = "" 
        ttExport.OriginatorCity  = ""
        ttExport.OriginatorStateCountry = ""
        ttExport.OriginatorPostalCode = ""
        .
end.

if partner.owner-id = 86 then assign
   ttExport.VendorName = 'ArcelorMittal Burns Harbor'   
   ttExport.VendorAddr1 = '250 West US HWY 12'  
   ttExport.VendorAddr2 = ''    
   ttExport.VendorCity = 'Burns Harbor' 
   ttExport.VendorStateCountry = 'IN'   
   ttExport.VendorPostalCode = '46304-9745'     
   ttExport.ExporterName = 'ArcelorMittal USA Inc.'     
   ttExport.ExporterAddr1 = 'One South Dearborn St.'    
   ttExport.ExporterAddr2 = ''  
   ttExport.ExporterCity = 'Chicago'    
   ttExport.ExporterStateCountry = 'IL' 
   ttExport.ExporterPostalCode = '60603'        
   ttExport.OriginatorName = 'ArcelorMittal Burns Harbor'       
   ttExport.OriginatorAddr1 = '250 West US HWY 12'      
   ttExport.OriginatorAddr2 = ''        
   ttExport.OriginatorCity = 'Burns Harbor'     
   ttExport.OriginatorStateCountry = 'IN'       
   ttExport.OriginatorPostalCode = '46304-9745' 
   ttExport.OriginatorAttention = ''.
        
if partner.owner-id = 202 then assign
   ttExport.VendorName = 'ArcelorMittal USA Inc.'       
   ttExport.VendorAddr1 = '3210 Watling St.'    
   ttExport.VendorAddr2 = ''    
   ttExport.VendorCity = 'East Chicago' 
   ttExport.VendorStateCountry = 'IN'   
   ttExport.VendorPostalCode = '46312'  
   ttExport.ExporterName = 'ArcelorMittal USA Inc.'     
   ttExport.ExporterAddr1 = 'One South Dearborn St.'    
   ttExport.ExporterAddr2 = ''  
   ttExport.ExporterCity = 'Chicago'    
   ttExport.ExporterStateCountry = 'IL' 
   ttExport.ExporterPostalCode = '60603'        
   ttExport.OriginatorName = 'ArcelorMittal USA Inc.'   
   ttExport.OriginatorAddr1 = '3210 Watling St.'        
   ttExport.OriginatorAddr2 = ''        
   ttExport.OriginatorCity = 'East Chicago'     
   ttExport.OriginatorStateCountry = 'IN'       
   ttExport.OriginatorPostalCode = '46312'      
   ttExport.OriginatorAttention = ''.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


