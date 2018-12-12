&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  server/common/j0276.p    
    Purpose     :  Create batch gaugechart printing records in ga-print

    Syntax      :

    Description :

    Author(s)   :  Jim Lefeld
    Created     :  06/05/08
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{includes/i0001.i}
{includes/i0034.i}

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
def var v-job like ga-print.job.

for each work-coil 
  where start-date >= s-lo-date
    and start-date <= s-hi-date 
    no-lock,
  each inv where inv.inv-id = work-coil.coil-id and inv.plant-id = g-plant-id 
  no-lock,
  each partner where partner.partner-id = inv.partner-id
    and partner.owner-id = s-owner-id no-lock :

  if work-coil.cpk = 0 and work-coil.avg-gauge = 0
  and work-coil.max-footage = 0 and work-coil.max-gauge = 0
  and work-coil.min-footage = 0 and work-coil.min-gauge = 0 
  and work-coil.samples = 0 and work-coil.std-dev = 0 then next.
       
  v-job = string(work-coil.start-date)
      + " - " + string(s-owner-id)
      + " - Daily Charts".
  create ga-print.
  assign ga-print.plant-id = g-plant-id
         ga-print.inv-id   = inv.inv-id
         ga-print.job      = v-job.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


