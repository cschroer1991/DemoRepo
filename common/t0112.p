&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0112.p
    Purpose     : Build and return the gttLoc table from loc.

    Syntax      : run t0112.p [on appserver]
                      (input piPlantID,
                       input piTypeCode,
                       output table gttLoc [append]).

    Description :

    Author(s)   : Rick Terrell, Bravepoint, Inc.
    Created     : 07/27/04
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table gttLoc no-undo 
    like loc
    use-index loc
    index ixloctype  plant-id loc typecode.

define input  parameter piPlantID  as integer    no-undo.
define input  parameter piTypeCode as integer    no-undo.
define output parameter table for gttLoc.

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
define query locQuery for loc.

if piTypeCode lt 0 then
    open query locQuery
        for each loc no-lock
            where loc.plant-id eq piPlantID
            and   loc.typeCode ne 0.
else
    open query locQuery
        for each loc no-lock
            where loc.plant-id eq piPlantID
            and   loc.typeCode eq piTypecode.

get first locQuery.

do while not(query-off-end("locQuery")):
    create gttLoc.
    buffer-copy loc to gttLoc.
    get next locQuery.
end.

close query LocQuery.

return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


