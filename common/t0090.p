&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0090.p 
    Purpose     :

    Syntax      :

    Description : Get Density Value

    Author(s)   : Karen Kauffman
    Created     : 06/21/04 
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define input parameter piPartId like part.part-id.
define input  parameter piTypeID as integer    no-undo.
define output parameter poDensity like type.density.

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

if piPartID > 0 then do:
  find part where part.part-id = piPartId no-lock no-error.
  find type where type.type-id = part.type-id no-lock no-error.
end.
else do:
    find type where type.type-id = piTypeID no-lock no-error.
end.
if not available type then do:
    return "Metal Type is not available".
end.
find metal-cat where metal-cat.cat-id = type.cat-id no-lock no-error.


if type.density <> 0 and type.density <> ? then
    poDensity =  type.density.
else if type.type-id = 727 and available part then do:
    poDensity = (.278 - ( .00021 / part.gauge)).
end.
else if metal-cat.alum then 
    poDensity = .1.

else if metal-cat.descr begins "brass" or metal-cat.descr begins "Copper" 
    or metal-cat.descr begins "bronze" then do:
    poDensity =  .321.
end.
else  poDensity = .283.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


