&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : server/common/t0679.p
    Description : This program takes a coil and returns it's possible coil
                  field locations in a string.

    Author(s)   : zakh
    Created     : 10/23/11
    Notes       : Created for TIWO 57301
12/19/11 zakh 57463 Made changes to allow for schema addition of owner-id
03/15/12 bmf  59460 Add Perrysburg plant
04/23/12 bmf  60129 Change Perrysburg to user owner specific first.
07/23/12 zakh 59461 Only show one spot for Kenton
09/25/12 bmf  61660 Add owner-id logic
10/01/12 bmf  60898 Add user/gauge logic for Minster
09/26/13 bmf  64494 Add Tipp city plant
06/17/15 zakh 68742 Added CareGo logic
03/17/17 jwk  74477 Add Bowling Green plant 16.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter piPlant     as integer      no-undo.
define input  parameter pcBay       as character    no-undo.
define input  parameter piOwner     as integer      no-undo.
define input  parameter piUser      as integer      no-undo.
define input  parameter piWt        as integer      no-undo.
define input  parameter pdWdth      as decimal      no-undo.
define input  parameter pdOD        as decimal      no-undo.
define input  parameter pdGauge     as decimal      no-undo.
define output parameter pcList      as character    no-undo.

def var iCount as int no-undo.
def var cBayDesc as char no-undo.
def var iTruckCnt as int no-undo.
def var lCareGo as logical no-undo.
def var cRtn as character no-undo.

{includes/i0207.i}

define temp-table ttRows
    field cRow as char
    field iCap as int
    field cBay as char.

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
         HEIGHT             = 4.67
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
def var cUser as character.

/*calc OD if OD is blank*/
if can-do('1,3,7,8,9', string(piPlant)) and not pdOD > 0 then do:
    pdOD = sqrt((4.5 * piWt) / 
                     (.98 * (pdWdth))
                     + (24 * 24)).
    if not pdOD > 0 then return.
end.

lCareGo = dynamic-function("getAppAttribute", "CareGoPlantLive" + string(piPlant), "Log").

if piPlant = 2 or piPlant = 6 then do: 
    for each AreaCoilDim no-lock
        where AreaCoilDim.plant-id = piPlant
        and piOwner = AreaCoilDim.owner-id
        and piWt >= AreaCoilDim.min-wt
        and piWt <= AreaCoilDim.max-wt
        and pdWdth >= AreaCoilDim.min-wdth
        and pdWdth <= AreaCoilDim.max-wdth,
        each AreaLoc no-lock
            where AreaLoc.AreaID = AreaCoilDim.AreaID
            and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
        first loc no-lock
            where loc.loc = AreaLoc.loc
            and loc.plant-id = piPlant
            and (loc.Capacity - loc.NumCoils) > 0:
        create ttRows.
        assign 
            ttRows.cRow  = loc.loc
            ttRows.iCap  = loc.Capacity - loc.NumCoils.
    end.
    if not can-find(first ttRows no-lock) then
    do:
        for each AreaCoilDim no-lock
            where AreaCoilDim.plant-id = piPlant
            and (AreaCoilDim.owner-id = 0
                or AreaCoilDim.owner-id = ?)
            and piWt >= AreaCoilDim.min-wt
            and piWt <= AreaCoilDim.max-wt
            and pdWdth >= AreaCoilDim.min-wdth
            and pdWdth <= AreaCoilDim.max-wdth,
            each AreaLoc no-lock
                where AreaLoc.AreaID = AreaCoilDim.AreaID
                and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
            first loc no-lock
                where loc.loc = AreaLoc.loc
                and loc.plant-id = piPlant
                and (loc.Capacity - loc.NumCoils) > 0:
            create ttRows.
            assign 
                ttRows.cRow  = loc.loc
                ttRows.iCap  = loc.Capacity - loc.NumCoils.
        end.
    end.
end.
else if lCareGo and piPlant = 9 and (pcBay = "3" or pcBay = "4") then do:
    cUser = dynamic-function('getGlobalVar', 'UserID').
    create ttParams.
    assign
        ttParams.cName = "Width"
        ttParams.cValue = string(pdWdth).
    create ttParams.
    assign
        ttParams.cName = "Weight"
        ttParams.cValue = string(piWt).
    create ttParams.
    assign
        ttParams.cName = "OD"
        ttParams.cValue = string(pdOD).
        
    run common/t0765.p (input 'AvailSpots',
                        input cUser,
                        input table ttParams,
                        output cRtn).
    if num-entries(cRtn, "|") <> 2 then do:
        pcList = "UNKNOWN".
        return.
    end. /*if num-entries*/
    
    if entry(1, cRtn, "|") = "0" and entry(2, cRtn, "|") = "0" then do:
        pcList = "BAY 2".
        return.
    end. /*if entry(1, cRtn)*/
    
    if pcBay = "3" and integer(entry(1, cRtn, "|")) > 0 then do:
        pcList = "BAY 3 SBMS (" + entry(1, cRtn, "|") + ")".
        return.
    end. /*if pcBay = 3*/
    
    if pcBay = "4" and integer(entry(2, cRtn, "|")) > 0 then do:
        pcList = "BAY 4 SBMS (" + entry(2, cRtn, "|") + ")".
        return.
    end. /*if pcBay = 4*/
    
    if pcBay = "4" and entry(2, cRtn, "|") = "0" and integer(entry(1, cRtn, "|")) > 0 then do:
        pcList = "BAY 3 SBMS (" + entry(2, cRtn, "|") + ")".
        return.
    end. /*if pcBay = 4*/ 
    
    pcList = "UNKNOWN".
    return.
end. /*else if lCareGo*/
else if piPlant = 3 or piPlant = 8 or piPlant = 9 or piPlant = 14 or piPlant = 16 then do:
    for each AreaCoilDim no-lock
        where AreaCoilDim.plant-id = piPlant
        and piOwner = AreaCoilDim.owner-id
        and pdWdth >= AreaCoilDim.min-wdth
        and pdWdth <= AreaCoilDim.max-wdth
        and pdOD >= AreaCoilDim.min-od
        and pdOD <= AreaCoilDim.max-od,
        each AreaLoc no-lock
           where AreaLoc.AreaID = AreaCoilDim.AreaID
           and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
        first loc no-lock
           where loc.loc = AreaLoc.loc
           and loc.plant-id = piPlant
           and (loc.Capacity - loc.NumCoils) > 0:
        
        create ttRows.
        assign 
           ttRows.cRow  = loc.loc
           ttRows.iCap  = loc.Capacity - loc.NumCoils.
    end.
    if not can-find(first ttRows no-lock) then do:
        for each AreaCoilDim no-lock
                where AreaCoilDim.plant-id = piPlant
                and (AreaCoilDim.owner-id = 0
                     or AreaCoilDim.owner-id = ?)
                and pdWdth >= AreaCoilDim.min-wdth
                and pdWdth <= AreaCoilDim.max-wdth
                and pdOD >= AreaCoilDim.min-od
                and pdOD <= AreaCoilDim.max-od,
            each AreaLoc no-lock
                where AreaLoc.AreaID = AreaCoilDim.AreaID
                and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
           first loc no-lock
                where loc.loc = AreaLoc.loc
                and loc.plant-id eq piPlant
                and (loc.Capacity - loc.NumCoils) > 0:
        
           create ttRows.
           assign 
               ttRows.cRow  = loc.loc
               ttRows.iCap  = loc.Capacity - loc.NumCoils.
        end.
    end.
end.
else if piPlant = 7 then do: 
    for each AreaCoilDim no-lock
        where AreaCoilDim.plant-id = piPlant
        and pdWdth >= AreaCoilDim.min-wdth
        and pdWdth <= AreaCoilDim.max-wdth
        and pdOD >= AreaCoilDim.min-od
        and pdOD <= AreaCoilDim.max-od,
        each AreaLoc no-lock
            where AreaLoc.AreaID = AreaCoilDim.AreaID
            and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
        first loc no-lock
            where loc.loc = AreaLoc.loc
            and loc.plant-id = piPlant
            and (loc.Capacity - loc.NumCoils) > 0:
        
        create ttRows.
        assign 
            ttRows.cRow  = loc.loc
            ttRows.iCap  = loc.Capacity - loc.NumCoils.
    end.
end. 
else if piPlant = 1 then do:
    for each AreaCoilDim no-lock
        where AreaCoilDim.plant-id = piPlant
        and piOwner = AreaCoilDim.owner-id
        and piUser  = AreaCoilDim.user-id
        and piWt >= AreaCoilDim.min-wt
        and piWt <= AreaCoilDim.max-wt
        and pdGauge >= AreaCoilDim.min-gauge
        and pdGauge <= AreaCoilDim.max-gauge
        and pdWdth >= AreaCoilDim.min-wdth
        and pdWdth <= AreaCoilDim.max-wdth,
        each AreaLoc no-lock
           where AreaLoc.AreaID = AreaCoilDim.AreaID
           and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
/*         each choice no-lock                                                      */
/*            where choice.field-no eq 30                                           */
/*              and choice.plant-id eq piPlant                                      */
/*              and choice.misc1    eq (if pcBay > "" then pcBay else AreaLoc.bay)  */
/*              and choice.misc2    begins "yes",  /* can be auto assigned */       */
        each dock no-lock
           where dock.plant-id eq piPlant
             and dock.AreaBay  eq int((if pcBay > "" then pcBay else AreaLoc.bay))
             and dock.enabled
             and dock.type <= 1,
        first loc no-lock
           where loc.loc = AreaLoc.loc
           and loc.plant-id = piPlant
           and (loc.Capacity - loc.NumCoils) > 0:
        
        create ttRows.
        assign 
           ttRows.cRow  = loc.loc
           ttRows.iCap  = loc.Capacity - loc.NumCoils
           ttRows.cBay  = AreaLoc.bay.
    end.
    if not can-find(first ttRows no-lock) then do:
        for each AreaCoilDim no-lock
                where AreaCoilDim.plant-id = piPlant
                and AreaCoilDim.owner-id = piOwner
                and (AreaCoilDim.user-id = 0
                     or AreaCoilDim.user-id = ?)
                and piWt >= AreaCoilDim.min-wt
                and piWt <= AreaCoilDim.max-wt
                and pdGauge >= AreaCoilDim.min-gauge
                and pdGauge <= AreaCoilDim.max-gauge
                and pdWdth >= AreaCoilDim.min-wdth
                and pdWdth <= AreaCoilDim.max-wdth,
            each AreaLoc no-lock
                where AreaLoc.AreaID = AreaCoilDim.AreaID
                and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
/*             each choice no-lock                                                      */
/*                where choice.field-no eq 30                                           */
/*                  and choice.plant-id eq piPlant                                      */
/*                  and choice.misc1    eq (if pcBay > "" then pcBay else AreaLoc.bay)  */
/*                  and choice.misc2    begins "yes",  /* can be auto assigned */       */
            each dock no-lock
               where dock.plant-id eq piPlant
                 and dock.AreaBay  eq int((if pcBay > "" then pcBay else AreaLoc.bay))
                 and dock.enabled
                 and dock.type <= 1,
           first loc no-lock
                where loc.loc = AreaLoc.loc
                and loc.plant-id = piPlant
                and (loc.Capacity - loc.NumCoils) > 0:
        
           create ttRows.
           assign 
               ttRows.cRow  = loc.loc
               ttRows.iCap  = loc.Capacity - loc.NumCoils
               ttRows.cBay  = AreaLoc.bay.
        end.
    end.
    if not can-find(first ttRows no-lock) then do:
        for each AreaCoilDim no-lock
                where AreaCoilDim.plant-id = piPlant
                and AreaCoilDim.user-id = piUser
                and (AreaCoilDim.owner-id = 0
                     or AreaCoilDim.owner-id = ?)
                and piWt >= AreaCoilDim.min-wt
                and piWt <= AreaCoilDim.max-wt
                and pdGauge >= AreaCoilDim.min-gauge
                and pdGauge <= AreaCoilDim.max-gauge
                and pdWdth >= AreaCoilDim.min-wdth
                and pdWdth <= AreaCoilDim.max-wdth,
            each AreaLoc no-lock
                where AreaLoc.AreaID = AreaCoilDim.AreaID
                and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
/*             each choice no-lock                                                      */
/*                where choice.field-no eq 30                                           */
/*                  and choice.plant-id eq piPlant                                      */
/*                  and choice.misc1    eq (if pcBay > "" then pcBay else AreaLoc.bay)  */
/*                  and choice.misc2    begins "yes",  /* can be auto assigned */       */
            each dock no-lock
               where dock.plant-id eq piPlant
                 and dock.AreaBay  eq int((if pcBay > "" then pcBay else AreaLoc.bay))
                 and dock.enabled
                 and dock.type <= 1,
           first loc no-lock
                where loc.loc = AreaLoc.loc
                and loc.plant-id = piPlant
                and (loc.Capacity - loc.NumCoils) > 0:
        
           create ttRows.
           assign 
               ttRows.cRow  = loc.loc
               ttRows.iCap  = loc.Capacity - loc.NumCoils
               ttRows.cBay  = AreaLoc.bay.
        end.
    end.
    if not can-find(first ttRows no-lock) then do:
        for each AreaCoilDim no-lock
                where AreaCoilDim.plant-id = piPlant
                and (AreaCoilDim.owner-id = 0
                     or AreaCoilDim.owner-id = ?)
                and (AreaCoilDim.user-id = 0
                     or AreaCoilDim.user-id = ?)
                and piWt >= AreaCoilDim.min-wt
                and piWt <= AreaCoilDim.max-wt
                and pdGauge >= AreaCoilDim.min-gauge
                and pdGauge <= AreaCoilDim.max-gauge
                and pdWdth >= AreaCoilDim.min-wdth
                and pdWdth <= AreaCoilDim.max-wdth,
            each AreaLoc no-lock
                where AreaLoc.AreaID = AreaCoilDim.AreaID
                and AreaLoc.bay = (if pcBay > "" then pcBay else AreaLoc.bay),
/*             each choice no-lock                                                      */
/*                where choice.field-no eq 30                                           */
/*                  and choice.plant-id eq piPlant                                      */
/*                  and choice.misc1    eq (if pcBay > "" then pcBay else AreaLoc.bay)  */
/*                  and choice.misc2    begins "yes",  /* can be auto assigned */       */
            each dock no-lock
               where dock.plant-id eq piPlant
                 and dock.AreaBay  eq int((if pcBay > "" then pcBay else AreaLoc.bay))
                 and dock.enabled
                 and dock.type <= 1,
           first loc no-lock
                where loc.loc = AreaLoc.loc
                and loc.plant-id = piPlant
                and (loc.Capacity - loc.NumCoils) > 0:
        
           create ttRows.
           assign 
               ttRows.cRow  = loc.loc
               ttRows.iCap  = loc.Capacity - loc.NumCoils
               ttRows.cBay = AreaLoc.bay.
        end.
    end.
end.

if not can-find(first ttRows) then do:
    pcList = "*N/A*".
    return.
end.

iCount = 0.
for each ttRows break by iCap desc:
    iCount = iCount + 1.
    if iCount = 1 then
    do:
        if pcBay = "" 
        and piPlant = 1 then
        do:
/*           find first choice no-lock where choice.field-no eq 30                 */
/*                                       and choice.misc1    eq ttRows.cBay        */
/*                                       and choice.plant-id eq piPlant no-error.  */
          find first dock no-lock
             where dock.plant-id eq piPlant
               and dock.AreaBay  eq int(ttRows.cBay)
               and dock.enabled
               and dock.type <= 1 no-error.
          if available dock then
          do:
              for each truckarrivals no-lock
                  where truckarrivals.plant-id eq piPlant
                    and truckarrivals.dock eq dock.dock
                    and truckarrivals.completeDate eq ?:
                  assign iTruckCnt = iTruckCnt + 1.
              end.
              assign cBayDesc = dock.descr + "(" + string(iTruckCnt) + ") ".
          end.
          else
              assign cBayDesc = "Unknown".
          pcList = trim(cBayDesc) + " - " + trim(ttRows.cRow).    
        end.
        else
          pcList = trim(ttRows.cRow).
    end.
    else if iCount < 9 then
        pcList = pcList + ", " + trim(ttRows.cRow).
    if pcBay > "" then
        pcList = pcList + "(" + string(ttRows.iCap) + ")".
    if iCount = 1 and piPlant = 2 then leave.
end.
return.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


