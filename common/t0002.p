&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : t0002.p
    Purpose     : Load Menu Options
    
    date     By   Issue  Remark
  ========  ===  ======  ===============================================
  09/30/14  djt   66754  Only Build accessible programs into menu.
  12/22/14  djt   67691  Don't apply security restrictions from menu maint
  03/09/15  djt   68093  appserver log errors.
  01/05/17  djt   71509  Favorites menu project
  10/16/17  djt   76345  Favorite menu items colliding w/new menu entries\
  04/16/18  djt   77779  Display Wiki link in the menu bar if available
                         disable accounting menu for plant accounts.
  05/25/18  tjn   77945 Do not show Genesis programs in Menu that have no security area. 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


define temp-table gttmbMenu no-undo like mbMenu.

define variable cProcName as character no-undo.
define variable lAuthorized as logical no-undo.
define variable lDisableitems as logical no-undo.
define variable cLogon as character no-undo.
define variable cGroup as character no-undo.

define input parameter pcProcName as character no-undo.
define input parameter pcGroupID  as character no-undo.
define output parameter table for gttmbMenu.
define output parameter pcFuncArea as character no-undo.

def buffer bgtt for gttmbmenu.

DEFINE VARIABLE lSecurity AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lFavoriteBuild AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lMaintMode AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iIdx AS INTEGER     NO-UNDO.
DEFINE VARIABLE cVal AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cProp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMaxKey AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFavKey AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxMainSeq AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxMainKey AS INTEGER     NO-UNDO.

/*  def stream test. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-checkSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkSecurity Procedure 
FUNCTION checkSecurity RETURNS LOGICAL
  ( input pcProgName as char,
    input pcLogon as char,
    input pcGroup as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveMenuTree) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RemoveMenuTree Procedure 
FUNCTION RemoveMenuTree RETURNS LOGICAL
  ( input piMenuKey as int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
lDisableItems = can-find(first appattr
                         where appattr.attrkey = 'DisableUnauthorizedMenuItems'
                         and appattr.applog = yes).

if num-entries(pcProcName,'|') > 1 then do:
    run parseOpts.
    if lSecurity = no then lDisableItems = no.
    pcProcName = entry(1, pcProcName,'|').
end.
if pcGroupID ne "*" then
  find program no-lock 
       where program.ProgramName eq pcProcName 
       and   program.GroupID     eq pcGroupID no-error.
if avail program then
  assign pcFuncArea = program.FuncArea.

cLogon = dynamic-function("getglobalvar", "userid").
/* building this up since seems not to be available yet when menus being built */
cGroup = ''.
select max(key) into iMaxKey from mbmenu.
iFavKey = 0.
find first psi-user no-lock where psi-user.logon = cLogon no-error.
if avail psi-user then do:
    for each groupXRef no-lock
        where groupXref.user-id = psi-user.psiuserid
        and groupXref.type = 'Security',
        first groups no-lock
        where groups.groupID = groupXRef.groupID:
        cGroup = cGroup + groups.descr + ','.
    end.
    cgroup = trim(cgroup).
end.
if not avail psi-user then lDisableItems = no.  /* for when running release manager or other misc programs*/
if lFavoriteBuild = yes then do:
    /* just building a list of favorites for the favorites utility */
    if avail psi-user then do iIdx = 1 to num-entries(psi-user.favoritelist):
        find first mbMenu no-lock where mbMenu.key = int(entry(iIdx, psi-user.favoritelist)) no-error.
        if avail mbMenu then do:
           create gttmbMenu.
           buffer-copy mbMenu to gttmbMenu
               assign gttmbmenu.seqNo = (iIdx + 1) * 10.
        end.
    end.
end.
else do:
    for each mbMenu no-lock
        where mbMenu.ProcName eq pcProcName 
        and   (mbMenu.GroupID eq pcGroupID
               or pcGroupID eq "*"):
    
        lAuthorized = yes.
        if lDisableItems and (mbMenu.miType = 'Normal' or mbMenu.miType = 'Report') then do:
            cProcname = replace(mbmenu.miProcName,chr(92),chr(47)).
            if num-entries(cProcName,'/') > 1 then
               cProcName = entry(num-entries(cProcName,'/'), cProcName,'/').
            
            lAuthorized = checkSecurity(cProcName, cLogon, cGroup).
        end.
        if not lAuthorized then next.
        create gttmbMenu.
        buffer-copy mbMenu to gttmbMenu.
        if mbMenu.name = 'Fa&vorites' then iFavKey = mbmenu.key.
    
    end.
    /* if there are submenus that do not point anywhere, can them.*/
    if lDisableItems and pcProcname ne 'Default' then do:
        for each gttmbmenu where gttmbMenu.type = 'sm':
            find first bgtt where bgtt.ParentKey = gttmbMenu.key no-error.
            if not avail bgtt then delete gttmbMenu.
        end.
    end.
    /* Tack the favorites onto the favorite section */
    if lMaintMode = no then do:
        if avail psi-user
            and pcProcName = 'w0021.w'
            then do iIdx = 1 to num-entries(psi-user.favoritelist):
            find first mbMenu no-lock where mbMenu.key = int(entry(iIdx, psi-user.favoritelist)) no-error.
            if avail mbMenu then do:
               create gttmbMenu.
               buffer-copy mbMenu to gttmbMenu
                   assign
                   gttmbmenu.ParentKey = iFavKey
                   gttmbmenu.key = iMaxKey + 10
                   gttmbmenu.SeqNo = iIdx + 10.
               iMaxKey = iMaxKey + 10.
            end.
        end. /* building main menu */

        if avail psi-user
            and psi-user.PlantAccount = yes 
            and pcProcname = 'w0021.w' then do:
            /* take out the whole accounting menu */
            find first gttMbmenu
                where gttMbmenu.name = '&Acctg'
                and gttMbmenu.ParentKey = 0 no-error.
            if avail gttMbmenu then RemoveMenuTree(gttMbMenu.key).
        end.
        find first choice no-lock
            where choice.field-no = 73
            and choice.descr = pcProcName no-error.
        if avail choice then do:
            /* Add Wiki link to the menu items */
            select max(SeqNo) into iMaxMainSeq from mbmenu where mbmenu.Procname = pcProcName and mbmenu.parentKey = 0.
            select max(Key) into iMaxMainKey from mbmenu where mbmenu.Procname = pcProcName and mbmenu.parentKey = 0.
            find first bgtt where bgtt.Procname = pcprocName and bgtt.parentkey = 0 no-error.  /* not found if no menu avail */
            create gttMbMenu.
            assign
                gttMbmenu.ProcName = pcProcName
                gttMbmenu.groupID = (if avail bgtt then bgtt.groupID else 'General')
                gttMbmenu.SeqNo = (if iMaxMainSeq = ? then 1 else iMaxMainSeq + 1)
                gttMbMenu.key = (if iMaxMainKey = ? then 2 else iMaxMainKey + 2)
                gttMbmenu.Name = 'Wiki'
                gttMbmenu.type = 'mi'
                gttMbmenu.mitype = 'Normal'
                gttMbmenu.miProcName = substitute('wiki|&1', choice.misc1).
        end.
    end.  /* not in maint mode */
end.

/* output stream test to c:\temp\gttmbmenu2.txt append.                                            */
/* export stream test ''.                                                                          */
/* export stream test string(now).                                                                 */
/* export stream test substitute('Building for Proc name:&1   - Group: &2',pcProcname,pcgroupID).  */
/* for each gttmbmenu:                                                                             */
/*     export stream test delimiter '~011' gttmbmenu.                                              */
/* end.                                                                                            */
/* output stream test close.                                                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-parseOpts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE parseOpts Procedure 
PROCEDURE parseOpts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign
    lSecurity = yes
    lFavoriteBuild = no
    lMaintMode = no.

do iIdx = 2 to num-entries(pcProcName, '|'):
    assign
        cVal = ?
        cProp = entry(iIdx, pcProcName, '|').
    if num-entries(cProp, '=') > 0 then do:
        assign cVal = entry(2, cProp, '=')
               cProp = entry(1, cProp, '=').
        case cProp:
            when 'Secured' then lSecurity = logical(cVal).
            when 'BuildFav' then lFavoriteBuild = logical(cVal).
            when 'MaintMode' then lMaintMode = logical(cval).
        end case.
    end.
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-checkSecurity) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkSecurity Procedure 
FUNCTION checkSecurity RETURNS LOGICAL
  ( input pcProgName as char,
    input pcLogon as char,
    input pcGroup as char ) :

define variable iLoop as integer no-undo.
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*
  This is the same code as sessionlib, but it seems not to be available yet
  when the menus are built.
  */
   find first program no-lock
       where program.ProgramName = pcProgName no-error.

   if not avail program then
       return true.    /* not a program, but an internal function call */

   /* 77945 Do not show Genesis programs in Menu that have no security area */
   IF AVAIL program 
       and (program.SecurityAreaID = 0 or program.securityAreaID = ?) then 
       return FALSE.

   if avail Program
      and (program.FuncArea = ? or program.funcArea = '')
      then return true.

    /* If user id found, grant access */
    if avail Program
        and can-do(Program.FuncArea, pcLogon)
        then return true.

    /* if any group IDs are present, grant access */
    do iLoop = 1 to num-entries(pcGroup):
       if avail Program and can-do(Program.FuncArea, entry(iLoop,pcGroup))
           then return true.
    end.

    return false.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RemoveMenuTree) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RemoveMenuTree Procedure 
FUNCTION RemoveMenuTree RETURNS LOGICAL
  ( input piMenuKey as int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  for each gttMbmenu
      where gttMbmenu.ParentKey = piMenuKey:
      if gttMbMenu.type = 'sm' then do:
          RemoveMenuTree(gttmbmenu.key).
      end.
      else do:
          delete gttMbMenu.
      end.
  end.
  find first gttMbmenu where gttMbmenu.key = piMenuKey no-error.
  if avail gttmbMenu then delete gttMbMenu.

  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

