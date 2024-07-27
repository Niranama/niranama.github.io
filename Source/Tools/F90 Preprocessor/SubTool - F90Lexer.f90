
SUBMODULE (ModTool_F90PreProcess) SubTool_F90Lexer

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule contains routines that perform lexing tasks.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of submodule
    tCharStar, PARAMETER    :: SubModName = 'SubTool_F90Lexer'
    ! ------------------------------!
    !       Lexer_Parameters        !
    ! ------------------------------!
    !
    !  Parameters for f90lex utility
    !
    tCharLen(1), PARAMETER  :: ChrTab       = ACHAR(9)      ! tab character
    tCharLen(1), PARAMETER  :: ChrBackslash = ACHAR(92)     ! backslash character
    !
    !  A few source characteristics
    !
    tInteger, PARAMETER     :: LMaxVarName  = 31                    ! max. variable name length
    tInteger, PARAMETER     :: LMaxFileName = 64                    ! max. file name length
    tInteger, PARAMETER     :: NMaxContLine = 39                    ! max. # cont. lines
    tInteger, PARAMETER     :: NMaxStmCmt   = 4*(NMaxContLine+1)    ! same including embedded comments
    tInteger, PARAMETER     :: LMaxLine     = 132                   ! max. line length
    tInteger, PARAMETER     :: LMaxStm      = (LMaxLine-1)*NMaxContLine+LMaxLine    ! max. sttmt. length
    !
    !  Line codes
    !
    tInteger, PARAMETER     :: KL_Unavail   = -1   ! Unavailable
    tInteger, PARAMETER     :: KL_Normal    =  0   ! Not continued, non-comment
    tInteger, PARAMETER     :: KL_Last      =  1   ! Last line
    tInteger, PARAMETER     :: KL_Continue  =  2   ! Continued line
    tInteger, PARAMETER     :: KL_Comment   =  3   ! Comment line
    tInteger, PARAMETER     :: KL_False     =  4   ! False comment
    tInteger, PARAMETER     :: KL_Trail     =  5   ! Trailing comment line
    !
    !  Token codes
    !
    tInteger, PARAMETER     :: KK_Undef         =  0   ! Undefined
    tInteger, PARAMETER     :: KK_Comment       =  1   ! Comment string
    tInteger, PARAMETER     :: KK_EmbCmt        =  2   ! Embedded comment in continued instr.
    tInteger, PARAMETER     :: KK_ChrStr        =  3   ! Character string
    tInteger, PARAMETER     :: KK_Identifier    =  4   ! Identifier
    tInteger, PARAMETER     :: KK_IntNumVal     =  5   ! Integer Numerical value
    tInteger, PARAMETER     :: KK_Kind          =  6   ! _Kind (underscore)
    tInteger, PARAMETER     :: KK_Colon         =  7   ! :
    tInteger, PARAMETER     :: KK_SemiColon     =  8   ! ;
    tInteger, PARAMETER     :: KK_LParen        =  9   ! (
    tInteger, PARAMETER     :: KK_RParen        = 10   ! )
    tInteger, PARAMETER     :: KK_Slash         = 11   ! /
    tInteger, PARAMETER     :: KK_LParSlh       = 12   ! (/
    tInteger, PARAMETER     :: KK_RParSlh       = 13   ! /)
    tInteger, PARAMETER     :: KK_Command       = 14   ! $command (preprocessor)
    tInteger, PARAMETER     :: KK_Question      = 15   ! ?
    tInteger, PARAMETER     :: KK_Percent       = 16   ! %
    tInteger, PARAMETER     :: KK_PlusMinus     = 17   ! + or -
    tInteger, PARAMETER     :: KK_Concat        = 18   ! //
    tInteger, PARAMETER     :: KK_Assign        = 19   ! =
    tInteger, PARAMETER     :: KK_NE            = 20   ! /=
    tInteger, PARAMETER     :: KK_LE            = 21   ! <=
    tInteger, PARAMETER     :: KK_EQ            = 22   ! ==
    tInteger, PARAMETER     :: KK_GE            = 23   ! >=
    tInteger, PARAMETER     :: KK_PTR           = 24   ! =>
    tInteger, PARAMETER     :: KK_GT            = 25   ! >
    tInteger, PARAMETER     :: KK_LT            = 26   ! <
    tInteger, PARAMETER     :: KK_Star          = 27   ! *
    tInteger, PARAMETER     :: KK_Pow           = 28   ! **
    tInteger, PARAMETER     :: KK_Dot           = 29   ! .
    tInteger, PARAMETER     :: KK_Comma         = 30   ! ,
    tInteger, PARAMETER     :: KK_LogOp         = 31   ! .xxx.
    tInteger, PARAMETER     :: KK_EndOfStm      = 32   ! End of Statement (no token)
    tInteger, PARAMETER     :: KK_Ampersand     = 33   ! & (not continuation)
    tInteger, PARAMETER     :: KK_FalseCmt      = 34   ! False comment (i.e. !$HPF)
    tInteger, PARAMETER     :: KK_BlkName       = 35   ! Block name
    tInteger, PARAMETER     :: KK_DblColon      = 36   ! ::
    tInteger, PARAMETER     :: KK_Label         = 37   ! label
    tInteger, PARAMETER     :: KK_RealNumVal    = 38   ! Real Numerical value
    tInteger, PARAMETER     :: KK_OPeren        = 39   ! ( within defined type
    tInteger, PARAMETER     :: KK_Unknown       = 40   ! Other
    !   Macros
    tInteger, PARAMETER     :: NMaxArg = 64                                 ! Max # of arguments
    tInteger, PARAMETER     :: KK_Arg0 = 50                                 ! Base for macro arguments
    tInteger                :: Indx
    tInteger, PARAMETER     :: KK_ArgT(0:NMaxArg) = [(KK_Arg0+Indx, &
                                                     Indx = 0, NMaxArg)]    ! Macro arguments
    !
    !  parameters for variables to hold token stream
    ! 
    tInteger, PARAMETER     :: NMaxRep = LMaxStm
    tInteger, PARAMETER     :: NMaxRepGbl = 8*NMaxRep
    !
    ! ----------------------------------!
    !       Preprocessor_Parameters     !
    ! ----------------------------------!
    !
    !  Parameters for f90ppr utility
    !
    !
    !  False comments
    !
    tInteger,     PARAMETER :: NMax_FalseCmt = 16  ! max # of "False comments"
    tInteger,     PARAMETER :: LMax_FalseCmt =  8  ! max length of "False comments"
    !
    !  Defines
    !
    tInteger,     PARAMETER :: NMax_NestDef = 64  ! max nesting for DEFINEs
    !
    !  Tests
    !
    tInteger,     PARAMETER :: NMax_NestIf  = 64  ! max nesting for IFs, IFDEFs
    !
    !  Include files
    !
    tInteger,     PARAMETER :: NMax_NestInc = 16  ! maximum nesting
    tInteger,     PARAMETER :: LMax_NameInc = 96  ! maximum name length
    !
    !  Loop labels
    !
    tInteger,     PARAMETER :: NMax_NestLab = 16  ! maximum nesting
    tInteger,     PARAMETER :: LMax_LoopLab =  5  ! maximum label length
    !
    !  Current status in f90ppr utility
    !
    tCharStar, PARAMETER    :: ZP_Name  = "f90ppr" 

!** DERIVED TYPE DEFINITIONS
    ! na
    
!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! ------------------------------!
    !       Lexer_Variables         !
    ! ------------------------------!
    !
    !  variables to hold token stream
    ! 
    !-----------------------------------------------------------------------------------
    tCharLen(LMaxStm)           :: ZV_TokIdf            ! to hold identified
    tCharLen(LMaxLine), POINTER :: ZV_BufCmt(:)         ! comments buffer
    tInteger                    :: KK_TokT(1:LMaxStm)   ! codes
    tInteger                    :: IV_Name(1:LMaxStm)   ! names
    tInteger                    :: IV_Start(1:LMaxStm)  ! starting indexes
    tInteger                    :: IV_End(1:LMaxStm)    ! termination indexes
    !-----------------------------------------------------------------------------------
    tCharLen(NMaxRepGbl)        :: ZV_RepGbl            ! to hold replacements
    tInteger                    :: KK_RepT(1:NMaxRep)   ! codes
    tInteger                    :: IRepName(1:NMaxRep)  ! names
    tInteger                    :: IRepStart(1:NMaxRep) ! starting indexes
    tInteger                    :: IRepEnd(1:NMaxRep)   ! termination indexes
    tInteger                    :: IRepNext(1:NMaxRep)  ! next in chain
    tInteger                    :: IRepGbl = 0
    tInteger                    :: IRep  = 0
    !-----------------------------------------------------------------------------------
    !
    ! ----------------------------------!
    !       Preprocessor_Variables      !
    ! ----------------------------------!
    !
    !  Current status variables in f90ppr utility
    !
    tCharLen(1)             :: ZV_Blank = '!'               ! current blank line printing
    tCharLen(2*LMaxLine+1)  :: ZV_LineBuff                  ! advance line in buffer
    tCharLen(2*LMaxLine+1)  :: ZV_LineHeap(NMax_NestInc)    ! advance lines heap for include
    tInteger                :: N_HaveBuff = 0               ! How many lines do we have in advance
    tInteger                :: N_HaveHeap(NMax_NestInc)     ! and the heap
    tInteger                :: KL_Current = KL_Unavail      ! Type of current line
    tInteger                :: KL_Next = KL_Unavail         ! Type of next line
    tInteger                :: KL_NextHeap(NMax_NestInc)    ! and the heap
    tInteger                :: IfSkip = 0                   ! are we skipping code ?
    tCharLen(LMaxLine+1)    :: ZV_OrgStm(NMaxStmCmt)        ! original lines for current statement
                                                            ! (embedded comments when continuation)
    tInteger                :: NLineInStm = 1               ! How many lines do we have in statement
    tInteger                :: NLineInput(0:NMax_NestInc) = &
                              [(0, Indx = 0, NMax_NestInc)] ! number of lines input
    tInteger                :: LineLen = 72                 ! current, desirable, linelength
    tInteger                :: NIndentCurr =  0             ! current indentation
    tInteger                :: NIndentStep =  4             ! current step for indentation
    tInteger                :: NIndentProg =  4             ! Program unit additional indentation
    tInteger                :: INest_Proc  =  0             ! current procedure nesting
    tInteger                :: ILevel_Include = 0           ! current include level
    tCharLen(LMax_NameInc)  :: ZV_TabName(0:NMax_NestInc) = [ "standard input", &
                               ("              ", Indx = 1, NMax_NestInc)]  ! table of names
    !
    !  Interpret # as $
    !
    tInteger                :: IfSharpSameAsDollar = 0  ! is # same as $ ?
    !
    !  Case processing
    !
    tInteger                :: KC_Keyword = KC_Leave   ! Case for keywords
    tInteger                :: KC_UserIdf = KC_Leave   ! Case for user identifiers
    !
    !  False comments
    !
    tInteger                :: N_FalseCmt = 0               ! number of "False comments"
    tCharLen(LMax_FalseCmt) :: ZV_FalseCmt(NMax_FalseCmt)   ! the corresponding strings
    !
    !  Loop labels
    !
    tInteger                :: N_Label   = 0            ! number of labels
    tInteger                :: N_EndLoop = 0            ! number of loops ending on label
    tCharLen(LMax_LoopLab)  :: ZV_EndLoop(NMax_NestLab) ! the corresponding strings
    
    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!
MODULE SUBROUTINE Add_Token (ZTOK, LTOK, KKTOK)
!
!  add token to current stream, and reduce if end of statement
!
    IMPLICIT NONE
!
! arguments
    tInteger,        INTENT (IN)    :: LTOK, KKTOK
    tCharLen (LTOK), INTENT (IN)    :: ZTOK
!
! local variables
    tInteger, SAVE  :: NTOK = 0
    tInteger, SAVE  :: ITOKF = 0
    tInteger, SAVE  :: ITOKD
    tInteger        :: KSSTT
!
! execution
!
!
!  Add to current stream
!
    IF (LTOK > 0) THEN
        NTOK = NTOK + 1
        ITOKD = ITOKF + 1
        ITOKF = ITOKF + LTOK
        KK_TokT (NTOK) = KKTOK
        ZV_TokIdf (ITOKD:ITOKF) = ZTOK (1:LTOK)
        IV_Start (NTOK) = ITOKD
        IV_End (NTOK) = ITOKF
    END IF
!
!  Reduce if end of statement
!
    IF (KKTOK == KK_SemiColon) THEN
        NTOK = NTOK - 1
    END IF
    IF ((KKTOK == KK_SemiColon .OR. KKTOK == KK_EndOfStm) .AND. (NTOK > 0)) THEN
        CALL Parsing_Statement (NTOK, KSSTT)
        IF (IfSkip == 0) THEN
            CALL Rebuilding_Statement (NTOK, KSSTT)
        END IF
        NTOK = 0
        ITOKF = 0
    END IF
    RETURN
END SUBROUTINE Add_Token
!
!******************************************************************************
!
MODULE SUBROUTINE Lexing_FreeForm (AddTok, IFSTP, KSTA)
!
!  Read input file, lexing free-form into token stream, until a
!  simultaneous end-of-line end-of-statement is found.
!
    IMPLICIT NONE
!
! arguments
    PROCEDURE (Add_Tok)     :: AddTok
    tInteger, INTENT (IN)   :: IFSTP ! strip-out comments ?
    tInteger, INTENT (OUT)  :: KSTA ! status code
!
! local variables
    tCharLen (2*LMaxLine)   :: ZLIN
    tCharLen (LMaxStm)      :: ZTOK
    tCharLen (1)            :: ZDLM, ZCHR
    tInteger                :: IFCNT, IFCHC, NTOK, KKTOK, LLIN, ILIN
    tInteger                :: ILINI, LTOK, ICHR, INXT, ICMTI, LCMTI
!
! execution
!
    KSTA = 0
    IFCNT = 0
    IFCHC = 0
    NTOK = 0
    KKTOK = KK_Undef
!
    BODY: DO
        DO
            IF (KL_Current == KL_Last .OR. KL_Current == KL_Trail) THEN
                IF (IFCNT /= 0) THEN
                    KSTA = 2
                    CALL Outputing_Error ("Unexpected end of input")
                    EXIT BODY
                ELSE
                    IF (ILevel_Include > 0) THEN
                        ZV_LineBuff = ZV_LineHeap (ILevel_Include)
                        N_HaveBuff = N_HaveHeap (ILevel_Include)
                        KL_Next = KL_NextHeap (ILevel_Include)
                        ILevel_Include = ILevel_Include - 1
                        CLOSE (LU_Include)
                        LU_Include = LU_Include - 1
                        IF (ILevel_Include == 0) THEN
                            LU_Input = LU_StdIn
                        ELSE
                            LU_Input = LU_Include
                        END IF
                    ELSE
                        KSTA = - 1
                        EXIT BODY
                    END IF
                END IF
            END IF
!
!  Read a line
!
            CALL Reading_Line (LU_Input, ZLIN, KL_Current)
            IF (KL_Current == KL_Unavail .AND. ILevel_Include /= 0) THEN
                KL_Current = KL_Last
                CYCLE
            END IF
            EXIT
        END DO
!
        SELECT CASE (KL_Current)
        CASE (KL_Unavail)
            KSTA = 1
            CALL Outputing_Error ("Problem reading input")
            EXIT BODY
        CASE DEFAULT
            KSTA = 0
        END SELECT
        IF (IFCNT /= 0) THEN
            NLineInStm = NLineInStm + 1
            NLineInStm = MIN (NLineInStm, NMaxStmCmt)
        ELSE
            NLineInStm = 1
        END IF
        ZV_OrgStm (NLineInStm) = ZLIN
!
!  Recognize and skip full comments
!
        LLIN = LEN_TRIM (ZLIN)
        IF (LLIN == 0) THEN
            IF (IFCNT == 0) THEN
                CALL AddTok (ZV_Blank, LEN(ZV_Blank), KK_Comment)
                EXIT BODY
            ELSE
                CALL AddTok (ZV_Blank, LEN(ZV_Blank), KK_EmbCmt)
                CYCLE BODY
            END IF
        END IF
        ILIN = VERIFY (ZLIN(1:LLIN), ChrTab//" ")
        CMTL: DO
            IF (ILIN /= 0) THEN
                IF (ZLIN(ILIN:ILIN) /= "!") THEN
                    EXIT CMTL
                END IF
!
!  Do not skip "False comments"
!
                DO ICMTI = 1, N_FalseCmt
                    LCMTI = LEN_TRIM (ZV_FalseCmt(ICMTI))
                    ILINI = ILIN + LCMTI - 1
                    IF (LLIN > ILINI .AND. ZLIN(ILIN:ILINI) == ZV_FalseCmt(ICMTI) (1:LCMTI)) THEN
                        CALL AddTok (ZLIN(ILIN:ILINI), LCMTI, KK_FalseCmt)
                        ILIN = ILINI + 1
                        EXIT CMTL
                    END IF
                END DO
            ELSE
                IF (IFCNT /= 0) THEN
                    CALL AddTok (" ", 1, KK_EmbCmt)
                    CYCLE BODY
                END IF
            END IF
            IF (IFCNT == 0) THEN
                CALL AddTok (ZLIN, LLIN, KK_Comment)
                EXIT BODY
            ELSE
                CALL AddTok (ZLIN, LLIN, KK_EmbCmt)
                CYCLE BODY
            END IF
        END DO CMTL
!
!  Check for continued mark
!
        IF (IFCNT /= 0) THEN
            IF (ZLIN(ILIN:ILIN) == "&") THEN
                ILIN = ILIN + 1
            ELSE
                IF (IFCHC /= 0) THEN
                    KSTA = 3
                    CALL Outputing_Error ("Illegal continuation for string")
                    EXIT BODY
                ELSE
                    IF (KKTOK /= KK_Undef) THEN
                        CALL AddTok (ZTOK, LTOK, KKTOK)
                    END IF
                    KKTOK = KK_Undef
                END IF
            END IF
        END IF
        IFCNT = 0
        ICHR = ILIN - 1
!
!  Scan line
!
        DO
            DO
                IF (ICHR >=LLIN) THEN
                    IF (IFCNT == 0) THEN
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        EXIT BODY
                    ELSE
                        CYCLE BODY
                    END IF
                END IF
                ICHR = ICHR + 1
                ZCHR = ZLIN (ICHR:ICHR)
                IF (IFCHC == 0) THEN
                    SELECT CASE (ZCHR)
!
!  Spaces
!
                    CASE (ChrTab, ' ')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        KKTOK = KK_Undef
!
!  Letters
!
                    CASE ('A':'Z', 'a':'z')
                        IF (KKTOK == KK_Identifier .OR. KKTOK == KK_Command) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Identifier
                        END IF
!
!  Digits
!
                    CASE ('0':'9')
                        IF (KKTOK == KK_Identifier .OR. KKTOK == KK_IntNumVal) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_IntNumVal
                        END IF
!
!  Underscore (may be in identifier, or as a kind specifier)
!
                    CASE ('_')
                        SELECT CASE (KKTOK)
                        CASE (KK_Identifier)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                        CASE (KK_IntNumVal, KK_ChrStr)
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                            NTOK = NTOK + 1
                            CALL AddTok (ZCHR, 1, KK_Kind)
                            KKTOK = KK_Undef
                        CASE DEFAULT
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Identifier
                        END SELECT
!
!  Colon
!
                    CASE (':')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Colon)
                        KKTOK = KK_Undef
!
!  Semi-colon
!
                    CASE (';')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_SemiColon)
                        KKTOK = KK_Undef
!
!  Opening parenthesis
!
                    CASE ('(')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        LTOK = 1
                        ZTOK (LTOK:LTOK) = ZCHR
                        KKTOK = KK_LParen
!
!  Closing parenthesis
!
                    CASE (')')
                        IF (KKTOK == KK_Slash) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_RParSlh)
                            KKTOK = KK_Undef
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            CALL AddTok (ZCHR, 1, KK_RParen)
                            KKTOK = KK_Undef
                        END IF
!
!  Exclamation mark (start of comment)
!
                    CASE ('!')
                        IF (KKTOK /= KK_Undef .AND. IFCNT == 0) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        IF (IFCNT == 0) THEN
                            NTOK = NTOK + 1
                            CALL AddTok (ZLIN(ICHR:LLIN), (LLIN-ICHR+1), KK_Comment)
                            EXIT BODY
                        ELSE
                            IF (IFSTP == 0) CALL Writing_Statement (ZLIN, 0, ZLIN, 0, ZLIN(ICHR:LLIN), LLIN-ICHR+1, ICHR-1)
                            CYCLE BODY
                        END IF
!
!  Dollar (used as preprocessor command introduction)
!
                    CASE ('$')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        LTOK = 1
                        ZTOK (LTOK:LTOK) = '$'
                        KKTOK = KK_Command
!
!  Sharp (same as $ or !, depending on current status)
!
                    CASE ('#')
                        IF (IfSharpSameAsDollar /= 0) THEN
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = '$'
                            KKTOK = KK_Command
                        ELSE
                            IF (KKTOK /= KK_Undef .AND. IFCNT == 0) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            IF (IFCNT == 0) THEN
                                NTOK = NTOK + 1
                                CALL AddTok (ZLIN(ICHR:LLIN), (LLIN-ICHR+1), KK_Comment)
                                EXIT BODY
                            ELSE
                                IF (IFSTP == 0) CALL Writing_Statement (ZLIN, 0, ZLIN, 0, ZLIN(ICHR:LLIN), LLIN-ICHR+1, ICHR-1)
                                CYCLE BODY
                            END IF
                        END IF
!
!  Question mark
!
                    CASE ('?')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Question)
                        KKTOK = KK_Undef
!
!  Continuation mark
!
                    CASE ('&')
                        IFCNT = 1
                        IF (ICHR < LLIN) THEN
                            INXT = VERIFY (ZLIN(ICHR+1:LLIN), ChrTab//" ")
                            IF (INXT /= 0) THEN
                                IF (ZLIN(ICHR+INXT:ICHR+INXT) /= "!") THEN
                                    IF (KKTOK /= KK_Undef) THEN
                                        CALL AddTok (ZTOK, LTOK, KKTOK)
                                    END IF
                                    NTOK = NTOK + 1
                                    CALL AddTok (ZCHR, 1, KK_Ampersand)
                                    KKTOK = KK_Undef
                                    IFCNT = 0
                                END IF
                            END IF
                        END IF
!
!  Percent
!
                    CASE ('%')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Percent)
                        KKTOK = KK_Undef
!
!  Plus and Minus
!
                    CASE ('+', '-')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_PlusMinus)
                        KKTOK = KK_Undef
!
!  Slash
!
                    CASE ('/')
                        SELECT CASE (KKTOK)
                        CASE (KK_Slash)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_Concat)
                            KKTOK = KK_Undef
                        CASE (KK_LParen)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_LParSlh)
                            KKTOK = KK_Undef
                        CASE DEFAULT
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Slash
                        END SELECT
!
!  Star
!
                    CASE ('*')
                        IF (KKTOK == KK_Star) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_Pow)
                            KKTOK = KK_Undef
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Star
                        END IF
!
!  Superior
!
                    CASE ('>')
                        IF (KKTOK == KK_Assign) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_PTR)
                            KKTOK = KK_Undef
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_GT
                        END IF
!
!  Inferior
!
                    CASE ('<')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        LTOK = 1
                        ZTOK (LTOK:LTOK) = ZCHR
                        KKTOK = KK_LT
!
!  Equal
!
                    CASE ('=')
                        SELECT CASE (KKTOK)
                        CASE (KK_Slash)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_NE)
                            KKTOK = KK_Undef
                        CASE (KK_LT)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_LE)
                            KKTOK = KK_Undef
                        CASE (KK_Assign)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_EQ)
                            KKTOK = KK_Undef
                        CASE (KK_GE)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_GE)
                            KKTOK = KK_Undef
                        CASE DEFAULT
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Assign
                        END SELECT
!
!  Dot
!
                    CASE ('.')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Dot)
                        KKTOK = KK_Undef
!
!  Separator
!
                    CASE (',')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Comma)
                        KKTOK = KK_Undef
!
!  String delimiter
!
                    CASE ('"', "'")
                        IF (KKTOK == KK_ChrStr) THEN
                            IF (ZCHR == ZDLM) THEN
                                LTOK = LTOK + 1
                                ZTOK (LTOK:LTOK) = ZCHR
                            ELSE
                                ZDLM = ZCHR
                                LTOK = 1
                                ZTOK (LTOK:LTOK) = ZCHR
                                NTOK = NTOK + 1
                            END IF
                            IFCHC = 1
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            IF (ICHR == LLIN) THEN
                                CALL Outputing_Error ("Unmatched "//ZCHR)
                            ELSE
                                ZDLM = ZCHR
                                LTOK = 1
                                ZTOK (LTOK:LTOK) = ZCHR
                                NTOK = NTOK + 1
                                KKTOK = KK_ChrStr
                                IFCHC = 1
                            END IF
                        END IF
!
!  Other character
!
                    CASE DEFAULT
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Unknown)
                        KKTOK = KK_Undef
                    END SELECT
!
!  We are inside a char string
!
                ELSE
!
!  Test for end of current string
!
                    IF (ZCHR == ZDLM) THEN
                        LTOK = LTOK + 1
                        ZTOK (LTOK:LTOK) = ZCHR
                        IFCHC = 0
                    ELSE
!
!  Test for end of line
!
                        IF (ICHR == LLIN) THEN
                            IF (ZCHR == '&') THEN
                                IFCNT = 1
                                CYCLE BODY
                            ELSE
                                CALL Outputing_Error ("Unmatched "//ZDLM)
                            END IF
                        ELSE
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                        END IF
                    END IF
                END IF
            END DO
        END DO
    END DO BODY
    CALL AddTok (ZTOK, 0, KK_EndOfStm)
    RETURN
END SUBROUTINE Lexing_FreeForm
!
!******************************************************************************
!
MODULE SUBROUTINE Lexing_FixedForm (AddTok, IFSTP, KSTA)
!
!  Read input file, lexing fixed-form into token stream, until a
!  simultaneous end-of-line end-of-statement is found.
!
    IMPLICIT NONE
!
! arguments
    PROCEDURE (Add_Tok)     :: AddTok
    tInteger, INTENT (IN)   :: IFSTP    ! strip-out comments ?
    tInteger, INTENT (OUT)  :: KSTA     ! status code
!
! local variables
    tCharLen (2*LMaxLine)   :: ZLINW
    tCharLen (LMaxStm)      :: ZLIN
    tCharLen (LMaxStm)      :: ZTOK
    tCharLen (1)            :: ZDLM, ZCHR
    tInteger, SAVE          :: IFCTN = 0
    tInteger    :: LLIN, IFCHC, IFCNT, NTOK, KKTOK, IBEG, LLINW
    tInteger    :: ICMTI, LCMTI, ILIN, ILINW, ICHR, IFCTC, LTOK, INXT
!
! execution
!
    KSTA = 0
    LLIN = 0
    IFCHC = 0
    IFCNT = 0
    NTOK = 0
    KKTOK = KK_Undef
!
    BODY: DO
        LLIN = 0
        IBEG = 1
!
!  Read a line
!
        RDLIN: DO
            DO
                IFCTN = 0
                IF (KL_Current == KL_Last .OR. KL_Current == KL_Trail) THEN
                    IF (ILevel_Include > 0) THEN
                        ZV_LineBuff = ZV_LineHeap (ILevel_Include)
                        N_HaveBuff = N_HaveHeap (ILevel_Include)
                        KL_Next = KL_NextHeap (ILevel_Include)
                        ILevel_Include = ILevel_Include - 1
                        CLOSE (LU_Include)
                        LU_Include = LU_Include - 1
                        IF (ILevel_Include == 0) THEN
                            LU_Input = LU_StdIn
                        ELSE
                            LU_Input = LU_Include
                        END IF
                        IF (KL_Next /= KL_Last .OR. N_HaveBuff /= 0) EXIT
                    ELSE
                        KSTA = - 1
                        EXIT BODY
                    END IF
                ELSE IF (KL_Current == KL_Continue) THEN
                    IFCTN = 1
                END IF
!
                CALL Reading_Line (LU_Input, ZLINW, KL_Current)
                IF (KL_Current == KL_Unavail .AND. ILevel_Include /= 0) THEN
                    KL_Current = KL_Last
                    CYCLE
                END IF
                EXIT
            END DO
!
            SELECT CASE (KL_Current)
!
!  Unavailable
!
            CASE (KL_Unavail)
                KSTA = 1
                CALL Outputing_Error ("Problem reading input")
                EXIT BODY
!
!  "False comments"
!
            CASE (KL_False)
                LLINW = LEN_TRIM (ZLINW)
                DO ICMTI = 1, N_FalseCmt
                    LCMTI = LEN_TRIM (ZV_FalseCmt(ICMTI))
                    IF (LLINW > LCMTI .AND. ZLINW(1:LCMTI) == ZV_FalseCmt(ICMTI) (1:LCMTI)) THEN
                        CALL AddTok (ZLINW(1:LCMTI), LCMTI, KK_FalseCmt)
                        IBEG = LCMTI + 1
                        EXIT RDLIN
                    END IF
                END DO
!
!  True comments
!
            CASE (KL_Comment, KL_Trail)
                LLINW = LEN_TRIM (ZLINW)
                CALL AddTok (ZLINW, LLINW, KK_Comment)
                CALL AddTok (ZTOK, 0, KK_EndOfStm)
                CYCLE RDLIN
!
!  Non-comment, not continued
!
            CASE (KL_Normal, KL_Last)
                LLINW = LEN_TRIM (ZLINW)
                ILINW = VERIFY (ZLINW(1:LLINW), ChrTab//" ")
                ILIN = LLIN + 1
                LLIN = LLINW - ILINW + ILIN
                ZLIN (ILIN:LLIN) = ZLINW (ILINW:LLINW)
                EXIT RDLIN
!
!  Continued
!
            CASE (KL_Continue)
!
!  Check for trailing comment
!
                IF (IFCTN == 0) THEN
                    LLINW = 72
                ELSE
                    LLINW = 72 - 6
                END IF
                ICHR = IBEG - 1
                IFCTC = 0
                CHECK: DO
                    IF (ICHR >=LLINW) THEN
                        EXIT CHECK
                    END IF
                    ICHR = ICHR + 1
                    ZCHR = ZLINW (ICHR:ICHR)
                    IF (IFCTC == 0) THEN
                        IF (ZCHR == "!") THEN
                            IF (IFSTP == 0) THEN
                                CALL Writing_Statement (ZLINW, 0, ZLINW, 0, ZLINW(ICHR:LLINW), &
                                                        LLINW-ICHR+1, ICHR-1)
                            END IF
                            LLINW = ICHR - 1
                            EXIT CHECK
                        ELSE IF (ZCHR == '"' .OR. ZCHR == "'") THEN
                            ZDLM = ZCHR
                            IFCTC = 1
                        END IF
                    ELSE
                        IF (ZCHR == ZDLM) THEN
                            IFCTC = 0
                        END IF
                    END IF
                END DO CHECK
!
                ILINW = VERIFY (ZLINW(1:LLINW), ChrTab//" ")
                ILIN = LLIN + 1
                LLIN = LLINW - ILINW + ILIN
                ZLIN (ILIN:LLIN) = ZLINW (ILINW:LLINW)
                CYCLE RDLIN
            END SELECT
!
        END DO RDLIN
!
        ICHR = IBEG - 1
!
!  Scan line
!
        DO
            DO
                IF (ICHR >=LLIN) THEN
                    IF (IFCNT == 0) THEN
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        EXIT BODY
                    ELSE
                        CYCLE BODY
                    END IF
                END IF
                ICHR = ICHR + 1
                ZCHR = ZLIN (ICHR:ICHR)
                IF (IFCHC == 0) THEN
                    SELECT CASE (ZCHR)
!
!  Tabs
!
                    CASE (ChrTab)
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        KKTOK = KK_Undef
!
!  Spaces (are taken as significant, too complex to handle otherwise)
!
                    CASE (' ')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        KKTOK = KK_Undef
!
!  Letters
!
                    CASE ('A':'Z', 'a':'z')
                        IF (KKTOK == KK_Identifier .OR. KKTOK == KK_Command) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Identifier
                        END IF
!
!  Digits
!
                    CASE ('0':'9')
                        IF (KKTOK == KK_Identifier .OR. KKTOK == KK_IntNumVal) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_IntNumVal
                        END IF
!
!  Underscore (may be in identifier, or as a kind specifier)
!
                    CASE ('_')
                        SELECT CASE (KKTOK)
                        CASE (KK_Identifier)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                        CASE (KK_IntNumVal, KK_ChrStr)
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                            NTOK = NTOK + 1
                            CALL AddTok (ZCHR, 1, KK_Kind)
                            KKTOK = KK_Undef
                        CASE DEFAULT
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Identifier
                        END SELECT
!
!  Colon
!
                    CASE (':')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Colon)
                        KKTOK = KK_Undef
!
!  Semi-colon
!
                    CASE (';')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_SemiColon)
                        KKTOK = KK_Undef
!
!  Opening parenthesis
!
                    CASE ('(')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        LTOK = 1
                        ZTOK (LTOK:LTOK) = ZCHR
                        KKTOK = KK_LParen
!
!  Closing parenthesis
!
                    CASE (')')
                        IF (KKTOK == KK_Slash) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_RParSlh)
                            KKTOK = KK_Undef
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            CALL AddTok (ZCHR, 1, KK_RParen)
                            KKTOK = KK_Undef
                        END IF
!
!  Exclamation mark (start of comment)
!
                    CASE ('!')
                        IF (KKTOK /= KK_Undef .AND. IFCNT == 0) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        IF (IFCNT == 0) THEN
                            NTOK = NTOK + 1
                            CALL AddTok (ZLIN(ICHR:LLIN), (LLIN-ICHR+1), KK_Comment)
                            EXIT BODY
                        ELSE
                            IF (IFSTP == 0) THEN
                                CALL Writing_Statement (ZLIN, 0, ZLIN, 0, ZLIN(ICHR:LLIN), &
                                                        LLIN-ICHR+1, ICHR-1)
                            END IF
                            CYCLE BODY
                        END IF
!
!  Dollar (used as preprocessor command introduction)
!
                    CASE ('$')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        LTOK = 1
                        ZTOK (LTOK:LTOK) = '$'
                        KKTOK = KK_Command
!
!  Sharp (same as $ or !, depending on current status)
!
                    CASE ('#')
                        IF (IfSharpSameAsDollar /= 0) THEN
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = '$'
                            KKTOK = KK_Command
                        ELSE
                            IF (KKTOK /= KK_Undef .AND. IFCNT == 0) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            IF (IFCNT == 0) THEN
                                NTOK = NTOK + 1
                                CALL AddTok (ZLIN(ICHR:LLIN), (LLIN-ICHR+1), KK_Comment)
                                EXIT BODY
                            ELSE
                                IF (IFSTP == 0) THEN
                                    CALL Writing_Statement (ZLIN, 0, ZLIN, 0, ZLIN(ICHR:LLIN), &
                                                            LLIN-ICHR+1, ICHR-1)
                                END IF
                                CYCLE BODY
                            END IF
                        END IF
!
!  Question mark
!
                    CASE ('?')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Question)
                        KKTOK = KK_Undef
!
!  Continuation mark
!
                    CASE ('&')
                        IFCNT = 1
                        IF (ICHR < LLIN) THEN
                            INXT = VERIFY (ZLIN(ICHR+1:LLIN), ChrTab//" ")
                            IF (INXT /= 0) THEN
                                IF (ZLIN(ICHR+INXT:ICHR+INXT) /= "!") THEN
                                    IF (KKTOK /= KK_Undef) THEN
                                        CALL AddTok (ZTOK, LTOK, KKTOK)
                                    END IF
                                    NTOK = NTOK + 1
                                    CALL AddTok (ZCHR, 1, KK_Ampersand)
                                    KKTOK = KK_Undef
                                    IFCNT = 0
                                END IF
                            END IF
                        END IF
!
!  Percent
!
                    CASE ('%')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Percent)
                        KKTOK = KK_Undef
!
!  Plus and Minus
!
                    CASE ('+', '-')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_PlusMinus)
                        KKTOK = KK_Undef
!
!  Slash
!
                    CASE ('/')
                        SELECT CASE (KKTOK)
                        CASE (KK_Slash)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_Concat)
                            KKTOK = KK_Undef
                        CASE (KK_LParen)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_LParSlh)
                            KKTOK = KK_Undef
                        CASE DEFAULT
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Slash
                        END SELECT
!
!  Star
!
                    CASE ('*')
                        IF (KKTOK == KK_Star) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_Pow)
                            KKTOK = KK_Undef
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Star
                        END IF
!
!  Superior
!
                    CASE ('>')
                        IF (KKTOK == KK_Assign) THEN
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_PTR)
                            KKTOK = KK_Undef
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_GT
                        END IF
!
!  Inferior
!
                    CASE ('<')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        LTOK = 1
                        ZTOK (LTOK:LTOK) = ZCHR
                        KKTOK = KK_LT
!
!  Equal
!
                    CASE ('=')
                        SELECT CASE (KKTOK)
                        CASE (KK_Slash)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_NE)
                            KKTOK = KK_Undef
                        CASE (KK_LT)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_LE)
                            KKTOK = KK_Undef
                        CASE (KK_Assign)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_EQ)
                            KKTOK = KK_Undef
                        CASE (KK_GE)
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            CALL AddTok (ZTOK, LTOK, KK_GE)
                            KKTOK = KK_Undef
                        CASE DEFAULT
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            NTOK = NTOK + 1
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            KKTOK = KK_Assign
                        END SELECT
!
!  Dot
!
                    CASE ('.')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Dot)
                        KKTOK = KK_Undef
!
!  Separator
!
                    CASE (',')
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Comma)
                        KKTOK = KK_Undef
!
!  String delimiter
!
                    CASE ('"', "'")
                        IF (KKTOK == KK_ChrStr) THEN
                            IF (ZCHR == ZDLM) THEN
                                LTOK = LTOK + 1
                                ZTOK (LTOK:LTOK) = ZCHR
                            ELSE
                                ZDLM = ZCHR
                                LTOK = 1
                                ZTOK (LTOK:LTOK) = ZCHR
                                NTOK = NTOK + 1
                            END IF
                            IFCHC = 1
                        ELSE
                            IF (KKTOK /= KK_Undef) THEN
                                CALL AddTok (ZTOK, LTOK, KKTOK)
                            END IF
                            ZDLM = ZCHR
                            LTOK = 1
                            ZTOK (LTOK:LTOK) = ZCHR
                            NTOK = NTOK + 1
                            KKTOK = KK_ChrStr
                            IFCHC = 1
                        END IF
!
!  Other character
!
                    CASE DEFAULT
                        IF (KKTOK /= KK_Undef) THEN
                            CALL AddTok (ZTOK, LTOK, KKTOK)
                        END IF
                        NTOK = NTOK + 1
                        CALL AddTok (ZCHR, 1, KK_Unknown)
                        KKTOK = KK_Undef
                    END SELECT
!
!  We are inside a char string
!
                ELSE
!
!  Test for end of current string
!
                    IF (ZCHR == ZDLM) THEN
                        LTOK = LTOK + 1
                        ZTOK (LTOK:LTOK) = ZCHR
                        IFCHC = 0
                    ELSE
!
!  Test for end of line
!
                        IF (ICHR == LLIN) THEN
                            IF (ZCHR == '&') THEN
                                IFCNT = 1
                                CYCLE BODY
                            ELSE
                                CALL Outputing_Error ("Unmatched "//ZDLM)
                            END IF
                        ELSE
                            LTOK = LTOK + 1
                            ZTOK (LTOK:LTOK) = ZCHR
                        END IF
                    END IF
                END IF
            END DO
        END DO
    END DO BODY
    CALL AddTok (ZTOK, 0, KK_EndOfStm)
    RETURN
END SUBROUTINE Lexing_FixedForm
!
!******************************************************************************
!
SUBROUTINE Allocate_BufCmt (NSize)
!
! To allocate the 'ZV_BufCmt' variable
!
    IMPLICIT NONE
!
! argument
    tInteger, INTENT (IN) :: NSize
!
! execution
!
    ALLOCATE (ZV_BufCmt(NSize))
!
    RETURN
!
END SUBROUTINE Allocate_BufCmt
!
!**************************************************************************************
!
SUBROUTINE Resize_BufCmt (NSize)
!
! To resize the 'ZV_BufCmt' variable
!
    IMPLICIT NONE
!
! argument
    tInteger, INTENT (INOUT) :: NSize
!
! local variable
    tCharLen (LMaxLine), ALLOCATABLE :: TmpBuf (:)
!
! execution
!
    ALLOCATE (TmpBuf(NSize))
    TmpBuf (1:NSize) = ZV_BufCmt (1:NSize)
    DEALLOCATE (ZV_BufCmt)
    ALLOCATE (ZV_BufCmt(2*NSize))
    ZV_BufCmt (1:NSize) = TmpBuf (1:NSize)
    DEALLOCATE (TmpBuf)
    NSize = 2 * NSize
!
    RETURN
!
END SUBROUTINE Resize_BufCmt
!
!**************************************************************************************
!
SUBROUTINE Reading_Line (LUREA, ZLIN, KLLIN)
!
!  Read a line, with advance buffering, for fixed form, in order
!  to detect continued lines
!
    IMPLICIT NONE
!
! arguments
    tInteger,  INTENT (IN)  :: LUREA
    tCharStar, INTENT (OUT) :: ZLIN     ! The line
    tInteger,  INTENT (OUT) :: KLLIN    ! line type code
!
! parameters
    tCharStar,    PARAMETER :: ZCMT1 = "Cc!Xx*"
    tCharLen (1), PARAMETER :: ZCR   = ACHAR (13)
    tInteger,     PARAMETER :: ICNTF = 6
!
! local variables
    tInteger, SAVE  :: N_HaveBuffM = 0
    tInteger        :: KREA, LLINW, ICMTI, IHAV, LCMTI, ILINW
!
! execution
!
!  If don't have, read line
!
    IF (N_HaveBuff == 0) THEN
        READ (LUREA, "(a)", IOSTAT=KREA) ZV_LineBuff
!
        IF (KREA /= 0) THEN
            KLLIN = KL_Unavail
            RETURN
        ELSE
            N_HaveBuff = N_HaveBuff + 1
            KL_Next = KL_Normal
        END IF
        LLINW = LEN_TRIM (ZV_LineBuff)
!
!  Remove trailing <CR> if any
!
        IF (LLINW > 0) THEN
            IF (ZV_LineBuff(LLINW:LLINW) == ZCR) THEN
                ZV_LineBuff (LLINW:LLINW) = ' '
                LLINW = LLINW - 1
            END IF
        END IF
!
!  If fixed form, find out if line is comment
!
        IF (IfFixedIn /= 0) THEN
!
!  Recognize blank comments
!
            IF (LLINW == 0) THEN
                ZV_LineBuff (1:1) = '!'
                LLINW = 1
                KL_Next = KL_Comment
!
!  And other comments
!
            ELSE
                IF (INDEX(ZCMT1, ZV_LineBuff(1:1)) /= 0) THEN
                    ZV_LineBuff (1:1) = '!'
!
!  Do not skip "False comments"
!
                    KL_Next = KL_Comment
                    DO ICMTI = 1, N_FalseCmt
                        LCMTI = LEN_TRIM (ZV_FalseCmt(ICMTI))
                        IF (LLINW > LCMTI .AND. &
                            ZV_LineBuff(1:LCMTI) == ZV_FalseCmt(ICMTI) (1:LCMTI)) THEN
                            KL_Next = KL_False
                            EXIT
                        END IF
                    END DO
                END IF
            END IF
!
!  Handle past column 72 parts
!
            IF (KL_Next /= KL_Comment .AND. LLINW > 72) THEN
                ZV_LineBuff = ZV_LineBuff (1:72)
            END IF
        END IF
    END IF
!
!  Provide requested line
!
    IF (N_HaveBuff == 1) THEN
        IF (IfFixedIn == 0) THEN
            ZLIN = TRIM (ZV_LineBuff)
        ELSE
            ZLIN = ZV_LineBuff
        END IF
        KLLIN = KL_Next
    ELSE IF (N_HaveBuff > 1) THEN
        ZLIN = TRIM (ZV_BufCmt(1))
        KLLIN = KL_Comment
        DO IHAV = 1, N_HaveBuff - 2
            ZV_BufCmt (IHAV) = ZV_BufCmt (IHAV+1)
        END DO
    ELSE
        KLLIN = KL_Unavail
        RETURN
    END IF
    N_HaveBuff = N_HaveBuff - 1
    NLineInput (ILevel_Include) = NLineInput (ILevel_Include) + 1
!
!  If necessary, read lines in advance
!
    IF (N_HaveBuff == 0) THEN
        RDLIN: DO
            READ (LUREA, "(a)", IOSTAT=KREA) ZV_LineBuff
!
            SELECT CASE (KREA)
            CASE (1:)
                IF (N_HaveBuff == 0) THEN
                    IF (KL_Next == KL_Comment) THEN
                        KLLIN = KL_Trail
                    ELSE
                        KLLIN = KL_Last
                    END IF
                END IF
                CALL Outputing_Error ("Problem reading next line")
                RETURN
            CASE (:-1)
                IF (N_HaveBuff == 0) THEN
                    IF (KL_Next == KL_Comment) THEN
                        KLLIN = KL_Trail
                    ELSE
                        KLLIN = KL_Last
                    END IF
                END IF
                RETURN
            CASE (0)
                N_HaveBuff = N_HaveBuff + 1
                LLINW = LEN_TRIM (ZV_LineBuff)
!
!  Remove trailing <CR> if any
!
                IF (LLINW > 0) THEN
                    IF (ZV_LineBuff(LLINW:LLINW) == ZCR) THEN
                        ZV_LineBuff (LLINW:LLINW) = ' '
                        LLINW = LLINW - 1
                    END IF
                END IF
!
!  If fixed form,
!
                IF (IfFixedIn /= 0) THEN
!
!                find out if next line is comment
!
                    KL_Next = KL_Normal
!
!  Recognize blank comments
!
                    IF (LLINW == 0) THEN
                        KL_Next = KL_Comment
                        ZV_LineBuff (1:1) = '!'
                        LLINW = 1
                    ELSE IF (VERIFY(ZV_LineBuff(1:LLINW), ChrTab//" ") == 0) THEN
                        KL_Next = KL_Comment
                        ZV_LineBuff (1:1) = '!'
                        LLINW = 1
!
!  And other comments
!
                    ELSE
                        IF (INDEX(ZCMT1, ZV_LineBuff(1:1)) /= 0) THEN
                            ZV_LineBuff (1:1) = '!'
!
!  Exclude "False comments"
!
                            KL_Next = KL_Comment
                            DO ICMTI = 1, N_FalseCmt
                                LCMTI = LEN_TRIM (ZV_FalseCmt(ICMTI))
                                IF (LLINW > LCMTI .AND. &
                                    ZV_LineBuff(1:LCMTI) == ZV_FalseCmt(ICMTI) (1:LCMTI)) THEN
                                    KL_Next = KL_False
                                    EXIT RDLIN
                                END IF
                            END DO
                        END IF
                    END IF
!
!  Store comments for future use
!
                    IF (KL_Next == KL_Comment) THEN
                        IF (N_HaveBuff > N_HaveBuffM) THEN
                            IF (N_HaveBuffM == 0) THEN
                                N_HaveBuffM = 16
                                CALL Allocate_BufCmt (N_HaveBuffM)
                            ELSE
                                CALL Resize_BufCmt (N_HaveBuffM)
                            END IF
                        END IF
                        ZV_BufCmt (N_HaveBuff) (1:LMaxLine) = ZV_LineBuff (1:LLINW)
                        CYCLE RDLIN
                    END IF
!
!  Check for continuation mark
!
!
!  Handle past column 72 parts
!
                    IF (LLINW > 72) THEN
                        ZV_LineBuff = ZV_LineBuff (1:72)
                    END IF
                    KL_Next = KL_Normal
                    ILINW = VERIFY (ZV_LineBuff(1:LLINW), ChrTab//" ")
!
!  Exclude Pre-processing commands, which may start column 1
!
                    IF (ILINW /= 6 .AND. (ZV_LineBuff(ILINW:ILINW) == "$" .OR. &
                        ZV_LineBuff(ILINW:ILINW) == "#")) THEN
                        EXIT RDLIN
                    END IF
!
!  Handle TABs as ^<TAB>non-zero-digit = continuation
!
                    IF (LLINW > 2) THEN
                        IF (ZV_LineBuff(1:1) == ChrTab) THEN
                            IF (INDEX("123456789", ZV_LineBuff(2:2)) /= 0) THEN
                                KLLIN = KL_Continue
                                ZV_LineBuff = ZV_LineBuff (3:LLINW)
                                EXIT RDLIN
                            END IF
                        ELSE IF (LLINW > ICNTF) THEN
                            ILINW = VERIFY (ZV_LineBuff(1:LLINW), ChrTab//" 123456789")
                            IF (ILINW >=ICNTF .AND. ZV_LineBuff(ICNTF:ICNTF) /= ChrTab .AND. &
                                ZV_LineBuff(ICNTF:ICNTF) /= ' ') THEN
                                KLLIN = KL_Continue
                                ZV_LineBuff = ZV_LineBuff (ICNTF+1:LLINW)
                                EXIT RDLIN
                            END IF
                        ELSE
                            CYCLE RDLIN
                        END IF
                    END IF
                END IF
                EXIT RDLIN
            END SELECT
        END DO RDLIN
    END IF
    RETURN
END SUBROUTINE Reading_Line
!
!******************************************************************************

END SUBMODULE SubTool_F90Lexer
