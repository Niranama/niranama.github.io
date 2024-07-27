
SUBMODULE (ModTool_F90PreProcess:SubTool_F90Lexer) SubTool_F90Parser

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule contains routines that perform parsing tasks.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of submodule
    tCharStar, PARAMETER    :: SubModName = 'SubTool_F90Parser'
    ! ------------------------------!
    !       Parser_Parameters       !
    ! ------------------------------!
    !
    !  Parsing parameters for f90ppr utility
    ! 
    !
    !   Pre-processing commands
    !
    tCharStar, PARAMETER    :: ZA_Dollar    = "$"
    tCharStar, PARAMETER    :: ZA_Define    = "$DEFINE"
    tCharStar, PARAMETER    :: ZA_ElseIf    = "$ELIF"
    tCharStar, PARAMETER    :: ZA_Else      = "$ELSE"
    tCharStar, PARAMETER    :: ZA_End       = "$ENDIF"
    tCharStar, PARAMETER    :: ZA_EVal      = "$EVAL"
    tCharStar, PARAMETER    :: ZA_If        = "$IF"
    tCharStar, PARAMETER    :: ZA_IfDef     = "$IFDEF"
    tCharStar, PARAMETER    :: ZA_IfNDef    = "$IFNDEF"
    tCharStar, PARAMETER    :: ZA_Include   = "$INCLUDE"
    tCharStar, PARAMETER    :: ZA_Macro     = "$MACRO"
    tCharStar, PARAMETER    :: ZA_Undef     = "$UNDEF"
    !
    !  Codes for current statement parsing left context
    !
    tInteger, PARAMETER     :: KC_BeginUnknown  =   0  ! begin statement, nothing known
    tInteger, PARAMETER     :: KC_BeginExecute  =   1  ! begin executable stt
    tInteger, PARAMETER     :: KC_BeginLabStm   =   2  ! begin labelled stt
    tInteger, PARAMETER     :: KC_BeginNamedBlk =   3  ! begin named block stt
    tInteger, PARAMETER     :: KC_Command       =   4  ! within fppr command
    tInteger, PARAMETER     :: KC_Interface     =   5  ! within interface stt
    tInteger, PARAMETER     :: KC_Executable    =   6  ! within executable stt
    tInteger, PARAMETER     :: KC_Format        =   7  ! within format stt
    tInteger, PARAMETER     :: KC_IOStm         =   8  ! within I/O stt
    tInteger, PARAMETER     :: KC_DecAttb       =   9  ! within declaration attributes
    tInteger, PARAMETER     :: KC_Case          =  10  ! after CASE
    tInteger, PARAMETER     :: KC_Intent        =  11  ! within INTENT
    tInteger, PARAMETER     :: KC_Implicit      =  12  ! after IMPLICIT
    tInteger, PARAMETER     :: KC_Use           =  13  ! after USE
    tInteger, PARAMETER     :: KC_EntryFunc     =  14  ! after ENTRY or FUNCTION
    tInteger, PARAMETER     :: KC_Alloc         =  15  ! within allocation
    tInteger, PARAMETER     :: KC_IfElse        =  16  ! after IF (, ELSEIF (
    tInteger, PARAMETER     :: KC_Assign        =  17  ! after ASSIGN
    tInteger, PARAMETER     :: KC_Do            =  18  ! after DO
    tInteger, PARAMETER     :: KC_ProcAttb      =  19  ! within proc. attributes
    tInteger, PARAMETER     :: KC_Unknown       =  39  ! nothing known, but not keyword
    tInteger, PARAMETER     :: KC_Any           =  40  ! nothing known, may be keyword
    !
    !
    !  Codes for current statement parsing right context
    !
    tInteger, PARAMETER     :: KR_Unknown   =   0  ! nothing known, but not keyword
    tInteger, PARAMETER     :: KR_LastTok   =   1  ! last token
    tInteger, PARAMETER     :: KR_String    =   2  ! string
    tInteger, PARAMETER     :: KR_LParen    =   3  ! (
    tInteger, PARAMETER     :: KR_Any       =   4  ! nothing known, may be keyword
    !
    !  identifiers characteristics
    !
    tInteger, PARAMETER     :: NMaxIdf  = 8192 ! max # of identifiers
    tInteger, PARAMETER     :: LAvgName = 6    ! average length of names
    !
    !  Possible types
    !
    tInteger, PARAMETER     :: KW_Null =  0   ! empty
    !
    !  Fortran 90 keywords
    !
    tInteger, PARAMETER     :: KW_Command       =   1  ! pre-processor command
    tInteger, PARAMETER     :: KW_LogOp         =   2  ! logical operator (> 3rd token )
    tInteger, PARAMETER     :: KW_LogConst      =   3  ! logical constant (> 3rd token )
    tInteger, PARAMETER     :: KW_FmtItem       =   4  ! format item (> format (       )
    tInteger, PARAMETER     :: KW_IOKeyWrd      =   5  ! I/O keywrd  (> read (         )
    tInteger, PARAMETER     :: KW_TypeAttb      =   6  ! type attribute   (< name      )
    tInteger, PARAMETER     :: KW_AllocAct      =   7  ! allocation action (< (name)   )
    tInteger, PARAMETER     :: KW_GenericName   =   8  ! generic name (> interface     )
    tInteger, PARAMETER     :: KW_Procedure     =   9  ! procedure
    tInteger, PARAMETER     :: KW_Contain       =  10  ! CONTAINS
    tInteger, PARAMETER     :: KW_Default       =  11  ! DEFAULT
    tInteger, PARAMETER     :: KW_IntentAttb    =  12  ! Intent attribute
    tInteger, PARAMETER     :: KW_Assign        =  13  ! ASSIGN
    tInteger, PARAMETER     :: KW_Action2       =  14  ! action   (< [(]name[)]        )
    tInteger, PARAMETER     :: KW_Format        =  15  ! FORMAT
    tInteger, PARAMETER     :: KW_StringSpec    =  16  ! string spec. (< '...'         )
    tInteger, PARAMETER     :: KW_ToAssign      =  17  ! TO ( > assign)
    tInteger, PARAMETER     :: KW_Action3       =  18  ! action   (< name              )
    tInteger, PARAMETER     :: KW_Action4       =  19  ! action   ( alone              )
    tInteger, PARAMETER     :: KW_SelectCase    =  20  ! SELECT CASE
    tInteger, PARAMETER     :: KW_IOAct         =  21  ! i/o action (< (iolist)        )
    tInteger, PARAMETER     :: KW_Action5       =  22  ! action (< [name]              )
    tInteger, PARAMETER     :: KW_DecAct        =  23  ! declaration action (COMMON, ..)
    tInteger, PARAMETER     :: KW_ElseIf        =  24  ! ELSEIF
    tInteger, PARAMETER     :: KW_EndProc       =  25  ! END procedure
    tInteger, PARAMETER     :: KW_EndInterface  =  26  ! END INTERFACE
    tInteger, PARAMETER     :: KW_EndType       =  27  ! END TYPE
    tInteger, PARAMETER     :: KW_Function      =  28  ! FUNCTION
    tInteger, PARAMETER     :: KW_Hollerith     =  29  ! H
    tInteger, PARAMETER     :: KW_If            =  30  ! IF (
    tInteger, PARAMETER     :: KW_Implicit      =  31  ! IMPLICIT
    tInteger, PARAMETER     :: KW_Intent        =  32  ! INTENT
    tInteger, PARAMETER     :: KW_Interface     =  33  ! INTERFACE
    tInteger, PARAMETER     :: KW_None          =  34  ! NONE
    tInteger, PARAMETER     :: KW_Action6       =  35  ! action (< (name)              )
    tInteger, PARAMETER     :: KW_Use           =  36  ! USE
    tInteger, PARAMETER     :: KW_Only          =  37  ! ONLY
    tInteger, PARAMETER     :: KW_PPS           =  38  ! PRIVATE,PUBLIC,SEQUENCE
    tInteger, PARAMETER     :: KW_Result        =  39  ! RESULT
    tInteger, PARAMETER     :: KW_Stat          =  40  ! STAT
    tInteger, PARAMETER     :: KW_Then          =  41  ! THEN
    tInteger, PARAMETER     :: KW_Do            =  42  ! DO
    tInteger, PARAMETER     :: KW_While         =  43  ! WHILE
    tInteger, PARAMETER     :: KW_Else          =  44  ! ELSE
    tInteger, PARAMETER     :: KW_EndIf         =  45  ! END IF
    tInteger, PARAMETER     :: KW_EndDo         =  46  ! END DO
    tInteger, PARAMETER     :: KW_EndSelect     =  47  ! END SELECT
    tInteger, PARAMETER     :: KW_EndWhere      =  48  ! END WHERE
    tInteger, PARAMETER     :: KW_Where         =  49  ! WHERE
    tInteger, PARAMETER     :: KW_ElseWhere     =  50  ! ELSEWHERE
    tInteger, PARAMETER     :: KW_Case          =  51  ! CASE
    tInteger, PARAMETER     :: KW_Type          =  52  ! TYPE
    tInteger, PARAMETER     :: KW_ForAll        =  53  ! FORALL
    tInteger, PARAMETER     :: KW_EndForAll     =  54  ! END FORALL
    tInteger, PARAMETER     :: KW_GoTo          =  55  ! GOTO
    tInteger, PARAMETER     :: KW_ProcAttb      =  56  ! proc attribute   (< name      )
    tInteger, PARAMETER     :: KW_Data          =  57  ! DATA
    tInteger, PARAMETER     :: KW_System        = 255  ! last possible keyword
    !
    !  User-defined identifiers
    !
    tInteger, PARAMETER     :: KW_VarName   = 256  ! variable name
    tInteger, PARAMETER     :: KW_Intrinsic = 257  ! known intrinsic
    tInteger, PARAMETER     :: KW_Label     = 258  ! label ( ^num              )
    tInteger, PARAMETER     :: KW_BlkName   = 259  ! block name ( < :          )
    tInteger, PARAMETER     :: KW_DefName   = 260  ! defined name
    tInteger, PARAMETER     :: KW_External  = 261  ! external name (> procedure)
    tInteger, PARAMETER     :: KW_PreDefNum = 262  ! pre-defined numerical
    tInteger, PARAMETER     :: KW_PreDefStr = 263  ! pre-defined string
    !
    !   Macros
    !
    tInteger, PARAMETER     :: KW_Macro0 = 280                              ! macro name base
    tInteger, PARAMETER     :: KW_MacroT(0:NMaxArg) = [(KW_Macro0+Indx, &
                                                       Indx = 0, NMaxArg)]  ! Macro with i arguments
    !
    !  Fortran 90 statement types
    !
    tInteger, PARAMETER     :: KS_Unknown           =   0  ! nothing known
    tInteger, PARAMETER     :: KS_ProcStart         =   1  ! procedure start
    tInteger, PARAMETER     :: KS_ProcMid           =   2  ! procedure middle (contains)
    tInteger, PARAMETER     :: KS_ProcEnd           =   3  ! procedure end
    tInteger, PARAMETER     :: KS_IfStart           =   4  ! start block if
    tInteger, PARAMETER     :: KS_IfMid             =   5  ! middle block if (else)
    tInteger, PARAMETER     :: KS_IfEnd             =   6  ! end block if
    tInteger, PARAMETER     :: KS_WhereStart        =   7  ! start where
    tInteger, PARAMETER     :: KS_WhereMid          =   8  ! middle where
    tInteger, PARAMETER     :: KS_WhereEnd          =   9  ! end where
    tInteger, PARAMETER     :: KS_DoStart           =  10  ! start do
    tInteger, PARAMETER     :: KS_DoEnd             =  11  ! end do
    tInteger, PARAMETER     :: KS_InterfaceStart    =  12  ! start interface
    tInteger, PARAMETER     :: KS_InterfaceEnd      =  13  ! end interface
    tInteger, PARAMETER     :: KS_SelectStart       =  14  ! start select
    tInteger, PARAMETER     :: KS_SelectMid         =  15  ! middle select (case)
    tInteger, PARAMETER     :: KS_SelectEnd         =  16  ! end select
    tInteger, PARAMETER     :: KS_TypeStart         =  17  ! start type
    tInteger, PARAMETER     :: KS_TypeEnd           =  18  ! end type
    tInteger, PARAMETER     :: KS_PossibleIf        =  19  ! possible if
    tInteger, PARAMETER     :: KS_PPRCommand        =  20  ! pre-processor command
    tInteger, PARAMETER     :: KS_ForAllStart       =  21  ! start forall
    tInteger, PARAMETER     :: KS_ForAllEnd         =  22  ! end forall
    tInteger, PARAMETER     :: KS_Use               =  23  ! use
    tInteger, PARAMETER     :: KS_Execute           =  24  ! otherwise undefined executable
    tInteger, PARAMETER     :: KS_Declare           =  25  ! declarative statement
    tInteger, PARAMETER     :: KS_Any               =  26  ! data or format, appear anywhere
    tInteger, PARAMETER     :: KS_Implicit          =  27  ! IMPLICIT declaration

!** DERIVED TYPE DEFINITIONS
    !
    !  Type used for identifiers
    !
    TYPE NameType
        tInteger    :: INext        ! points to next identifier in chain
        tInteger    :: IRepC        ! points to replacement tokens chain
        tInteger    :: KeyVar       ! is it a keyword, a common name, a variable
        tInteger    :: IStartGbl    ! starting position in global chain
        tInteger    :: IEndGbl      ! ending position in global chain
        tInteger    :: IStartOut    ! starting position in output chain
        tInteger    :: IEndOut      ! ending position in output chain
    END TYPE NameType
    
!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! ------------------------------!
    !       Parser_Variables        !
    ! ------------------------------!
    !
    !  Parsing variables for f90ppr utility
    !
    !
    !   Pre-processing commands
    !
    tInteger                :: IW_Dollar
    tInteger                :: IW_Define
    tInteger                :: IW_ElseIf
    tInteger                :: IW_Else
    tInteger                :: IW_EndIf
    tInteger                :: IW_Eval
    tInteger                :: IW_If
    tInteger                :: IW_IfDef
    tInteger                :: IW_IfNDef
    tInteger                :: IW_Include
    tInteger                :: IW_Macro
    tInteger                :: IW_Undef
    !
    !  Global chain for identifiers
    !
    tCharLen(NMaxIdf*LAvgName)      :: ZV_NameGbl
    tInteger                        :: IV_NameGbl = 0
    !
    !  Global chain for output names of identifiers
    !
    tCharLen(NMaxIdf*LAvgName*2)    :: ZV_NameOut
    tInteger                        :: IV_NameOut = 0
    !
    !  Identifiers tables
    !
    TYPE(NameType)  :: TabName(1:NMaxIdf) = [(NameType(0, 0, 0, 0, 0, 0, 0), Indx = 1, NMaxIdf)]
    tInteger        :: IAlt = NMaxIdf + 1
    
    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!
MODULE SUBROUTINE Reducing_Statement (NTOK)
!
!  reduce lexed statement, to recognize constants, logical ops, ...
!
    IMPLICIT NONE
!
! arguments
    tInteger, INTENT (INOUT)    :: NTOK
!
! local variables
    tCharLen (1)    :: ZCHR
    tInteger        :: ITOK, ITOK1, ITOK2, INUMF, INUMF1, INUMF2
    tInteger        :: ITOKD, ITOKF, IHSH, INAMD, INAMF, NPAR, NPARL
!
! execution
!
!  Note that we always skip the first token, since it may not be any
!  of the clusters considered. This trick solves the problem of
!  label E01 [ = ..., for instance] which is not a floating point
!  constant, and saves time...
!
!  Look for logical operators and variables .xxxx.
!
    IF (INDEX(ZV_TokIdf(1:IV_End(NTOK)), '.') /= 0) THEN
        ITOK = 3
        DO
            ITOK = ITOK + 1
            IF (ITOK > NTOK) EXIT
            IF (KK_TokT(ITOK) == KK_Dot) THEN
                IF (KK_TokT(ITOK-2) == KK_Dot        .AND.              &
                    KK_TokT(ITOK-1) == KK_Identifier .AND.              &
                    VERIFY(ZV_TokIdf(IV_Start(ITOK-1):IV_End(ITOK-1)),  &
                           ZP_Lower//ZP_Upper) == 0) THEN
                    KK_TokT (ITOK-2) = KK_Identifier
                    IV_End (ITOK-2) = IV_End (ITOK)
                    ITOK2 = ITOK - 2
                    DO ITOK1 = ITOK + 1, NTOK
                        ITOK2 = ITOK2 + 1
                        KK_TokT (ITOK2) = KK_TokT (ITOK1)
                        IV_Start (ITOK2) = IV_Start (ITOK1)
                        IV_End (ITOK2) = IV_End (ITOK1)
                    END DO
                    NTOK = ITOK2
                END IF
            END IF
        END DO
!
!  Look for floating point constants #.#
!
        ITOK = 3
        DO
            ITOK = ITOK + 1
            IF (ITOK > NTOK) EXIT
            IF (KK_TokT(ITOK) == KK_IntNumVal) THEN
                IF (KK_TokT(ITOK-2) == KK_IntNumVal .AND. KK_TokT(ITOK-1) == KK_Dot) THEN
                    IV_End (ITOK-2) = IV_End (ITOK)
                    ITOK2 = ITOK - 2
                    KK_TokT (ITOK2) = KK_RealNumVal
                    DO ITOK1 = ITOK + 1, NTOK
                        ITOK2 = ITOK2 + 1
                        KK_TokT (ITOK2) = KK_TokT (ITOK1)
                        IV_Start (ITOK2) = IV_Start (ITOK1)
                        IV_End (ITOK2) = IV_End (ITOK1)
                    END DO
                    NTOK = ITOK2
                END IF
            END IF
        END DO
!
!  Look for floating point constants #.
!
        ITOK = 2
        DO
            ITOK = ITOK + 1
            IF (ITOK > NTOK) EXIT
            IF (KK_TokT(ITOK) == KK_Dot) THEN
                IF (KK_TokT(ITOK-1) == KK_IntNumVal) THEN
                    IV_End (ITOK-1) = IV_End (ITOK)
                    ITOK2 = ITOK - 1
                    KK_TokT (ITOK2) = KK_RealNumVal
                    DO ITOK1 = ITOK + 1, NTOK
                        ITOK2 = ITOK2 + 1
                        KK_TokT (ITOK2) = KK_TokT (ITOK1)
                        IV_Start (ITOK2) = IV_Start (ITOK1)
                        IV_End (ITOK2) = IV_End (ITOK1)
                    END DO
                    NTOK = ITOK2
                END IF
            END IF
        END DO
!
!  Look for floating point constants .#
!
        ITOK = 2
        DO
            ITOK = ITOK + 1
            IF (ITOK > NTOK) EXIT
            IF (KK_TokT(ITOK) == KK_IntNumVal) THEN
                IF (KK_TokT(ITOK-1) == KK_Dot) THEN
                    IV_End (ITOK-1) = IV_End (ITOK)
                    KK_TokT (ITOK-1) = KK_RealNumVal
                    ITOK2 = ITOK - 1
                    DO ITOK1 = ITOK + 1, NTOK
                        ITOK2 = ITOK2 + 1
                        KK_TokT (ITOK2) = KK_TokT (ITOK1)
                        IV_Start (ITOK2) = IV_Start (ITOK1)
                        IV_End (ITOK2) = IV_End (ITOK1)
                    END DO
                    NTOK = ITOK2
                END IF
            END IF
        END DO
    END IF
!
!  Look for exponent notation
!
    ITOK = 2
    DO
        ITOK = ITOK + 1
        IF (ITOK > NTOK) EXIT
        IF (KK_TokT(ITOK) == KK_Identifier .AND. (KK_TokT(ITOK-1) == KK_IntNumVal) &
                                            .OR. (KK_TokT(ITOK-1) == KK_RealNumVal)) THEN
!
!  The following forms are possible: [EeDd][+-]#
!                                    [EeDd]#
!                                    [Ee]#_x
!                                    _x (comming from ._x)
!
            ZCHR = ZV_TokIdf (IV_Start(ITOK) :IV_Start(ITOK))
            IF (INDEX("EeDd", ZCHR) /= 0) THEN
                ITOK1 = ITOK + 1
                IF (ITOK1 < NTOK) THEN
                    IF (IV_Start(ITOK) == IV_End(ITOK) .AND. KK_TokT(ITOK1) == KK_PlusMinus) THEN
                        ITOK1 = ITOK1 + 1
                        IF (KK_TokT(ITOK1) == KK_IntNumVal) THEN
!  [EeDd][+-]#
                            IV_End (ITOK-1) = IV_End (ITOK1)
                            KK_TokT (ITOK-1) = KK_RealNumVal
                            ITOK2 = ITOK - 1
                            DO ITOK1 = ITOK1 + 1, NTOK
                                ITOK2 = ITOK2 + 1
                                KK_TokT (ITOK2) = KK_TokT (ITOK1)
                                IV_Start (ITOK2) = IV_Start (ITOK1)
                                IV_End (ITOK2) = IV_End (ITOK1)
                            END DO
                            NTOK = ITOK2
                            CYCLE
                        END IF
                    END IF
                END IF
                IF (ITOK1 <= NTOK) THEN
                    IF (IV_Start(ITOK) == IV_End(ITOK) .AND. KK_TokT(ITOK1) == KK_IntNumVal) THEN
!  [EeDd]# (as 2 tokens)
                        IV_End (ITOK-1) = IV_End (ITOK1)
                        KK_TokT (ITOK-1) = KK_RealNumVal
                        ITOK2 = ITOK - 1
                        DO ITOK1 = ITOK1 + 1, NTOK
                            ITOK2 = ITOK2 + 1
                            KK_TokT (ITOK2) = KK_TokT (ITOK1)
                            IV_Start (ITOK2) = IV_Start (ITOK1)
                            IV_End (ITOK2) = IV_End (ITOK1)
                        END DO
                        NTOK = ITOK2
                        CYCLE
                    END IF
                END IF
                IF (IV_Start(ITOK) < IV_End(ITOK)) THEN
                    INUMF = VERIFY (ZV_TokIdf(IV_Start(ITOK)+1:IV_End(ITOK)), "0123456789")
                    IF (INUMF == 0) THEN
!  [EeDd]# (as a single identifier)
                        IV_End (ITOK-1) = IV_End (ITOK)
                        KK_TokT (ITOK-1) = KK_RealNumVal
                        ITOK2 = ITOK - 1
                        DO ITOK1 = ITOK + 1, NTOK
                            ITOK2 = ITOK2 + 1
                            KK_TokT (ITOK2) = KK_TokT (ITOK1)
                            IV_Start (ITOK2) = IV_Start (ITOK1)
                            IV_End (ITOK2) = IV_End (ITOK1)
                        END DO
                        NTOK = ITOK2
                    ELSE
                        INUMF1 = IV_Start (ITOK) + INUMF
                        IF (INUMF1 > IV_Start(ITOK)+1 .AND. ZV_TokIdf(INUMF1:INUMF1) == '_' &
                                                      .AND. INUMF1 < IV_End(ITOK)) THEN
!  [EeDd]#_x (Dd are not standard)
                            IV_End (ITOK-1) = INUMF1 - 1
                            KK_TokT (ITOK-1) = KK_RealNumVal
                            ITOK2 = NTOK + 2
                            DO ITOK1 = NTOK, ITOK, - 1
                                ITOK2 = ITOK2 - 1
                                KK_TokT (ITOK2) = KK_TokT (ITOK1)
                                IV_Start (ITOK2) = IV_Start (ITOK1)
                                IV_End (ITOK2) = IV_End (ITOK1)
                            END DO
                            IV_Start (ITOK) = INUMF1
                            IV_End (ITOK) = INUMF1
                            KK_TokT (ITOK) = KK_Kind
                            ITOK = ITOK + 1
                            IV_Start (ITOK) = INUMF1 + 1
                            INUMF2 = VERIFY (ZV_TokIdf(IV_Start(ITOK) :IV_End(ITOK)), "0123456789")
                            IF (INUMF2 == 0) THEN
                                KK_TokT (ITOK) = KK_IntNumVal
                            ELSE
                                KK_TokT (ITOK) = KK_Identifier
                            END IF
                            NTOK = NTOK + 1
                            ITOK = ITOK + 1
                        END IF
                    END IF
                END IF
            ELSE IF (ZCHR == '_' .AND. IV_Start(ITOK) < IV_End(ITOK)) THEN
!  _x
                ITOK2 = NTOK + 2
                DO ITOK1 = NTOK, ITOK, - 1
                    ITOK2 = ITOK2 - 1
                    KK_TokT (ITOK2) = KK_TokT (ITOK1)
                    IV_Start (ITOK2) = IV_Start (ITOK1)
                    IV_End (ITOK2) = IV_End (ITOK1)
                END DO
                IV_End (ITOK) = IV_Start (ITOK)
                KK_TokT (ITOK) = KK_Kind
                IV_Start (ITOK+1) = IV_End (ITOK) + 1
                ITOK = ITOK + 1
                INUMF2 = VERIFY (ZV_TokIdf(IV_Start(ITOK) :IV_End(ITOK)), "0123456789")
                IF (INUMF2 == 0) THEN
                    KK_TokT (ITOK) = KK_IntNumVal
                ELSE
                    KK_TokT (ITOK) = KK_Identifier
                END IF
                NTOK = NTOK + 1
                ITOK = ITOK + 1
            END IF
        END IF
    END DO
!
!  Remove embedded blanks in numerical constants
!
    ITOK = 2
    ITOK1 = 2
    DO
        ITOK = ITOK + 1
        IF (ITOK > NTOK) EXIT
        IF ((KK_TokT(ITOK) == KK_IntNumVal  .AND. KK_TokT(ITOK1) == KK_IntNumVal) .OR. &
            (KK_TokT(ITOK) == KK_RealNumVal .AND. KK_TokT(ITOK1) == KK_IntNumVal) .OR. &
            (KK_TokT(ITOK) == KK_IntNumVal .AND. KK_TokT(ITOK1) == KK_RealNumVal)) THEN
            IV_End (ITOK1) = IV_End (ITOK)
        ELSE
            ITOK1 = ITOK1 + 1
            IF (ITOK1 < ITOK) THEN
                KK_TokT (ITOK1) = KK_TokT (ITOK)
                IV_Start (ITOK1) = IV_Start (ITOK)
                IV_End (ITOK1) = IV_End (ITOK)
            END IF
        END IF
    END DO
    IF (ITOK1 < NTOK) NTOK = ITOK1
!
!  find :: (must be outside of parentheses)
!
    ITOK = 2
    NPARL = 0
    DO
        ITOK = ITOK + 1
        IF (ITOK > NTOK) EXIT
        IF (KK_TokT(ITOK) == KK_LParen) THEN
            NPARL = NPARL + 1
            CYCLE
        ELSE IF (KK_TokT(ITOK) == KK_RParen) THEN
            NPARL = NPARL - 1
            CYCLE
        ELSE IF (NPARL > 0) THEN
            CYCLE
        END IF
        IF (KK_TokT(ITOK) == KK_Colon .AND. KK_TokT(ITOK-1) == KK_Colon) THEN
            IV_End (ITOK-1) = IV_End (ITOK)
            KK_TokT (ITOK-1) = KK_DblColon
            DO ITOK1 = ITOK + 1, NTOK
                KK_TokT (ITOK1-1) = KK_TokT (ITOK1)
                IV_Start (ITOK1-1) = IV_Start (ITOK1)
                IV_End (ITOK1-1) = IV_End (ITOK1)
            END DO
            NTOK = NTOK - 1
            EXIT! (There should only be one :: per instruction)
        END IF
    END DO
!
!  Now, look for double word keywords
!  (Here, we start again from the first token)
!
    ITOK = 1
    ITOK1 = 1
    EXPL: DO
        ITOK = ITOK + 1
        IF (ITOK > NTOK) EXIT EXPL
        IF (KK_TokT(ITOK) == KK_Identifier .AND. KK_TokT(ITOK1) == KK_Identifier) THEN
            ITOKD = IV_Start (ITOK1)
            ITOKF = IV_End (ITOK)
            IHSH = Hashing_String (ZV_TokIdf(ITOKD:ITOKF))
            IF (TabName(IHSH)%KeyVar /= 0 .AND. TabName(IHSH)%KeyVar <= KW_System) THEN
                DO
                    INAMD = TabName(IHSH)%IStartGbl
                    INAMF = TabName(IHSH)%IEndGbl
                    IF (Is_Same_String(ZV_TokIdf(ITOKD:ITOKF), ZV_NameGbl(INAMD:INAMF))) THEN
                        IV_End (ITOK1) = IV_End (ITOK)
                        DO ITOK2 = ITOK + 1, NTOK
                            KK_TokT (ITOK2-1) = KK_TokT (ITOK2)
                            IV_Start (ITOK2-1) = IV_Start (ITOK2)
                            IV_End (ITOK2-1) = IV_End (ITOK2)
                        END DO
                        ITOK = ITOK - 1
                        NTOK = NTOK - 1
                        CYCLE EXPL
                    END IF
                    IF (TabName(IHSH)%INext /= 0) THEN
                        IHSH = TabName(IHSH)%INext
                    ELSE
                        EXIT
                    END IF
                END DO
            END IF
        END IF
        ITOK1 = ITOK1 + 1
    END DO EXPL
!
!  Look for parentheses within defined types
!
    ITOK = 4
    DO
        ITOK = ITOK + 1
        IF (ITOK > NTOK) EXIT
        IF (KK_TokT(ITOK) == KK_Percent) THEN
            IF (KK_TokT(ITOK-1) == KK_RParen) THEN
                NPAR = 1
                DO ITOK1 = ITOK - 3, 2, - 1
                    IF (KK_TokT(ITOK1) == KK_RParen) NPAR = NPAR + 1
                    IF (KK_TokT(ITOK1) == KK_LParen .OR. KK_TokT(ITOK1) == KK_OPeren) THEN
                        NPAR = NPAR - 1
                        IF (NPAR == 0) THEN
                            KK_TokT (ITOK1) = KK_OPeren
                            EXIT
                        END IF
                    END IF
                END DO
            END IF
            IF ((ITOK+2) < NTOK) THEN
                IF (KK_TokT(ITOK+2) == KK_LParen) KK_TokT (ITOK+2) = KK_OPeren
            END IF
            ITOK = ITOK + 1
        END IF
    END DO
    RETURN
END SUBROUTINE Reducing_Statement
!
!******************************************************************************
!
MODULE SUBROUTINE Parsing_Statement (NTOK, KSSTT)
!
!  Parse statement (partially)
!
    IMPLICIT NONE
!
! arguments
    tInteger, INTENT (INOUT)    :: NTOK
    tInteger, INTENT (OUT)      :: KSSTT
!
! local variables
    tInteger, SAVE  :: KCTXC = KC_BeginUnknown
    tInteger        :: ITOKS, KCTOK, ITOKD, ITOKF, KWIDF, IFDOS
    tInteger        :: ITOK, KRTOK, IFDOE, ILAB, LPAR
!
    ITOKS = 1
    KCTOK = KCTXC
    KSSTT = KS_Unknown
!
!  Reduce "token clusters"
!
    IF (NTOK > ITOKS) THEN
        CALL Reducing_Statement (NTOK)
    END IF
!
!  process $commands
!
    IF (KK_TokT(ITOKS) == KK_Command) THEN
        ITOKD = IV_Start (ITOKS)
        ITOKF = IV_End (ITOKS)
        CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_BeginUnknown, KR_Unknown, &
                                 KWIDF, IV_Name(ITOKS))
        CALL Processing_Command (IV_Name(ITOKS), NTOK)
        KSSTT = KS_PPRCommand
        RETURN
    END IF
!
!  Are-we skipping ?
!
    IF (IfSkip /= 0) THEN
        RETURN
    END IF
!
!  Label ?
!
    IFDOS = 0
    IFDOE = 0
    IF (KK_TokT(ITOKS) == KK_IntNumVal) THEN
        ITOKD = IV_Start (ITOKS)
        ITOKF = IV_End (ITOKS)
        IF ((ITOKF-ITOKD+1) <= LMax_LoopLab) THEN
            KK_TokT (ITOKS) = KK_Label
            KCTOK = KC_BeginLabStm
            ITOKS = ITOKS + 1
            N_EndLoop = 0
            DO ILAB = N_Label, 1, - 1
                IF (ZV_TokIdf(ITOKD:ITOKF) == TRIM(ZV_EndLoop(ILAB) (1:LMax_LoopLab))) THEN
                    N_EndLoop = N_EndLoop + 1
                    IFDOE = 1
                    ZV_EndLoop (ILAB) (1:LMax_LoopLab) = ZV_EndLoop (N_Label) (1:LMax_LoopLab)
                    N_Label = N_Label - 1
                ELSE
                    EXIT
                END IF
            END DO
        END IF
    END IF
!
!  Block name ?
!
    IF (ITOKS+1 < NTOK) THEN
        IF (KK_TokT(ITOKS+1) == KK_Colon .AND. KK_TokT(ITOKS) == KK_Identifier) THEN
            KK_TokT (ITOKS) = KK_BlkName
            KCTOK = KC_BeginNamedBlk
            ITOKS = ITOKS + 2
        END IF
    END IF
!
!  Identify some keywords
!
    LPAR = 0
    DO ITOK = ITOKS, NTOK
        IF (KK_TokT(ITOK) == KK_Identifier) THEN
            ITOKD = IV_Start (ITOK)
            ITOKF = IV_End (ITOK)
            KRTOK = KR_Unknown
            IF (ITOK == NTOK) THEN
                KRTOK = KR_LastTok
            ELSE
                SELECT CASE (KK_TokT(ITOK+1))
                CASE (KK_ChrStr)
                    KRTOK = KR_String
                CASE (KK_LParen, KK_OPeren)
                    KRTOK = KR_LParen
                END SELECT
            END IF
            CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KCTOK, KRTOK, KWIDF, IV_Name(ITOK))
            IF (ITOK < NTOK) THEN
                SELECT CASE (KWIDF)
                CASE (KW_TypeAttb)
                    KCTOK = KC_DecAttb
                CASE (KW_ProcAttb)
                    KCTOK = KC_ProcAttb
                CASE (KW_AllocAct)
                    KCTOK = KC_Alloc
                CASE (KW_Procedure, KW_Function)
                    KCTOK = KC_EntryFunc
                CASE (KW_Assign)
                    IF (KK_TokT(ITOK+1) == KK_IntNumVal) KK_TokT (ITOK+1) = KK_Label
                    KCTOK = KC_Assign
                CASE (KW_Format)
                    KCTOK = KC_Format
                CASE (KW_IOAct)
                    KCTOK = KC_IOStm
                CASE (KW_ElseIf, KW_If)
                    KCTOK = KC_IfElse
                CASE (KW_Implicit)
                    KCTOK = KC_Implicit
                CASE (KW_Intent)
                    KCTOK = KC_Intent
                CASE (KW_Interface)
                    KCTOK = KC_Interface
                CASE (KW_Use)
                    KCTOK = KC_Use
                CASE (KW_Do)
                    KCTOK = KC_Do
                CASE (KW_Case)
                    KCTOK = KC_Case
                CASE (KW_GoTo)
                    IF (KK_TokT(ITOK+1) == KK_IntNumVal) KK_TokT (ITOK+1) = KK_Label
                    IF (KCTOK == KC_BeginUnknown .OR. KCTOK == KC_BeginExecute .OR. &
                        KCTOK == KC_BeginLabStm  .OR. KCTOK == KC_BeginNamedBlk) THEN
                        KCTOK = KC_Executable
                    END IF
                CASE DEFAULT
                    IF (KCTOK == KC_BeginUnknown .OR. KCTOK == KC_BeginExecute .OR. &
                        KCTOK == KC_BeginLabStm  .OR. KCTOK == KC_BeginNamedBlk) THEN
                        KCTOK = KC_Executable
                    END IF
                END SELECT
            END IF
            SELECT CASE (KWIDF)
            CASE (KW_Procedure, KW_Function)
                KSSTT = KS_ProcStart
            CASE (KW_Contain)
                KSSTT = KS_ProcMid
            CASE (KW_EndProc)
                KSSTT = KS_ProcEnd
            CASE (KW_If)
                KSSTT = KS_PossibleIf
            CASE (KW_Then)
                IF (KSSTT == KS_PossibleIf) THEN
                    KSSTT = KS_IfStart
                END IF
            CASE (KW_Else, KW_ElseIf)
                KSSTT = KS_IfMid
            CASE (KW_EndIf)
                KSSTT = KS_IfEnd
            CASE (KW_Where)
                KSSTT = KS_WhereStart
            CASE (KW_ElseWhere)
                KSSTT = KS_WhereMid
            CASE (KW_EndWhere)
                KSSTT = KS_WhereEnd
            CASE (KW_Do)
                KSSTT = KS_DoStart
            CASE (KW_EndDo)
                KSSTT = KS_DoEnd
                N_EndLoop = 1
            CASE (KW_Interface)
                KSSTT = KS_InterfaceStart
            CASE (KW_EndInterface)
                KSSTT = KS_InterfaceEnd
            CASE (KW_SelectCase)
                KSSTT = KS_SelectStart
            CASE (KW_Case)
                KSSTT = KS_SelectMid
            CASE (KW_EndSelect)
                KSSTT = KS_SelectEnd
            CASE (KW_Type)
                KSSTT = KS_TypeStart
            CASE (KW_EndType)
                KSSTT = KS_TypeEnd
            CASE (KW_ForAll)
                KSSTT = KS_ForAllStart
            CASE (KW_EndForAll)
                KSSTT = KS_ForAllEnd
            END SELECT
        ELSE IF (KK_TokT(ITOK) == KK_LParen .OR. KK_TokT(ITOK) == KK_OPeren) THEN
            LPAR = LPAR + 1
        ELSE IF (KK_TokT(ITOK) == KK_RParen) THEN
            IF (ITOK >=NTOK) EXIT
            LPAR = LPAR - 1
            IF (LPAR == 0) THEN
                SELECT CASE (KCTOK)
                CASE DEFAULT
                    CONTINUE
                CASE (KC_Intent)
                    KCTOK = KC_DecAttb
                CASE (KC_IOStm)
                    KCTOK = KC_Executable
                END SELECT
!
!  non-block where and forall
!
                IF ((KSSTT == KS_WhereStart .OR. KSSTT == KS_ForAllStart) .AND. &
                    (KK_TokT(ITOK+1) /= KK_Comment .AND. KK_TokT(ITOK+1) /= KK_EmbCmt)) THEN
                    KSSTT = KS_Unknown
                END IF
            END IF
!
!  Labels and DO loops
!
        ELSE IF (KK_TokT(ITOK) == KK_Assign) THEN
            IF (KSSTT == KS_DoStart .AND. IFDOS == 0) THEN
                IFDOS = 1
            END IF
        ELSE IF (KK_TokT(ITOK) == KK_IntNumVal) THEN
            IF (KSSTT == KS_DoStart .AND. IFDOS == 0) THEN
                IFDOS = 1
                ITOKD = IV_Start (ITOK)
                ITOKF = IV_End (ITOK)
                IF ((ITOKF-ITOKD+1) <= LMax_LoopLab) THEN
                    KK_TokT (ITOK) = KK_Label
                    IF (N_Label >= NMax_NestLab) THEN
                        CALL Outputing_Error ("DO loops too deeply nested")
                    ELSE
                        N_Label = N_Label + 1
                        ZV_EndLoop (N_Label) (1:LMax_LoopLab) = ZV_TokIdf (ITOKD:ITOKF)
                    END IF
                END IF
            END IF
        END IF
    END DO
    IF (IFDOE /= 0) KSSTT = KS_DoEnd
    RETURN
END SUBROUTINE Parsing_Statement
!
!******************************************************************************
!
MODULE SUBROUTINE Initializing_Names
!
!  Initialise the names for f90ppr
!
    IMPLICIT NONE
!
! execution
!
    ZV_NameGbl (     1:  1259) = "&
        &$$DEFINE$ELIF$ELSE$ENDIF$EVAL$IF$IFDEF$IFNDEF$INCLUDE$MACRO$UNDEF.AND..EQ..EQV..FALSE..GE..GT..LE..LT..NE..NEQV..NOT..OR..TRUE.ABS&
        &ACOSASINATANATAN2ACCESSACTIONADVANCEALLOCATABLEALLOCATEASSIGNASSIGNMENTBACKSPACEBLANKBLOCKDATACALLCASECHARACTERCLOSECOMMONCOMPLEXC&
        &ONTAINSCONTINUECOSCOSHCYCLEDATADEALLOCATEDEFAULTDELIMDIMENSIONDIRECTDODOUBLEPRECISIONELEMENTALELSEELSEIFELSEWHEREENDENDBLOCKDATAEN&
        &DDOENDFILEENDFORALLENDFUNCTIONENDIFENDINTERFACEENDMODULEENDPROGRAMENDSELECTENDSUBROUTINEENDTYPEENDWHEREENTRYEOREQUIVALENCEERREXIST&
        &EXITEXPEXTERNALFILEFMTFORALLFORMFORMATFORMATTEDFUNCTIONGOTOHIFIMPLICITININCLUDEINOUTINQUIREINTINTEGERINTENTINTERFACEINTRINSICIOLEN&
        &GTHIOSTATKINDLENLOGLOG10LOGICALMAXMINMODMODULEMODULEPROCEDURENAMENAMEDNAMELISTNEXTRECNINTNMLNONENULLIFYNUMBEROONLYOPENOPENEDOPERAT&
        &OROPTIONALOUTPADPARAMETERPAUSEPOINTERPOSITIONPRINTPRIVATEPROCEDUREPROGRAMPUBLICPUREREADREADWRITEREALRECRECLRECURSIVERESULTRETURNRE&
        &WINDSAVESINSINHSELECTCASESELECTED_INT_KINDSELECTED_REAL_KINDSEQUENCESEQUENTIALSIZESQRTSTATSTATUSSTOPSUBROUTINETANTANHTARGETTHENTOT&
        &YPETYPEUNFORMATTEDUNITUSEWHEREWHILEWRITEFPPR_LEAVEFPPR_LOWERFPPR_UPPERFPPR_FALSE_CMTFPPR_KWD_CASEFPPR_USR_CASEFPPR_FXD_INFPPR_FXD_&
        &OUTFPPR_USE_SHARPFPPR_MAX_LINEFPPR_PGM_INDENTFPPR_STP_INDENTFPPR_NMBR_LINESFPPR_BLANK_CMT"
    IV_NameGbl =   1259
!
    ZV_NameOut (     1:  1278) = "&
        &$$Define$ElIf$Else$EndIf$Eval$If$IfDef$IfnDef$Include$Macro$UnDef.And..Eq..Eqv..False..Ge..Gt..Le..Lt..Ne..Neqv..Not..Or..True.Abs&
        &AcosAsinAtanAtan2AccessActionAdvanceAllocatableAllocateAssignAssignmentBackspaceBlankBlock DataCallCaseCharacterCloseCommonComplex&
        &ContainsContinueCosCoshCycleDataDeallocateDefaultDelimDimensionDirectDoDouble PrecisionElementalElseElse IfElsewhereEndEnd Block D&
        &ataEnd DoEndfileEnd ForallEnd FunctionEnd IfEnd InterfaceEnd ModuleEnd ProgramEnd SelectEnd SubroutineEnd TypeEnd WhereEntryEorEqu&
        &ivalenceErrExistExitExpExternalFileFmtForallFormFormatFormattedFunctionGo ToHIfImplicitInIncludeInoutInquireIntIntegerIntentInterf&
        &aceIntrinsicIoLengthIoStatKindLenLogLog10LogicalMaxMinModModuleModule ProcedureNameNamedNamelistNextRecNintNmlNoneNullifyNumberOOn&
        &lyOpenOpenedOperatorOptionalOutPadParameterPausePointerPositionPrintPrivateProcedureProgramPublicPureReadReadWriteRealRecReclRecur&
        &siveResultReturnRewindSaveSinSinhSelect CaseSelected_Int_KindSelected_Real_KindSequenceSequentialSizeSqrtStatStatusStopSubroutineT&
        &anTanhTargetThenToTypeTypeUnformattedUnitUseWhereWhileWriteFPPR_leaveFPPR_lowerFPPR_upperFPPR_false_cmtFPPR_kwd_caseFPPR_usr_caseF&
        &PPR_fxd_inFPPR_fxd_outFPPR_use_sharpFPPR_max_lineFPPR_pgm_indentFPPR_stp_indentFPPR_nmbr_linesFPPR_blank_cmt"
!
    IV_NameOut =   1278
!
    TabName (    5) = NameType (    0,    0,   42,  1071,  1075,  1090,  1094)
    TabName (   28) = NameType (    0,    0,   29,   580,   580,   597,   597)
    TabName (   35) = NameType (    0,    0,   16,   760,   760,   778,   778)
    TabName (   48) = NameType (    0,    0,    6,   637,   645,   654,   662)
    TabName (   74) = NameType (    0,    0,   56,   860,   863,   878,   881)
    TabName (   92) = NameType (    0,    0,   50,   365,   373,   368,   376)
    TabName (  164) = NameType (    0,    0,  263,  1162,  1173,  1181,  1192)
    TabName (  179) = NameType (    0,    0,   21,  1076,  1080,  1095,  1099)
    TabName (  190) = NameType (    0,    0,    2,    71,    74,    71,    74)
    TabName (  228) = NameType (    0,    0,   14,   202,   210,   202,   210)
    TabName (  274) = NameType (    0,    0,    5,   818,   825,   836,   843)
    TabName (  276) = NameType (    0,    0,    1,    25,    29,    25,    29)
    TabName (  292) = NameType (    0,    0,   14,   909,   914,   927,   932)
    TabName (  325) = NameType (    0,    0,   20,   926,   935,   944,   954)
    TabName (  393) = NameType ( 8191,    0,   52,  1040,  1043,  1059,  1062)
    TabName (  426) = NameType (    0,    0,   42,   329,   330,   330,   331)
    TabName (  433) = NameType (    0,    0,    5,   716,   720,   734,   738)
    TabName (  454) = NameType (    0,    0,   39,   897,   902,   915,   920)
    TabName (  458) = NameType (    0,    0,   22,   826,   830,   844,   848)
    TabName (  467) = NameType (    0,    0,    6,   797,   805,   815,   823)
    TabName (  495) = NameType (    0,    0,    1,    46,    53,    46,    53)
    TabName (  502) = NameType (    0,    0,   30,   581,   582,   598,   599)
    TabName (  510) = NameType (    0,    0,   12,   591,   592,   608,   609)
    TabName (  511) = NameType (    0,    0,  257,   612,   614,   629,   631)
    TabName (  557) = NameType (    0,    0,    5,   536,   539,   552,   555)
    TabName (  561) = NameType (    0,    0,    5,   646,   653,   663,   670)
    TabName (  564) = NameType (    0,    0,    2,    87,    90,    87,    90)
    TabName (  568) = NameType (    0,    0,    5,   769,   774,   787,   792)
    TabName (  577) = NameType (    0,    0,   22,   593,   599,   610,   616)
    TabName (  597) = NameType (    0,    0,    5,   979,   988,   998,  1007)
    TabName (  609) = NameType (    0,    0,    6,   660,   663,   677,   680)
    TabName (  651) = NameType (    0,    0,  263,  1231,  1245,  1250,  1264)
    TabName (  698) = NameType (    0,    0,   17,  1038,  1039,  1057,  1058)
    TabName (  719) = NameType (    0,    0,   34,   743,   746,   761,   764)
    TabName (  721) = NameType (    0,    0,   44,   355,   358,   357,   360)
    TabName (  743) = NameType (    0,    0,    6,   314,   322,   315,   323)
    TabName (  747) = NameType (    0,    0,    5,   712,   715,   730,   733)
    TabName (  751) = NameType (    0,    0,   25,   466,   478,   479,   492)
    TabName (  758) = NameType (    0,    0,    6,   233,   241,   234,   242)
    TabName (  767) = NameType (    0,    0,    6,   811,   817,   829,   835)
    TabName (  797) = NameType (    0,    0,    8,   775,   782,   793,   800)
    TabName (  819) = NameType (    0,    0,    2,    91,    94,    91,    94)
    TabName (  822) = NameType (    0,    0,    1,    39,    45,    39,    45)
    TabName (  824) = NameType (    0,    0,   57,   288,   291,   289,   292)
    TabName (  834) = NameType (    0,    0,    3,    80,    86,    80,    86)
    TabName (  852) = NameType (    0,    0,  262,  1091,  1100,  1110,  1119)
    TabName (  895) = NameType (    0,    0,   21,   864,   867,   882,   885)
    TabName (  903) = NameType (    0,    0,    6,   877,   880,   895,   898)
    TabName (  919) = NameType (    0,    0,   38,   915,   918,   933,   936)
    TabName (  924) = NameType (    0,    0,    1,     2,     8,     2,     8)
    TabName (  937) = NameType (    0,    0,    5,   884,   887,   902,   905)
    TabName (  948) = NameType (    0,    0,    5,   868,   876,   886,   894)
    TabName (  966) = NameType (    0,    0,   25,   410,   420,   417,   428)
    TabName ( 1050) = NameType (    0,    0,    6,   528,   535,   544,   551)
    TabName ( 1128) = NameType ( 8190,    0,    5,   559,   567,   575,   583)
    TabName ( 1219) = NameType (    0,    0,    6,   664,   666,   681,   683)
    TabName ( 1238) = NameType (    0,    0,   37,   761,   764,   779,   782)
    TabName ( 1240) = NameType (    0,    0,    8,   192,   201,   192,   201)
    TabName ( 1293) = NameType (    0,    0,   32,   622,   627,   639,   644)
    TabName ( 1317) = NameType (    0,    0,    9,   847,   853,   865,   871)
    TabName ( 1326) = NameType (    0,    0,   19,   268,   275,   269,   276)
    TabName ( 1333) = NameType (    0,    0,    2,   107,   112,   107,   112)
    TabName ( 1372) = NameType (    0,    0,   22,   903,   908,   921,   926)
    TabName ( 1382) = NameType ( 8192,    0,   10,   260,   267,   261,   268)
    TabName ( 1385) = NameType (    0,    0,   46,   389,   393,   394,   399)
    TabName ( 1389) = NameType (    0,    0,    5,   160,   166,   160,   166)
    TabName ( 1399) = NameType (    0,    0,  257,   993,   996,  1012,  1015)
    TabName ( 1413) = NameType (    0,    0,   35,   747,   753,   765,   771)
    TabName ( 1426) = NameType (    0,    0,   47,   457,   465,   469,   478)
    TabName ( 1450) = NameType (    0,    0,  257,   682,   684,   699,   701)
    TabName ( 1461) = NameType (    0,    0,   45,   421,   425,   429,   434)
    TabName ( 1496) = NameType (    0,    0,    6,  1028,  1033,  1047,  1052)
    TabName ( 1543) = NameType (    0,    0,    1,    60,    65,    60,    65)
    TabName ( 1566) = NameType (    0,    0,    9,   216,   224,   216,   225)
    TabName ( 1576) = NameType (    0,    0,  257,   685,   687,   702,   704)
    TabName ( 1607) = NameType (    0,    0,    1,     9,    13,     9,    13)
    TabName ( 1608) = NameType (    0,    0,  257,  1024,  1027,  1043,  1046)
    TabName ( 1667) = NameType (    0,    0,   25,   377,   388,   380,   393)
    TabName ( 1668) = NameType (    0,    0,  257,   688,   690,   705,   707)
    TabName ( 1682) = NameType (    0,    0,   28,   568,   575,   584,   591)
    TabName ( 1686) = NameType (    0,    0,   21,   765,   768,   783,   786)
    TabName ( 1736) = NameType (    0,    0,    1,    54,    59,    54,    59)
    TabName ( 1750) = NameType (    0,    0,    6,   697,   711,   714,   729)
    TabName ( 1776) = NameType (    0,    0,    1,    14,    18,    14,    18)
    TabName ( 1777) = NameType (    0,    0,   48,   486,   493,   501,   509)
    TabName ( 1830) = NameType (    0,    0,    5,   323,   328,   324,   329)
    TabName ( 1931) = NameType (    0,    0,    5,   740,   742,   758,   760)
    TabName ( 1937) = NameType (    0,    0,    5,   729,   735,   747,   753)
    TabName ( 1956) = NameType (    0,    0,    6,   675,   681,   692,   698)
    TabName ( 1964) = NameType (    0,    0,    5,   154,   159,   154,   159)
    TabName ( 1974) = NameType (    0,    0,   23,   247,   252,   248,   253)
    TabName ( 1977) = NameType (    0,    0,   40,   997,  1000,  1016,  1019)
    TabName ( 1992) = NameType (    0,    0,    1,    30,    32,    30,    32)
    TabName ( 2009) = NameType (    0,    0,    2,    95,    98,    95,    98)
    TabName ( 2023) = NameType (    0,    0,    5,  1059,  1062,  1078,  1081)
    TabName ( 2031) = NameType (    0,    0,    6,   331,   345,   332,   347)
    TabName ( 2085) = NameType (    0,    0,  257,   128,   130,   128,   130)
    TabName ( 2119) = NameType (    0,    0,    5,   211,   215,   211,   215)
    TabName ( 2166) = NameType (    0,    0,   18,   494,   498,   510,   514)
    TabName ( 2195) = NameType (    0,    0,    5,   654,   659,   671,   676)
    TabName ( 2211) = NameType (    0,    0,   22,  1007,  1010,  1026,  1029)
    TabName ( 2254) = NameType (    0,    0,  263,  1174,  1187,  1193,  1206)
    TabName ( 2264) = NameType (    0,    0,    2,    99,   102,    99,   102)
    TabName ( 2268) = NameType (    0,    0,   23,   721,   728,   739,   746)
    TabName ( 2285) = NameType (    0,    0,   38,   854,   859,   872,   877)
    TabName ( 2287) = NameType (    0,    0,  257,   936,   952,   955,   971)
    TabName ( 2295) = NameType (    0,    0,   22,   806,   810,   824,   828)
    TabName ( 2297) = NameType (    0,    0,    5,   794,   796,   812,   814)
    TabName ( 2303) = NameType (    0,    0,    6,   253,   259,   254,   260)
    TabName ( 2311) = NameType (    0,    0,   23,   502,   512,   518,   528)
    TabName ( 2353) = NameType (    0,    0,   54,   401,   409,   407,   416)
    TabName ( 2354) = NameType (    0,    0,  257,   670,   674,   687,   691)
    TabName ( 2364) = NameType (    0,    0,   12,   791,   793,   809,   811)
    TabName ( 2401) = NameType (    0,    0,    5,   549,   552,   565,   568)
    TabName ( 2421) = NameType (    0,    0,  263,  1125,  1137,  1144,  1156)
    TabName ( 2499) = NameType (    0,    0,    2,   113,   117,   113,   117)
    TabName ( 2515) = NameType (    0,    0,    8,   838,   846,   856,   864)
    TabName ( 2587) = NameType (    0,    0,    2,   103,   106,   103,   106)
    TabName ( 2601) = NameType (    0,    0,   15,   553,   558,   569,   574)
    TabName ( 2637) = NameType (    0,    0,    3,   122,   127,   122,   127)
    TabName ( 2669) = NameType (    0,    0,   25,   438,   446,   448,   457)
    TabName ( 2687) = NameType (    0,    0,  262,  1101,  1110,  1120,  1129)
    TabName ( 2718) = NameType (    0,    0,   31,   583,   590,   600,   607)
    TabName ( 2771) = NameType (    0,    0,   22,   283,   287,   284,   288)
    TabName ( 2847) = NameType (    0,    0,  263,  1138,  1150,  1157,  1169)
    TabName ( 2863) = NameType (    0,    0,    5,   309,   313,   310,   314)
    TabName ( 2878) = NameType (    0,    0,  257,   143,   147,   143,   147)
    TabName ( 2884) = NameType (    0,    0,  257,   276,   278,   277,   279)
    TabName ( 2914) = NameType (    0,    0,  262,  1081,  1090,  1100,  1109)
    TabName ( 2934) = NameType (    0,    0,   38,   831,   837,   849,   855)
    TabName ( 2942) = NameType (    0,    0,    5,   881,   883,   899,   901)
    TabName ( 2960) = NameType (    0,    0,  257,   131,   134,   131,   134)
    TabName ( 3032) = NameType (    0,    0,    1,    19,    24,    19,    24)
    TabName ( 3040) = NameType (    0,    0,    5,  1048,  1058,  1067,  1077)
    TabName ( 3042) = NameType (    0,    0,   49,  1066,  1070,  1085,  1089)
    TabName ( 3053) = NameType (    0,    0,    6,   167,   177,   167,   177)
    TabName ( 3061) = NameType (    0,    0,  263,  1246,  1259,  1265,  1278)
    TabName ( 3091) = NameType (    0,    0,  257,   736,   739,   754,   757)
    TabName ( 3097) = NameType (    0,    0,    2,   118,   121,   118,   121)
    TabName ( 3098) = NameType (    0,    0,  257,   922,   925,   940,   943)
    TabName ( 3125) = NameType (    0,    0,    9,  1011,  1020,  1030,  1039)
    TabName ( 3128) = NameType (    0,    0,    5,   516,   520,   532,   536)
    TabName ( 3134) = NameType (    0,    0,    5,  1001,  1006,  1020,  1025)
    TabName ( 3192) = NameType (    0,    0,    1,    33,    38,    33,    38)
    TabName ( 3197) = NameType (    0,    0,   11,   302,   308,   303,   309)
    TabName ( 3259) = NameType (    0,    0,   55,   576,   579,   592,   596)
    TabName ( 3271) = NameType (    0,    0,    5,   148,   153,   148,   153)
    TabName ( 3299) = NameType (    0,    0,    5,   989,   992,  1008,  1011)
    TabName ( 3309) = NameType (    0,    0,    6,   783,   790,   801,   808)
    TabName ( 3310) = NameType (    0,    0,  257,   919,   921,   937,   939)
    TabName ( 3360) = NameType (    0,    0,    6,   615,   621,   632,   638)
    TabName ( 3373) = NameType (    0,    0,  257,   953,   970,   972,   989)
    TabName ( 3386) = NameType (    0,    0,  257,   135,   138,   135,   138)
    TabName ( 3403) = NameType (    0,    0,   53,   543,   548,   559,   564)
    TabName ( 3430) = NameType (    0,    0,   25,   374,   376,   377,   379)
    TabName ( 3447) = NameType (    0,    0,   27,   479,   485,   493,   500)
    TabName ( 3461) = NameType (    0,    0,    5,   499,   501,   515,   517)
    TabName ( 3463) = NameType (    0,    0,  257,  1021,  1023,  1040,  1042)
    TabName ( 3480) = NameType (    0,    0,   12,   600,   604,   617,   621)
    TabName ( 3484) = NameType (    0,    0,   41,  1034,  1037,  1053,  1056)
    TabName ( 3488) = NameType (    0,    0,   56,   888,   896,   906,   914)
    TabName ( 3497) = NameType (    0,    0,  263,  1111,  1124,  1130,  1143)
    TabName ( 3501) = NameType (    0,    0,  263,  1216,  1230,  1235,  1249)
    TabName ( 3509) = NameType (    0,    0,  263,  1151,  1161,  1170,  1180)
    TabName ( 3512) = NameType (    0,    0,    5,   513,   515,   529,   531)
    TabName ( 3525) = NameType (    0,    0,    2,    66,    70,    66,    70)
    TabName ( 3535) = NameType (    0,    0,    7,   292,   301,   293,   302)
    TabName ( 3539) = NameType (    0,    0,  257,   139,   142,   139,   142)
    TabName ( 3561) = NameType (    0,    0,  263,  1201,  1215,  1220,  1234)
    TabName ( 3611) = NameType (    0,    0,   14,   394,   400,   400,   406)
    TabName ( 3612) = NameType (    0,    0,  257,   525,   527,   541,   543)
    TabName ( 3676) = NameType (    0,    0,   13,   186,   191,   186,   191)
    TabName ( 3718) = NameType (    0,    0,    5,   540,   542,   556,   558)
    TabName ( 3719) = NameType (    0,    0,    9,   691,   696,   708,   713)
    TabName ( 3729) = NameType (    0,    0,   38,   971,   978,   990,   997)
    TabName ( 3733) = NameType (    0,    0,    5,   754,   759,   772,   777)
    TabName ( 3803) = NameType (    0,    0,    7,   178,   185,   178,   185)
    TabName ( 3854) = NameType (    0,    0,   21,   242,   246,   243,   247)
    TabName ( 3895) = NameType (    0,    0,    2,    75,    79,    75,    79)
    TabName ( 3922) = NameType (    0,    0,   25,   447,   456,   458,   468)
    TabName ( 3930) = NameType (    0,    0,   33,   628,   636,   645,   653)
    TabName ( 3968) = NameType (    0,    0,   18,   225,   228,   226,   229)
    TabName ( 4013) = NameType (    0,    0,   21,   605,   611,   622,   628)
    TabName ( 4031) = NameType (    0,    0,   26,   426,   437,   435,   447)
    TabName ( 4032) = NameType (    0,    0,   24,   359,   364,   361,   367)
    TabName ( 4034) = NameType (    0,    0,   22,   521,   524,   537,   540)
    TabName ( 4038) = NameType (    0,    0,  257,   279,   282,   280,   283)
    TabName ( 4049) = NameType (    0,    0,   36,  1063,  1065,  1082,  1084)
    TabName ( 4061) = NameType (    0,    0,   56,   346,   354,   348,   356)
    TabName ( 4080) = NameType (    0,    0,   51,   229,   232,   230,   233)
    TabName ( 4083) = NameType (    0,    0,   16,     1,     1,     1,     1)
    TabName ( 8190) = NameType (    0, 1128,  263,  1188,  1200,  1207,  1219)
    TabName ( 8191) = NameType (    0,  393,    6,  1044,  1047,  1063,  1066)
    TabName ( 8192) = NameType (    0, 1382,  257,   667,   669,   684,   686)
    IAlt =   8190
    RETURN
!
END SUBROUTINE Initializing_Names
!
!******************************************************************************
!
MODULE SUBROUTINE Testing_Identifier (ZNAM, KCTOK, KRTOK, KWNAM, INAM)
!
!  Try to make out what this identifier is
!
    IMPLICIT NONE
!
! arguments
    tCharStar, INTENT (IN)  :: ZNAM
    tInteger,  INTENT (IN)  :: KCTOK, KRTOK
    tInteger,  INTENT (OUT) :: KWNAM, INAM
!
! local variables
    tInteger    :: LNAM, IHSH, IFPUT, IBCK, INAMD, INAMF, INAMS
!
! execution
!
    LNAM = LEN_TRIM (ZNAM)
    IHSH = Hashing_String (ZNAM(1:LNAM))
    IFPUT = 0
    INAM = 0
    IF (TabName(IHSH)%KeyVar == 0) THEN
        KWNAM = KW_VarName
        IFPUT = 1
        INAM = IHSH
        IBCK = 0
    ELSE
        DO
            INAMD = TabName(IHSH)%IStartGbl
            INAMF = TabName(IHSH)%IEndGbl
            IF (Is_Same_String(ZNAM(1:LNAM), ZV_NameGbl(INAMD:INAMF))) THEN
                KWNAM = TabName(IHSH)%KeyVar
                IF (Is_Keyword_Possible(KWNAM, KCTOK, KRTOK)) THEN
                    INAM = IHSH
                    EXIT
                END IF
            END IF
            IF (TabName(IHSH)%INext == 0) THEN
                IAlt = IAlt - 1
                IF (TabName(IAlt)%KeyVar /= 0) THEN
                    CALL Outputing_Error ("insufficient name space, " // &
                                          "raise max # of names and try again")
                    STOP
                END IF
                KWNAM = KW_VarName
                IFPUT = 1
                INAM = IAlt
                IBCK = IHSH
                TabName(IHSH)%INext = INAM
                EXIT
            ELSE
                IHSH = TabName(IHSH)%INext
            END IF
        END DO
    END IF
!
    IF (IFPUT /= 0) THEN
        INAMD = IV_NameGbl + 1
        IV_NameGbl = IV_NameGbl + LNAM
        IF (IV_NameGbl > NMaxIdf*LAvgName) THEN
            CALL Outputing_Error ("insufficient name space (identifiers), " // &
                                  "raise length of global chain and try again")
            STOP
        END IF
        INAMS = IV_NameOut + 1
        IV_NameOut = IV_NameOut + LNAM
        IF (IV_NameOut > NMaxIdf*LAvgName*2) THEN
            CALL Outputing_Error ("insufficient name space (out. idf. names), " // &
                                  "raise length of global chain and try again")
            STOP
        END IF
        TabName (INAM) = NameType (0, IBCK, KWNAM, INAMD, IV_NameGbl, INAMS, IV_NameOut)
        ZV_NameGbl (INAMD:IV_NameGbl) = ZNAM (1:LNAM)
        ZV_NameOut (INAMS:IV_NameOut) = ZNAM (1:LNAM)
    END IF
    RETURN
END SUBROUTINE Testing_Identifier
!
!******************************************************************************
!
FUNCTION Is_Keyword_Possible (KWNAM, KCTXT, KRCTX) RESULT(Flag)
!
!  Test if keyword kwnam possible in context kctxt kwnam krctx
!
    IMPLICIT NONE
!
! arguments
    tInteger, INTENT (IN)   :: KWNAM, KCTXT, KRCTX
    tLogical                :: Flag
!
! execution
!
    IF (KCTXT == KC_Any .AND. KRCTX == KR_Any) THEN
        Flag = .TRUE.
        RETURN
    END IF
    SELECT CASE (KWNAM)
    CASE DEFAULT
        Flag = .TRUE.
    CASE (KW_Command)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_Command)
            Flag = .TRUE.
        END SELECT
    CASE (KW_LogOp, KW_LogConst)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .TRUE.
        CASE (KC_Unknown, KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_BeginNamedBlk)
            Flag = .FALSE.
        END SELECT
    CASE (KW_FmtItem)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Format)
            Flag = .TRUE.
        END SELECT
    CASE (KW_IOKeyWrd)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_IOStm)
            Flag = .TRUE.
        END SELECT
    CASE (KW_TypeAttb)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_DecAttb, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_ProcAttb)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_DecAttb, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_AllocAct, KW_IOAct, KW_Action6)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_Any)
            Flag = (KRCTX == KR_LParen)
        END SELECT
    CASE (KW_GenericName)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Interface, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Procedure)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_ProcAttb, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Contain)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_Any)
            Flag = (KRCTX == KR_LastTok)
        END SELECT
    CASE (KW_Default)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Case, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_IntentAttb)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Intent, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_StringSpec)
        SELECT CASE (KRCTX)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KR_String)
            Flag = .TRUE.
        END SELECT
    CASE (KW_ToAssign)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Assign)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Assign)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Action2, KW_Action3, KW_Case, KW_GoTo)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_IfElse)
            Flag = (KRCTX /= KR_LastTok)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Action4, KW_ElseWhere)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_IfElse)
            Flag = (KRCTX == KR_LastTok)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Format)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginLabStm, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_SelectCase, KW_Where)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_BeginNamedBlk, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_ForAll)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Action5)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_IfElse, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_DecAct)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown)
            Flag = (KRCTX /= KR_LastTok)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Data)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown)
            Flag = (KRCTX /= KR_LastTok)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_ElseIf)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown)
            Flag = (KRCTX == KR_LParen)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_EndProc)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_EndType)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Function)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = (KRCTX == KR_LParen)
        CASE (KC_BeginUnknown, KC_DecAttb, KC_ProcAttb, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Hollerith)
! No support for Hollerith
        Flag = .FALSE.
    CASE (KW_If)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_BeginNamedBlk)
            Flag = (KRCTX == KR_LParen)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Implicit)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Intent)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_DecAttb)
            Flag = (KRCTX == KR_LParen)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Interface)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_None)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Implicit, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Use)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown)
            Flag = (KRCTX /= KR_LastTok)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Only)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Use, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_PPS)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_DecAttb, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Result)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_EntryFunc, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Stat)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Alloc, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Then)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_IfElse, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Do)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_BeginLabStm, KC_BeginNamedBlk, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_While)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_Do, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Else, KW_EndIf, KW_EndDo, KW_EndSelect, KW_EndWhere, KW_EndInterface, KW_EndForAll)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown, KC_BeginExecute, KC_Any)
            Flag = .TRUE.
        END SELECT
    CASE (KW_Type)
        SELECT CASE (KCTXT)
        CASE DEFAULT
            Flag = .FALSE.
        CASE (KC_BeginUnknown)
            Flag = (KRCTX /= KR_LParen)
        CASE (KC_Any)
            Flag = .TRUE.
        END SELECT
    END SELECT
END FUNCTION Is_Keyword_Possible
!
!******************************************************************************
!
MODULE SUBROUTINE Initializing_Commands
!
!  Initialize pointers to pre-processor commands
!
    IMPLICIT NONE
!
! execution
!
    IW_Dollar  = IW_Name (ZA_Dollar)
    IW_Define  = IW_Name (ZA_Define)
    IW_ElseIf  = IW_Name (ZA_ElseIf)
    IW_Else    = IW_Name (ZA_Else)
    IW_EndIf   = IW_Name (ZA_End)
    IW_Eval    = IW_Name (ZA_EVal)
    IW_If      = IW_Name (ZA_If)
    IW_IfDef   = IW_Name (ZA_IfDef)
    IW_IfNDef  = IW_Name (ZA_IfNDef)
    IW_Include = IW_Name (ZA_Include)
    IW_Macro   = IW_Name (ZA_Macro)
    IW_Undef   = IW_Name (ZA_Undef)
!
    RETURN
CONTAINS
    FUNCTION IW_Name (ZNAM) RESULT(ResVal)
!
! arguments
        tCharStar, INTENT (IN)  :: ZNAM
        tInteger                :: ResVal
!
! local variables
        tInteger :: LNAM, IHSH, INAMD, INAMF
!
! execution
!
        LNAM = LEN_TRIM (ZNAM)
        IHSH = Hashing_String (ZNAM(1:LNAM))
        IF (TabName(IHSH)%KeyVar == 0) THEN
            ResVal = 0
        ELSE
            DO
                INAMD = TabName(IHSH)%IStartGbl
                INAMF = TabName(IHSH)%IEndGbl
                IF (Is_Same_String(ZNAM(1:LNAM), ZV_NameGbl(INAMD:INAMF))) THEN
                    ResVal = IHSH
                    EXIT
                END IF
                IF (TabName(IHSH)%INext == 0) THEN
                    ResVal = 0
                    EXIT
                ELSE
                    IHSH = TabName(IHSH)%INext
                END IF
            END DO
        END IF
    END FUNCTION IW_Name
END SUBROUTINE Initializing_Commands
!
!******************************************************************************
!
MODULE SUBROUTINE Rebuilding_Statement (NTOK, KSSTT)
!
!  rebuild current statement
!
    IMPLICIT NONE
!
! arguments
    tInteger, INTENT (IN)   :: NTOK
    tInteger, INTENT (IN)   :: KSSTT
!
! local variables
    tInteger                :: IREPST(NMax_NestDef)
    tInteger                :: IREPAT(NMaxArg)
    tCharLen (2*LMaxStm)    :: ZSTT
    tCharLen (LMaxLine)     :: ZCMT, ZLAB
    tCharLen (LMaxStm)      :: ZTOKW
    tInteger    :: IDOE, LSTT, LLAB, LPAR, IREP1, IRepGbl1, ITOKD
    tInteger    :: LTOK, KKPRV, IFBLK, LCMT, INSTR, ITOK, LTOKW
    tInteger    :: INAM, KWNAM, IREPD, NARGE, NARG, ITOK1, IFVLD
    tInteger    :: KWORK, IARG, ILTTO, IDEB, IREPS, KKTOK, ITOKF
!
! execution
!
!  Return if pre-processor command
!
    IF (KSSTT == KS_PPRCommand) RETURN
!
!  Need to change indent value ?
!
    SELECT CASE (KSSTT)
    CASE (KS_ProcMid)
        INest_Proc = INest_Proc - 1
        IF (INest_Proc == 0) NIndentCurr = NIndentCurr - NIndentProg
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_ProcEnd)
        INest_Proc = INest_Proc - 1
        IF (INest_Proc == 0) NIndentCurr = NIndentCurr - NIndentProg
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_IfMid)
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_IfEnd)
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_ForAllEnd)
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_WhereMid)
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_WhereEnd)
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_DoEnd)
        DO IDOE = 1, N_EndLoop
            NIndentCurr = NIndentCurr - NIndentStep
        END DO
    CASE (KS_InterfaceEnd)
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_SelectMid)
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_SelectEnd)
        NIndentCurr = NIndentCurr - NIndentStep
    CASE (KS_TypeEnd)
        NIndentCurr = NIndentCurr - NIndentStep
    END SELECT
    NIndentCurr = MAX (NIndentCurr, 0)
    LSTT = 0
    LLAB = 0
    LPAR = 0
    IREP1 = IRep
    IRepGbl1 = IRepGbl
    IF (NTOK == 0) THEN
!
!  blank lines
!
        CALL Writing_Statement (ZLAB, LLAB, ZV_Blank, 0, ZV_Blank, LEN_TRIM(ZV_Blank), 0)
    ELSE IF (NTOK == 1 .AND. KK_TokT(1) == KK_Comment) THEN
        ITOKD = IV_Start (1)
        ITOKF = IV_End (1)
        LTOK = ITOKF - ITOKD + 1
        CALL Writing_Statement (ZLAB, LLAB, ZV_Blank, 0, ZV_TokIdf(ITOKD:ITOKF), LTOK, 0)
    ELSE IF (NTOK == 1 .AND. KK_TokT(1) == KK_EmbCmt) THEN
        ITOKD = IV_Start (1)
        ITOKF = IV_End (1)
        LTOK = ITOKF - ITOKD + 1
        CALL Writing_Statement (ZLAB, LLAB, ZV_Blank, 0, ZV_TokIdf(ITOKD:ITOKF), LTOK, 0)
    ELSE IF (NTOK == 1 .AND. KK_TokT(1) == KK_FalseCmt) THEN
        ITOKD = IV_Start (1)
        ITOKF = IV_End (1)
        LTOK = ITOKF - ITOKD + 1
        CALL Writing_Statement (ZV_TokIdf(ITOKD:ITOKF), LTOK, ZV_Blank, 0, ZV_Blank, 0, 0)
    ELSE
        KKPRV = KK_Unknown
        IFBLK = 0
        LCMT = 0
        INSTR = 0
        ITOK = 0
        DO
            IF (INSTR == 0) THEN
                ITOK = ITOK + 1
                IF (ITOK > NTOK) EXIT
                ITOKD = IV_Start (ITOK)
                ITOKF = IV_End (ITOK)
                LTOKW = ITOKF - ITOKD + 1
                ZTOKW (1:LTOKW) = ZV_TokIdf (ITOKD:ITOKF)
                KKTOK = KK_TokT (ITOK)
                IF (KKTOK == KK_Identifier) THEN
                    INAM = IV_Name (ITOK)
                    KWNAM = TabName(INAM)%KeyVar
                    IF (KWNAM == KW_DefName .AND. TabName(INAM)%IRepC /= 0) THEN
                        INSTR = 1
                        IREPD = TabName(INAM)%IRepC
                        CYCLE
                    ELSE IF (KWNAM >=KW_Macro0 .AND. TabName(INAM)%IRepC /= 0) THEN
!
!  Analyse macro arguments
!
                        NARGE = KWNAM - KW_Macro0
                        NARG = 1
                        IREPAT (NARG) = IREP1 + 1
                        ITOK1 = ITOK + 1
                        MACA: DO
                            IF (NTOK < ITOK1+2*NARGE) THEN
                                IFVLD = 0
                                EXIT MACA
                            END IF
                            IF (KK_TokT(ITOK1) /= KK_LParen .AND. KK_TokT(ITOK1) /= KK_OPeren) THEN
                                IFVLD = 0
                                EXIT MACA
                            END IF
                            LPAR = 1
                            DO
                                ITOK1 = ITOK1 + 1
                                IF (ITOK1 > NTOK) THEN
                                    IFVLD = 0
                                    EXIT MACA
                                END IF
!
!  Add token to current arg list
!
                                ITOKD = IV_Start (ITOK1)
                                ITOKF = IV_End (ITOK1)
                                LTOKW = ITOKF - ITOKD + 1
                                IREPS = IRepGbl1 + 1
                                IRepGbl1 = IRepGbl1 + LTOKW
                                IF (IRepGbl1 > NMaxRepGbl) THEN
                                    WRITE (LU_Error,*) "insufficient name space (out. repl. names)"
                                    WRITE (LU_Error,*) "raise length of global chain and try again"
                                    STOP
                                END IF
                                IREP1 = IREP1 + 1
                                IF (IREP1 > NMaxRep) THEN
                                    WRITE (LU_Error,*) "insufficient name space (out. repl. names)"
                                    WRITE (LU_Error,*) "raise # of repl. names and try again"
                                    STOP
                                END IF
                                KK_RepT (IREP1) = KK_TokT (ITOK1)
                                IRepStart (IREP1) = IREPS
                                IRepEnd (IREP1) = IRepGbl1
                                IRepNext (IREP1) = IREP1 + 1
                                ZV_RepGbl (IREPS:IRepGbl1) = ZV_TokIdf (ITOKD:ITOKF)
!
! Check for next argument or last one
! Find out nature of names
!
                                SELECT CASE (KK_TokT(ITOK1))
                                CASE (KK_RParen)
                                    LPAR = LPAR - 1
                                    IF (LPAR == 0) THEN
                                        IF (NARG == NARGE) THEN
                                            IREP1 = IREP1 - 1
                                            IRepNext (IREP1) = 0
                                            ITOK = ITOK1
                                            IFVLD = 1
                                        ELSE
                                            IFVLD = 0
                                        END IF
                                        EXIT MACA
                                    END IF
                                CASE (KK_LParen, KK_OPeren)
                                    LPAR = LPAR + 1
                                CASE (KK_Comma)
                                    IF (LPAR == 1) THEN
                                        IREP1 = IREP1 - 1
                                        IRepNext (IREP1) = 0
                                        NARG = NARG + 1
                                        IREPAT (NARG) = IREP1 + 1
                                    END IF
                                CASE (KK_Identifier)
                                    CALL Testing_Identifier (ZV_RepGbl(IREPS:IRepGbl1), &
                                                             KC_Unknown, KR_Unknown,    &
                                                             KWORK, IRepName(IREP1))
                                CASE DEFAULT
                                    CONTINUE
                                END SELECT
                            END DO
                        END DO MACA
!
! If it was legal macro, apply it
!
                        IF (IFVLD /= 0) THEN
                            INSTR = 1
                            IREPD = TabName(INAM)%IRepC
                            CYCLE
                        END IF
                    ELSE
                        ITOKD = TabName(INAM)%IStartOut
                        ITOKF = TabName(INAM)%IEndOut
                        LTOKW = ITOKF - ITOKD + 1
                        ZTOKW (1:LTOKW) = ZV_NameOut (ITOKD:ITOKF)
                    END IF
                END IF
            ELSE
                IF (IREPD == 0) THEN
                    INSTR = INSTR - 1
                    IF (INSTR /= 0) THEN
                        IREPD = IREPST (INSTR)
                    END IF
                    CYCLE
                END IF
                ITOKD = IRepStart (IREPD)
                ITOKF = IRepEnd (IREPD)
                LTOKW = ITOKF - ITOKD + 1
                ZTOKW (1:LTOKW) = ZV_RepGbl (ITOKD:ITOKF)
                KKTOK = KK_RepT (IREPD)
                IF (KKTOK == KK_Identifier) THEN
                    INAM = IRepName (IREPD)
                    KWNAM = TabName(INAM)%KeyVar
                    IF (KWNAM == KW_DefName .AND. TabName(INAM)%IRepC /= 0) THEN
                        IREPST (INSTR) = IRepNext (IREPD)
                        INSTR = INSTR + 1
                        IF (INSTR > NMax_NestDef) THEN
                            WRITE (LU_Error,*) "defines should not be nested deeper than", &
                                               NMax_NestDef
                            EXIT
                        END IF
                        IREPD = TabName(INAM)%IRepC
                        CYCLE
                    END IF
                ELSE IF (KKTOK > KK_Arg0) THEN
                    IREPST (INSTR) = IRepNext (IREPD)
                    INSTR = INSTR + 1
                    IF (INSTR > NMax_NestDef) THEN
                        WRITE (LU_Error,*) "defines should not be nested deeper than", &
                                           NMax_NestDef
                        EXIT
                    END IF
                    IARG = KKTOK - KK_Arg0
                    IREPD = IREPAT (IARG)
                    CYCLE
                END IF
                IREPD = IRepNext (IREPD)
            END IF
            IF (LTOKW+LSTT+2 > 2*LMaxStm) THEN
                CALL Outputing_Error ("Substitution leads to line length overflow")
                EXIT
            END IF
!
!  Construct line inserting spaces where necessary
!
            SELECT CASE (KKTOK)
            CASE (KK_Comment)   ! Comment string
                IF (IFBLK == 0) THEN
                    LTOK = LTOKW + 1
                    ZCMT (1:LTOK) = " " // ZTOKW (1:LTOKW)
                ELSE
                    LTOK = LTOKW
                    ZCMT (1:LTOK) = ZTOKW (1:LTOKW)
                END IF
                LCMT = LTOK
                EXIT
            CASE (KK_EmbCmt)    ! Embedded comment string
                IF (IFBLK == 0) THEN
                    LTOK = LTOKW + 1
                    ZCMT (1:LTOK) = " " // ZTOKW (1:LTOKW)
                ELSE
                    LTOK = LTOKW
                    ZCMT (1:LTOK) = ZTOKW (1:LTOKW)
                END IF
                LCMT = LTOK
                LTOK = 0
            CASE (KK_FalseCmt)  ! False comment
                LLAB = LTOKW
                ZLAB (1:LLAB) = ZTOKW (1:LTOKW)
                LTOK = 0
            CASE (KK_Label)     ! Numerical label
                IF (LSTT == 0) THEN
                    LLAB = LTOKW
                    ZLAB (1:LLAB) = ZTOKW (1:LTOKW)
                    LTOK = 0
                ELSE
                    LTOK = LTOKW + 2
                    ZSTT (LSTT+1:LSTT+LTOK) = " " // ZTOKW (1:LTOKW) // " "
                    IFBLK = 1
                END IF
            ! Character string, _Kind (underscore), Block name, 
            ! Integer Numerical value and Real Numerical value
            CASE (KK_ChrStr, KK_Kind, KK_BlkName, KK_IntNumVal, KK_RealNumVal)
                LTOK = LTOKW
                ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW)
                IFBLK = 0
            CASE (KK_Percent)       ! %
                LTOK = LTOKW
                IF (IFBLK /= 0) LSTT = LSTT - 1
                ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW)
                IFBLK = 0
            CASE (KK_Comma)         ! ,
                LTOK = LTOKW + 1
                IF (IFBLK /= 0) LSTT = LSTT - 1
                ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW) // " "
                IFBLK = 1
            CASE (KK_Identifier)    ! Identifier
                IF (KWNAM <= KW_System .AND. KC_Keyword /= KC_Leave) THEN
                    CALL Changing_Case (ZTOKW(1:LTOKW), KC_Keyword)
                ELSE IF (KWNAM > KW_System .AND. KC_UserIdf /= KC_Leave) THEN
                    CALL Changing_Case (ZTOKW(1:LTOKW), KC_UserIdf)
                END IF
                IF (KWNAM == KW_LogOp .AND. IFBLK == 0) THEN
                    LTOK = LTOKW + 2
                    ZSTT (LSTT+1:LSTT+LTOK) = " " // ZTOKW (1:LTOKW) // " "
                    IFBLK = 1
                ELSE IF (KWNAM == KW_LogOp .AND. IFBLK /= 0) THEN
                    LTOK = LTOKW + 1
                    ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW) // " "
                    IFBLK = 1
                ELSE IF (KWNAM == KW_Action5) THEN
                    IF (IFBLK /= 0 .OR. LSTT == 0) THEN
                        LTOK = LTOKW + 1
                        ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW) // " "
                    ELSE
                        LTOK = LTOKW + 2
                        ZSTT (LSTT+1:LSTT+LTOK) = " " // ZTOKW (1:LTOKW) // " "
                    END IF
                    IFBLK = 1
                ELSE IF ((KKPRV == KK_Identifier .OR. KKPRV == KK_IntNumVal .OR. &
                          KKPRV == KK_RealNumVal) .AND. IFBLK == 0) THEN
                    LTOK = LTOKW + 1
                    ZSTT (LSTT+1:LSTT+LTOK) = " " // ZTOKW (1:LTOKW)
                    IFBLK = 0
                ELSE
                    LTOK = LTOKW
                    ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW)
                    IFBLK = 0
                END IF
            CASE (KK_Colon)     ! :
                IF (LPAR <= 0) THEN
                    LTOK = LTOKW + 1
                    ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW) // " "
                    IFBLK = 1
                ELSE
                    LTOK = LTOKW
                    ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW)
                    IFBLK = 0
                END IF
            ! ;, $command (preprocessor), .
            CASE (KK_SemiColon, KK_Command, KK_Dot)
                LTOK = LTOKW + 1
                ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW) // " "
                IFBLK = 1
            CASE (KK_Question)  ! ?
                LTOK = 0
                IF (IFBLK /= 0) LSTT = LSTT - 1
                IFBLK = 0
            CASE (KK_RParen)    ! )
                LPAR = LPAR - 1
                LTOK = LTOKW + 1
                IF (IFBLK /= 0) LSTT = LSTT - 1
                ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW) // " "
                IFBLK = 1
            CASE (KK_LParen)    ! (
                LPAR = LPAR + 1
                IF (LPAR <= 1 .AND. IFBLK == 0) THEN
                    LTOK = LTOKW + 1
                    ZSTT (LSTT+1:LSTT+LTOK) = " " // ZTOKW (1:LTOKW)
                ELSE
                    LTOK = LTOKW
                    ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW)
                END IF
                IFBLK = 0
            CASE (KK_OPeren)    ! ( within defined type
                LPAR = LPAR + 1
                LTOK = LTOKW
                ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW)
                IFBLK = 0
            ! /, + or -, //, *, **, =
            CASE (KK_Slash, KK_PlusMinus, KK_Concat, KK_Star, KK_Pow, KK_Assign)
                IF (LPAR <= 0 .AND. IFBLK == 0) THEN
                    LTOK = LTOKW + 2
                    ZSTT (LSTT+1:LSTT+LTOK) = " " // ZTOKW (1:LTOKW) // " "
                    IFBLK = 1
                ELSE IF (LPAR <= 0) THEN
                    LTOK = LTOKW + 1
                    ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW) // " "
                    IFBLK = 1
                ELSE
                    IF (IFBLK /= 0) LSTT = LSTT - 1
                    LTOK = LTOKW
                    ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW)
                    IFBLK = 0
                END IF
            ! (/, /=, <=, ==, >=, =>, >, <, ::, .xxx., /)
            CASE (KK_LParSlh, KK_NE, KK_LE, KK_EQ, KK_GE, KK_PTR, KK_GT, KK_LT, &
                  KK_DblColon, KK_LogOp, KK_RParSlh)
                IF (IFBLK == 0) THEN
                    LTOK = LTOKW + 2
                    ZSTT (LSTT+1:LSTT+LTOK) = " " // ZTOKW (1:LTOKW) // " "
                ELSE
                    LTOK = LTOKW + 1
                    ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW) // " "
                END IF
                IFBLK = 1
            CASE DEFAULT
                LTOK = LTOKW
                ZSTT (LSTT+1:LSTT+LTOK) = ZTOKW (1:LTOKW)
                IFBLK = 0
            END SELECT
            LSTT = LSTT + LTOK
            KKPRV = KKTOK
        END DO
        DO
            IF (ZSTT(LSTT:LSTT) /= " " .OR. LSTT <= 1) EXIT
            LSTT = LSTT - 1
        END DO
        CALL Writing_Statement (ZLAB, LLAB, ZSTT, LSTT, ZCMT, LCMT, NIndentCurr)
!
! provide original code commented
!
        IF (ANY(KK_TokT(1:NTOK) == KK_EmbCmt) .AND. NLineInStm > 1) THEN
            DO ILTTO = 1, NLineInStm
                IDEB = VERIFY (ZV_OrgStm(ILTTO), ' '//'!'//ChrTab)
                IDEB = MIN (IDEB, 1+LEN_TRIM(ZP_Name)+1)
                ZCMT = '!' // ZP_Name // ZV_OrgStm (ILTTO) (IDEB:)
                LCMT = LEN_TRIM (ZCMT)
                CALL Writing_Statement (ZV_Blank, 0, ZV_Blank, 0, ZCMT, LCMT, 0)
            END DO
        END IF
    END IF
!
!  Need to change indent value ?
!
    SELECT CASE (KSSTT)
    CASE (KS_ProcStart)
        IF (INest_Proc == 0) NIndentCurr = NIndentCurr + NIndentProg
        INest_Proc = INest_Proc + 1
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_ProcMid)
        IF (INest_Proc == 0) NIndentCurr = NIndentCurr + NIndentProg
        INest_Proc = INest_Proc + 1
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_IfStart)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_IfMid)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_ForAllStart)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_WhereStart)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_WhereMid)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_DoStart)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_InterfaceStart)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_SelectStart)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_SelectMid)
        NIndentCurr = NIndentCurr + NIndentStep
    CASE (KS_TypeStart)
        NIndentCurr = NIndentCurr + NIndentStep
    END SELECT

    IF (KSSTT == KS_ProcEnd) THEN
        WRITE (LU_File, '(A)') ' '
        WRITE (LU_File, '(A)') '!******************************************************************************'
        WRITE (LU_File, '(A)') ' '
    END IF
    
    RETURN
END SUBROUTINE Rebuilding_Statement
!
!******************************************************************************
!
MODULE SUBROUTINE Writing_Statement (ZLAB, LLAB, ZSTT, LSTT, ZCMT, LCMT, NIndentCurrI)
!
!  write a label, a statement, and a trailing comment
!
    IMPLICIT NONE
!
! arguments
    tCharStar, INTENT (IN)  :: ZLAB, ZSTT, ZCMT
    tInteger,  INTENT (IN)  :: LLAB, LSTT, LCMT, NIndentCurrI
!
! parameters
    tCharStar, PARAMETER    :: ZFMTS = "(/)"
    tCharStar, PARAMETER    :: ZFMT1N = "(a)"
    tCharStar, PARAMETER    :: ZFMT1X = "(a,'&')"
    tCharStar, PARAMETER    :: ZFMTL = "('# line ',i8,a)"
    tInteger,  PARAMETER    :: LSPLTM = 5   ! decisions for splitting
!
! local variables
    tCharLen (1)        :: ZDLM
    tCharLen (LMaxLine) :: ZHEAW
    tCharLen (10)       :: ZSET
    tInteger, SAVE      :: NLINO = 0
    tInteger    :: LSPLT, LCMTW, NNDTW, LINEW, LSTTW, NCNT, IFIN
    tInteger    :: IFCHC, NAMPW, IDEB, NNDTW1, NNDTW2, ILST, ICHR
    tInteger    :: IFCHP, NFILW, IFINW, NLINW, IDEBW, IBCK, LLIN
!
! execution
!
    LCMTW = LCMT
!
!  Null strings
!
    IF (LSTT <= 0) THEN
        IF (LCMT /= 0) THEN
            NNDTW = NIndentCurrI
            LCMTW = LCMT
            IF (LCMTW+NNDTW > LMaxLine) THEN
                NNDTW = 0
                LCMTW = MIN (LCMT, LMaxLine)
            END IF
            IF (LLAB == 0) THEN
                WRITE (LU_File, ZFMT1N) REPEAT (" ", NNDTW) // ZCMT (1:LCMTW)
            ELSE
                NNDTW = MAX (1, NNDTW-LLAB)
                WRITE (LU_File, ZFMT1N) ZLAB (1:LLAB) // REPEAT (" ", NNDTW) // ZCMT (1:LCMTW)
            END IF
        ELSE
            IF (LLAB == 0) THEN
                WRITE (LU_File, ZFMTS)
            ELSE
                WRITE (LU_File, ZFMT1N) ZLAB (1:LLAB)
            END IF
        END IF
        NLINO = NLINO + 1
        RETURN
    END IF
!
!  Write line number
!
    IF (IfLineNumber /= 0) THEN
        WRITE (LU_File, ZFMTL) NLineInput (ILevel_Include), ' "' // &
                               TRIM (ZV_TabName(ILevel_Include)) // '"'
    END IF
!
!  Find a reasonnable step
!
    IF (IfFixedOut == 0) THEN
        NNDTW = NIndentCurrI
        LINEW = LineLen
    ELSE
        NNDTW = MAX (NIndentCurrI, 6)   ! fixed form code starts after column 6
        LINEW = 72                      ! fixed form code ends on column 72
    END IF
    LSTTW = LSTT + MAX (LLAB+1-NNDTW, 0)
    IF (LSTTW > LINEW-2-NNDTW) THEN
        CALL ChooseSplitCriteria()
    ELSE
        LSPLT = 0
    END IF
    IF (LSPLT >=LSPLTM) THEN
        IF (IfFixedOut == 0) THEN
            LINEW = LMaxLine
            CALL ChooseSplitCriteria()
        END IF
        IF (LSPLT >=LSPLTM) THEN
            IF (IfFixedOut == 0) THEN
                NNDTW = 0
            ELSE
                NNDTW = 6   ! fixed form code starts columns 6
            END IF
            LSTTW = LSTT + MAX (LLAB+1-NNDTW, 0)
            CALL ChooseSplitCriteria()
            IF (LSPLT >=LSPLTM) THEN
                NCNT = (LSTTW+LINEW-NNDTW-3) / (LINEW-2-NNDTW)
                WRITE (LU_Error,*) "More than max # of continuation lines"
                WRITE (LU_Error,*) "output lines", NLINO + 1, " - ", NLINO + NCNT
            END IF
        END IF
    END IF
!
    IFIN = 0
    IFCHC = 0
    NAMPW = 2
    DO
        IDEB = IFIN + 1
        IFIN = IFIN + LINEW - NAMPW - NNDTW
        IF (IDEB == 1) THEN
            IFIN = IFIN + 1         ! no need for & at line start
            IF (LLAB /= 0) THEN
                NNDTW1 = MAX (NNDTW, LLAB+1)
                IFIN = IFIN + NNDTW - NNDTW1
            END IF
        ELSE
            IF (NNDTW > 1) THEN
                IFIN = IFIN + 1     ! leading & will not use character
            END IF
        END IF
        IFIN = MIN (IFIN, LSTT)
        IFCHP = IFCHC
        IF (IFIN < LSTT .AND. LSPLT < LSPLTM) THEN
            ILST = IDEB
            DO ICHR = IDEB, IFIN
                IF (IFCHC == 0 .AND. (ZSTT(ICHR:ICHR) == "'" .OR. ZSTT(ICHR:ICHR) == '"')) THEN
                    IFCHC = 1
                    ZDLM = ZSTT (ICHR:ICHR)
                ELSE IF (IFCHC == 1 .AND. ZSTT(ICHR:ICHR) == ZDLM) THEN
                    IFCHC = 0
                    ILST = ICHR
                END IF
            END DO
            IF (IFCHC == 0) THEN
                SELECT CASE (LSPLT)
                CASE DEFAULT
                    ZSET = " "
                CASE (1)
                    ZSET = " +-"
                CASE (2)
                    ZSET = " +-,:="
                CASE (3)
                    ZSET = " +-,:=/*"
                CASE (4)
                    ZSET = " +-,:=/*><"
                END SELECT
                IBCK = SCAN (ZSTT(ILST+1:IFIN+1), ZSET, BACK=.TRUE.)
                IF (IBCK > 0) THEN
                    IFIN = ILST + IBCK
                END IF
                NAMPW = 3
            ELSE
                NAMPW = 2
            END IF
        END IF
!
!  First line of instruction
!
        IF (IDEB == 1) THEN
            IF (IFIN == LSTT) THEN
                IF (LCMT == 0) THEN
                    IF (LLAB == 0) THEN
                        WRITE (LU_File, ZFMT1N) REPEAT (" ", NNDTW) // ZSTT (IDEB:IFIN)
                    ELSE
                        NNDTW2 = NNDTW1 - LLAB
                        WRITE (LU_File, ZFMT1N) ZLAB (1:LLAB) // REPEAT (" ", NNDTW2) // &
                                                ZSTT (IDEB:IFIN)
                    END IF
                ELSE
                    IF (LLAB == 0) THEN
                        LLIN = IFIN - IDEB + 1 + NNDTW
                        IF (LLIN <= LINEW) THEN
                            WRITE (LU_File, ZFMT1N) REPEAT (" ", NNDTW) // ZSTT (IDEB:IFIN) // &
                                                    ZCMT (1:LCMTW)
                        ELSE
                            WRITE (LU_File, ZFMT1N) REPEAT (" ", NNDTW) // ZSTT (IDEB:IFIN)
                            NLINO = NLINO + 1
                            WRITE (LU_File, ZFMT1N) ADJUSTL (ZCMT(1:LCMTW))
                        END IF
                    ELSE
                        NNDTW2 = NNDTW1 - LLAB
                        LLIN = IFIN - IDEB + 1 + NNDTW1
                        IF (LLIN <= LINEW) THEN
                            WRITE (LU_File, ZFMT1N) ZLAB (1:LLAB) // REPEAT (" ", NNDTW2) // &
                                                    ZSTT (IDEB:IFIN) // ZCMT (1:LCMTW)
                        ELSE
                            WRITE (LU_File, ZFMT1N) ZLAB (1:LLAB) // REPEAT (" ", NNDTW2) // &
                                                    ZSTT (IDEB:IFIN)
                            NLINO = NLINO + 1
                            WRITE (LU_File, ZFMT1N) ADJUSTL (ZCMT(1:LCMTW))
                        END IF
                    END IF
                END IF
                NLINO = NLINO + 1
                EXIT
            ELSE
                IF (IfFixedOut == 0) THEN
                    IF (LLAB == 0) THEN
                        WRITE (LU_File, ZFMT1X) REPEAT (" ", NNDTW) // ZSTT (IDEB:IFIN)
                    ELSE
                        NNDTW2 = NNDTW1 - LLAB
                        WRITE (LU_File, ZFMT1X) ZLAB (1:LLAB) // REPEAT (" ", NNDTW2) // &
                                                ZSTT (IDEB:IFIN)
                    END IF
                ELSE
                    NFILW = 72 - NNDTW - (IFIN-IDEB+1)
                    IF (LLAB == 0) THEN
                        WRITE (LU_File, ZFMT1X) REPEAT (" ", NNDTW) // ZSTT (IDEB:IFIN) // &
                                                REPEAT (" ", NFILW)
                    ELSE
                        NNDTW2 = NNDTW1 - LLAB
                        WRITE (LU_File, ZFMT1X) ZLAB (1:LLAB) // REPEAT (" ", NNDTW2) // &
                                                ZSTT (IDEB:IFIN) // REPEAT (" ", NFILW)
                    END IF
                END IF
            END IF
!
!  Other line of instruction
!
        ELSE
            NNDTW1 = MAX (NNDTW, 1)
            IF (IfFixedOut == 0) THEN
                ZHEAW = REPEAT (" ", NNDTW1-1) // "& "
                IF (IFCHP == 0) THEN
                    NNDTW1 = NNDTW1 + 1
                END IF
            ELSE IF (IFCHP /= 0) THEN
                ZHEAW = REPEAT (" ", 5) // "&"
                NNDTW1 = 6
            ELSE
                ZHEAW = REPEAT (" ", 5) // "&"
            END IF
            IF (IFIN == LSTT) THEN
                IF (LCMT == 0) THEN
                    WRITE (LU_File, ZFMT1N) ZHEAW (1:NNDTW1) // ZSTT (IDEB:IFIN)
                ELSE
                    LLIN = IFIN - IDEB + 1 + NNDTW1
                    IF (LLIN <= LINEW) THEN
                        WRITE (LU_File, ZFMT1N) ZHEAW (1:NNDTW1) // ZSTT (IDEB:IFIN) // &
                                                ZCMT (1:LCMTW)
                    ELSE
                        WRITE (LU_File, ZFMT1N) ZHEAW (1:NNDTW1) // ZSTT (IDEB:IFIN)
                        NLINO = NLINO + 1
                        WRITE (LU_File, ZFMT1N) ADJUSTL (ZCMT(1:LCMTW))
                    END IF
                END IF
                NLINO = NLINO + 1
                EXIT
            ELSE
                IF (IfFixedOut == 0) THEN
                    WRITE (LU_File, ZFMT1X) ZHEAW (1:NNDTW1) // ZSTT (IDEB:IFIN)
                ELSE
                    NFILW = 72 - NNDTW1 + 1 - (IFIN-IDEB+2)
                    WRITE (LU_File, ZFMT1X) ZHEAW (1:NNDTW1) // ZSTT (IDEB:IFIN) // &
                                            REPEAT (" ", NFILW)
                END IF
            END IF
        END IF
        NLINO = NLINO + 1
    END DO
    RETURN
CONTAINS
    SUBROUTINE ChooseSplitCriteria()
!
! Choose assumptions governing splitting at end of lines
!
        LSPLT = 0
        LEVELS: DO
            IF (LSPLT >=LSPLTM) EXIT LEVELS
            IFINW = 0
            IFCHC = 0
            NLINW = 0
            NAMPW = 2
            DO
                IDEBW = IFINW + 1
                IFINW = IFINW + LINEW - NAMPW - NNDTW
                IF (IDEBW == 1) THEN
                    IFINW = IFINW + 1       ! no need for & at line start
                    IF (LLAB /= 0) THEN
                        NNDTW1 = MAX (NNDTW, LLAB+1)
                        IFINW = IFINW + NNDTW - NNDTW1
                    END IF
                ELSE
                    IF (NNDTW > 1) THEN
                        IFINW = IFINW + 1   ! leading & will not use character
                    END IF
                END IF
                IFINW = MIN (IFINW, LSTT)
!
!  Need to split - Use current criteria
!
                IF (IFINW < LSTT) THEN
                    ILST = IDEBW
                    DO ICHR = IDEBW, IFINW
                        IF (IFCHC == 0 .AND. (ZSTT(ICHR:ICHR) == "'" .OR. &
                            ZSTT(ICHR:ICHR) == '"')) THEN
                            IFCHC = 1
                            ZDLM = ZSTT (ICHR:ICHR)
                        ELSE IF (IFCHC == 1 .AND. ZSTT(ICHR:ICHR) == ZDLM) THEN
                            IFCHC = 0
                            ILST = ICHR
                        END IF
                    END DO
                    IF (IFCHC == 0) THEN
                        SELECT CASE (LSPLT)
                        CASE DEFAULT
                            ZSET = " "
                        CASE (1)
                            ZSET = " +-"
                        CASE (2)
                            ZSET = " +-,:="
                        CASE (3)
                            ZSET = " +-,:=/*"
                        CASE (4)
                            ZSET = " +-,:=/*><"
                        END SELECT
                        IBCK = SCAN (ZSTT(ILST+1:IFINW+1), ZSET, BACK=.TRUE.)
                        IF (IBCK > 0) THEN
                            IFINW = ILST + IBCK
                            NAMPW = 3
                        ELSE
                            LSPLT = LSPLT + 1
                            CYCLE LEVELS
                        END IF
                    ELSE
                        IBCK = INDEX (ZSTT(ILST+1:IFINW+1), ZDLM, BACK=.TRUE.)
                        IF (IBCK > 0) THEN
                            IFINW = ILST + IBCK - 1
                            NAMPW = 3
                            IFCHC = 0
                        ELSE
                            NAMPW = 2
                        END IF
                    END IF
                END IF
!
!  First line of instruction
!
                IF (IDEBW == 1) THEN
                    IF (IFINW == LSTT) THEN
                        NLINW = NLINW + 1
                        EXIT
                    END IF
!
!  Other line of instruction
!
                ELSE
                    IF (IFINW == LSTT) THEN
                        NLINW = NLINW + 1
                        EXIT
                    END IF
                END IF
                NLINW = NLINW + 1
                IF (NLINW > NMaxContLine) EXIT
            END DO
            IF (NLINW <= NMaxContLine) THEN
                RETURN
            ELSE
                LSPLT = LSPLT + 1
            END IF
        END DO LEVELS
    END SUBROUTINE ChooseSplitCriteria
END SUBROUTINE Writing_Statement
!
!******************************************************************************
!
SUBROUTINE Changing_Case (ZSTR, KC_Case)
!
!  Case change
!
    IMPLICIT NONE
!
! arguments
    tCharStar, INTENT (INOUT)   :: ZSTR
    tInteger,  INTENT (IN)      :: KC_Case
!
! local variables
    tInteger    :: LSTR, ISTR, IRNK
!
! execution
!
    LSTR = LEN_TRIM (ZSTR)
    SELECT CASE (KC_Case)
    CASE (KC_Upper)
        DO ISTR = 1, LSTR
            IRNK = INDEX (ZP_Lower, ZSTR(ISTR:ISTR))
            IF (IRNK > 0) ZSTR (ISTR:ISTR) = ZP_Upper (IRNK:IRNK)
        END DO
    CASE (KC_Lower)
        DO ISTR = 1, LSTR
            IRNK = INDEX (ZP_Upper, ZSTR(ISTR:ISTR))
            IF (IRNK > 0) ZSTR (ISTR:ISTR) = ZP_Lower (IRNK:IRNK)
        END DO
    END SELECT
END SUBROUTINE Changing_Case
!
!******************************************************************************

END SUBMODULE SubTool_F90Parser
