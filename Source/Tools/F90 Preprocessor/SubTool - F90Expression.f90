
SUBMODULE (ModTool_F90PreProcess:SubTool_F90Parser) SubTool_F90Expression

!** PURPOSE OF THIS SUBMODULE:
    ! This submodule contains routines that perform expression-evaluating tasks.

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! name of submodule
    tCharStar, PARAMETER    :: SubModName = 'SubTool_F90Expression'
    !
    ! ----------------------------------!
    !       Expression_Parameters       !
    ! ----------------------------------!
    !
    !  Parameters for f90 expressions evaluation
    !
    !
    !  Operator & operand names
    !
    tCharStar, PARAMETER    :: ZO_AND   = ".AND."
    tCharStar, PARAMETER    :: ZO_EQ    = ".EQ."
    tCharStar, PARAMETER    :: ZO_GE    = ".GE."
    tCharStar, PARAMETER    :: ZO_GT    = ".GT."
    tCharStar, PARAMETER    :: ZO_LE    = ".LE."
    tCharStar, PARAMETER    :: ZO_LT    = ".LT."
    tCharStar, PARAMETER    :: ZO_NE    = ".NE."
    tCharStar, PARAMETER    :: ZO_NOT   = ".NOT."
    tCharStar, PARAMETER    :: ZO_OR    = ".OR."
    !
    tCharStar, PARAMETER    :: ZO_TRUE  = ".TRUE."
    tCharStar, PARAMETER    :: ZO_FALSE = ".FALSE."
    !
    tCharStar, PARAMETER    :: ZO_INT   = "INT"
    tCharStar, PARAMETER    :: ZO_NINT  = "NINT"
    tCharStar, PARAMETER    :: ZO_SIN   = "SIN"
    tCharStar, PARAMETER    :: ZO_COS   = "COS"
    tCharStar, PARAMETER    :: ZO_TAN   = "TAN"
    tCharStar, PARAMETER    :: ZO_ATAN  = "ATAN"
    tCharStar, PARAMETER    :: ZO_LOG   = "LOG"
    tCharStar, PARAMETER    :: ZO_EXP   = "EXP"
    tCharStar, PARAMETER    :: ZO_LOG10 = "LOG10"
    tCharStar, PARAMETER    :: ZO_SQRT  = "SQRT"
    tCharStar, PARAMETER    :: ZO_MOD   = "MOD"
    tCharStar, PARAMETER    :: ZO_MAX   = "MAX"
    tCharStar, PARAMETER    :: ZO_MIN   = "MIN"
    tCharStar, PARAMETER    :: ZO_ATAN2 = "ATAN2"
    tCharStar, PARAMETER    :: ZO_ASIN  = "ASIN"
    tCharStar, PARAMETER    :: ZO_ACOS  = "ACOS"
    tCharStar, PARAMETER    :: ZO_SINH  = "SINH"
    tCharStar, PARAMETER    :: ZO_COSH  = "COSH"
    tCharStar, PARAMETER    :: ZO_TANH  = "TANH"
    tCharStar, PARAMETER    :: ZO_ABS   = "ABS"
    tCharStar, PARAMETER    :: ZO_KIND  = "KIND"
    tCharStar, PARAMETER    :: ZO_SIK   = "SELECTED_INT_KIND"
    tCharStar, PARAMETER    :: ZO_SRK   = "SELECTED_REAL_KIND"
    !
    !  pre-defined parameters
    !
    tCharStar, PARAMETER    :: ZO_BlankCmt      = "FPPR_BLANK_CMT"
    tCharStar, PARAMETER    :: ZO_FalseCmt      = "FPPR_FALSE_CMT"
    tCharStar, PARAMETER    :: ZO_KeyCase       = "FPPR_KWD_CASE"
    tCharStar, PARAMETER    :: ZO_UserCase      = "FPPR_USR_CASE"
    tCharStar, PARAMETER    :: ZO_FixedIn       = "FPPR_FXD_IN"
    tCharStar, PARAMETER    :: ZO_FixedOut      = "FPPR_FXD_OUT"
    tCharStar, PARAMETER    :: ZO_UseSharp      = "FPPR_USE_SHARP"
    tCharStar, PARAMETER    :: ZO_LMax          = "FPPR_MAX_LINE"
    tCharStar, PARAMETER    :: ZO_StepIndent    = "FPPR_STP_INDENT"
    tCharStar, PARAMETER    :: ZO_ProgIndent    = "FPPR_PGM_INDENT"
    tCharStar, PARAMETER    :: ZO_LineNum       = "FPPR_NMBR_LINES"
    !
    tCharStar, PARAMETER    :: ZA_Leave         = "FPPR_LEAVE"
    tCharStar, PARAMETER    :: ZA_Lower         = "FPPR_LOWER"
    tCharStar, PARAMETER    :: ZA_Upper         = "FPPR_UPPER"
    !
    !  Operator & operand codes
    !  ** Beware, operands must be in increasing priority order
    !
    tInteger, PARAMETER     :: KO_Undef         =  0   ! Undefined
    tInteger, PARAMETER     :: KO_LogNumVal     = 13   ! Logical Numerical value
    tInteger, PARAMETER     :: KO_IntNumVal     = 19   ! Integer Numerical value
    tInteger, PARAMETER     :: KO_RealNumVal    = 25   ! Real Numerical value
    tInteger, PARAMETER     :: KO_NOT           = 33   ! Not
    tInteger, PARAMETER     :: KO_OR            = 34   ! Or
    tInteger, PARAMETER     :: KO_AND           = 35   ! And
    tInteger, PARAMETER     :: KO_GT            = 36   ! >
    tInteger, PARAMETER     :: KO_GE            = 37   ! >=
    tInteger, PARAMETER     :: KO_EQ            = 38   ! ==
    tInteger, PARAMETER     :: KO_LT            = 39   ! <
    tInteger, PARAMETER     :: KO_LE            = 40   ! <=
    tInteger, PARAMETER     :: KO_NE            = 41   ! /=
    tInteger, PARAMETER     :: KO_Minus         = 42   ! -
    tInteger, PARAMETER     :: KO_Plus          = 43   ! +
    tInteger, PARAMETER     :: KO_Multiply      = 44   ! *
    tInteger, PARAMETER     :: KO_Divide        = 45   ! /
    tInteger, PARAMETER     :: KO_Power         = 46   ! **
    tInteger, PARAMETER     :: KO_INT           = 47   ! Int
    tInteger, PARAMETER     :: KO_NINT          = 48   ! Nint
    tInteger, PARAMETER     :: KO_SIN           = 49   ! Sin
    tInteger, PARAMETER     :: KO_COS           = 50   ! Cos
    tInteger, PARAMETER     :: KO_TAN           = 51   ! Tan
    tInteger, PARAMETER     :: KO_ATAN          = 52   ! Atan
    tInteger, PARAMETER     :: KO_LOG           = 53   ! Log
    tInteger, PARAMETER     :: KO_EXP           = 54   ! Exp
    tInteger, PARAMETER     :: KO_LOG10         = 55   ! Log10
    tInteger, PARAMETER     :: KO_SQRT          = 56   ! Sqrt
    tInteger, PARAMETER     :: KO_MOD           = 57   ! Mod
    tInteger, PARAMETER     :: KO_MAX           = 58   ! Max
    tInteger, PARAMETER     :: KO_MIN           = 59   ! Min
    tInteger, PARAMETER     :: KO_ATAN2         = 60   ! Atan2
    tInteger, PARAMETER     :: KO_ASIN          = 61   ! Asin
    tInteger, PARAMETER     :: KO_ACOS          = 62   ! Acos
    tInteger, PARAMETER     :: KO_SINH          = 63   ! Sinh
    tInteger, PARAMETER     :: KO_COSH          = 64   ! Cosh
    tInteger, PARAMETER     :: KO_TANH          = 65   ! Tanh
    tInteger, PARAMETER     :: KO_ABS           = 66   ! Abs
    tInteger, PARAMETER     :: KO_KIND          = 67   ! Kind
    tInteger, PARAMETER     :: KO_SIK           = 68   ! Selected_Int_Kind
    tInteger, PARAMETER     :: KO_SRK           = 69   ! Selected_Real_Kind
    tInteger, PARAMETER     :: KO_UnderScore    = 90   ! _   (of kind)
    tInteger, PARAMETER     :: KO_Separator     = 91   ! ,
    tInteger, PARAMETER     :: KO_LParen        = 92   ! (
    tInteger, PARAMETER     :: KO_RParen        = 93   ! )
    tInteger, PARAMETER     :: KO_MODI          = 94   ! .Mod.
    tInteger, PARAMETER     :: KO_MAXI          = 95   ! .Max.
    tInteger, PARAMETER     :: KO_MINI          = 96   ! .Min.
    tInteger, PARAMETER     :: KO_ATAN2I        = 97   ! .Atan2.
    tInteger, PARAMETER     :: KO_SRKI          = 98   ! .S_R_K.
    tInteger, PARAMETER     :: KO_Other         = 99   ! Other
    !
    !  Parameters for variables for f90 expression analysis
    !
    tInteger, PARAMETER     :: NMaxXpr    = LMaxStm
    tInteger, PARAMETER     :: NMaxXprGbl = 8*NMaxXpr

!** DERIVED TYPE DEFINITIONS
    !
    !  type for operator & operand names
    !
    TYPE OPRnOPD
        tDouble     :: DVal
        tInteger    :: IOprd
        tInteger    :: IPrev
        tInteger    :: INext
    END TYPE OPRnOPD
    
!** INTERFACE DEFINITIONS
    ! na

!** GENERIC DECLARATIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    !
    ! ----------------------------------!
    !       Expression_Variables        !
    ! ----------------------------------!
    !
    !  Operator & operand names
    !
    tInteger                :: IW_AND
    tInteger                :: IW_EQ
    tInteger                :: IW_GE
    tInteger                :: IW_GT
    tInteger                :: IW_LE
    tInteger                :: IW_LT
    tInteger                :: IW_NE
    tInteger                :: IW_NOT
    tInteger                :: IW_OR
    !
    tInteger                :: IW_TRUE
    tInteger                :: IW_FALSE
    !
    tInteger                :: IW_INT
    tInteger                :: IW_NINT
    tInteger                :: IW_SIN
    tInteger                :: IW_COS
    tInteger                :: IW_TAN
    tInteger                :: IW_ATAN
    tInteger                :: IW_LOG
    tInteger                :: IW_EXP
    tInteger                :: IW_LOG10
    tInteger                :: IW_SQRT
    tInteger                :: IW_MOD
    tInteger                :: IW_MAX
    tInteger                :: IW_MIN
    tInteger                :: IW_ATAN2
    tInteger                :: IW_ASIN
    tInteger                :: IW_ACOS
    tInteger                :: IW_SINH
    tInteger                :: IW_COSH
    tInteger                :: IW_TANH
    tInteger                :: IW_ABS
    tInteger                :: IW_KIND
    tInteger                :: IW_SIK
    tInteger                :: IW_SRK
    !
    !  variables for pre-defined parameters
    !
    tInteger                :: IW_BlankCmt
    tInteger                :: IW_FalseCmt
    tInteger                :: IW_KeyCase
    tInteger                :: IW_UserCase
    tInteger                :: IW_FixedIn
    tInteger                :: IW_FixedOut
    tInteger                :: IW_UseSharp
    tInteger                :: IW_LMax
    tInteger                :: IW_StepIndent
    tInteger                :: IW_ProgIndent
    tInteger                :: IW_LineNum
    !
    tInteger                :: IW_Leave
    tInteger                :: IW_Lower
    tInteger                :: IW_Upper
    !
    !  Variables for f90 expression analysis
    !
    TYPE(OPRnOPD)           :: OpXpr(1:NMaxXpr)   ! oper[and|ator]
    tInteger                :: IXprGbl = 0

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):
!
MODULE SUBROUTINE Processing_Command (IWNAM, NTOK)
!
!  Process pre-processor command
!
    IMPLICIT NONE
!
! arguments
    tInteger, INTENT (IN)   :: IWNAM
    tInteger, INTENT (IN)   :: NTOK
!
! parameters
    tCharLen (1), PARAMETER :: ZDLM = ACHAR (1)
!
! local variables
    tInteger, SAVE          :: IFLEV = 0
    tInteger, SAVE          :: IFLEVR = 0
    tInteger                :: LZFIC
    tInteger                :: INAMOD
    tInteger                :: INAMOF
    tInteger, SAVE          :: IFDONT(NMax_NestIf) = 0
    tInteger                :: INAMAT(NMaxArg)
    tCharLen (2*LMaxStm)    :: ZSTTW
    tCharLen (LMaxStm)      :: ZTOKW
    tCharLen (LMaxLine)     :: ZREPW
    tCharLen (2)            :: ZNUM
    tCharLen (LMax_NameInc) :: ZFIC
    tLogical                :: IFEVL
    tInteger    :: ITOK, ITOKD, ITOKF, KWTOK, INAM, KERR, KCCUR, INAM1
    tInteger    :: JVAL, LNAM, INAMS, IREPS, IREPD, ITOKN, ITOK1, KRTOK
    tInteger    :: KREP, ISLH, ITOK0, IFVLD, NARG, NTOK1, LSTTW, IARG
    tInteger    :: INAMD, INAMF, IIND, LSTTW1, KKTOK, LTOKW, ISTT
!
! execution
!
    BODY: DO
!
!   $DEFINE
!
        IF (IWNAM == IW_Define) THEN
            IF (IfSkip /= 0) EXIT BODY
            ITOK = 2
            IF (ITOK > NTOK) EXIT BODY
            ITOKD = IV_Start (ITOK)
            ITOKF = IV_End (ITOK)
            IF (KK_TokT(ITOK) == KK_Identifier) THEN
                CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, KR_Unknown, &
                                         KWTOK, INAM)
!
!   pre-defined keyword
!
                IF (TabName(INAM)%KeyVar == KW_PreDefNum .OR. &
                    TabName(INAM)%KeyVar == KW_PreDefStr) THEN
!
!   Blank comment lines
!
                    IF (INAM == IW_BlankCmt) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            ZV_Blank = ' '
                            EXIT BODY
                        ELSE
                            CALL Lexing_Expression (ITOK, NTOK, KERR)
                            IF (KERR /= 0) EXIT BODY
                            CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                            IF (KERR /= 0) EXIT BODY
                            IF (IFEVL) THEN
                                ZV_Blank = ' '
                            ELSE
                                ZV_Blank = '!'
                            END IF
                        END IF
!
!   False comments
!
                    ELSE IF (INAM == IW_FalseCmt) THEN
                        IF (N_FalseCmt == NMax_FalseCmt) THEN
                            CALL Outputing_Error ("Too many false comments")
                            EXIT BODY
                        END IF
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            CALL Outputing_Error ("Missing comment symbol")
                            EXIT BODY
                        END IF
                        IF (NTOK == 4 .AND. KK_TokT(ITOK) == KK_Command         .AND. &
                            IV_Start(ITOK) == IV_End(ITOK)                      .AND. &
                            ZV_TokIdf(IV_Start(NTOK)+1:IV_Start(NTOK)+1) == "!" .AND. &
                            KK_TokT(NTOK) == KK_ChrStr) THEN
                            ITOKD = IV_Start (NTOK) + 1
                            ITOKF = IV_End (NTOK) - 1
                        ELSE IF (KK_TokT(ITOK) == KK_Comment .OR. KK_TokT(ITOK) == KK_EmbCmt) THEN
                            ITOKD = IV_Start (ITOK)
                            ITOKF = IV_End (ITOK)
                        ELSE
                            CALL Outputing_Error ("Symbol is not comment")
                            EXIT BODY
                        END IF
                        N_FalseCmt = N_FalseCmt + 1
                        ZV_FalseCmt (N_FalseCmt) = ZV_TokIdf (ITOKD:ITOKF)
!
!   Case processing
!
                    ELSE IF (INAM == IW_KeyCase .OR. INAM == IW_UserCase) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            KCCUR = KC_Leave
                            EXIT BODY
                        END IF
                        IF (KK_TokT(ITOK) == KK_Identifier) THEN
                            ITOKD = IV_Start (ITOK)
                            ITOKF = IV_End (ITOK)
                            CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, &
                                                     KR_Unknown, KWTOK, INAM1)
                            IF (INAM1 == IW_Leave) THEN
                                KCCUR = KC_Leave
                            ELSE IF (INAM1 == IW_Lower) THEN
                                KCCUR = KC_Lower
                            ELSE IF (INAM1 == IW_Upper) THEN
                                KCCUR = KC_Upper
                            ELSE
                                CALL Lexing_Expression (ITOK, NTOK, KERR)
                                IF (KERR /= 0) EXIT BODY
                                CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                                IF (KERR /= 0) EXIT BODY
                                KCCUR = Nint (OpXpr(1)%DVal)
                                IF (KCCUR /= KC_Leave .AND. KCCUR /= KC_Lower .AND. &
                                    KCCUR /= KC_Upper) THEN
                                    CALL Outputing_Error ("Not a case definition")
                                    EXIT BODY
                                END IF
                            END IF
                        ELSE
                            CALL Lexing_Expression (ITOK, NTOK, KERR)
                            IF (KERR /= 0) EXIT BODY
                            CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                            IF (KERR /= 0) EXIT BODY
                            KCCUR = Nint (OpXpr(1)%DVal)
                            IF (KCCUR /= KC_Leave .AND. KCCUR /= KC_Lower .AND. &
                                KCCUR /= KC_Upper) THEN
                                CALL Outputing_Error ("Not a case definition")
                                EXIT BODY
                            END IF
                        END IF
                        IF (INAM == IW_KeyCase) THEN
                            KC_Keyword = KCCUR
                        ELSE IF (INAM == IW_UserCase) THEN
                            KC_UserIdf = KCCUR
                        END IF
!
!   Line length for splitting
!
                    ELSE IF (INAM == IW_LMax) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            EXIT BODY
                        END IF
                        CALL Lexing_Expression (ITOK, NTOK, KERR)
                        IF (KERR /= 0) EXIT BODY
                        CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                        IF (KERR /= 0) EXIT BODY
                        JVAL = Nint (OpXpr(1)%DVal)
                        IF (JVAL < 2 .OR. JVAL > LMaxLine) THEN
                            CALL Outputing_Error ("Not an allowed line length")
                            EXIT BODY
                        END IF
                        LineLen = JVAL
!
!   step for indentation
!
                    ELSE IF (INAM == IW_StepIndent) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            EXIT BODY
                        END IF
                        CALL Lexing_Expression (ITOK, NTOK, KERR)
                        IF (KERR /= 0) EXIT BODY
                        CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                        IF (KERR /= 0) EXIT BODY
                        JVAL = Nint (OpXpr(1)%DVal)
                        IF (JVAL < 0 .OR. JVAL > LMaxLine/2) THEN
                            CALL Outputing_Error ("Not an allowed indentation step")
                            EXIT BODY
                        END IF
                        NIndentStep = JVAL
!
!   additional indentation for program unit
!
                    ELSE IF (INAM == IW_ProgIndent) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            EXIT BODY
                        END IF
                        CALL Lexing_Expression (ITOK, NTOK, KERR)
                        IF (KERR /= 0) EXIT BODY
                        CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                        IF (KERR /= 0) EXIT BODY
                        JVAL = Nint (OpXpr(1)%DVal)
                        IF (JVAL <-NIndentStep .OR. JVAL > LMaxLine/4) THEN
                            CALL Outputing_Error ("Not an allowed program unit indentation")
                            EXIT BODY
                        END IF
                        NIndentProg = JVAL
!
!   Output line numbering information
!
                    ELSE IF (INAM == IW_LineNum) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            IfLineNumber = 1
                            EXIT BODY
                        ELSE
                            CALL Lexing_Expression (ITOK, NTOK, KERR)
                            IF (KERR /= 0) EXIT BODY
                            CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                            IF (KERR /= 0) EXIT BODY
                            IF (IFEVL) THEN
                                IfLineNumber = 1
                            ELSE
                                IfLineNumber = 0
                            END IF
                        END IF
!
!   Input in Free or Fixed form
!
                    ELSE IF (INAM == IW_FixedIn) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            IfFixedIn = 1
                            EXIT BODY
                        ELSE
                            CALL Lexing_Expression (ITOK, NTOK, KERR)
                            IF (KERR /= 0) EXIT BODY
                            CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                            IF (KERR /= 0) EXIT BODY
                            IF (IFEVL) THEN
                                IfFixedIn = 1
                            ELSE
                                IfFixedIn = 0
                            END IF
                        END IF
!
!   Output in Free or Fixed form
!
                    ELSE IF (INAM == IW_FixedOut) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            IfFixedOut = 1
                            EXIT BODY
                        ELSE
                            CALL Lexing_Expression (ITOK, NTOK, KERR)
                            IF (KERR /= 0) EXIT BODY
                            CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                            IF (KERR /= 0) EXIT BODY
                            IF (IFEVL) THEN
                                IfFixedOut = 1
                            ELSE
                                IfFixedOut = 0
                            END IF
                        END IF
!
!   Treat Sharp as dollar
!
                    ELSE IF (INAM == IW_UseSharp) THEN
                        ITOK = 3
                        IF (NTOK < ITOK) THEN
                            IfSharpSameAsDollar = 1
                            EXIT BODY
                        ELSE
                            CALL Lexing_Expression (ITOK, NTOK, KERR)
                            IF (KERR /= 0) EXIT BODY
                            CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                            IF (KERR /= 0) EXIT BODY
                            IF (IFEVL) THEN
                                IfSharpSameAsDollar = 1
                            ELSE
                                IfSharpSameAsDollar = 0
                            END IF
                        END IF
                    END IF
                    EXIT BODY
                END IF
                TabName(INAM)%KeyVar = KW_DefName
!
!   $"xxxxx" (not to be analysed), possibly multiple instruction
!
                ITOK = 3
                IF (NTOK == 4 .AND. KK_TokT(ITOK) == KK_Command .AND. &
                    IV_Start(ITOK) == IV_End(ITOK) .AND. KK_TokT(NTOK) == KK_ChrStr) THEN
                    ITOKD = IV_Start (NTOK) + 1
                    ITOKF = IV_End (NTOK) - 1
                    LNAM = ITOKF - ITOKD + 1
                    INAMS = IV_NameOut + 1
                    IV_NameOut = IV_NameOut + LNAM
                    IF (IV_NameOut > NMaxIdf*LAvgName*2) THEN
                        CALL Outputing_Error ("insufficient name space (out. idf. names)")
                        CALL Outputing_Error ("raise length of global chain and try again")
                        STOP
                    END IF
                    TabName(INAM)%IStartOut = INAMS
                    TabName(INAM)%IEndOut = IV_NameOut
                    ZV_NameOut (INAMS:IV_NameOut) = ZV_TokIdf (ITOKD:ITOKF)
!
!   expression (to be analysed)
!
                ELSE IF (ITOK <= NTOK) THEN
                    ITOKD = IV_Start (ITOK)
                    ITOKF = IV_End (NTOK)
                    LNAM = ITOKF - ITOKD + 1
                    INAMS = IV_NameOut + 1
                    IV_NameOut = IV_NameOut + LNAM
                    IF (IV_NameOut > NMaxIdf*LAvgName*2) THEN
                        CALL Outputing_Error ("insufficient name space (out. idf. names)")
                        CALL Outputing_Error ("raise length of global chain and try again")
                        STOP
                    END IF
                    IREPS = IRepGbl + 1
                    IRepGbl = IRepGbl + LNAM
                    IF (IRepGbl > NMaxRepGbl) THEN
                        CALL Outputing_Error ("insufficient name space (out. repl. names)")
                        CALL Outputing_Error ("raise length of global chain and try again")
                        STOP
                    END IF
                    IREPD = IRep + 1
                    IRep = IRep + (NTOK-ITOK+1)
                    IF (IRep > NMaxRep) THEN
                        CALL Outputing_Error ("insufficient name space (out. repl. names)")
                        CALL Outputing_Error ("raise # of repl. names and try again")
                        STOP
                    END IF
                    TabName(INAM)%IRepC = IREPD
                    TabName(INAM)%IStartOut = INAMS
                    TabName(INAM)%IEndOut = IV_NameOut
                    ZV_NameOut (INAMS:IV_NameOut) = ZV_TokIdf (ITOKD:ITOKF)
                    ZV_RepGbl (IREPS:IRepGbl) = ZV_TokIdf (ITOKD:ITOKF)
                    ITOKN = IV_End (NTOK)
                    DO ITOK1 = ITOK, NTOK
                        ITOKD = IV_Start (ITOK1)
                        ITOKF = IV_End (ITOK1)
                        KK_RepT (IREPD) = KK_TokT (ITOK1)
                        IF (KK_RepT(IREPD) == KK_Identifier) THEN
                            IF (ITOK1 == NTOK) THEN
                                KRTOK = KR_Any
                            ELSE
                                SELECT CASE (KK_TokT(ITOK1+1))
                                CASE (KK_ChrStr)
                                    KRTOK = KR_String
                                CASE (KK_LParen)
                                    KRTOK = KR_LParen
                                CASE DEFAULT
                                    KRTOK = KR_Any
                                END SELECT
                            END IF
                            CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Any, &
                                                     KRTOK, KWTOK, IRepName(IREPD))
                        END IF
                        IRepStart (IREPD) = ITOKD + IRepGbl - ITOKN
                        IRepEnd (IREPD) = ITOKF + IRepGbl - ITOKN
                        IRepNext (IREPD) = IREPD + 1
                        IREPD = IREPD + 1
                    END DO
                    IRepNext (IRep) = 0
                END IF
                EXIT BODY
            ELSE
                CALL Outputing_Error ("Cannot redefine keyword"//ZV_TokIdf(ITOKD:ITOKF))
                EXIT BODY
            END IF
!
!   $UNDEF
!
        ELSE IF (IWNAM == IW_Undef) THEN
            IF (IfSkip /= 0) EXIT BODY
            ITOK = 2
            IF (ITOK > NTOK) EXIT BODY
            ITOKD = IV_Start (ITOK)
            ITOKF = IV_End (ITOK)
            IF (KK_TokT(ITOK) == KK_Identifier) THEN
                CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, KR_Unknown, &
                                         KWTOK, INAM)
                IF (TabName(INAM)%KeyVar == KW_DefName) THEN
                    LNAM = ITOKF - ITOKD + 1
                    INAMS = IV_NameOut + 1
                    IV_NameOut = IV_NameOut + LNAM
                    IF (IV_NameOut > NMaxIdf*LAvgName*2) THEN
                        CALL Outputing_Error ("insufficient name space (out. idf. names)")
                        CALL Outputing_Error ("raise length of global chain and try again")
                        STOP
                    END IF
                    TabName(INAM)%KeyVar = KW_VarName
                    TabName(INAM)%IStartOut = INAMS
                    TabName(INAM)%IEndOut = IV_NameOut
                    ZV_NameOut (INAMS:IV_NameOut) = ZV_TokIdf (ITOKD:ITOKF)
                END IF
                EXIT BODY
            ELSE
                CALL Outputing_Error ("Cannot undefine keyword"//ZV_TokIdf(ITOKD:ITOKF))
                EXIT BODY
            END IF
!
!   $EVAL
!
        ELSE IF (IWNAM == IW_Eval) THEN
            IF (IfSkip /= 0) EXIT BODY
            ITOK = 2
            IF (ITOK > NTOK) EXIT BODY
            ITOKD = IV_Start (ITOK)
            ITOKF = IV_End (ITOK)
            IF (KK_TokT(ITOK) == KK_Identifier) THEN
                CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, KR_Unknown, &
                                         KWTOK, INAM)
                ITOK = 3
                IF (NTOK < ITOK) THEN
                    ZREPW = ".True."
                ELSE
                    CALL Lexing_Expression (ITOK, NTOK, KERR)
                    IF (KERR /= 0) EXIT BODY
                    CALL Evaluating_Expression_As_String (ZREPW, KREP, KERR)
                    IF (KERR /= 0) EXIT BODY
                END IF
                TabName(INAM)%KeyVar = KW_DefName
!
                LNAM = LEN_TRIM (ZREPW)
                INAMS = IV_NameOut + 1
                IV_NameOut = IV_NameOut + LNAM
                IF (IV_NameOut > NMaxIdf*LAvgName*2) THEN
                    CALL Outputing_Error ("insufficient name space (out. idf. names)")
                    CALL Outputing_Error ("raise length of global chain and try again")
                    STOP
                END IF
                IREPS = IRepGbl + 1
                IRepGbl = IRepGbl + LNAM
                IF (IRepGbl > NMaxRepGbl) THEN
                    CALL Outputing_Error ("insufficient name space (out. repl. names)")
                    CALL Outputing_Error ("raise length of global chain and try again")
                    STOP
                END IF
                IREPD = IRep + 1
                IRep = IRep + 1
                IF (IRep > NMaxRep) THEN
                    CALL Outputing_Error ("insufficient name space (out. repl. names)")
                    CALL Outputing_Error ("raise # of repl. names and try again")
                    STOP
                END IF
                TabName(INAM)%IRepC = IREPD
                TabName(INAM)%IStartOut = INAMS
                TabName(INAM)%IEndOut = IV_NameOut
                ZV_NameOut (INAMS:IV_NameOut) = TRIM (ZREPW)
                ZV_RepGbl (IREPS:IRepGbl) = TRIM (ZREPW)
                ITOKN = IV_End (NTOK)
                SELECT CASE (KREP)
                CASE (KO_LogNumVal)
                    KK_RepT (IREPD) = KK_LogOp
                CASE (KO_IntNumVal)
                    KK_RepT (IREPD) = KK_IntNumVal
                CASE (KO_RealNumVal)
                    KK_RepT (IREPD) = KK_RealNumVal
                CASE DEFAULT
                    KK_RepT (IREPD) = KK_Undef
                END SELECT
                IRepStart (IREPD) = IRepGbl + 1 - LEN_TRIM (ZREPW)
                IRepEnd (IREPD) = IRepGbl
                IRepNext (IREPD) = IREPD + 1
                IREPD = IREPD + 1
                IRepNext (IRep) = 0
                EXIT BODY
            ELSE
                CALL Outputing_Error ("Cannot redefine keyword"//ZV_TokIdf(ITOKD:ITOKF))
                EXIT BODY
            END IF
!
!   $IF
!
        ELSE IF (IWNAM == IW_If) THEN
            ITOK = 2
            IF (ITOK > NTOK) EXIT BODY
            IF (IFLEV >=NMax_NestIf) THEN
                CALL Outputing_Error ("Tests are too deeply nested")
                EXIT BODY
            END IF
            IFLEV = IFLEV + 1
            IFDONT (IFLEV) = - 1
            IF (IfSkip == 0) THEN
                CALL Lexing_Expression (ITOK, NTOK, KERR)
                IF (KERR /= 0) EXIT BODY
                CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                IF (KERR /= 0) EXIT BODY
                IF (IFEVL) THEN
                    IfSkip = 0
                    IFDONT (IFLEV) = 1
                    IFLEVR = IFLEV
                    EXIT BODY
                ELSE
                    IfSkip = 1
                    EXIT BODY
                END IF
            END IF
!
!   $IFDEF
!
        ELSE IF (IWNAM == IW_IfDef) THEN
            ITOK = 2
            IF (ITOK > NTOK) EXIT BODY
            IF (IFLEV >=NMax_NestIf) THEN
                CALL Outputing_Error ("Tests are too deeply nested")
                EXIT BODY
            END IF
            IFLEV = IFLEV + 1
            IFDONT (IFLEV) = - 2
            IF (IfSkip == 0) THEN
                ITOKD = IV_Start (ITOK)
                ITOKF = IV_End (ITOK)
                IF (KK_TokT(ITOK) == KK_Identifier) THEN
                    CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, KR_Unknown, &
                                             KWTOK, INAM)
                    IF (TabName(INAM)%KeyVar == KW_DefName) THEN
                        IfSkip = 0
                        IFDONT (IFLEV) = 2
                        IFLEVR = IFLEV
                    ELSE
                        IfSkip = 1
                    END IF
                    EXIT BODY
                ELSE
                    CALL Outputing_Error ("Cannot test word "//ZV_TokIdf(ITOKD:ITOKF))
                    EXIT BODY
                END IF
            END IF
!
!   $IFNDEF
!
        ELSE IF (IWNAM == IW_IfNDef) THEN
            ITOK = 2
            IF (ITOK > NTOK) EXIT BODY
            IF (IFLEV >=NMax_NestIf) THEN
                CALL Outputing_Error ("Tests are too deeply nested")
                EXIT BODY
            END IF
            IFLEV = IFLEV + 1
            IFDONT (IFLEV) = - 2
            IF (IfSkip == 0) THEN
                ITOKD = IV_Start (ITOK)
                ITOKF = IV_End (ITOK)
                IF (KK_TokT(ITOK) == KK_Identifier) THEN
                    CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, KR_Unknown, &
                                             KWTOK, INAM)
                    IF (TabName(INAM)%KeyVar /= KW_DefName) THEN
                        IfSkip = 0
                        IFDONT (IFLEV) = 2
                        IFLEVR = IFLEV
                    ELSE
                        IfSkip = 1
                    END IF
                ELSE
                    CALL Outputing_Error ("Cannot test word "//ZV_TokIdf(ITOKD:ITOKF))
                END IF
            END IF
!
!   $ELSE
!
        ELSE IF (IWNAM == IW_Else) THEN
            IF (IFLEVR == IFLEV-1) THEN
                IFLEVR = IFLEVR + 1
                IF (IFDONT(IFLEVR) <= 0) THEN
                    IfSkip = 0
                    IFDONT (IFLEVR) = Abs (IFDONT(IFLEV))
                ELSE
                    IfSkip = 1
                END IF
            ELSE IF (IFLEVR == IFLEV) THEN
                IFLEVR = IFLEVR - 1
                IfSkip = 1
            END IF
!
!   $ELIF
!
        ELSE IF (IWNAM == IW_ElseIf) THEN
            ITOK = 2
            IF (ITOK > NTOK) EXIT BODY
            IF (IFLEVR == IFLEV-1) THEN
                IFLEVR = IFLEVR + 1
                IF (IFDONT(IFLEVR) ==-1) THEN
                    CALL Lexing_Expression (ITOK, NTOK, KERR)
                    IF (KERR /= 0) EXIT BODY
                    CALL Evaluating_Expression_As_Logical (IFEVL, KERR)
                    IF (KERR /= 0) EXIT BODY
                    IF (IFEVL) THEN
                        IfSkip = 0
                        IFDONT (IFLEVR) = 1
                    ELSE
                        IfSkip = 1
                        IFLEVR = IFLEVR - 1
                    END IF
                ELSE IF (IFDONT(IFLEVR) ==-2) THEN
                    ITOKD = IV_Start (ITOK)
                    ITOKF = IV_End (ITOK)
                    IF (KK_TokT(ITOK) == KK_Identifier) THEN
                        CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, &
                                                 KR_Unknown, KWTOK, INAM)
                        IF (TabName(INAM)%KeyVar == KW_DefName) THEN
                            IfSkip = 0
                            IFDONT (IFLEVR) = 2
                        ELSE
                            IfSkip = 1
                            IFLEVR = IFLEVR - 1
                        END IF
                    ELSE
                        CALL Outputing_Error ("Cannot test word "//ZV_TokIdf(ITOKD:ITOKF))
                        EXIT BODY
                    END IF
                ELSE
                    IfSkip = 1
                END IF
            ELSE IF (IFLEVR == IFLEV) THEN
                IFLEVR = IFLEVR - 1
                IfSkip = 1
            END IF
!
!   $ENDIF
!
        ELSE IF (IWNAM == IW_EndIf) THEN
            IFLEV = IFLEV - 1
            IF (IFLEVR >=IFLEV) THEN
                IFLEVR = IFLEV
                IfSkip = 0
            END IF
!
!   $INCLUDE
!
        ELSE IF (IWNAM == IW_Include) THEN
            IF (IfSkip /= 0) EXIT BODY
            ITOK = 2
            IF (ITOK > NTOK) EXIT BODY
            ITOKD = IV_Start (ITOK)
            ITOKF = IV_End (ITOK)
            SELECT CASE (KK_TokT(ITOK))
            CASE (KK_ChrStr)
                ITOKD = ITOKD + 1
                ITOKF = ITOKF - 1
                ZFIC = ZV_TokIdf (ITOKD:ITOKF)
                LZFIC = ITOKF - ITOKD + 1
            CASE (KK_Identifier)
                CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, KR_Unknown, &
                                         KWTOK, INAM)
                IF (TabName(INAM)%IRepC /= 0) THEN
                    IF (KK_RepT(TabName(INAM)%IRepC) == KK_ChrStr) THEN
                        INAMOD = TabName(INAM)%IStartOut + 1
                        INAMOF = TabName(INAM)%IEndOut - 1
                        ZFIC = ZV_NameOut (INAMOD:INAMOF)
                        LZFIC = INAMOF - INAMOD + 1
                    ELSE
                        CALL Outputing_Error ("Include file name "   // &
                                              ZV_TokIdf(ITOKD:ITOKF) // &
                                              " must evaluate to string")
                        EXIT BODY
                    END IF
                ELSE
                    CALL Outputing_Error ("Include file name " // &
                                          ZV_TokIdf(ITOKD:ITOKF) // " is undefined")
                    EXIT BODY
                END IF
            CASE DEFAULT
                CALL Outputing_Error ("Include file name must be string " // &
                                      "or evaluate to string")
                EXIT BODY
            END SELECT
!
            LU_Include = LU_Include + 1
            OPEN (LU_Include, FILE=ZFIC(1:LZFIC), ACTION="read", IOSTAT=KERR)
            IF (KERR /= 0) THEN
                CALL Outputing_Error ("Unable to open include file: "//ZFIC(1:LZFIC))
                CLOSE (LU_Include, IOSTAT=KERR)
                LU_Include = LU_Include - 1
                EXIT BODY
            END IF
            IF (ILevel_Include < NMax_NestInc) THEN
                ILevel_Include = ILevel_Include + 1
            ELSE
                CALL Outputing_Error ("Include files too deeply nested")
                EXIT BODY
            END IF
            ISLH = INDEX (ZFIC(1:LZFIC), '/', BACK=.TRUE.)
            IF (ISLH /= 0) THEN
                ZFIC = ZFIC (ISLH:LZFIC)
                LZFIC = LZFIC + ISLH - 1
            END IF
            ZV_TabName (ILevel_Include) = ZFIC (1:LZFIC)
            NLineInput (ILevel_Include) = 0
            ZV_LineHeap (ILevel_Include) = ZV_LineBuff
            N_HaveHeap (ILevel_Include) = N_HaveBuff
            N_HaveBuff = 0
            KL_NextHeap (ILevel_Include) = KL_Next
            KL_Current = KL_Normal
            LU_Input = LU_Include
!
!   $MACRO
!
        ELSE IF (IWNAM == IW_Macro) THEN
            IF (IfSkip /= 0) EXIT BODY
            IF (NTOK < 6) EXIT BODY
!
!   Analyse Macro to check that it is valid
!
            ITOK0 = 2
            MACA: DO
                IF (KK_TokT(ITOK0) /= KK_Identifier .OR. KK_TokT(ITOK0+1) /= KK_LParen) THEN
                    IFVLD = 0
                    EXIT MACA
                END IF
                NARG = 1
                DO ITOK = ITOK0 + 3, NTOK, 2
                    IF (KK_TokT(ITOK-1) /= KK_Identifier) THEN
                        IFVLD = 0
                        EXIT MACA
                    ELSE
                        ITOKD = IV_Start (ITOK-1)
                        ITOKF = IV_End (ITOK-1)
                        CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, &
                                                 KR_Unknown, KWTOK, INAM)
                        INAMAT (NARG) = INAM
                    END IF
                    IF (KK_TokT(ITOK) == KK_Comma) THEN
                        NARG = NARG + 1
                        CYCLE
                    ELSE IF (KK_TokT(ITOK) == KK_RParen) THEN
                        IFVLD = 1
                        EXIT MACA
                    ELSE
                        IFVLD = 0
                        EXIT MACA
                    END IF
                END DO
                IFVLD = 0
                EXIT MACA
            END DO MACA
            ITOKD = IV_Start (ITOK0)
            ITOKF = IV_End (ITOK0)
            IF (IFVLD /= 0) THEN
                CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, KR_Unknown, &
                                         KWTOK, INAM)
                TabName(INAM)%KeyVar = KW_MacroT (NARG)
                TabName(INAM)%IRepC = IRep + 1
                IF (KK_TokT(NTOK) == KK_Comment .OR. KK_TokT(NTOK) == KK_EmbCmt) THEN
                    NTOK1 = NTOK - 1
                ELSE
                    NTOK1 = NTOK
                END IF
                ITOK = ITOK + 1
!
!   $"xxxxx" (not to be analysed), possibly multiple instruction
!
                IF (NTOK1 == ITOK+1 .AND. KK_TokT(ITOK) == KK_Command .AND. &
                    IV_Start(ITOK) == IV_End(ITOK) .AND. KK_TokT(NTOK1) == KK_ChrStr) THEN
                    ITOKD = IV_Start (NTOK1) + 1
                    ITOKF = IV_End (NTOK1) - 1
                    LSTTW = ITOKF - ITOKD + 4
!
!   Use non-printable char as delim
!
                    ZSTTW (1:LSTTW) = ZDLM // " " // ZV_TokIdf (ITOKD:ITOKF) // ZDLM
                    DO IARG = 1, NARG
                        INAM = INAMAT (IARG)
                        INAMD = TabName(INAM)%IStartGbl
                        INAMF = TabName(INAM)%IEndGbl
                        LNAM = INAMF - INAMD + 1
                        WRITE (ZNUM, "(i2)") IARG
                        DO
                            IIND = INDEX (ZSTTW(1:LSTTW), ZV_NameGbl(INAMD:INAMF))
                            IF (IIND == 0) THEN
                                EXIT
                            ELSE
                                LSTTW1 = LSTTW - LNAM + 4
                                ZSTTW (1:LSTTW1) = ZSTTW (1:IIND-1) // ZDLM // ZNUM // &
                                                   ZDLM // ZSTTW (IIND+LNAM:LSTTW)
                                LSTTW = LSTTW1
                            END IF
                        END DO
                    END DO
!
!   zsttw is now "xxx"nn"xxxxx"nn"xxxxx". Build token chain
!
                    KKTOK = KK_Unknown
                    LTOKW = 0
                    ISTT = 2
                    DO
                        IF (ISTT > LSTTW) THEN
                            EXIT
                        END IF
                        IF (ZSTTW(ISTT:ISTT) == ZDLM .AND. KKTOK == KK_Unknown) THEN
                            IF (LTOKW /= 0) THEN
                                IREPS = IRepGbl + 1
                                IRepGbl = IRepGbl + LTOKW
                                IF (IRepGbl > NMaxRepGbl) THEN
                                    CALL Outputing_Error ("insufficient name space (out. repl. names)")
                                    CALL Outputing_Error ("raise length of global chain and try again")
                                    STOP
                                END IF
                                IRep = IRep + 1
                                IF (IRep > NMaxRep) THEN
                                    CALL Outputing_Error ("insufficient name space (out. repl. names)")
                                    CALL Outputing_Error ("raise # of repl. names and try again")
                                    STOP
                                END IF
                                ZV_RepGbl (IREPS:IRepGbl) = ZTOKW (1:LTOKW)
                                KK_RepT (IRep) = KKTOK
                                IRepStart (IRep) = IREPS
                                IRepEnd (IRep) = IRepGbl
                                IRepNext (IRep) = IRep + 1
                            END IF
                            KKTOK = KK_Arg0
                            LTOKW = 0
                        ELSE IF (ZSTTW(ISTT:ISTT) == ZDLM .AND. KKTOK == KK_Arg0) THEN
                            IRep = IRep + 1
                            IF (IRep > NMaxRep) THEN
                                CALL Outputing_Error ("insufficient name space (out. repl. names)")
                                CALL Outputing_Error ("raise # of repl. names and try again")
                                STOP
                            END IF
                            READ (ZTOKW(1:LTOKW), "(i2)") IARG
                            KK_RepT (IRep) = KK_ArgT (IARG)
                            IRepStart (IRep) = 0
                            IRepEnd (IRep) = 0
                            IRepNext (IRep) = IRep + 1
                            KKTOK = KK_Unknown
                            LTOKW = 0
                        ELSE
                            LTOKW = LTOKW + 1
                            ZTOKW (LTOKW:LTOKW) = ZSTTW (ISTT:ISTT)
                        END IF
                        ISTT = ISTT + 1
                    END DO
                    IRepNext (IRep) = 0
!
!   expression (to be analysed)
!
                ELSE IF (ITOK <= NTOK1) THEN
                    ITOKD = IV_Start (ITOK)
                    ITOKF = IV_End (NTOK1)
                    LNAM = ITOKF - ITOKD + 1
                    INAMS = IV_NameOut + 1
                    IV_NameOut = IV_NameOut + LNAM
                    IF (IV_NameOut > NMaxIdf*LAvgName*2) THEN
                        CALL Outputing_Error ("insufficient name space (out. idf. names)")
                        CALL Outputing_Error ("raise length of global chain and try again")
                        STOP
                    END IF
                    IREPS = IRepGbl + 1
                    IRepGbl = IRepGbl + LNAM
                    IF (IRepGbl > NMaxRepGbl) THEN
                        CALL Outputing_Error ("insufficient name space (out. repl. names)")
                        CALL Outputing_Error ("raise length of global chain and try again")
                        STOP
                    END IF
                    IREPD = IRep + 1
                    IRep = IRep + (NTOK1-ITOK+1)
                    IF (IRep > NMaxRep) THEN
                        CALL Outputing_Error ("insufficient name space (out. repl. names)")
                        CALL Outputing_Error ("raise # of repl. names and try again")
                        STOP
                    END IF
                    TabName(INAM)%IRepC = IREPD
                    TabName(INAM)%IStartOut = INAMS
                    TabName(INAM)%IEndOut = IV_NameOut
                    ZV_NameOut (INAMS:IV_NameOut) = ZV_TokIdf (ITOKD:ITOKF)
                    ZV_RepGbl (IREPS:IRepGbl) = ZV_TokIdf (ITOKD:ITOKF)
                    ITOKN = IV_End (NTOK1)
                    DO ITOK1 = ITOK, NTOK1
                        ITOKD = IV_Start (ITOK1)
                        ITOKF = IV_End (ITOK1)
                        KK_RepT (IREPD) = KK_TokT (ITOK1)
                        IF (KK_RepT(IREPD) == KK_Identifier) THEN
                            CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Unknown, &
                                                     KR_Unknown, KWTOK, IRepName(IREPD))
                            DO IARG = 1, NARG
                                IF (IRepName(IREPD) == INAMAT(IARG)) THEN
                                    KK_RepT (IREPD) = KK_ArgT (IARG)
                                    EXIT
                                END IF
                            END DO
                        END IF
                        IRepStart (IREPD) = ITOKD + IRepGbl - ITOKN
                        IRepEnd (IREPD) = ITOKF + IRepGbl - ITOKN
                        IRepNext (IREPD) = IREPD + 1
                        IREPD = IREPD + 1
                    END DO
                    IRepNext (IRep) = 0
                END IF
                EXIT BODY
            ELSE
                CALL Outputing_Error ("Illegal macro expression for "//ZV_TokIdf(ITOKD:ITOKF))
                EXIT BODY
            END IF
!
        END IF
        EXIT BODY
    END DO BODY
!
    RETURN
END SUBROUTINE Processing_Command
!
!******************************************************************************
!
MODULE SUBROUTINE Initializing_Expressions
!
!  Initialize pointers to expression operators
!
    IMPLICIT NONE
!
! execution
!
    IW_AND = IW_Name (ZO_AND)
    IW_EQ  = IW_Name (ZO_EQ)
    IW_GE  = IW_Name (ZO_GE)
    IW_GT  = IW_Name (ZO_GT)
    IW_LE  = IW_Name (ZO_LE)
    IW_LT  = IW_Name (ZO_LT)
    IW_NE  = IW_Name (ZO_NE)
    IW_NOT = IW_Name (ZO_NOT)
    IW_OR  = IW_Name (ZO_OR)
!
    IW_TRUE  = IW_Name (ZO_TRUE)
    IW_FALSE = IW_Name (ZO_FALSE)
!
    IW_INT   = IW_Name (ZO_INT)
    IW_NINT  = IW_Name (ZO_NINT)
    IW_SIN   = IW_Name (ZO_SIN)
    IW_COS   = IW_Name (ZO_COS)
    IW_TAN   = IW_Name (ZO_TAN)
    IW_ATAN  = IW_Name (ZO_ATAN)
    IW_LOG   = IW_Name (ZO_LOG)
    IW_EXP   = IW_Name (ZO_EXP)
    IW_LOG10 = IW_Name (ZO_LOG10)
    IW_SQRT  = IW_Name (ZO_SQRT)
    IW_MOD   = IW_Name (ZO_MOD)
    IW_MAX   = IW_Name (ZO_MAX)
    IW_MIN   = IW_Name (ZO_MIN)
    IW_ATAN2 = IW_Name (ZO_ATAN2)
    IW_ASIN  = IW_Name (ZO_ASIN)
    IW_ACOS  = IW_Name (ZO_ACOS)
    IW_SINH  = IW_Name (ZO_SINH)
    IW_COSH  = IW_Name (ZO_COSH)
    IW_TANH  = IW_Name (ZO_TANH)
    IW_ABS   = IW_Name (ZO_ABS)
    IW_KIND  = IW_Name (ZO_KIND)
    IW_SIK   = IW_Name (ZO_SIK)
    IW_SRK   = IW_Name (ZO_SRK)
!
    IW_BlankCmt   = IW_Name (ZO_BlankCmt)
    IW_FalseCmt   = IW_Name (ZO_FalseCmt)
    IW_KeyCase    = IW_Name (ZO_KeyCase)
    IW_UserCase   = IW_Name (ZO_UserCase)
    IW_FixedIn    = IW_Name (ZO_FixedIn)
    IW_FixedOut   = IW_Name (ZO_FixedOut)
    IW_UseSharp   = IW_Name (ZO_UseSharp)
    IW_LMax       = IW_Name (ZO_LMax)
    IW_StepIndent = IW_Name (ZO_StepIndent)
    IW_ProgIndent = IW_Name (ZO_ProgIndent)
    IW_LineNum    = IW_Name (ZO_LineNum)
!
    IW_Leave = IW_Name (ZA_Leave)
    IW_Lower = IW_Name (ZA_Lower)
    IW_Upper = IW_Name (ZA_Upper)
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
        tInteger    :: LNAM, IHSH, INAMD, INAMF
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
END SUBROUTINE Initializing_Expressions
!
!******************************************************************************
!
SUBROUTINE Lexing_Expression (ITOK0, NTOK, KERR)
!
!  prepare lexed statement for analysis as an expression
!
    IMPLICIT NONE
!
! arguments
    tInteger, INTENT (IN)   :: ITOK0
    tInteger, INTENT (IN)   :: NTOK
!
! local variables
    tDouble             :: DXPT
    tInteger            :: IREPST(NMax_NestDef)
    tCharLen (LMaxStm)  :: ZTOKW
    tInteger    :: KERR, INSTR, ITOK, IXPRG, ITOKD, ITOKF, KKTOK
    tInteger    :: KWTOK, INAM, KWNAM, IREPD, NARGE, NARG, ITOK1
    tInteger    :: IFVLD, LPAR, LTOKW, IREPS
!
! execution
!
    KERR = 0
    INSTR = 0
    ITOK = ITOK0 - 1
    IXPRG = 0
    IXprGbl = 0
    DO
        IF (INSTR == 0) THEN
            ITOK = ITOK + 1
            IF (ITOK > NTOK) EXIT
            ITOKD = IV_Start (ITOK)
            ITOKF = IV_End (ITOK)
            KKTOK = KK_TokT (ITOK)
            IF (KKTOK == KK_Identifier) THEN
                CALL Testing_Identifier (ZV_TokIdf(ITOKD:ITOKF), KC_Any, KR_Any, KWTOK, INAM)
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
                    ITOK1 = ITOK + 1
                    MACA: DO
                        IF (NTOK < ITOK1+2*NARGE) THEN
                            IFVLD = 0
                            EXIT MACA
                        END IF
                        IF (KK_TokT(ITOK1) /= KK_LParen) THEN
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
                            IREPS = IRepGbl + 1
                            IRepGbl = IRepGbl + LTOKW
                            IF (IRepGbl > NMaxRepGbl) THEN
                                CALL Outputing_Error ("insufficient name space (out. repl. names)")
                                CALL Outputing_Error ("raise length of global chain and try again")
                                STOP
                            END IF
                            IRep = IRep + 1
                            IF (IRep > NMaxRep) THEN
                                CALL Outputing_Error ("insufficient name space (out. repl. names)")
                                CALL Outputing_Error ("raise # of repl. names and try again")
                                STOP
                            END IF
                            KK_RepT (IRep) = KK_TokT (ITOK1)
                            IRepStart (IRep) = IREPS
                            IRepEnd (IRep) = IRepGbl
                            IRepNext (IRep) = IRep + 1
                            ZV_RepGbl (IREPS:IRepGbl) = ZV_TokIdf (ITOKD:ITOKF)
!
! Check for next argument or last one
!
                            SELECT CASE (KK_TokT(ITOK1))
                            CASE (KK_RParen)
                                LPAR = LPAR - 1
                                IF (LPAR == 0) THEN
                                    IF (NARG == NARGE) THEN
                                        IRep = IRep - 1
                                        IRepNext (IRep) = 0
                                        ITOK = ITOK1
                                        IFVLD = 1
                                    ELSE
                                        IFVLD = 0
                                    END IF
                                    EXIT MACA
                                END IF
                            CASE (KK_LParen)
                                LPAR = LPAR + 1
                            CASE (KK_Comma)
                                IF (LPAR == 1) THEN
                                    IRep = IRep - 1
                                    IRepNext (IRep) = 0
                                    NARG = NARG + 1
                                END IF
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
            ELSE
                LTOKW = ITOKF - ITOKD + 1
                ZTOKW (1:LTOKW) = ZV_TokIdf (ITOKD:ITOKF)
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
                        CALL Outputing_Error ("defines nesting overflow")
                        EXIT
                    END IF
                    IREPD = TabName(INAM)%IRepC
                    CYCLE
                ELSE
                    ITOKD = TabName(INAM)%IStartOut
                    ITOKF = TabName(INAM)%IEndOut
                    LTOKW = ITOKF - ITOKD + 1
                    ZTOKW (1:LTOKW) = ZV_NameOut (ITOKD:ITOKF)
                END IF
            END IF
            IREPD = IRepNext (IREPD)
        END IF
        IF (LTOKW+IXPRG > NMaxXprGbl) THEN
            CALL Outputing_Error ("Substitution leads to expression length overflow")
            EXIT
        END IF
        SELECT CASE (KKTOK)
        CASE DEFAULT
            KERR = 1
            CALL Outputing_Error ("term not allowed in expression: "//ZTOKW(1:LTOKW))
            EXIT
        CASE (KK_Comment, KK_EmbCmt)    ! Comment string
            EXIT
!
!  Constants
!
        CASE (KK_IntNumVal)             ! Integer Numerical value
            IXprGbl = IXprGbl + 1
            READ (ZTOKW(1:LTOKW),*, IOSTAT=KERR) DXPT
            IF (KERR /= 0) THEN
                CALL Outputing_Error ("term not allowed in expression: "//ZTOKW(1:LTOKW))
                EXIT
            END IF
            OpXpr (IXprGbl) = OPRnOPD (DXPT, KO_IntNumVal, IXprGbl-1, IXprGbl+1)
        CASE (KK_RealNumVal)            ! Real Numerical value
            IXprGbl = IXprGbl + 1
            READ (ZTOKW(1:LTOKW),*, IOSTAT=KERR) DXPT
            IF (KERR /= 0) THEN
                CALL Outputing_Error ("term not allowed in expression: "//ZTOKW(1:LTOKW))
                EXIT
            END IF
            OpXpr (IXprGbl) = OPRnOPD (DXPT, KO_RealNumVal, IXprGbl-1, IXprGbl+1)
        CASE (KK_Identifier)            ! Identifier
!
!  Old form comparisons
!
            IF (KWNAM == KW_LogOp) THEN
                IF (INAM == IW_AND) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_AND, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_EQ) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_EQ, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_GE) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_GE, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_GT) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_GT, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_LE) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_LE, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_LT) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_LT, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_NE) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_NE, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_NOT) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_NOT, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_OR) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_OR, IXprGbl-1, IXprGbl+1)
                ELSE
                    KERR = 1
                    CALL Outputing_Error ("term not allowed in expression: "//ZTOKW(1:LTOKW))
                    EXIT
                END IF
!
!  Logical constants
!
            ELSE IF (KWNAM == KW_LogConst) THEN
                IF (INAM == IW_TRUE) THEN
                    IXprGbl = IXprGbl + 1
                    DXPT = 1.0D0
                    OpXpr (IXprGbl) = OPRnOPD (DXPT, KO_IntNumVal, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_FALSE) THEN
                    IXprGbl = IXprGbl + 1
                    DXPT = 0.0D0
                    OpXpr (IXprGbl) = OPRnOPD (DXPT, KO_IntNumVal, IXprGbl-1, IXprGbl+1)
                ELSE
                    KERR = 1
                    CALL Outputing_Error ("term not allowed in expression: "//ZTOKW(1:LTOKW))
                    EXIT
                END IF
!
!  Known intrinsics
!
            ELSE IF (KWNAM == KW_Intrinsic) THEN
                IF (INAM == IW_INT) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_INT, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_NINT) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_NINT, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_SIN) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_SIN, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_COS) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_COS, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_TAN) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_TAN, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_ATAN) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_ATAN, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_LOG) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_LOG, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_EXP) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_EXP, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_LOG10) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_LOG10, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_SQRT) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_SQRT, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_MOD) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_MOD, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_MAX) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_MAX, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_MIN) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_MIN, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_ATAN2) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_ATAN2, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_ASIN) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_ASIN, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_ACOS) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_ACOS, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_SINH) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_SINH, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_COSH) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_COSH, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_TANH) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_TANH, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_ABS) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_ABS, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_KIND) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_KIND, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_SIK) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_SIK, IXprGbl-1, IXprGbl+1)
                ELSE IF (INAM == IW_SRK) THEN
                    IXprGbl = IXprGbl + 1
                    OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_SRK, IXprGbl-1, IXprGbl+1)
                ELSE
                    KERR = 1
                    CALL Outputing_Error ("term not allowed in expression: "//ZTOKW(1:LTOKW))
                    EXIT
                END IF
            ELSE
                KERR = 1
                CALL Outputing_Error ("term not allowed in expression: "//ZTOKW(1:LTOKW))
                EXIT
            END IF
!
!  "Separators"
!
        CASE (KK_RParen)    ! )
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_RParen, IXprGbl-1, IXprGbl+1)
        CASE (KK_LParen)    ! (
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_LParen, IXprGbl-1, IXprGbl+1)
        CASE (KK_Comma)     ! ,
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_Separator, IXprGbl-1, IXprGbl+1)
!
!  Comparisons
!
        CASE (KK_GT)        ! >
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_GT, IXprGbl-1, IXprGbl+1)
        CASE (KK_GE)        ! >=
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_GE, IXprGbl-1, IXprGbl+1)
        CASE (KK_NE)        ! /=
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_NE, IXprGbl-1, IXprGbl+1)
        CASE (KK_LE)        ! <=
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_LE, IXprGbl-1, IXprGbl+1)
        CASE (KK_LT)        ! <
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_LT, IXprGbl-1, IXprGbl+1)
        CASE (KK_EQ)        ! ==
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_EQ, IXprGbl-1, IXprGbl+1)
!
!  Operators
!
        CASE (KK_Slash)     ! /
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_Divide, IXprGbl-1, IXprGbl+1)
        CASE (KK_PlusMinus) ! + or -
            IXprGbl = IXprGbl + 1
            IF (ZTOKW(1:1) == "+") THEN
                OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_Plus, IXprGbl-1, IXprGbl+1)
            ELSE
                OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_Minus, IXprGbl-1, IXprGbl+1)
            END IF
        CASE (KK_Star)      ! *
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_Multiply, IXprGbl-1, IXprGbl+1)
        CASE (KK_Pow)       ! **
            IXprGbl = IXprGbl + 1
            OpXpr (IXprGbl) = OPRnOPD (0.0D0, KO_Power, IXprGbl-1, IXprGbl+1)
        END SELECT
    END DO
!
    OpXpr(IXprGbl)%INext = 0
    RETURN
END SUBROUTINE Lexing_Expression
!
!******************************************************************************
!
SUBROUTINE Evaluating_Expression_As_Logical (IFRES, KERR)
!
!  analyse lexed expression and evaluate result as logical
!
    IMPLICIT NONE
!
! arguments
    tLogical, INTENT (OUT)  :: IFRES
    tInteger, INTENT (OUT)  :: KERR
!
! local variables
    tDouble     :: DRES
    tInteger    :: KRES
!
! execution
!
    KERR = 0
    IFRES = .TRUE.
    CALL Valuing_Expression (DRES, KRES, KERR)
    IFRES = (DRES /= 0.0D0)
    RETURN
END SUBROUTINE Evaluating_Expression_As_Logical
!
!******************************************************************************
!
SUBROUTINE Valuing_Expression (DRESW, KRESW, KERR)
!
!  analyse lexed expression and value result in a character string
!
    IMPLICIT NONE
!
! arguments
    tDouble,  INTENT (OUT)  :: DRESW
    tInteger, INTENT (OUT)  :: KRESW, KERR
!
! local variables
    tInteger    :: IXPRE, IXPRW, IXPRA, LPAR, KOPRDW, IPRV
    tInteger    :: KOOPR, IXPRD, IXPRF, KOPR2, KOPR3, KOPR4
    tInteger    :: IXPR1, IXPR2, IXPR3, IXPR4, NOPR, KRES
    tInteger    :: IPRVW, IOPR, KOPR, KOPD1, KOPD2
!
! execution
!
    KERR = 0
    KRESW = KO_RealNumVal
    DRESW = 1.0D0
    IXPRE = IXprGbl
    BODY: DO
        IF (IXPRE < 1) THEN
            CALL Outputing_Error ("Empty expression")
            KERR = 1
            EXIT BODY
        ELSE IF (IXPRE == 1) THEN
            IF (OpXpr(IXPRE)%IOprd == KO_IntNumVal .OR. OpXpr(IXPRE)%IOprd == KO_RealNumVal) THEN
                KRESW = OpXpr(IXPRE)%IOprd
                DRESW = OpXpr(IXPRE)%DVal
            ELSE
                CALL Outputing_Error ("Illegal expression")
                KERR = 2
            END IF
            EXIT BODY
        ELSE
!
!  Change Fun (A, B) into (A) .Fun. (B) where it appears
!
            IXPRW = 1
            IXPRA = IXPRE
            DO
                SELECT CASE (OpXpr(IXPRW)%IOprd)
                CASE (KO_MOD, KO_MAX, KO_MIN, KO_ATAN2, KO_SRK)
!
!  Find corresponding separator
!
                    LPAR = 0
                    IXPR1 = OpXpr(IXPRW)%INext
                    DO
                        SELECT CASE (OpXpr(IXPR1)%IOprd)
                        CASE (KO_RParen)
                            LPAR = LPAR - 1
                            IF (LPAR <= 0) THEN
                                IF (OpXpr(IXPRW)%IOprd == KO_SRK) THEN
                                    IXPRW = OpXpr(IXPRW)%INext
                                    EXIT
                                END IF
                                CALL Outputing_Error ("Missing argument")
                                KERR = 3
                                EXIT BODY
                            END IF
                        CASE (KO_LParen)
                            LPAR = LPAR + 1
                        CASE (KO_Separator)
                            KOPRDW = OpXpr(IXPRW)%IOprd
                            IPRV = OpXpr(IXPRW)%IPrev
                            OpXpr (IXPRW) = OpXpr (OpXpr(IXPRW)%INext)
                            OpXpr(IXPRW)%IPrev = IPRV
                            OpXpr(OpXpr(IXPRW)%INext)%IPrev = IXPRW
                            IF (IXPRA+2 > NMaxXpr) THEN
                                CALL Outputing_Error ("Operation stack too small, increase # of" // &
                                                      " operators/ands and try again")
                                KERR = 4
                                EXIT BODY
                            END IF
                            SELECT CASE (KOPRDW)
                            CASE (KO_MOD)
                                KOOPR = KO_MODI
                            CASE (KO_MAX)
                                KOOPR = KO_MAXI
                            CASE (KO_MIN)
                                KOOPR = KO_MINI
                            CASE (KO_ATAN2)
                                KOOPR = KO_ATAN2I
                            CASE (KO_SRK)
                                KOOPR = KO_SRKI
                            CASE DEFAULT
                                KOOPR = KO_Other
                            END SELECT
                            IXPR2 = OpXpr(IXPR1)%IPrev
                            IXPR4 = OpXpr(IXPR1)%INext
                            OpXpr (IXPR1) = OPRnOPD (0.0D0, KO_RParen, IXPR2, IXPRA+1)
                            OpXpr (IXPRA+1) = OPRnOPD (0.0D0, KOOPR, IXPR1, IXPRA+2)
                            OpXpr (IXPRA+2) = OPRnOPD (0.0D0, KO_LParen, IXPRA+1, IXPR4)
                            OpXpr(IXPR4)%IPrev = IXPRA + 2
                            IXPRA = IXPRA + 2
                            EXIT
                        END SELECT
                        IXPR1 = OpXpr(IXPR1)%INext
                        IF (IXPR1 <= 0) THEN
                            CALL Outputing_Error ("Illegal expression")
                            KERR = 5
                            EXIT BODY
                        END IF
                    END DO
                CASE DEFAULT
                    IXPRW = OpXpr(IXPRW)%INext
                END SELECT
                IF (IXPRW <= 0) EXIT
            END DO
!
!  Reduce expressions
!
            IXPRW = 1
            IXPRD = 1
            DO
                SELECT CASE (OpXpr(IXPRW)%IOprd)
                CASE (KO_RParen)
                    IXPRF = IXPRW
                    IF (OpXpr(IXPRW)%IPrev == 0) THEN
                        CALL Outputing_Error ("Unexpected )")
                        KERR = 6
                        EXIT BODY
                    END IF
                    CALL Reduce_Expression()
                    IF (OpXpr(IXPRD)%INext == 0 .AND. OpXpr(IXPRD)%IPrev == 0) EXIT
                    IXPRD = 1
                    IXPRW = 1
                    CYCLE
                CASE (KO_LParen)
                    IXPRD = IXPRW
                CASE (KO_Separator)
                    CALL Outputing_Error ("Illegal expression")
                    KERR = 5
                    EXIT BODY
                CASE DEFAULT
                END SELECT
                IXPR1 = OpXpr(IXPRW)%INext
                IF (IXPR1 <= 0) THEN
                    IXPRF = IXPRW
                    CALL Reduce_Expression()
                    EXIT
                ELSE
                    IXPRW = IXPR1
                END IF
            END DO
            IF (KERR /= 0) THEN
                CALL Outputing_Error ("Illegal expression")
                KERR = 5
                EXIT BODY
            ELSE
                KRESW = OpXpr(IXPRD)%IOprd
                DRESW = OpXpr(IXPRD)%DVal
            END IF
        END IF
        EXIT BODY
    END DO BODY
    RETURN
CONTAINS
    SUBROUTINE Reduce_Expression()
!
!   Reduce an expression with no inner parentheses
!
!
! local variables
        tInteger, DIMENSION (NMaxXpr)   :: KOPRT, IXPRT
        tDouble                         :: DOPD1, DOPD2, DRES
!
! execution
!
!   Remove enclosing parentheses if any
!
        IXPR4 = IXPRF
        DO
            KOPR4 = OpXpr(IXPR4)%IOprd
            IXPR3 = OpXpr(IXPR4)%IPrev
            IF (IXPR3 == 0) THEN
                IF (KOPR4 /= KO_IntNumVal .AND. KOPR4 /= KO_RealNumVal) THEN
                    KERR = 7
                END IF
                RETURN
            END IF
            KOPR2 = OpXpr(IXPRD)%IOprd
            IXPR2 = OpXpr(IXPRD)%INext
            IF (KOPR4 == KO_RParen .AND. KOPR2 == KO_LParen) THEN
                IF (OpXpr(IXPR4)%INext /= 0) THEN
                    OpXpr(OpXpr(IXPR4)%INext)%IPrev = IXPR3
                END IF
                OpXpr(IXPR3)%INext = OpXpr(IXPR4)%INext
                IPRVW = OpXpr(IXPRD)%IPrev
                OpXpr (IXPRD) = OpXpr (IXPR2)
                OpXpr(IXPRD)%IPrev = IPRVW
                OpXpr(OpXpr(IXPRD)%INext)%IPrev = IXPRD
                IXPRF = IXPR3
            ELSE
                EXIT
            END IF
        END DO
!
!   Find and apply unary operators, and store binary ones
!
        IXPR4 = IXPRF
        IXPR3 = OpXpr(IXPR4)%IPrev
        KOPR4 = OpXpr(IXPR4)%IOprd
        IXPR2 = OpXpr(IXPRD)%INext
!
        NOPR = 0
        DO
            IF (IXPRF == IXPRD) EXIT
            IF (IXPR3 /= IXPRD) THEN
                IXPR2 = OpXpr(IXPR3)%IPrev
                KOPR2 = OpXpr(IXPR2)%IOprd
            ELSE
                IXPR2 = 0
                KOPR2 = KO_Other
            END IF
            KOPR3 = OpXpr(IXPR3)%IOprd
            KOPR4 = OpXpr(IXPR4)%IOprd
            IF (KOPR4 == KO_IntNumVal .OR. KOPR4 == KO_RealNumVal) THEN
                IF (KOPR3 /= KO_IntNumVal .AND. KOPR3 /= KO_RealNumVal .AND. &
                    KOPR2 /= KO_IntNumVal .AND. KOPR2 /= KO_RealNumVal) THEN
!
!   Apply unary operator at 3 to value at 4
!
                    DOPD1 = OpXpr(IXPR4)%DVal
                    CALL Apply_Unary_Operator (KOPR3, KOPR4, DOPD1, KRES, DRES)
                    IF (KERR /= 0) THEN
                        RETURN
                    END IF
                    OpXpr(IXPR3)%DVal = DRES
                    OpXpr(IXPR3)%IOprd = KRES
                    OpXpr(IXPR3)%INext = OpXpr(IXPR4)%INext
                    IF (OpXpr(IXPR3)%INext /= 0) THEN
                        OpXpr(OpXpr(IXPR3)%INext)%IPrev = IXPR3
                    END IF
                ELSE
                    SELECT CASE (KOPR3)
                    CASE DEFAULT
                        CONTINUE
                    CASE (KO_OR, KO_AND, KO_GT, KO_GE, KO_LT, KO_LE, KO_NE, KO_Minus, &
                          KO_Plus, KO_Multiply, KO_Divide, KO_Power, KO_EQ, KO_MODI,  &
                          KO_MAXI, KO_MINI, KO_ATAN2I, KO_SRKI)
                        NOPR = NOPR + 1
                        KOPRT (NOPR) = KOPR3
                        IXPRT (NOPR) = IXPR3
                    END SELECT
                END IF
            END IF
            IF (IXPR2 == 0) EXIT
            IXPR4 = IXPR3
            IXPR3 = IXPR2
        END DO
!
!   Apply binary operators in priority order
!
        IF (NOPR > 0) THEN
            DO
                IOPR = ELEMENT (MAXLOC(KOPRT(1:NOPR)), 1)
                KOPR = KOPRT (IOPR)
                IF (KOPR == 0) EXIT
                IXPR3 = IXPRT (IOPR)
                IXPR2 = OpXpr(IXPR3)%IPrev
                IXPR4 = OpXpr(IXPR3)%INext
                DOPD1 = OpXpr(IXPR2)%DVal
                DOPD2 = OpXpr(IXPR4)%DVal
                KOPD1 = OpXpr(IXPR2)%IOprd
                KOPD2 = OpXpr(IXPR4)%IOprd
                CALL Apply_Binary_Operator (KOPR, KOPD1, KOPD2, DOPD1, DOPD2, KRES, DRES)
                IF (KERR /= 0) THEN
                    RETURN
                END IF
                OpXpr(IXPR2)%DVal = DRES
                OpXpr(IXPR2)%IOprd = KRES
                OpXpr(IXPR2)%INext = OpXpr(IXPR4)%INext
                IF (OpXpr(IXPR2)%INext /= 0) THEN
                    OpXpr(OpXpr(IXPR2)%INext)%IPrev = IXPR2
                END IF
                KOPRT (IOPR) = 0
            END DO
        END IF
!
        RETURN
    END SUBROUTINE Reduce_Expression
!
!******************************************************************************
!
    SUBROUTINE Apply_Unary_Operator (KOPR, KOPD, DOPD, KRES, DRES)
!
!   Apply unary operator kopr to operand dopd of type kopd
!
!
! arguments
        tInteger, INTENT (IN)   :: KOPR, KOPD
        tDouble,  INTENT (IN)   :: DOPD
        tInteger, INTENT (OUT)  :: KRES
        tDouble,  INTENT (OUT)  :: DRES
!
! execution
!
        SELECT CASE (KOPR)
        CASE (KO_NOT)       ! Not
            IF (DOPD /= 0.0D0) THEN
                DRES = 0.0D0
            ELSE
                DRES = 1.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_Minus)     ! -
            DRES = - DOPD
            KRES = KOPD
        CASE (KO_Plus)      ! +
            DRES = DOPD
            KRES = KOPD
        CASE (KO_INT)       ! Int
            DRES = INT (DOPD)
            KRES = KO_IntNumVal
        CASE (KO_NINT)      ! Nint
            DRES = NINT (DOPD)
            KRES = KO_IntNumVal
        CASE (KO_SIN)       ! Sin
            DRES = SIN (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_COS)       ! Cos
            DRES = COS (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_TAN)       ! Tan
            DRES = TAN (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_ATAN)      ! Atan
            DRES = ATAN (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_LOG)       ! Log
            DRES = LOG (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_EXP)       ! Exp
            DRES = EXP (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_LOG10)     ! Log10
            DRES = LOG10 (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_SQRT)      ! Sqrt
            DRES = SQRT (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_ASIN)      ! Asin
            DRES = ASIN (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_ACOS)      ! Acos
            DRES = ACOS (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_SINH)      ! Sinh
            DRES = SINH (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_COSH)      ! Cosh
            DRES = COSH (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_TANH)      ! Tanh
            DRES = TANH (DOPD)
            KRES = KO_RealNumVal
        CASE (KO_ABS)       ! Abs
            DRES = ABS (DOPD)
            KRES = KOPD
        CASE (KO_SIK)       ! Selected_int_kind
            DRES = SELECTED_INT_KIND (NINT(DOPD))
            KRES = KO_IntNumVal
        CASE (KO_SRK)       ! Selected_real_kind
            DRES = SELECTED_REAL_KIND (NINT(DOPD))
            KRES = KOPD
        CASE (KO_LParen)    ! Nothing
            DRES = DOPD
            KRES = KOPD
        CASE DEFAULT        ! Other
            KERR = 9
        END SELECT
        RETURN
    END SUBROUTINE Apply_Unary_Operator
!
!******************************************************************************
!
    SUBROUTINE Apply_Binary_Operator (KOPR, KOPD1, KOPD2, DOPD1, DOPD2, KRES, DRES)
!
!   Apply binary operator kopr to operands dopdi of type kopdi
!
!
! arguments
        tInteger, INTENT (IN)   :: KOPR, KOPD1, KOPD2
        tDouble,  INTENT (IN)   :: DOPD1, DOPD2
        tInteger, INTENT (OUT)  :: KRES
        tDouble,  INTENT (OUT)  :: DRES
!
! execution
!
        SELECT CASE (KOPR)
        CASE (KO_OR)        ! Or
            IF (DOPD1 /= 0.0D0 .OR. DOPD2 /= 0.0D0) THEN
                DRES = 1.0D0
            ELSE
                DRES = 0.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_AND)       ! And
            IF (DOPD1 /= 0.0D0 .AND. DOPD2 /= 0.0D0) THEN
                DRES = 1.0D0
            ELSE
                DRES = 0.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_GT)        ! >
            IF (DOPD1 > DOPD2) THEN
                DRES = 1.0D0
            ELSE
                DRES = 0.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_GE)        ! >=
            IF (DOPD1 >=DOPD2) THEN
                DRES = 1.0D0
            ELSE
                DRES = 0.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_EQ)        ! ==
            IF (DOPD1 == DOPD2) THEN
                DRES = 1.0D0
            ELSE
                DRES = 0.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_LT)        ! <
            IF (DOPD1 < DOPD2) THEN
                DRES = 1.0D0
            ELSE
                DRES = 0.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_LE)        ! <=
            IF (DOPD1 <= DOPD2) THEN
                DRES = 1.0D0
            ELSE
                DRES = 0.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_NE)        ! /=
            IF (DOPD1 /= DOPD2) THEN
                DRES = 1.0D0
            ELSE
                DRES = 0.0D0
            END IF
            KRES = KO_IntNumVal
        CASE (KO_Minus)     ! -
            DRES = DOPD1 - DOPD2
            IF (KOPD1 == KO_IntNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                KRES = KO_IntNumVal
            ELSE
                KRES = KO_RealNumVal
            END IF
        CASE (KO_Plus)      ! +
            DRES = DOPD1 + DOPD2
            IF (KOPD1 == KO_IntNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                KRES = KO_IntNumVal
            ELSE
                KRES = KO_RealNumVal
            END IF
        CASE (KO_Multiply)  ! *
            DRES = DOPD1 * DOPD2
            IF (KOPD1 == KO_IntNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                KRES = KO_IntNumVal
            ELSE
                KRES = KO_RealNumVal
            END IF
        CASE (KO_Divide)    ! /
            IF (KOPD1 == KO_IntNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                DRES = NINT (DOPD1) / NINT (DOPD2)
                KRES = KO_IntNumVal
            ELSE
                DRES = DOPD1 / DOPD2
                KRES = KO_RealNumVal
            END IF
        CASE (KO_Power)     ! **
            IF (KOPD1 == KO_IntNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                DRES = NINT (DOPD1) ** NINT (DOPD2)
                KRES = KO_IntNumVal
            ELSE IF (KOPD1 == KO_RealNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                DRES = DOPD1 ** NINT (DOPD2)
                KRES = KO_RealNumVal
            ELSE
                DRES = DOPD1 ** DOPD2
                KRES = KO_RealNumVal
            END IF
        CASE (KO_MODI)      ! .Mod.
            IF (KOPD1 == KO_IntNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                DRES = MOD (NINT(DOPD1), NINT(DOPD2))
                KRES = KO_IntNumVal
            ELSE
                DRES = MOD (DOPD1, DOPD2)
                KRES = KO_RealNumVal
            END IF
        CASE (KO_MAXI)      ! .Max.
            DRES = MAX (DOPD1, DOPD2)
            IF (KOPD1 == KO_IntNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                KRES = KO_IntNumVal
            ELSE
                KRES = KO_RealNumVal
            END IF
        CASE (KO_MINI)      ! .Min.
            DRES = MIN (DOPD1, DOPD2)
            IF (KOPD1 == KO_IntNumVal .AND. KOPD2 == KO_IntNumVal) THEN
                KRES = KO_IntNumVal
            ELSE
                KRES = KO_RealNumVal
            END IF
        CASE (KO_ATAN2I)    ! .Atan2.
            DRES = ATAN2 (DOPD1, DOPD2)
            KRES = KO_RealNumVal
        CASE (KO_SRKI)      ! .S_R_K.
            DRES = SELECTED_REAL_KIND (NINT(DOPD1), NINT(DOPD2))
            KRES = KO_IntNumVal
        CASE DEFAULT ! Other
            KERR = 11
        END SELECT
        RETURN
    END SUBROUTINE Apply_Binary_Operator
!
!******************************************************************************
!
    FUNCTION ELEMENT (J, I) RESULT(ResVal)
!
! arguments
        tInteger, INTENT (IN)   :: J(:)
        tInteger, INTENT (IN) :: I
        tInteger                :: ResVal
!
! execution
!
        ResVal = J (I)
        RETURN
    END FUNCTION ELEMENT
!
!******************************************************************************
!
END SUBROUTINE Valuing_Expression
!
!******************************************************************************
!
SUBROUTINE Evaluating_Expression_As_String (ZRES, KRES, KERR)
!
!  analyse lexed expression and evaluate result as string
!
    IMPLICIT NONE
!
! arguments
    tCharStar, INTENT (OUT) :: ZRES
    tInteger,  INTENT (OUT) :: KRES, KERR
!
! local variables
    tDouble     :: DRES
!
! execution
!
    KERR = 0
    CALL Valuing_Expression (DRES, KRES, KERR)
    SELECT CASE (KRES)
    CASE (KO_LogNumVal)
        IF (DRES /= 0.0D0) THEN
            ZRES = ".True."
        ELSE
            ZRES = ".False."
        END IF
    CASE (KO_IntNumVal)
        WRITE (ZRES,*) Nint (DRES)
        ZRES = ADJUSTL (ZRES)
    CASE (KO_RealNumVal)
        WRITE (ZRES,*) DRES
        ZRES = ADJUSTL (ZRES)
    CASE DEFAULT
        ZRES = ".True."
    END SELECT
    RETURN
END SUBROUTINE Evaluating_Expression_As_String
!
!******************************************************************************
!
MODULE SUBROUTINE Outputing_Error (ZSTR)
!
!  Output error message
!
    IMPLICIT NONE
!
! arguments
    tCharStar, INTENT (IN)  :: ZSTR
!
! parameters
    tCharStar, PARAMETER    :: ZFMT = "(a, ', line ', a, ': Error:', ' ', a)"
!
! local variables
    tCharLen (11)   :: ZNUM
!
! execution
!
!
    WRITE (ZNUM, "(i11)")  NLineInput (ILevel_Include)
    WRITE (LU_Error, ZFMT) TRIM (ZV_TabName(ILevel_Include)), TRIM (ADJUSTL(ZNUM)), TRIM (ZSTR)
    RETURN
END SUBROUTINE Outputing_Error
!
!******************************************************************************

END SUBMODULE SubTool_F90Expression
