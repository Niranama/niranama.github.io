

SUBMODULE (ModBase_ChrStr) SubBase_ChrStr_Assignment

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *assignment* procedures.  These
!   procedures are conversion routines (intended for use in an assignment expression)
!   between a character string and an other (Fortran intrinsic) type.

!** USE STATEMENTS:
    USE ModBase_CharConv,   ONLY: Int32ToChar    => I32_ToChar_CC, &
                                  Int64ToChar    => I64_ToChar_CC, &
                                  Int32FromChar  => I32_FromChar_CC_FortNum, &
                                  Int64FromChar  => I64_FromChar_CC_FortNum, &
                                  SingleToChar   => RealSP_ToString_DragonBox, &
                                  SingleFromChar => RealSP_FromString_Lemire, &
                                  DoubleToChar   => RealDP_ToString_DragonBox, &
                                  DoubleFromChar => RealDP_FromString_Lemire, &
                                  QuadToChar     => RealQP_ToString_DragonBox, &
                                  QuadFromChar   => RealQP_FromString_YY
    USE, INTRINSIC  :: IEEE_ARITHMETIC

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE SUBROUTINES OR FUNCTIONS:

MODULE SUBROUTINE ChrStr_From_IByte(cStr,IntNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 8-bit integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT) :: cStr     ! character string
    tByte,      INTENT(IN)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = Int32ToChar(ToInteger(IntNum))

    RETURN

END SUBROUTINE ChrStr_From_IByte

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_IShort(cStr,IntNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 16-bit integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT) :: cStr     ! character string
    tShort,     INTENT(IN)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = Int32ToChar(ToInteger(IntNum))

    RETURN

END SUBROUTINE ChrStr_From_IShort

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_Integer(cStr,IntNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 32-bit integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT) :: cStr     ! character string
    tInteger,   INTENT(IN)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = Int32ToChar(IntNum)

    RETURN

END SUBROUTINE ChrStr_From_Integer

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_ILong(cStr,IntNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 64-bit integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT) :: cStr     ! character string
    tLong,      INTENT(IN)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = Int64ToChar(IntNum)

    RETURN

END SUBROUTINE ChrStr_From_ILong

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_RSingle(cStr,RealNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 32-bit real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT) :: cStr     ! character string
    tSingle,    INTENT(IN)  :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = SingleToChar(RealNum)

    RETURN

END SUBROUTINE ChrStr_From_RSingle

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_RDouble(cStr,RealNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 64-bit real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT) :: cStr     ! character string
    tDouble,    INTENT(IN)  :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = DoubleToChar(RealNum)

    RETURN

END SUBROUTINE ChrStr_From_RDouble

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_RQuad(cStr,RealNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 128-bit real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT) :: cStr     ! character string
    tQuad,      INTENT(IN)  :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = QuadToChar(RealNum)

    RETURN

END SUBROUTINE ChrStr_From_RQuad

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_CSingle(cStr,CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 32-bit complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc,  INTENT(OUT)    :: cStr     ! character string
    tCmpxSingle, INTENT(IN)     :: CmpxNum  ! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = '(' // SingleToChar(CmpxNum%RE) // ', ' // SingleToChar(CmpxNum%IM) // ')'

    RETURN

END SUBROUTINE ChrStr_From_CSingle

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_CDouble(cStr,CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 64-bit complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc,  INTENT(OUT)    :: cStr     ! character string
    tCmpxDouble, INTENT(IN)     :: CmpxNum  ! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = '(' // DoubleToChar(CmpxNum%RE) // ', ' // DoubleToChar(CmpxNum%IM) // ')'

    RETURN

END SUBROUTINE ChrStr_From_CDouble

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_CQuad(cStr,CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a 128-bit complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT) :: cStr     ! character string
    tCmpxQuad,  INTENT(IN)  :: CmpxNum  ! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = '(' // QuadToChar(CmpxNum%RE) // ', ' // QuadToChar(CmpxNum%IM) // ')'

    RETURN

END SUBROUTINE ChrStr_From_CQuad

!******************************************************************************

MODULE SUBROUTINE ChrStr_From_Logical(cStr,Boolean)

!** PURPOSE OF THIS ROUTINE:
    ! To assign a character string from a logical value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc, INTENT(OUT)  :: cStr     ! character string
    tLogical,   INTENT(IN)   :: Boolean  ! logical value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! assign character string
    IF (Boolean) THEN
        cStr = 'TRUE'
    ELSE
        cStr = 'FALSE'
    END IF

    RETURN

END SUBROUTINE ChrStr_From_Logical

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_IByte(IntNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tByte,     INTENT(OUT)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    IntNum = ToByte(Int32FromChar(cStr))

    RETURN

END SUBROUTINE ChrStr_To_IByte

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_IShort(IntNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tShort,    INTENT(OUT)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    IntNum = ToShort(Int32FromChar(cStr))

    RETURN

END SUBROUTINE ChrStr_To_IShort

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_Integer(IntNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tInteger,  INTENT(OUT)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    IntNum = Int32FromChar(cStr)

    RETURN

END SUBROUTINE ChrStr_To_Integer

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_ILong(IntNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tLong,     INTENT(OUT)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    IntNum = Int64FromChar(cStr)

    RETURN

END SUBROUTINE ChrStr_To_ILong

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_RSingle(RealNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tSingle,   INTENT(OUT)  :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    RealNum = SingleFromChar(cStr)

    RETURN

END SUBROUTINE ChrStr_To_RSingle

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_RDouble(RealNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tDouble,   INTENT(OUT)  :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    RealNum = DoubleFromChar(cStr)
    
    RETURN

END SUBROUTINE ChrStr_To_RDouble

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_RQuad(RealNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tQuad,     INTENT(OUT)  :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    RealNum = QuadFromChar(cStr)

    RETURN

END SUBROUTINE ChrStr_To_RQuad

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_CSingle(CmpxNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,   INTENT(IN)     :: cStr     ! character string
    tCmpxSingle, INTENT(OUT)    :: CmpxNum  ! complex number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSingle, PARAMETER      :: MinSingle = -HUGE(1.0_kSP)
    tSingle, PARAMETER      :: MaxSingle =  HUGE(1.0_kSP)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: NumFlag  ! number flag
    tQuad                   :: RealVal  ! working variable
    tQuad                   :: ImagVal  ! working variable
    CLASS(*), ALLOCATABLE   :: NumVal   ! working variable

!** FLOW

    ! set default values for real and imaginary parts
    RealVal = IEEE_VALUE(Zero, IEEE_SIGNALING_NAN)
    ImagVal = RealVal
    
    ! check whether the string is a complex number
    NumFlag = IsStringNumber(cStr, NumVal=NumVal)
    IF (NumFlag.EQ.3) THEN
        SELECT TYPE (NumVal)
        TYPE IS (tCmpxQuad)
            ! get real part
            RealVal = REAL(NumVal, KIND=kQP)
            IF (RealVal > MaxSingle) THEN
                RealVal = IEEE_VALUE(One, IEEE_POSITIVE_INF)
            ELSEIF (RealVal < MinSingle) THEN
                RealVal = IEEE_VALUE(One, IEEE_NEGATIVE_INF)
            END IF
            ! get imaginary part
            ImagVal = AIMAG(NumVal)
            IF (ImagVal > MaxSingle) THEN
                ImagVal = IEEE_VALUE(One, IEEE_POSITIVE_INF)
            ELSEIF (ImagVal < MinSingle) THEN
                ImagVal = IEEE_VALUE(One, IEEE_NEGATIVE_INF)
            END IF
        END SELECT
    END IF

    ! set complex number
    CmpxNum = CMPLX(RealVal, ImagVal, KIND=kSP)

    RETURN

END SUBROUTINE ChrStr_To_CSingle

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_CDouble(CmpxNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,   INTENT(IN)     :: cStr     ! character string
    tCmpxDouble, INTENT(OUT)    :: CmpxNum  ! complex number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER      :: MinDouble = -HUGE(1.0_kDP)
    tDouble, PARAMETER      :: MaxDouble =  HUGE(1.0_kDP)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: NumFlag  ! number flag
    tQuad                   :: RealVal  ! working variable
    tQuad                   :: ImagVal  ! working variable
    CLASS(*), ALLOCATABLE   :: NumVal   ! working variable

!** FLOW

    ! set default values for real and imaginary parts
    RealVal = IEEE_VALUE(Zero, IEEE_SIGNALING_NAN)
    ImagVal = RealVal
    
    ! check whether the string is a complex number
    NumFlag = IsStringNumber(cStr, NumVal=NumVal)
    IF (NumFlag.EQ.3) THEN
        SELECT TYPE (NumVal)
        TYPE IS (tCmpxQuad)
            ! get real part
            RealVal = REAL(NumVal, KIND=kQP)
            IF (RealVal > MaxDouble) THEN
                RealVal = IEEE_VALUE(One, IEEE_POSITIVE_INF)
            ELSEIF (RealVal < MinDouble) THEN
                RealVal = IEEE_VALUE(One, IEEE_NEGATIVE_INF)
            END IF
            ! get imaginary part
            ImagVal = AIMAG(NumVal)
            IF (ImagVal > MaxDouble) THEN
                ImagVal = IEEE_VALUE(One, IEEE_POSITIVE_INF)
            ELSEIF (ImagVal < MinDouble) THEN
                ImagVal = IEEE_VALUE(One, IEEE_NEGATIVE_INF)
            END IF
        END SELECT
    END IF

    ! set complex number
    CmpxNum = CMPLX(RealVal, ImagVal, KIND=kDP)

    RETURN

END SUBROUTINE ChrStr_To_CDouble

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_CQuad(CmpxNum,cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tCmpxQuad, INTENT(OUT)  :: CmpxNum  ! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: NumFlag  ! number flag
    tQuad                   :: RealVal  ! working variable
    tQuad                   :: ImagVal  ! working variable
    CLASS(*), ALLOCATABLE   :: NumVal   ! working variable

!** FLOW

    ! set default values for real and imaginary parts
    RealVal = IEEE_VALUE(Zero, IEEE_SIGNALING_NAN)
    ImagVal = RealVal
    
    ! check whether the string is a complex number
    NumFlag = IsStringNumber(cStr, NumVal=NumVal)
    IF (NumFlag.EQ.3) THEN
        SELECT TYPE (NumVal)
        TYPE IS (tCmpxQuad)
            ! get real part
            RealVal = REAL(NumVal, KIND=kQP)
            ! get imaginary part
            ImagVal = AIMAG(NumVal)
        END SELECT
    END IF

    ! set complex number
    CmpxNum = CMPLX(RealVal, ImagVal, KIND=kQP)

    RETURN

END SUBROUTINE ChrStr_To_CQuad

!******************************************************************************

MODULE SUBROUTINE ChrStr_To_Logical(Boolean, cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a logical value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tLogical,  INTENT(OUT)  :: Boolean  ! logical value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical  :: LogFlag

!** FLOW

    LogFlag = IsStringLogical(cStr, Boolean)

    RETURN

END SUBROUTINE ChrStr_To_Logical

!******************************************************************************

END SUBMODULE SubBase_ChrStr_Assignment

!******************************************************************************
