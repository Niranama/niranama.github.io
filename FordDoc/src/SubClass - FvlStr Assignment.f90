
SUBMODULE (Class_FvlStr) SubClass_FvlStr_Assignment

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *assignment* procedures.  These
!   procedures are conversion routines (intended for use in an assignment expression)
!   between a *FvlStr* object and an other (Fortran intrinsic) type.

!** USE STATEMENTS:
    USE, INTRINSIC  :: IEEE_ARITHMETIC
    USE ModBase_ChrStr,     ONLY: ParseCSingle, ParseCDouble, ParseCQuad, ParseLogical
    USE ModBase_CharConv,   ONLY: Int32FromChar  => I32_FromChar_CC_FortNum, &
                                  Int64FromChar  => I64_FromChar_CC_FortNum, &
                                  SingleFromChar => RealSP_FromString_Lemire, &
                                  DoubleFromChar => RealDP_FromString_Lemire, &
                                  QuadFromChar   => RealQP_FromString_YY, &
                                  Int32ToChar    => I32_ToChar_CC, &
                                  Int64ToChar    => I64_ToChar_CC, &
                                  SingleToChar   => RealSP_ToString_DragonBox, &
                                  DoubleToChar   => RealDP_ToString_DragonBox, &
                                  QuadToChar     => RealQP_ToString_DragonBox

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

MODULE SUBROUTINE FvlStr_From_CharacterString(vStr,cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tCharStar,     INTENT(IN)   :: cStr     !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    vStr%cStr = cStr

    RETURN

END SUBROUTINE FvlStr_From_CharacterString

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_CharacterArray(vStr,cArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a character array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tChar,         INTENT(IN)   :: cArr(:)  !! character array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, I

!** FLOW
    
    Length = SIZE(cArr)
    ALLOCATE(CHARACTER(LEN=Length) :: vStr%cStr)
    FORALL (I=1:Length) vStr%cStr(I:I) = cArr(I)

    RETURN

END SUBROUTINE FvlStr_From_CharacterArray

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_IByte(vStr,IntNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from an 8-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tByte,         INTENT(IN)   :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = Int32ToChar(ToInteger(IntNum))

    RETURN

END SUBROUTINE FvlStr_From_IByte

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_IShort(vStr,IntNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a 16-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tShort,        INTENT(IN)   :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = Int32ToChar(ToInteger(IntNum))

    RETURN

END SUBROUTINE FvlStr_From_IShort

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_Integer(vStr,IntNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tInteger,      INTENT(IN)   :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = Int32ToChar(IntNum)

    RETURN

END SUBROUTINE FvlStr_From_Integer

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_ILong(vStr,IntNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tLong,         INTENT(IN)   :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = Int64ToChar(IntNum)

    RETURN

END SUBROUTINE FvlStr_From_ILong

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_RSingle(vStr,RealNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a 32-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tSingle,       INTENT(IN)   :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = SingleToChar(RealNum)

    RETURN

END SUBROUTINE FvlStr_From_RSingle

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_RDouble(vStr,RealNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a 64-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tDouble,       INTENT(IN)   :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = DoubleToChar(RealNum)

    RETURN

END SUBROUTINE FvlStr_From_RDouble

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_RQuad(vStr,RealNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a 128-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tQuad,         INTENT(IN)   :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = QuadToChar(RealNum)

    RETURN

END SUBROUTINE FvlStr_From_RQuad

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_CSingle(vStr,CmpxNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a single-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tCmpxSingle,   INTENT(IN)   :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = '(' // SingleToChar(CmpxNum%RE) // ', ' // SingleToChar(CmpxNum%IM) // ')'

    RETURN

END SUBROUTINE FvlStr_From_CSingle

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_CDouble(vStr,CmpxNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a double-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tCmpxDouble,   INTENT(IN)   :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = '(' // DoubleToChar(CmpxNum%RE) // ', ' // DoubleToChar(CmpxNum%IM) // ')'

    RETURN

END SUBROUTINE FvlStr_From_CDouble

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_CQuad(vStr,CmpxNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a quadruple-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tCmpxQuad,     INTENT(IN)   :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = '(' // QuadToChar(CmpxNum%RE) // ', ' // QuadToChar(CmpxNum%IM) // ')'

    RETURN

END SUBROUTINE FvlStr_From_CQuad

!**************************************************************************************

MODULE SUBROUTINE FvlStr_From_Logical(vStr,Boolean)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To assign a FvlStr object from a default logical value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(OUT)  :: vStr     !! FvlStr object
    tLogical,      INTENT(IN)   :: Boolean  !! logical value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (Boolean) THEN
        vStr%cStr = 'TRUE'
    ELSE
        vStr%cStr = 'FALSE'
    END IF

    RETURN

END SUBROUTINE FvlStr_From_Logical

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_CharAlloc(cStr, vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a character string (allocatable).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tCharAlloc,   INTENT(OUT)   :: cStr     !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        cStr = vStr%cStr
    ELSE
        ALLOCATE(tCharLen(0) :: cStr)
    END IF

    RETURN

END SUBROUTINE FvlStr_To_CharAlloc

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_CharArray_Alloc(cArr, vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a character array (allocatable).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
    tChar, ALLOCATABLE, INTENT(OUT) :: cArr(:)  !! character array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Length

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        Length = vStr%Length()
        ALLOCATE(cArr(Length))
        FORALL (I=1:Length) cArr(I) = vStr%cStr(I:I)
    ELSE
        ALLOCATE(cArr(0))
    END IF

    RETURN

END SUBROUTINE FvlStr_To_CharArray_Alloc

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_IByte(IntNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to an 8-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tByte,        INTENT(OUT)   :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        IntNum = ToByte(Int32FromChar(vStr%cStr))
    ELSE
        ! set output to mininum value
        IntNum = ToByte(Z'80')
    END IF

    RETURN

END SUBROUTINE FvlStr_To_IByte

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_IShort(IntNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a 16-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tShort,       INTENT(OUT)   :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        IntNum = ToShort(Int32FromChar(vStr%cStr))
    ELSE
        ! set output to mininum value
        IntNum = ToShort(Z'8000')
    END IF

    RETURN

END SUBROUTINE FvlStr_To_IShort

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_Integer(IntNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tInteger,     INTENT(OUT)   :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        IntNum = Int32FromChar(vStr%cStr)
    ELSE
        ! set output to mininum value
        IntNum = ToInteger(Z'80000000')
    END IF
    
    RETURN

END SUBROUTINE FvlStr_To_Integer

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_ILong(IntNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tLong,        INTENT(OUT)   :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        IntNum = Int64FromChar(vStr%cStr)
    ELSE
        ! set output to mininum value
        IntNum = ToLong(Z'8000000000000000')
    END IF

    RETURN

END SUBROUTINE FvlStr_To_ILong

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_RSingle(RealNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a 32-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tSingle,      INTENT(OUT)   :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        RealNum = SingleFromChar(vStr%cStr)
    ELSE
        ! set output to NAN
        RealNum = IEEE_VALUE(0.0E0_kSingle, IEEE_SIGNALING_NAN)
    END IF
    
    RETURN

END SUBROUTINE FvlStr_To_RSingle

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_RDouble(RealNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a 64-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tDouble,      INTENT(OUT)   :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        RealNum = DoubleFromChar(vStr%cStr)
    ELSE
        ! set output to NAN
        RealNum = IEEE_VALUE(0.0E0_kDouble, IEEE_SIGNALING_NAN)
    END IF
    
    RETURN

END SUBROUTINE FvlStr_To_RDouble

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_RQuad(RealNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a 128-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tQuad,        INTENT(OUT)   :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        RealNum = QuadFromChar(vStr%cStr)
    ELSE
        ! set output to NAN
        RealNum = IEEE_VALUE(0.0E0_kQuad, IEEE_SIGNALING_NAN)
    END IF
    
    RETURN

END SUBROUTINE FvlStr_To_RQuad

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_CSingle(CmpxNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a single-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tCmpxSingle,  INTENT(OUT)   :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSingle     :: RealVal  ! working variable

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! convert character string to complex number
        CmpxNum = ParseCSingle(vStr%cStr)
    ELSE
        ! set output to (NAN,NAN)
        RealVal = IEEE_VALUE(0.0E0_kSingle, IEEE_SIGNALING_NAN)
        CmpxNum = CMPLX(RealVal, RealVal, KIND=kSingle)
    END IF
    
    RETURN

END SUBROUTINE FvlStr_To_CSingle

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_CDouble(CmpxNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a double-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tCmpxDouble,  INTENT(OUT)   :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: RealVal  ! working variable

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! convert character string to complex number
        CmpxNum = ParseCDouble(vStr%cStr)
    ELSE
        ! set output to (NAN,NAN)
        RealVal = IEEE_VALUE(0.0E0_kDouble, IEEE_SIGNALING_NAN)
        CmpxNum = CMPLX(RealVal, RealVal, KIND=kDouble)
    END IF
    
    RETURN

END SUBROUTINE FvlStr_To_CDouble

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_CQuad(CmpxNum,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a quadruple-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tCmpxQuad,    INTENT(OUT)   :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tQuad     :: RealVal  ! working variable

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! convert character string to complex number
        CmpxNum = ParseCQuad(vStr%cStr)
    ELSE
        ! set output to (NAN,NAN)
        RealVal = IEEE_VALUE(0.0E0_kQuad, IEEE_SIGNALING_NAN)
        CmpxNum = CMPLX(RealVal, RealVal, KIND=kQuad)
    END IF
    
    RETURN

END SUBROUTINE FvlStr_To_CQuad

!**************************************************************************************

MODULE SUBROUTINE FvlStr_To_Logical(Boolean,vStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a FvlStr object to a default logical value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,     INTENT(OUT)   :: Boolean  !! logical value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! convert character string to logical value
        Boolean = ParseLogical(vStr%cStr)
    ELSE
        ! set value to FalseVal
        Boolean = FalseVal
    END IF

    RETURN

END SUBROUTINE FvlStr_To_Logical

!**************************************************************************************

END SUBMODULE SubClass_FvlStr_Assignment

!******************************************************************************
