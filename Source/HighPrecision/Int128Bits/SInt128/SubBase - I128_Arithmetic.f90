
SUBMODULE (ModBase_SInt128) SubBase_I128_Arithmetic

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to arithmetic
!   operations of the <a href="../module/modbase_sint128.html">SInt128</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tSInt128        TYPE(SInt128)
#define     tUInt128        TYPE(UInt128)

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    ADDITION OPERATIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION I128_UnaryPlus(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return result of the unary plus sign of the Sint128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: InVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = InVal

    RETURN

END FUNCTION I128_UnaryPlus

!******************************************************************************

MODULE FUNCTION I128_Plus_I32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt32,  INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, ToInt64(RhsVal), 0_kInt64, OutVal%Low, Carry)
    IF (RhsVal < 0) THEN
        OutVal%High = LhsVal%High - 1_kInt64 + Carry
    ELSE
        OutVal%High = LhsVal%High + Carry
    END IF

    RETURN

END FUNCTION I128_Plus_I32

!******************************************************************************

MODULE FUNCTION I32_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64       :: Carry

!** FLOW

    CALL AddU64(ToInt64(LhsVal), RhsVal%Low, 0_kInt64, OutVal%Low, Carry)
    IF (LhsVal < 0) THEN
        OutVal%High = RhsVal%High - 1_kInt64 + Carry
    ELSE
        OutVal%High = RhsVal%High + Carry
    END IF

    RETURN

END FUNCTION I32_Plus_I128

!******************************************************************************

MODULE FUNCTION I128_Plus_I64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt64,  INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, RhsVal, 0_kInt64, OutVal%Low, Carry)
    IF (RhsVal < 0_kInt64) THEN
        OutVal%High = LhsVal%High - 1_kInt64 + Carry
    ELSE
        OutVal%High = LhsVal%High + Carry
    END IF

    RETURN

END FUNCTION I128_Plus_I64

!******************************************************************************

MODULE FUNCTION I64_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,  INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(LhsVal, RhsVal%Low, 0_kInt64, OutVal%Low, Carry)
    IF (LhsVal < 0_kInt64) THEN
        OutVal%High = RhsVal%High - 1_kInt64 + Carry
    ELSE
        OutVal%High = RhsVal%High + Carry
    END IF

    RETURN

END FUNCTION I64_Plus_I128

!******************************************************************************

MODULE FUNCTION I128_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition of two SInt128 objects (Lhs + Rhs).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, RhsVal%Low, 0_kInt64, OutVal%Low, Carry)
    CALL AddU64(LhsVal%High, RhsVal%High, Carry, OutVal%High)

    RETURN

END FUNCTION I128_Plus_I128

!******************************************************************************

MODULE SUBROUTINE I128_Increment(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To increase value of the input by 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Val%Low == -1_kInt64) THEN
        Val%High = Val%High + 1_kInt64
        Val%Low  = 0_kInt64
    ELSE
        Val%Low  = Val%Low + 1_kInt64
    END IF

    RETURN

END SUBROUTINE I128_Increment

!******************************************************************************

MODULE SUBROUTINE I128_Add_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  This = This + Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt32,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry, OutLo

!** FLOW

    CALL AddU64(This%Low, ToInt64(Other), 0_kInt64, OutLo, Carry)
    This%Low  = OutLo
    IF (Other < 0_kInt64) THEN
        This%High = This%High - 1_kInt64 + Carry
    ELSE
        This%High = This%High + Carry
    END IF

    RETURN

END SUBROUTINE I128_Add_I32

!******************************************************************************

MODULE SUBROUTINE I128_Add_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  This = This + Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt64,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry, OutLo

!** FLOW

    CALL AddU64(This%Low, Other, 0_kInt64, OutLo, Carry)
    This%Low  = OutLo
    IF (Other < 0_kInt64) THEN
        This%High = This%High - 1_kInt64 + Carry
    ELSE
        This%High = This%High + Carry
    END IF

    RETURN

END SUBROUTINE I128_Add_I64

!******************************************************************************

MODULE SUBROUTINE I128_Add_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  This = This + Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt128, INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry, OutLo, OutHi

!** FLOW

    CALL AddU64(This%Low, Other%Low, 0_kInt64, OutLo, Carry)
    CALL AddU64(This%High, Other%High, Carry, OutHi)
    This%Low  = OutLo
    This%High = OutHi

    RETURN

END SUBROUTINE I128_Add_I128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  SUBTRACTION OPERATIONS                  ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION I128_Negate(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To negate the Uint128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: InVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = NOT(InVal%High)
    IF (InVal%Low == 0_kInt64) OutVal%High = OutVal%High + 1_kInt64
    OutVal%Low = NOT(InVal%Low) + 1_kInt64

    RETURN

END FUNCTION I128_Negate

!******************************************************************************

MODULE FUNCTION I128_Minus_I32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt32,  INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, ToInt64(RhsVal), 0_kInt64, OutVal%Low, Borrow)
    IF (RhsVal < 0) THEN
        OutVal%High = LhsVal%High + 1_kInt64 - Borrow
    ELSE
        OutVal%High = LhsVal%High - Borrow
    END IF

    RETURN

END FUNCTION I128_Minus_I32

!******************************************************************************

MODULE FUNCTION I32_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(ToInt64(LhsVal), RhsVal%Low, 0_kInt64, OutVal%Low, Borrow)
    IF (LhsVal < 0) THEN
        OutVal%High = -(RhsVal%High + 1_kInt64 + Borrow)
    ELSE
        OutVal%High = -(RhsVal%High + Borrow)
    END IF

    RETURN

END FUNCTION I32_Minus_I128

!******************************************************************************

MODULE FUNCTION I128_Minus_I64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt64,  INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, RhsVal, 0_kInt64, OutVal%Low, Borrow)
    IF (RhsVal < 0_kInt64) THEN
        OutVal%High = LhsVal%High + 1_kInt64 - Borrow
    ELSE
        OutVal%High = LhsVal%High - Borrow
    END IF

    RETURN

END FUNCTION I128_Minus_I64

!******************************************************************************

MODULE FUNCTION I64_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,  INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(LhsVal, RhsVal%Low, 0_kInt64, OutVal%Low, Borrow)
    IF (LhsVal < 0_kInt64) THEN
        OutVal%High = -(RhsVal%High + 1_kInt64 + Borrow)
    ELSE
        OutVal%High = -(RhsVal%High + Borrow)
    END IF

    RETURN

END FUNCTION I64_Minus_I128

!******************************************************************************

MODULE FUNCTION I128_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction of two SInt128 objects (Lhs - Rhs).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, RhsVal%Low, 0_kInt64, OutVal%Low, Borrow)
    CALL SubU64(LhsVal%High, RhsVal%High, Borrow, OutVal%High)

    RETURN

END FUNCTION I128_Minus_I128

!******************************************************************************

MODULE SUBROUTINE I128_Decrement(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To decrease value of the input by 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Val%Low == 0_kInt64) THEN
        Val%High = Val%High - 1_kInt64
        Val%Low  = -1_kInt64
    ELSE
        Val%Low  = Val%Low - 1_kInt64
    END IF

    RETURN

END SUBROUTINE I128_Decrement

!******************************************************************************

MODULE SUBROUTINE I128_Subtract_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  This = This - Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt32,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow, OutLo

!** FLOW

    CALL SubU64(This%Low, ToInt64(Other), 0_kInt64, OutLo, Borrow)
    This%Low = OutLo
    IF (Other < 0) THEN
        This%High = This%High + 1_kInt64 - Borrow
    ELSE
        This%High = This%High - Borrow
    END IF

    RETURN

END SUBROUTINE I128_Subtract_I32

!******************************************************************************

MODULE SUBROUTINE I128_Subtract_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  This = This - Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt64,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow, OutLo

!** FLOW

    CALL SubU64(This%Low, Other, 0_kInt64, OutLo, Borrow)
    This%Low  = OutLo
    IF (Other < 0_kInt64) THEN
        This%High = This%High + 1_kInt64 - Borrow
    ELSE
        This%High = This%High - Borrow
    END IF

    RETURN

END SUBROUTINE I128_Subtract_I64

!******************************************************************************

MODULE SUBROUTINE I128_Subtract_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  This = This - Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt128, INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow, OutLo, OutHi

!** FLOW

    CALL SubU64(This%Low, Other%Low, 0_kInt64, OutLo, Borrow)
    CALL SubU64(This%High, Other%High, Borrow, OutHi)
    This%Low  = OutLo
    This%High = OutHi

    RETURN

END SUBROUTINE I128_Subtract_I128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 MULTIPLICATION OPERATIONS                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE I128_Times_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  This = This * Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt32,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X_Lo, Y_Lo, Y_Hi, ProductHi
    tUInt64     :: AbsOther

!** FLOW

    AbsOther = ABS(Other)

    ! perform 'UMul128_Upper64'
    X_Lo = IAND(AbsOther, Mask32)
    Y_Lo = IAND(This%Low, Mask32)
    Y_Hi = SHIFTR(This%Low, 32)
    ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)

    This%High = AbsOther * This%High + ProductHi
    This%Low  = AbsOther * This%Low

    IF (Other < 0) This = -This

    RETURN

END SUBROUTINE I128_Times_I32

!******************************************************************************

MODULE SUBROUTINE I128_Times_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  This = This * Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt64,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: AbsOther

!** FLOW

    AbsOther = ABS(Other)

    This%High = This%High * AbsOther + UMul128_Upper64(This%Low, AbsOther)
    This%Low  = This%Low * AbsOther

    IF (Other < 0_kInt64) This = -This

    RETURN

END SUBROUTINE I128_Times_I64

!******************************************************************************

MODULE SUBROUTINE I128_Times_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  This = This * Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(INOUT) :: This
    tSInt128, INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    This%High = This%Low * Other%High + This%High * Other%Low + &
                UMul128_Upper64(This%Low, Other%Low)
    This%Low  = This%Low * Other%Low

    RETURN

END SUBROUTINE I128_Times_I128

!******************************************************************************

MODULE FUNCTION I32_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X_Lo, Y_Lo, Y_Hi, ProductHi
    tUInt64     :: AbsLhs

!** FLOW

    AbsLhs = ABS(LhsVal)

    ! perform 'UMul128_Upper64'
    X_Lo = IAND(AbsLhs, Mask32)
    Y_Lo = IAND(RhsVal%Low, Mask32)
    Y_Hi = SHIFTR(RhsVal%Low, 32)
    ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)

    OutVal%High = AbsLhs * RhsVal%High + ProductHi
    OutVal%Low  = AbsLhs * RhsVal%Low

    IF (LhsVal < 0) OutVal = -OutVal

    RETURN

END FUNCTION I32_Multiply_I128

!******************************************************************************

MODULE FUNCTION I128_Multiply_I32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt32,  INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X_Lo, Y_Lo, Y_Hi, ProductHi
    tUInt64     :: AbsRhs

!** FLOW

    AbsRhs = ABS(RhsVal)

    ! perform 'UMul128_Upper64'
    X_Lo = IAND(AbsRhs, Mask32)
    Y_Lo = IAND(LhsVal%Low, Mask32)
    Y_Hi = SHIFTR(LhsVal%Low, 32)
    ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)

    OutVal%High = LhsVal%High * AbsRhs + ProductHi
    OutVal%Low  = LhsVal%Low * AbsRhs

    IF (RhsVal < 0) OutVal = -OutVal

    RETURN

END FUNCTION I128_Multiply_I32

!******************************************************************************

MODULE FUNCTION I64_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,  INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: AbsLhs

!** FLOW

    AbsLhs = ABS(LhsVal)

    OutVal%High = AbsLhs * RhsVal%High + UMul128_Upper64(AbsLhs, RhsVal%Low)
    OutVal%Low  = AbsLhs * RhsVal%Low

    IF (LhsVal < 0_kInt64) OutVal = -OutVal

    RETURN

END FUNCTION I64_Multiply_I128

!******************************************************************************

MODULE FUNCTION I128_Multiply_I64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt64,  INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: AbsRhs

!** FLOW

    AbsRhs = ABS(RhsVal)

    OutVal%High = LhsVal%High * AbsRhs + UMul128_Upper64(LhsVal%Low, AbsRhs)
    OutVal%Low  = LhsVal%Low * AbsRhs

    IF (RhsVal < 0_kInt64) OutVal = -OutVal

    RETURN

END FUNCTION I128_Multiply_I64

!******************************************************************************

MODULE FUNCTION I128_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication of two SInt128 objects (Lhs * Rhs).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: LhsVal
    tSInt128, INTENT(IN)    :: RhsVal
    tSInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = LhsVal%Low * RhsVal%High + LhsVal%High * RhsVal%Low + &
                  UMul128_Upper64(LhsVal%Low, RhsVal%Low)
    OutVal%Low  = LhsVal%Low * RhsVal%Low

    RETURN

END FUNCTION I128_Multiply_I128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++             DIVISION/MODULATION OPERATIONS               ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE I128_DivMod_I32(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.  <br>
    !  Return both quotient and remainder.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt32,  INTENT(IN)    :: Divisor
    tSInt128, INTENT(OUT)   :: Quotient
    tSInt128, INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END SUBROUTINE I128_DivMod_I32

!******************************************************************************

MODULE SUBROUTINE I128_DivMod_I64(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.  <br>
    !  Return both quotient and remainder.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt64,  INTENT(IN)    :: Divisor
    tSInt128, INTENT(OUT)   :: Quotient
    tSInt128, INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END SUBROUTINE I128_DivMod_I64

!******************************************************************************

MODULE SUBROUTINE I128_DivMod_I128(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.  <br>
    !  Return both quotient and remainder.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt128, INTENT(IN)    :: Divisor
    tSInt128, INTENT(OUT)   :: Quotient
    tSInt128, INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: UQuotient, URemainder

!** FLOW

    IF ((Dividend == MinI128).AND.(Divisor == -OneI128)) THEN
        ! not applicable for unsigned binary on two's complement
        CALL Handle_ErrLevel('I128_Divide_I128', ModName, ErrSevere, &
                          'Dividend = MinI128 and Divisor = -1')
        RETURN
    END IF

    CALL UDivMod(UABS(Dividend), UABS(Divisor), UQuotient, URemainder)

    IF ((Dividend%High < 0_kInt64) .NEQV. (Divisor%High < 0_kInt64)) UQuotient = -UQuotient
    IF (Dividend%High < 0_kInt64) URemainder = -URemainder

    Quotient%High  = BitCastToSigned(UQuotient%High)
    Quotient%Low   = UQuotient%Low
    Remainder%High = BitCastToSigned(URemainder%High)
    Remainder%Low  = URemainder%Low

    RETURN

END SUBROUTINE I128_DivMod_I128

!******************************************************************************

MODULE SUBROUTINE I128_Over_I32(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  This = This / Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128,          INTENT(INOUT)    :: This
    tSInt32,           INTENT(IN)       :: Other
    tSInt32, OPTIONAL, INTENT(OUT)      :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Dividend, Rem

!** FLOW

    Dividend = This
    CALL DivMod(Dividend, SInt128(Other), This, Rem)
    IF (PRESENT(Remainder)) Remainder = Rem

    RETURN

END SUBROUTINE I128_Over_I32

!******************************************************************************

MODULE SUBROUTINE I128_Over_I64(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  This = This / Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128,          INTENT(INOUT)    :: This
    tSInt64,           INTENT(IN)       :: Other
    tSInt64, OPTIONAL, INTENT(OUT)      :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Dividend, Rem

!** FLOW

    Dividend = This
    CALL DivMod(Dividend, SInt128(Other), This, Rem)
    IF (PRESENT(Remainder)) Remainder = Rem

    RETURN

END SUBROUTINE I128_Over_I64

!******************************************************************************

MODULE SUBROUTINE I128_Over_I128(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  This = This / Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128,           INTENT(INOUT)   :: This
    tSInt128,           INTENT(IN)      :: Other
    tSInt128, OPTIONAL, INTENT(OUT)     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Dividend, Rem

!** FLOW

    Dividend = This
    IF (PRESENT(Remainder)) THEN
        CALL DivMod(Dividend, Other, This, Remainder)
    ELSE
        CALL DivMod(Dividend, Other, This, Rem)
    END IF

    RETURN

END SUBROUTINE I128_Over_I128

!******************************************************************************

MODULE FUNCTION I128_Divide_I32(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt32,  INTENT(IN)    :: Divisor
    tSInt128                :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Remainder

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END FUNCTION I128_Divide_I32

!******************************************************************************

MODULE FUNCTION I128_Divide_I64(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt64,  INTENT(IN)    :: Divisor
    tSInt128                :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Remainder

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END FUNCTION I128_Divide_I64

!******************************************************************************

MODULE FUNCTION I128_Divide_I128(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division of two SInt128 objects (Dividend / Divisor)
    !  and return the quotient.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt128, INTENT(IN)    :: Divisor
    tSInt128                :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Remainder

!** FLOW

    CALL DivMod(Dividend, Divisor, Quotient, Remainder)

    RETURN

END FUNCTION I128_Divide_I128

!******************************************************************************

MODULE FUNCTION I128_Mod_I32(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform modulation:  Remainder = Dividend MOD Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt32,  INTENT(IN)    :: Divisor
    tSInt128                :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Quotient

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END FUNCTION I128_Mod_I32

!******************************************************************************

MODULE FUNCTION I128_Mod_I64(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform modulation:  Remainder = Dividend MOD Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt64,  INTENT(IN)    :: Divisor
    tSInt128                :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Quotient

!** FLOW

    CALL DivMod(Dividend, SInt128(Divisor), Quotient, Remainder)

    RETURN

END FUNCTION I128_Mod_I64

!******************************************************************************

MODULE FUNCTION I128_Mod_I128(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division of two SInt128 objects (Dividend / Divisor)
    !  and return the remainder.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Dividend
    tSInt128, INTENT(IN)    :: Divisor
    tSInt128                :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128    :: Quotient

!** FLOW

    CALL DivMod(Dividend, Divisor, Quotient, Remainder)

    RETURN

END FUNCTION I128_Mod_I128

!******************************************************************************

END SUBMODULE SubBase_I128_Arithmetic

!******************************************************************************
