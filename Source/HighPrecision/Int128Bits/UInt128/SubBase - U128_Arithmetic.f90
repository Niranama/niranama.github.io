
SUBMODULE (ModBase_UInt128) SubBase_U128_Arithmetic

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to arithmetic
!   operations of the <a href="../module/modbase_uint128.html">UInt128</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
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

MODULE FUNCTION U128_UnaryPlus(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return result of the unary plus sign of the Uint128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = InVal

    RETURN

END FUNCTION U128_UnaryPlus

!******************************************************************************

MODULE FUNCTION U128_Plus_U32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt32,  INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, ToUnsignedLong(RhsVal), 0_kInt64, OutVal%Low, Carry)
    OutVal%High = LhsVal%High + Carry

    RETURN

END FUNCTION U128_Plus_U32

!******************************************************************************

MODULE FUNCTION U32_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,  INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(ToUnsignedLong(LhsVal), RhsVal%Low, 0_kInt64, OutVal%Low, Carry)
    OutVal%High = RhsVal%High + Carry

    RETURN

END FUNCTION U32_Plus_U128

!******************************************************************************

MODULE FUNCTION U128_Plus_U64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt64,  INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, RhsVal, 0_kInt64, OutVal%Low, Carry)
    OutVal%High = LhsVal%High + Carry

    RETURN

END FUNCTION U128_Plus_U64

!******************************************************************************

MODULE FUNCTION U64_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(LhsVal, RhsVal%Low, 0_kInt64, OutVal%Low, Carry)
    OutVal%High = RhsVal%High + Carry

    RETURN

END FUNCTION U64_Plus_U128

!******************************************************************************

MODULE FUNCTION U128_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  OutVal = LhsVal + RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL AddU64(LhsVal%Low, RhsVal%Low, 0_kInt64, OutVal%Low, Carry)
    CALL AddU64(LhsVal%High, RhsVal%High, Carry, OutVal%High)

    RETURN

END FUNCTION U128_Plus_U128

!******************************************************************************

MODULE SUBROUTINE U128_Increment(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To increase value of the input by 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: Val

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

END SUBROUTINE U128_Increment

!******************************************************************************

MODULE SUBROUTINE U128_Add_U32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  This = This + Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt32,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry, OutLo

!** FLOW

    CALL AddU64(This%Low, ToUnsignedLong(Other), 0_kInt64, OutLo, Carry)
    This%Low  = OutLo
    This%High = This%High + Carry

    RETURN

END SUBROUTINE U128_Add_U32

!******************************************************************************

MODULE SUBROUTINE U128_Add_U64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  This = This + Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt64,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry, OutLo

!** FLOW

    CALL AddU64(This%Low, Other, 0_kInt64, OutLo, Carry)
    This%Low  = OutLo
    This%High = This%High + Carry

    RETURN

END SUBROUTINE U128_Add_U64

!******************************************************************************

MODULE SUBROUTINE U128_Add_U128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform addition:  This = This + Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt128, INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry, OutLo, OutHi

!** FLOW

    CALL AddU64(This%Low, Other%Low, 0_kInt64, OutLo, Carry)
    CALL AddU64(This%High, Other%High, Carry, OutHi)
    This%Low  = OutLo
    This%High = OutHi

    RETURN

END SUBROUTINE U128_Add_U128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  SUBTRACTION OPERATIONS                  ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION U128_Negate(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To negate the Uint128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = NOT(InVal%High)
    IF (InVal%Low == 0_kInt64) OutVal%High = OutVal%High + 1_kInt64
    OutVal%Low = NOT(InVal%Low) + 1_kInt64

    RETURN

END FUNCTION U128_Negate

!******************************************************************************

MODULE FUNCTION U128_Minus_U32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt32,  INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, ToUnsignedLong(RhsVal), 0_kInt64, OutVal%Low, Borrow)
    OutVal%High = LhsVal%High - Borrow

    RETURN

END FUNCTION U128_Minus_U32

!******************************************************************************

MODULE FUNCTION U32_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,  INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(ToUnsignedLong(LhsVal), RhsVal%Low, 0_kInt64, OutVal%Low, Borrow)
    OutVal%High = -(RhsVal%High + Borrow)

    RETURN

END FUNCTION U32_Minus_U128

!******************************************************************************

MODULE FUNCTION U128_Minus_U64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt64,  INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, RhsVal, 0_kInt64, OutVal%Low, Borrow)
    OutVal%High = LhsVal%High - Borrow

    RETURN

END FUNCTION U128_Minus_U64

!******************************************************************************

MODULE FUNCTION U64_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(LhsVal, RhsVal%Low, 0_kInt64, OutVal%Low, Borrow)
    OutVal%High = -(RhsVal%High + Borrow)

    RETURN

END FUNCTION U64_Minus_U128

!******************************************************************************

MODULE FUNCTION U128_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  OutVal = LhsVal - RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow

!** FLOW

    CALL SubU64(LhsVal%Low, RhsVal%Low, 0_kInt64, OutVal%Low, Borrow)
    CALL SubU64(LhsVal%High, RhsVal%High, Borrow, OutVal%High)

    RETURN

END FUNCTION U128_Minus_U128

!******************************************************************************

MODULE SUBROUTINE U128_Decrement(Val)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To decrease value of the input by 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    !

!** FLOW

    IF (Val%Low == 0_kInt64) THEN
        Val%High = Val%High - 1_kInt64
        Val%Low  = -1_kInt64
    ELSE
        Val%Low  = Val%Low - 1_kInt64
    END IF

    RETURN

END SUBROUTINE U128_Decrement

!******************************************************************************

MODULE SUBROUTINE U128_Subtract_U32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  This = This - Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt32,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow, OutLo

!** FLOW

    CALL SubU64(This%Low, ToUnsignedLong(Other), 0_kInt64, OutLo, Borrow)
    This%Low  = OutLo
    This%High = This%High - Borrow

    RETURN

END SUBROUTINE U128_Subtract_U32

!******************************************************************************

MODULE SUBROUTINE U128_Subtract_U64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  This = This - Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt64,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow, OutLo

!** FLOW

    CALL SubU64(This%Low, Other, 0_kInt64, OutLo, Borrow)
    This%Low  = OutLo
    This%High = This%High - Borrow

    RETURN

END SUBROUTINE U128_Subtract_U64

!******************************************************************************

MODULE SUBROUTINE U128_Subtract_U128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform subtraction:  This = This - Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt128, INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Borrow, OutLo, OutHi

!** FLOW

    CALL SubU64(This%Low, Other%Low, 0_kInt64, OutLo, Borrow)
    CALL SubU64(This%High, Other%High, Borrow, OutHi)
    This%Low  = OutLo
    This%High = OutHi

    RETURN

END SUBROUTINE U128_Subtract_U128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 MULTIPLICATION OPERATIONS                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE U128_Times_U32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  This = This * Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt32,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X_Lo, Y_Lo, Y_Hi
    tUInt64     :: Lo_Lo, Cross

!** FLOW

    X_Lo = IAND(ToInt64(Other), Mask32)
    Y_Lo = IAND(This%Low, Mask32)
    Y_Hi = SHIFTR(This%Low, 32)
    Lo_Lo = X_Lo*Y_Lo
    Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi

    This%High = This%High*Other + SHIFTR(Cross, 32)
    This%Low  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

    RETURN

END SUBROUTINE U128_Times_U32

!******************************************************************************

MODULE SUBROUTINE U128_Times_U64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  This = This * Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt64,  INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry, ProductLow

!** FLOW

    CALL UMul128(This%Low, Other, Carry, ProductLow)
    This%High = This%High*Other + Carry
    This%Low  = ProductLow

    RETURN

END SUBROUTINE U128_Times_U64

!******************************************************************************

MODULE SUBROUTINE U128_Times_U128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  This = This * Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(INOUT) :: This
    tUInt128, INTENT(IN)    :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry, ProductLow

!** FLOW

    CALL UMul128(This%Low, Other%Low, Carry, ProductLow)
    This%High = This%Low*Other%High + This%High*Other%Low + Carry
    This%Low  = ProductLow

    RETURN

END SUBROUTINE U128_Times_U128

!******************************************************************************

MODULE FUNCTION U32_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,  INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X_Lo, Y_Lo, Y_Hi
    tUInt64     :: Lo_Lo, Cross

!** FLOW

    X_Lo = IAND(ToInt64(LhsVal), Mask32)
    Y_Lo = IAND(RhsVal%Low, Mask32)
    Y_Hi = SHIFTR(RhsVal%Low, 32)
    Lo_Lo = X_Lo*Y_Lo
    Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi

    OutVal%High = LhsVal*RhsVal%High + SHIFTR(Cross, 32)
    OutVal%Low  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

    RETURN

END FUNCTION U32_Multiply_U128

!******************************************************************************

MODULE FUNCTION U128_Multiply_U32(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt32,  INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X_Lo, Y_Lo, Y_Hi
    tUInt64     :: Lo_Lo, Cross

!** FLOW

    X_Lo = IAND(ToInt64(RhsVal), Mask32)
    Y_Lo = IAND(LhsVal%Low, Mask32)
    Y_Hi = SHIFTR(LhsVal%Low, 32)
    Lo_Lo = X_Lo*Y_Lo
    Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi

    OutVal%High = RhsVal*LhsVal%High + SHIFTR(Cross, 32)
    OutVal%Low  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

    RETURN

END FUNCTION U128_Multiply_U32

!******************************************************************************

MODULE FUNCTION U64_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL UMul128(LhsVal, RhsVal%Low, Carry, OutVal%Low)
    OutVal%High = LhsVal*RhsVal%High + Carry

    RETURN

END FUNCTION U64_Multiply_U128

!******************************************************************************

MODULE FUNCTION U128_Multiply_U64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt64,  INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL UMul128(LhsVal%Low, RhsVal, Carry, OutVal%Low)
    OutVal%High = LhsVal%High*RhsVal + Carry

    RETURN

END FUNCTION U128_Multiply_U64

!******************************************************************************

MODULE FUNCTION U128_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication:  OutVal = LhsVal * RhsVal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry

!** FLOW

    CALL UMul128(LhsVal%Low, RhsVal%Low, Carry, OutVal%Low)
    OutVal%High = LhsVal%Low*RhsVal%High + LhsVal%High*RhsVal%Low + Carry

    RETURN

END FUNCTION U128_Multiply_U128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++             DIVISION/MODULATION OPERATIONS               ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE U128_DivMod_U32(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor. <br>
    !  Return both quotient and remainder.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt32,  INTENT(IN)    :: Divisor
    tUInt128, INTENT(OUT)   :: Quotient
    tUInt128, INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END SUBROUTINE U128_DivMod_U32

!******************************************************************************

MODULE SUBROUTINE U128_DivMod_U64(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor. <br>
    !  Return both quotient and remainder.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt64,  INTENT(IN)    :: Divisor
    tUInt128, INTENT(OUT)   :: Quotient
    tUInt128, INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END SUBROUTINE U128_DivMod_U64

!******************************************************************************

MODULE SUBROUTINE U128_DivMod_U128(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor. <br>
    !  Return both quotient and remainder.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt128, INTENT(IN)    :: Divisor
    tUInt128, INTENT(OUT)   :: Quotient
    tUInt128, INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Dividend%High < 0_kInt64) THEN
        CALL U128_DivMod_Intx(Dividend, Divisor, Quotient, Remainder)
    ELSE
        CALL U128_DivMod_Java(Dividend, Divisor, Quotient, Remainder)
    END IF

    RETURN

END SUBROUTINE U128_DivMod_U128

!******************************************************************************

MODULE SUBROUTINE U128_Over_U32(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  This = This / Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128,          INTENT(INOUT)    :: This
    tUInt32,           INTENT(IN)       :: Other
    tUInt32, OPTIONAL, INTENT(OUT)      :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Dividend, Rem

!** FLOW

    Dividend = This
    CALL U128_DivMod_U128(Dividend, UInt128(Other, AsUnsigned), This, Rem)
    IF (PRESENT(Remainder)) Remainder = Rem

    RETURN

END SUBROUTINE U128_Over_U32

!******************************************************************************

MODULE SUBROUTINE U128_Over_U64(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  This = This / Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128,          INTENT(INOUT)    :: This
    tUInt64,           INTENT(IN)       :: Other
    tUInt64, OPTIONAL, INTENT(OUT)      :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Dividend, Rem

!** FLOW

    Dividend = This
    CALL U128_DivMod_U128(Dividend, UInt128(Other, AsUnsigned), This, Rem)
    IF (PRESENT(Remainder)) Remainder = Rem

    RETURN

END SUBROUTINE U128_Over_U64

!******************************************************************************

MODULE SUBROUTINE U128_Over_U128(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  This = This / Other.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128,           INTENT(INOUT)   :: This
    tUInt128,           INTENT(IN)      :: Other
    tUInt128, OPTIONAL, INTENT(OUT)     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Dividend, Rem

!** FLOW

    Dividend = This
    IF (PRESENT(Remainder)) THEN
        CALL U128_DivMod_U128(Dividend, Other, This, Remainder)
    ELSE
        CALL U128_DivMod_U128(Dividend, Other, This, Rem)
    END IF

    RETURN

END SUBROUTINE U128_Over_U128

!******************************************************************************

MODULE FUNCTION U128_Divide_U32(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt32,  INTENT(IN)    :: Divisor
    tUInt128                :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Remainder

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END FUNCTION U128_Divide_U32

!******************************************************************************

MODULE FUNCTION U128_Divide_U64(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt64,  INTENT(IN)    :: Divisor
    tUInt128                :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Remainder

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END FUNCTION U128_Divide_U64

!******************************************************************************

MODULE FUNCTION U128_Divide_U128(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division:  Quotient = Dividend / Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt128, INTENT(IN)    :: Divisor
    tUInt128                :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Remainder

!** FLOW

    CALL U128_DivMod_U128(Dividend, Divisor, Quotient, Remainder)

    RETURN

END FUNCTION U128_Divide_U128

!******************************************************************************

MODULE FUNCTION U128_Mod_U32(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform modulation:  Remainder = Dividend MOD Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt32,  INTENT(IN)    :: Divisor
    tUInt128                :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Quotient

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END FUNCTION U128_Mod_U32

!******************************************************************************

MODULE FUNCTION U128_Mod_U64(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform modulation:  Remainder = Dividend MOD Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt64,  INTENT(IN)    :: Divisor
    tUInt128                :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Quotient

!** FLOW

    CALL U128_DivMod_U128(Dividend, UInt128(Divisor, AsUnsigned), Quotient, Remainder)

    RETURN

END FUNCTION U128_Mod_U64

!******************************************************************************

MODULE FUNCTION U128_Mod_U128(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform modulation:  Remainder = Dividend MOD Divisor.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt128, INTENT(IN)    :: Divisor
    tUInt128                :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: Quotient

!** FLOW

    CALL U128_DivMod_U128(Dividend, Divisor, Quotient, Remainder)

    RETURN

END FUNCTION U128_Mod_U128

!******************************************************************************

MODULE SUBROUTINE U128_DivMod_Java(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division of two UInt128 objects (Dividend / Divisor)
    !  and return both the quotient and the remainder. <br>
    !  This routine is based on reference #2.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt128, INTENT(IN)    :: Divisor
    tUInt128, INTENT(OUT)   :: Quotient
    tUInt128, INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: NumerLZ
    tUInt32     :: DenomLZ
    tUInt32     :: DenomTZ
    tSInt32     :: CompFlag

!** FLOW

    IF (Divisor == ZeroU128) THEN
        ! division by zero
        Quotient  = ZeroU128
        Remainder = ZeroU128
        CALL Handle_ErrLevel('U128_DivRem', ModName, ErrSevere, 'The divisor must not be zero.')
        RETURN
    END IF

    CompFlag = CompareU128(Dividend%High, Dividend%Low, Divisor%High, Divisor%Low)
    IF (CompFlag < 0) THEN
        ! divisor > dividend
        Quotient  = ZeroU128
        Remainder = Dividend
        RETURN
    ELSEIF (CompFlag == 0) THEN
        ! divisor == dividend
        Quotient  = OneU128
        Remainder = ZeroU128
        RETURN
    END IF

    NumerLZ = LEADZ(Dividend)
    DenomLZ = LEADZ(Divisor)
    DenomTZ = TRAILZ(Divisor)

    IF (DenomLZ == 128) THEN
        CALL Handle_ErrLevel('PositiveDivision', ModName, ErrSevere, 'Divide by zero.')
        RETURN
    ELSEIF (IOR(Dividend%High, Divisor%High) == 0_kInt64) THEN
        ! dividend and divisor fit in an unsigned
        CALL UDivMod(Dividend%Low, Divisor%Low, Quotient%Low, Remainder%Low)
        Quotient%High  = 0_kInt64
        Remainder%High = 0_kInt64
        RETURN
    ELSEIF (DenomLZ == 127) THEN
        ! divisor is 1
        Quotient  = Dividend
        Remainder = ZeroU128
        RETURN
    ELSEIF ((DenomTZ + DenomLZ) == 127) THEN
        ! only one bit set (i.e., power of 2), so just shift
        Quotient  = SHIFTR(Dividend, DenomTZ)
        Remainder = IAND(Dividend, Divisor - OneU128)
        RETURN
    END IF

    IF ((DenomLZ - NumerLZ) > 15) THEN
        ! fast divide when the values differ by this many orders of magnitude
        CALL FastDivision(Dividend, Divisor, Quotient, Remainder)
    ELSE
        CALL BinaryDivision(Dividend, Divisor, NumerLZ, DenomLZ, Quotient, Remainder)
    END IF

    RETURN

CONTAINS

    SUBROUTINE BinaryDivision(Numerator, Denominator, NumerLZ, DenomLZ, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform division of two UInt128 objects (Dividend / Divisor)
        !  using binary-division algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: Numerator, Denominator
        tUInt32,  INTENT(IN)   :: NumerLZ, DenomLZ
        tUInt128, INTENT(OUT)  :: Quotient
        tUInt128, INTENT(OUT)  :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: Shift
        tUInt64     :: NumerHi, NumerLo, DenomHi, DenomLo
        tUInt64     :: QuotHi, QuotLo

    !** FLOW

        ! initialize
        NumerHi = Numerator%High
        NumerLo = Numerator%Low

        ! shift should always be posivite since the routine is only called
        ! when divisor magnitude is less than dividen magnitude
        Shift   = DenomLZ - NumerLZ
        IF (Shift >= 64) THEN
            DenomHi = SHIFTL(Denominator%Low, Shift - 64)
            DenomLo = 0_kInt64
        ELSE
            DenomHi = IOR(SHIFTL(Denominator%High, Shift), SHIFTR(Denominator%Low, 64 - Shift))
            DenomLo = SHIFTL(Denominator%Low, Shift)
        END IF
        QuotHi = 0_kInt64
        QuotLo = 0_kInt64

        DO
            ! quotient = SHIFTL(quotient, 1)
            QuotHi = IOR(SHIFTL(QuotHi, 1), SHIFTR(QuotLo, 63))
            QuotLo = SHIFTL(QuotLo, 1)

            ! if (dividend >= divisor)
            IF (CompareU128(NumerHi, NumerLo, DenomHi, DenomLo) >= 0) THEN
                ! dividend = dividend - divisor
                IF (IEOR(NumerLo, MinI64) < IEOR(DenomLo, MinI64)) THEN
                    NumerHi = NumerHi - DenomHi - 1_kInt64
                    NumerLo = NumerLo - DenomLo
                ELSE
                    NumerHi = NumerHi - DenomHi
                    NumerLo = NumerLo - DenomLo
                END IF

                ! quotient = IOR(quotient, 1)
                QuotLo = IOR(QuotLo, 1_kInt64)
            END IF

            ! divisor = SHIFTR(divisor, 1)
            DenomLo = IOR(SHIFTR(DenomLo, 1), SHIFTL(SHIFTL(DenomHi, 1), 62))
            DenomHi = SHIFTR(DenomHi, 1)

            IF (Shift == 0) EXIT
            Shift = Shift - 1
        END DO

        ! set output
        Quotient%High  = QuotHi
        Quotient%Low   = QuotLo
        Remainder%High = NumerHi
        Remainder%Low  = NumerLo

        RETURN

    END SUBROUTINE BinaryDivision

    !**************************************************************************

    SUBROUTINE FastDivision(Dividend, Divisor, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform division of two UInt128 objects (Dividend / Divisor)
        !  using fast-division algorithm(s).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: Dividend
        tUInt128, INTENT(IN)    :: Divisor
        tUInt128, INTENT(OUT)   :: Quotient
        tUInt128, INTENT(OUT)   :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: NLZ
        tUInt64     :: NumHi, NumLo
        tUInt64     :: V1Hi, U1Hi, U1Lo
        tUInt64     :: QLo, RLo

    !** FLOW

        IF (Divisor%High == 0_kInt64) THEN
            IF (CompareUnsigned(Dividend%High, Divisor%Low) < 0) THEN
                CALL DivideU128ByU64(Dividend%High, Dividend%Low, Divisor%Low, &
                                    Quotient%Low, Remainder%Low)
                Quotient%High  = 0_kInt64
                Remainder%High = 0_kInt64
            ELSE
                CALL UDivMod(Dividend%High, Divisor%Low, Quotient%High, NumHi)
                NumLo = Dividend%Low
                CALL DivideU128ByU64(NumHi, NumLo, Divisor%Low, Quotient%Low, Remainder%Low)
                Remainder%High = 0_kInt64
            END IF
        ELSE
            NLZ = LEADZ(Divisor%High)
            ! v1 = divisor << nlz
            IF (NLZ < 64) THEN
                V1Hi = IOR(SHIFTL(Divisor%High, NLZ), SHIFTR(Divisor%Low, 64 - NLZ))
            ELSE
                V1Hi = SHIFTL(Divisor%Low, NLZ - 64)
            END IF
            ! u1 = dividend >>> 1
            U1Lo = IOR(SHIFTR(Dividend%Low, 1), SHIFTL(SHIFTL(Dividend%High, 1), 62))
            U1Hi = SHIFTR(Dividend%High, 1)
            CALL DivideU128ByU64(U1Hi, U1Lo, V1Hi, QLo, RLo)
            ! q1 = q1 >>> (63 - nlz)
            QLo  = SHIFTR(QLo, 63 - NLZ)
            ! if (q1 /= 0)
            IF (QLo /= 0_kInt64) QLo = QLo - 1_kInt64
            ! r = dividend - q1 * divisor
            Remainder = Dividend - (Divisor * QLo)
            Quotient%High = 0_kInt64
            IF (CompareU128(Remainder%High, Remainder%Low, Divisor%High, Divisor%Low) >= 0) THEN
                ! quotient++
                QLo = QLo + 1_kInt64
                IF (QLo == 0_kInt64) Quotient%High = 1_kInt64
                ! remainder -= divisor
                CALL Subtract(Remainder, Divisor)
            END IF
            Quotient%Low = QLo
        END IF

        RETURN

    END SUBROUTINE FastDivision

    !**************************************************************************

    SUBROUTINE DivideU128ByU64(DividendHi, DividendLo, Divisor, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform division of 128-bit unsigned integer by 64-bit unsigned integer
        !  and return quotient and remainder as 64-bit unsigned integers. <br>
        !  This routine is only applicable for cases where the divisor is greater
        !   than the upper half of the dividend (i.e. Divisor .UGT. DividendHi). <br>
        ! Note: This routine is based on 'divlu' of Hacker's delight and its derivative
        !       (division routine in Fast 128-bit math library for Java).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: DividendHi, DividendLo
        tUInt64, INTENT(IN)     :: Divisor
        tUInt64, INTENT(OUT)    :: Quotient
        tUInt64, INTENT(OUT)    :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER  :: Mask = ToInt64(Z'01000000FFFFFFFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: Shift
        tUInt64     :: NHi, NLo         ! N -> numerator
        tUInt64     :: LoHi, LoLo
        tUInt64     :: RHat, UHat
        tUInt64     :: Denom, DHi, DLo  ! D -> denominator
        tUInt64     :: QHi, QLo

    !** FLOW

        ! initialize
        Denom = Divisor
        NHi   = DividendHi
        NLo   = DividendLo
        Shift = LEADZ(Denom)

        IF (Shift /= 0) THEN
            Denom = SHIFTL(Denom, Shift)
            NHi = IOR(SHIFTL(NHi, Shift), SHIFTR(NLo, 64 - Shift))
            NLo = SHIFTL(NLo, Shift)
        END IF

        DHi  = SHIFTR(Denom, 32)
        DLo  = IAND(Denom, Mask)
        LoHi = SHIFTR(NLo, 32)
        LoLo = IAND(NLo, Mask)

        ! Compute NHi quotient digit.
        CALL UDivMod(NHi, DHi, QHi, RHat)

        ! qhat >>> 32 == qhat > base
        DO WHILE ((SHIFTR(QHi, 32) /= 0_kInt64).OR. &
                  (IEOR(QHi * DLo, MinI64) > IEOR(IOR(SHIFTL(RHat, 32), LoHi), MinI64)))
            QHi = QHi - 1_kInt64
            RHat = RHat + DHi
            IF (SHIFTR(RHat, 32) /= 0_kInt64) EXIT
        END DO

        UHat = IOR(SHIFTL(NHi, 32), LoHi) - QHi * Denom

        ! Compute NLo quotient digit.
        CALL UDivMod(UHat, DHi, QLo, RHat)

        DO WHILE ((SHIFTR(QLo, 32) /= 0_kInt64).OR.   &
                  (IEOR(QLo * DLo, MinI64) > IEOR(IOR(SHIFTL(RHat, 32), LoLo), MinI64)))
            QLo = QLo - 1
            RHat = RHat + DHi
            IF (SHIFTR(RHat, 32) /= 0_kInt64) EXIT
        END DO

        Quotient  = IOR(SHIFTL(QHi, 32), QLo)
        Remainder = SHIFTR(IOR(SHIFTL(UHat, 32), LoLo) - QLo * Denom, Shift)

        RETURN

    END SUBROUTINE DivideU128ByU64

    !**************************************************************************

    FUNCTION CompareU128(LhsHi, LhsLo, RhsHi, RhsLo) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To compare LHS and RHS where both numbers are treated as unsigned. <br>
        ! - Return -1 if LHS < RHS. <br>
        ! - Return  0 if LHS == RHS. <br>
        ! - Return +1 if LHS > RHS.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: LhsHi, LhsLo, RhsHi, RhsLo
        tSInt32             :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: ULHS, URHS

    !** FLOW

        ! Flag = CompareUnsigned(LhsHi, RhsHi)
        ULHS = IEOR(LhsHi, MinI64)
        URHS = IEOR(RhsHi, MinI64)
        IF (ULHS < URHS) THEN
            Flag = -1
        ELSEIF (ULHS > URHS) THEN
            Flag = +1
        ELSE
            Flag = 0
        END IF

        IF (Flag == 0) THEN
            ! Flag = CompareUnsigned(LhsLo, RhsLo)
            ULHS = IEOR(LhsLo, MinI64)
            URHS = IEOR(RhsLo, MinI64)
            IF (ULHS < URHS) THEN
                Flag = -1
            ELSEIF (ULHS > URHS) THEN
                Flag = +1
            ELSE
                Flag = 0
            END IF
        END IF

        RETURN

    END FUNCTION CompareU128

    !**************************************************************************

END SUBROUTINE U128_DivMod_Java

!******************************************************************************

MODULE SUBROUTINE U128_DivMod_IntX(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform division of two UInt128 objects (Dividend / Divisor)
    !  and return both the quotient and the remainder. <br>
    !  This routine is based on reference #3.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Dividend
    tUInt128, INTENT(IN)    :: Divisor
    tUInt128, INTENT(OUT)   :: Quotient
    tUInt128, INTENT(OUT)   :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: LSh, RSh
    tUInt64     :: NumerHi, NumerLo, DenomHi, DenomLo
    tUInt64     :: NumerEx, Denom, RshMask
    tUInt64     :: V, R1, R2, Q1, Q2, LHS, RHS

!** FLOW

    IF (Divisor == ZeroU128) THEN
        ! division by zero
        Quotient  = ZeroU128
        Remainder = ZeroU128
        CALL Handle_ErrLevel('U128_DivMod', ModName, ErrSevere, 'The divisor must not be zero.')
        RETURN
    END IF

    IF (Divisor%High == 0) THEN
        LSh = LEADZ(Divisor%Low)
        IF (LSh == 0) THEN
            RSh = 0
            RShMask = 0_kInt64
        ELSE
            RSh = 64 - LSh
            RShMask = -1_kInt64
        END IF
        Denom   = SHIFTL(Divisor%Low, LSh)
        NumerLo = SHIFTL(Dividend%Low, LSh)
        NumerHi = IOR(SHIFTL(Dividend%High, LSh), IAND(SHIFTR(Dividend%Low, RSh), RShMask))
        NumerEx = IAND(SHIFTR(Dividend%High, RSh), RShMask)

        V = Reciprocal_2By1(Denom)
        CALL UDivRem_2By1(NumerEx, NumerHi, Denom, V, Q1, R1)
        CALL UDivRem_2By1(R1, NumerLo, Denom, V, Q2, R2)
        Quotient%High = Q1
        Quotient%Low  = Q2
        Remainder%High = 0_kInt64
        Remainder%Low  = SHIFTR(R2, LSh)
        RETURN
    END IF

    IF (IEOR(Divisor%High, MinI64) > IEOR(Dividend%High, MinI64)) THEN
        ! divisor > dividend
        Quotient  = ZeroU128
        Remainder = Dividend
        RETURN
    END IF

    LSh = LEADZ(Divisor%High)
    IF (LSh == 0) THEN
        IF (IEOR(Divisor%High, MinI64) < IEOR(Dividend%High, MinI64)) THEN
            LHS = 1_kInt64
        ELSE
            LHS = 0_kInt64
        END IF
        IF (IEOR(Divisor%Low, MinI64) <= IEOR(Dividend%Low, MinI64)) THEN
            RHS = 1_kInt64
        ELSE
            RHS = 0_kInt64
        END IF
        Quotient%High = 0_kInt64
        Quotient%Low  = IOR(LHS, RHS)
        IF (Quotient%Low  == 0_kInt64) THEN
            Remainder = Dividend
        ELSE
            ! Remainder = Dividend - Divisor
            Remainder%Low  = Dividend%Low - Divisor%Low
            IF (IEOR(Dividend%Low, MinI64) < IEOR(Divisor%Low, MinI64)) THEN
                Remainder%High = Dividend%High - Divisor%High - 1_kInt64
            ELSE
                Remainder%High = Dividend%High - Divisor%High
            END IF
        END IF
        RETURN
    END IF

    RSh = 64 - LSh

    DenomLo = SHIFTL(Divisor%Low, LSh)
    DenomHi = IOR(SHIFTL(Divisor%High, LSh), SHIFTR(Divisor%Low, RSh))
    NumerLo = SHIFTL(Dividend%Low, LSh)
    NumerHi = IOR(SHIFTL(Dividend%High, LSh), SHIFTR(Dividend%Low, RSh))
    NumerEx = SHIFTR(Dividend%High, RSh)

    V = Reciprocal_3By2(DenomHi, DenomLo)
    CALL UDivRem_3By2(NumerEx, NumerHi, NumerLo, DenomHi, DenomLo, V, Q2, R1, R2)

    Quotient%High = 0_kInt64
    Quotient%Low  = Q2
    Remainder%High = SHIFTR(R1, LSh)
    Remainder%Low  = IOR(SHIFTR(R2, LSh), SHIFTL(R1, 64 - LSh))

    RETURN

    CONTAINS

    FUNCTION Reciprocal_2By1(D) RESULT(R)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To compute the reciprocal (2**128 - 1) / D - 2**64 for normalized D
        !  based on Algorithm 2 from "Improved division by invariant integers".

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: D
        tUInt64             :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: D0, D9, D40, D63, E, T
        tUInt64     :: PHi, PLo, V0, V1, V2, V3

    !** FLOW

        D9  = SHIFTR(D, 55)
        V0  = ToInt64(RecTable(ToInt32(D9) - 256))

        D40 = SHIFTR(D, 24) + 1
        V1  = SHIFTL(V0, 11) - SHIFTR((V0 * V0) * D40, 40) - 1_kInt64

        V2  = SHIFTL(V1, 13) + SHIFTR(V1 * (ToInt64(Z'1000000000000000') - V1 * D40), 47)

        D0  = IAND(D, 1_kInt64)
        D63 = SHIFTR(D, 1) + D0     ! ceil(D/2)
        E   = IAND(SHIFTR(V2, 1), (0_kInt64 - D0)) - V2 * D63
        CALL UMul128(V2, E, PHi, PLo)
        V3  = SHIFTR(PHi, 1) + SHIFTL(V2, 31)

        CALL UMul128(V3, D, PHi, PLo)
        T = PLo + D
        IF (IEOR(T, MinI64) < IEOR(PLo, MinI64)) PHi = PHi + 1
        R = V3 - PHi - D

        RETURN

    END FUNCTION Reciprocal_2By1

    !**************************************************************************

    FUNCTION Reciprocal_3By2(DHi, DLo) RESULT(R)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To compute the reciprocal (2**128 - 1) / D - 2**64 for normalized D
        !  based on Algorithm 2 from "Improved division by invariant integers".

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: DHi, DLo
        tUInt64             :: R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: V, P, THi, TLo

    !** FLOW

        V = Reciprocal_2By1(DHi)
        P = DHi * V
        P = P + DLo
        IF (IEOR(P, MinI64) < IEOR(DLo, MinI64)) THEN
            V = V - 1_kInt64
            IF (IEOR(P, MinI64) >= IEOR(DHi, MinI64)) THEN
                V = V - 1_kInt64
                P = P - DHi
            END IF
            P = P - DHi
        END IF

        CALL UMul128(V, DLo, THi, TLo)

        P = P + THi
        IF (IEOR(P, MinI64) < IEOR(THi, MinI64)) THEN
            V = V - 1_kInt64
            IF (IEOR(P, MinI64) >= IEOR(DHi, MinI64)) THEN
                IF ((IEOR(P,   MinI64) >  IEOR(DHi, MinI64)).OR.&
                    (IEOR(TLo, MinI64) >= IEOR(DLo, MinI64))) V = V - 1_kInt64
            END IF
        END IF
        R = V

        RETURN

    END FUNCTION Reciprocal_3By2

    !**************************************************************************

    SUBROUTINE UDivRem_2By1(UHi, ULo, D, V, Q, R)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform 128-bit integer division by 64-bit integer.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: UHi, ULo, D, V
        tUInt64, INTENT(OUT)    :: Q, R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: QHi, QLo, NewLo

    !** FLOW

        ! Q128 = V*UHi
        CALL UMul128(V, UHi, QHi, QLo)

        ! Q128 = Q128 + U128
        NewLo = QLo + ULo
        IF (IEOR(NewLo, MinI64) < IEOR(QLo, MinI64)) THEN
            QHi = QHi + UHi + 1_kInt64
        ELSE
            QHi = QHi + UHi
        END IF
        QLo = NewLo

        QHi = QHi + 1_kInt64

        R = ULo - QHi*D

        IF (IEOR(R, MinI64) > IEOR(QLo, MinI64)) THEN
            QHi = QHi - 1_kInt64
            R = R + D
        END IF

        IF (IEOR(R, MinI64) >= IEOR(D, MinI64)) THEN
            QHi = QHi + 1_kInt64
            R = R - D
        END IF
        Q = QHi

        RETURN

    END SUBROUTINE UDivRem_2By1

    !**************************************************************************

    SUBROUTINE UDivRem_3By2(U2, U1, U0, DHi, DLo, V, Q, RHi, RLo)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform 192-bit integer division by 128-bit integer.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: U2, U1, U0, DHi, DLo, V
        tUInt64, INTENT(OUT)    :: Q, RHi, RLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: QHi, QLo, NewLo, R1
        tUInt64     :: THi, TLo, SHi, SLo
        tLogical    :: Flag

    !** FLOW

        ! Q128 = V*U2
        CALL UMul128(V, U2, QHi, QLo)

        ! Q128 = Q128 + UInt128(U2, U1)
        NewLo = QLo + U1
        IF (IEOR(NewLo, MinI64) < IEOR(QLo, MinI64)) THEN
            QHi = QHi + U2 + 1_kInt64
        ELSE
            QHi = QHi + U2
        END IF
        QLo = NewLo

        R1 = U1 - QHi * DHi

        ! T128 = DLo*QHi
        CALL UMul128(DLo, QHi, THi, TLo)

        ! R128 = UInt128(R1, U0) - T128 - D128
        SLo  = U0 - TLo
        IF (IEOR(U0, MinI64) < IEOR(TLo, MinI64)) THEN
            SHi = R1 - THi - 1_kInt64
        ELSE
            SHi = R1 - THi
        END IF
        RLo  = SLo - DLo
        IF (IEOR(SLo, MinI64) < IEOR(DLo, MinI64)) THEN
            RHi = SHi - DHi - 1_kInt64
        ELSE
            RHi = SHi - DHi
        END IF

        R1 = RHi

        QHi = QHi + 1_kInt64

        IF (R1 .UGE. QLo) THEN
            QHi = QHi - 1_kInt64
            ! R128 = R128 + D128
            NewLo = RLo + DLo
            IF (IEOR(NewLo, MinI64) < IEOR(RLo, MinI64)) THEN
                RHi = RHi + DHi + 1_kInt64
            ELSE
                RHi = RHi + DHi
            END IF
            RLo = NewLo
        END IF

        IF (RHi == DHi) THEN
            Flag = (IEOR(RLo, MinI64) >= IEOR(DLo, MinI64))
        ELSE
            Flag = (IEOR(RHi, MinI64) >= IEOR(DHi, MinI64))
        END IF
        IF (Flag) THEN
            QHi = QHi + 1_kInt64
            ! R128 = R128 - D128
            NewLo = RLo - DLo
            IF (IEOR(RLo, MinI64) < IEOR(DLo, MinI64)) THEN
                RHi = RHi - DHi - 1_kInt64
            ELSE
                RHi = RHi - DHi
            END IF
            RLo = NewLo
        END IF

        Q = QHi

        RETURN

    END SUBROUTINE UDivRem_3By2

    !**************************************************************************

END SUBROUTINE U128_DivMod_IntX

!******************************************************************************

END SUBMODULE SubBase_U128_Arithmetic

!******************************************************************************
