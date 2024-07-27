
SUBMODULE (ModBase_SInt128) SubBase_I128_Experimental

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for experimental routines of the
!   <a href="../module/modbase_sint128.html">SInt128</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tUInt128        TYPE(UInt128)
#define     tSInt128        TYPE(SInt128)

!** MODULE PARAMETERS:
    tSInt64,  PARAMETER :: MinInt64 = ToInt64(Z'8000000000000000')    ! -9223372036854775808

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    ADDITION OPERATIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION I128_Addition_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = LhsVal + RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: LhsVal
    tSInt128, INTENT(IN)   :: RhsVal
    tSInt32,  INTENT(IN)   :: Algo
    tSInt128               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        CALL Add_A1(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (2)
        CALL Add_A2(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (3)
        CALL Add_A3(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (4)
        CALL Add_A4(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    END SELECT

    RETURN

    CONTAINS

    PURE SUBROUTINE AddU64(X, Y, CarryIn, Sum, CarryOut)

    !DIR$ ATTRIBUTES INLINE :: AddU64

    !** PURPOSE OF THIS SUBROUTINE:
        ! To return the sum with carry of X, Y and CarryIn: Sum = X + Y + CarryIn.
        ! The carry input must be 0 or 1; otherwise the behavior is undefined.
        ! The carry output is guaranteed to be 0 or 1.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: X, Y, CarryIn
        tUInt64, INTENT(OUT)  :: Sum, CarryOut
        OPTIONAL              :: CarryOut

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Sum = X + Y + CarryIn
        ! The sum will overflow if both top bits are set (x & y) or if one of them
        ! is (x | y), and a carry from the lower place happened. If such a carry
        ! happens, the top bit will be 1 + 0 + 1 = 0 (&^ sum).
        IF (PRESENT(CarryOut)) THEN
            CarryOut = SHIFTR(IOR(IAND(X, Y), IAND(IOR(X, Y), NOT(Sum))), 63)
        END IF

        RETURN

    END SUBROUTINE AddU64

    !**************************************************************************

    PURE SUBROUTINE Add_A1(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Add_A1

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition:  OutVal = LhsVal + RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo + RhsLo
        IF (IEOR(OutLo, MinInt64) < IEOR(LhsLo, MinInt64)) THEN     ! unsigned comparison
            OutHi = LhsHi + RhsHi + 1_kInt64
        ELSE
            OutHi = LhsHi + RhsHi
        END IF

        RETURN

    END SUBROUTINE Add_A1

    !******************************************************************************

    PURE SUBROUTINE Add_A2(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Add_A2

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition:  OutVal = LhsVal + RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: ResHi, ResLo

    !** FLOW

        ResHi = LhsHi + RhsHi
        ResLo = LhsLo + RhsLo
        CALL AddResult(ResHi, ResLo, LhsLo, OutHi, OutLo)

        RETURN

    END SUBROUTINE Add_A2

    !******************************************************************************

    PURE SUBROUTINE AddResult(ResHi, ResLo, LhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: AddResult

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check for carry and add one to the result if needed

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, ResLo
        tSInt64, INTENT(IN)   :: ResHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (ResLo .ULT. LhsLo) THEN
            OutHi = ResHi + 1_kInt64
            OutLo = ResLo
        ELSE
            OutHi = ResHi
            OutLo = ResLo
        END IF

        RETURN

    END SUBROUTINE AddResult

    !**************************************************************************

    PURE SUBROUTINE Add_A3(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Add_A3

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition:  OutVal = LhsVal + RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: Carry

    !** FLOW

        CALL AddU64(LhsLo, RhsLo, 0_kInt64, OutLo, Carry)
        CALL AddU64(LhsHi, RhsHi, Carry, OutHi)

        RETURN

    END SUBROUTINE Add_A3

    !**************************************************************************

    PURE SUBROUTINE Add_A4(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Add_A4

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition:  OutVal = LhsVal + RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo + RhsLo
        OutHi = LhsHi + RhsHi
        IF (OutLo .ULT. LhsLo) OutHi = OutHi + 1_kInt64

        RETURN

    END SUBROUTINE Add_A4

    !**************************************************************************

END FUNCTION I128_Addition_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  SUBTRACTION OPERATIONS                  ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION I128_Subtraction_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = LhsVal - RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: LhsVal
    tSInt128, INTENT(IN)   :: RhsVal
    tSInt32,  INTENT(IN)   :: Algo
    tSInt128               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        CALL Sub_A1(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (2)
        CALL Sub_A2(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (3)
        CALL Sub_A3(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (4)
        CALL Sub_A4(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    END SELECT

    RETURN

    CONTAINS

    PURE SUBROUTINE SubU64(X, Y, BorrowIn, Diff, BorrowOut)

    !DIR$ ATTRIBUTES INLINE :: SubU64

    !** PURPOSE OF THIS SUBROUTINE:
        ! To return the difference of X, Y and BorrowIn: Diff = X - Y - BorrowIn.
        ! The borrow input must be 0 or 1; otherwise the behavior is undefined.
        ! The borrow output is guaranteed to be 0 or 1.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: X, Y, BorrowIn
        tUInt64, INTENT(OUT)  :: Diff, BorrowOut
        OPTIONAL              :: BorrowOut

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Diff = X - Y - BorrowIn
        ! The difference will underflow if the top bit of x is not set and the top
        ! bit of y is set (^x & y) or if they are the same (^(x ^ y)) and a Borrow
        ! from the lower place happens. If that Borrow happens, the result will be
        ! 1 - 1 - 1 = 0 - 0 - 1 = 1 (& diff).
        IF (PRESENT(BorrowOut)) THEN
            BorrowOut = SHIFTR(IOR(IAND(NOT(X), Y), IAND(NOT(IEOR(X, Y)), Diff)), 63)
        END IF

        RETURN

    END SUBROUTINE SubU64

    !**************************************************************************

    PURE SUBROUTINE Sub_A1(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Sub_A1

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction:  OutVal = LhsVal - RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo - RhsLo
        IF (IEOR(LhsLo, MinInt64) < IEOR(RhsLo, MinInt64)) THEN     ! unsigned comparison
            OutHi = LhsHi - RhsHi - 1_kInt64
        ELSE
            OutHi = LhsHi - RhsHi
        END IF

        RETURN

    END SUBROUTINE Sub_A1

    !******************************************************************************

    PURE SUBROUTINE Sub_A2(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Sub_A2

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction:  OutVal = LhsVal - RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: ResHi, ResLo

    !** FLOW

        ResHi = LhsHi - RhsHi
        ResLo = LhsLo - RhsLo
        CALL SubResult(ResHi, ResLo, LhsLo, RhsLo, OutHi, OutLo)

        RETURN

    END SUBROUTINE Sub_A2

    !******************************************************************************

    PURE SUBROUTINE SubResult(ResHi, ResLo, LhsLo, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: SubResult

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check for carry and add one to the result if needed

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: ResLo, LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: ResHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LhsLo .ULT. RhsLo) THEN
            OutHi = ResHi - 1_kInt64
            OutLo = ResLo
        ELSE
            OutHi = ResHi
            OutLo = ResLo
        END IF

        RETURN

    END SUBROUTINE SubResult

    !**************************************************************************

    PURE SUBROUTINE Sub_A3(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Sub_A3

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction:  OutVal = LhsVal - RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: Borrow

    !** FLOW

        CALL SubU64(LhsLo, RhsLo, 0_kInt64, OutLo, Borrow)
        CALL SubU64(LhsHi, RhsHi, Borrow, OutHi)

        RETURN

    END SUBROUTINE Sub_A3

    !**************************************************************************

    PURE SUBROUTINE Sub_A4(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Sub_A4

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction:  OutVal = LhsVal - RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo - RhsLo
        OutHi = LhsHi - RhsHi
        IF (OutLo .UGT. LhsLo) OutHi = OutHi - 1_kInt64

        RETURN

    END SUBROUTINE Sub_A4

    !**************************************************************************

END FUNCTION I128_Subtraction_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 MULTIPLICATION OPERATIONS                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION I128_Multiplication_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = LhsVal * RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: LhsVal
    tSInt128, INTENT(IN)   :: RhsVal
    tSInt32,  INTENT(IN)   :: Algo
    tSInt128               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        CALL Mul_A1(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (2)
        CALL Mul_A2(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (3)
        CALL Mul_A3(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (4)
        CALL Mul_A4(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    END SELECT

    RETURN

    CONTAINS

    PURE SUBROUTINE Mul_A1(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Mul_A1

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication:  OutVal = LhsVal * RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: X_Lo, X_Hi, Y_Lo, Y_Hi
        tUInt64       :: Hi_Lo, Cross, ProductHi

    !** FLOW

        ! perform "Unsigned Multiply High"
        X_Lo = IAND(LhsLo, Mask32)
        X_Hi = SHIFTR(LhsLo, 32)
        Y_Lo = IAND(RhsLo, Mask32)
        Y_Hi = SHIFTR(RhsLo, 32)
        Hi_Lo = X_Hi*Y_Lo
        Cross = SHIFTR(X_Lo*Y_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
        ProductHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi

        OutHi = LhsLo * RhsHi + LhsHi * RhsLo + ProductHi
        OutLo = LhsLo * RhsLo

        RETURN

    END SUBROUTINE Mul_A1

    !******************************************************************************

    PURE SUBROUTINE Mul_A2(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Mul_A2

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication:  OutVal = LhsVal * RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutHi = LhsLo*RhsHi +  LhsHi*RhsLo + UMul128_Upper64(LhsLo, RhsLo)
        OutLo = LhsLo*RhsLo

        RETURN

    END SUBROUTINE Mul_A2

    !******************************************************************************

    PURE SUBROUTINE Mul_A3(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Mul_A3

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication:  OutVal = LhsVal * RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: X_Lo, X_Hi, Y_Lo, Y_Hi
        tUInt64       :: Hi_Lo, Cross, ProductHi

    !** FLOW

        ! perform "Unsigned Multiply High"
        X_Lo = IAND(LhsLo, Mask32)
        X_Hi = SHIFTR(LhsLo, 32)
        Y_Lo = IAND(RhsLo, Mask32)
        Y_Hi = SHIFTR(RhsLo, 32)

        IF ((X_Hi == 0_kInt64).OR.(Y_Hi == 0_kInt64)) THEN
            IF ((X_Hi == 0_kInt64).AND.(Y_Hi == 0_kInt64)) THEN
                ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32), 32)
            ELSEIF (X_Hi == 0_kInt64) THEN
                ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)
            ELSE
                ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Hi*Y_Lo, 32)
            END IF
        ELSE
            Hi_Lo = X_Hi*Y_Lo
            Cross = SHIFTR(X_Lo*Y_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
            ProductHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
        END IF

        OutHi = LhsLo * RhsHi + LhsHi * RhsLo + ProductHi
        OutLo = LhsLo * RhsLo

        RETURN

    END SUBROUTINE Mul_A3

    !******************************************************************************

    PURE SUBROUTINE Mul_A4(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: Mul_A4

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication:  OutVal = LhsVal * RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tSInt64, INTENT(IN)   :: LhsHi, RhsHi
        tUInt64, INTENT(OUT)  :: OutLo
        tSInt64, INTENT(OUT)  :: OutHi

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64           :: A32, A00, B32, B00
        tUInt64           :: A32B00Hi, A32B00Lo
        tUInt64           :: A00B32Hi, A00B32Lo

    !** FLOW

        A32 = SHIFTR(LhsLo, 32)
        A00 = IAND(LhsLo, Mask32)
        B32 = SHIFTR(RhsLo, 32)
        B00 = IAND(RhsLo, Mask32)

        OutHi = LhsHi*RhsLo + LhsLo*RhsHi + A32*B32
        OutLo = A00*B00

        A32B00Lo = A32*B00
        A32B00Hi = SHIFTR(A32B00Lo, 32)
        A32B00Lo = SHIFTL(A32B00Lo, 32)
        A00B32Lo = A00*B32
        A00B32Hi = SHIFTR(A00B32Lo, 32)
        A00B32Lo = SHIFTL(A00B32Lo, 32)

        OutHi = OutHi + A32B00Hi
        OutLo = OutLo + A32B00Lo

        IF (IEOR(OutLo, MinInt64) < IEOR(A32B00Lo, MinInt64)) THEN
            OutHi = OutHi + 1_kInt64
        END IF

        OutHi = OutHi + A00B32Hi
        OutLo = OutLo + A00B32Lo

        IF (IEOR(OutLo, MinInt64) < IEOR(A00B32Lo, MinInt64)) THEN
            OutHi = OutHi + 1_kInt64
        END IF

        RETURN

    END SUBROUTINE Mul_A4

    !******************************************************************************

END FUNCTION I128_Multiplication_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++             DIVISION/MODULATION OPERATIONS               ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE I128_DivMod(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two SInt128 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: Dividend
    tSInt128, INTENT(IN)   :: Divisor
    tSInt128, INTENT(OUT)  :: Quotient
    tSInt128, INTENT(OUT)  :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical   :: NegNumer, NegDenom
    tSInt128   :: PosNumer, PosDenom

!** FLOW

    ! check if input are applicable
    IF ((Dividend == MinI128).AND.(Divisor == -OneI128)) THEN
        ! not applicable for unsigned binary on two's complement
        CALL Handle_ErrLevel('I128_DivMod', ModName, ErrSevere, &
                          'Dividend = MinI128 and Divisor = -1')
        RETURN
    END IF

    ! check whether dividend and divisor are negative
    NegNumer = IsNegative(Dividend)
    NegDenom = IsNegative(Divisor)
    IF (NegNumer) THEN
        ! PosNumer = -Dividend
        PosNumer%High = NOT(Dividend%High)
        IF (Dividend%Low == 0_kInt64) PosNumer%High = PosNumer%High + 1_kInt64
        PosNumer%Low = NOT(Dividend%Low) + 1_kInt64
    ELSE
        PosNumer = Dividend
    END IF
    IF (NegDenom) THEN
        ! PosDenom = -Divisor
        PosDenom%High = NOT(Divisor%High)
        IF (Divisor%Low == 0_kInt64) PosDenom%High = PosDenom%High + 1_kInt64
        PosDenom%Low = NOT(Divisor%Low) + 1_kInt64
    ELSE
        PosDenom = Divisor
    END IF

    ! perform division for positive numbers
    CALL PositiveDivision(PosNumer, PosDenom, Quotient, Remainder)

    ! negate results if needed
    IF (NegNumer .NEQV. NegDenom) THEN
        ! Quotient = -Quotient
        Quotient%High = NOT(Quotient%High)
        IF (Quotient%Low == 0_kInt64) Quotient%High = Quotient%High + 1_kInt64
        Quotient%Low = NOT(Quotient%Low) + 1_kInt64
    END IF
    IF (NegNumer) THEN
        ! Remainder = -Remainder
        Remainder%High = NOT(Remainder%High)
        IF (Remainder%Low == 0_kInt64) Remainder%High = Remainder%High + 1_kInt64
        Remainder%Low = NOT(Remainder%Low) + 1_kInt64
    END IF

    RETURN

CONTAINS

    SUBROUTINE PositiveDivision(Dividend, Divisor, Quotient, Remainder)

        ! To perform division of two SInt128 objects where both are positive numbers

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: Dividend
        tSInt128, INTENT(IN)   :: Divisor
        tSInt128, INTENT(OUT)  :: Quotient
        tSInt128, INTENT(OUT)  :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32    :: DividendLeadingZeros
        tSInt32    :: DivisorLeadingZeros
        tSInt32    :: DivisorTrailingZeros
        tSInt32    :: CompFlag

    !** FLOW

        CompFlag = CompareUnsigned_I128(Dividend%High, Dividend%Low, Divisor%High, Divisor%Low)
        IF (CompFlag < 0) THEN
            Quotient  = ZeroI128
            Remainder = Dividend
            RETURN
        ELSEIF (CompFlag == 0) THEN
            Quotient  = OneI128
            Remainder = ZeroI128
            RETURN
        END IF

        DividendLeadingZeros = LEADZ(Dividend)
        DivisorLeadingZeros  = LEADZ(Divisor)
        DivisorTrailingZeros = TRAILZ(Divisor)

        IF (DivisorLeadingZeros == 128) THEN
            CALL Handle_ErrLevel('PositiveDivision', ModName, ErrSevere, 'Divide by zero.')
            RETURN
        ELSEIF (IOR(Dividend%High, Divisor%High) == 0_kInt64) THEN
            ! dividend and divisor fit in an unsigned
            Quotient  = SInt128(0_kInt64, Dividend%Low.UDIV.Divisor%Low)
            Remainder = SInt128(0_kInt64, UMOD(Dividend%Low, Divisor%Low))
            RETURN
        ELSEIF (DivisorLeadingZeros == 127) THEN
            ! divisor is 1
            Quotient  = Dividend
            Remainder = ZeroI128
            RETURN
        ELSEIF ((DivisorTrailingZeros + DivisorLeadingZeros) == 127) THEN
            ! only one bit set (i.e., power of 2), so just shift
            !  quotient = dividend >>> divisorTrailingZeros
            Quotient = SHIFTR(Dividend, DivisorTrailingZeros)
            !  remainder = dividend & (divisor - 1)
            Remainder = IAND(Dividend, Divisor - OneI128)
            RETURN
        END IF

        IF (DivisorLeadingZeros - DividendLeadingZeros > 15) THEN
            ! fastDivide when the values differ by this many orders of magnitude
            CALL FastDivision(Dividend, Divisor, Quotient, Remainder)
        ELSE
            CALL BinaryDivision(Dividend, Divisor, Quotient, Remainder)
        END IF

        RETURN

    END SUBROUTINE PositiveDivision

    !******************************************************************************

    SUBROUTINE BinaryDivision(Numerator, Denominator, Quotient, Remainder)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: Numerator
        tSInt128, INTENT(IN)   :: Denominator
        tSInt128, INTENT(OUT)  :: Quotient
        tSInt128, INTENT(OUT)  :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32        :: Shift, NumerLZ, DenomLZ
        tUInt64        :: NumerHi, NumerLo, DenomHi, DenomLo
        tUInt64        :: QuotHi, QuotLo

    !** FLOW

        ! initialize
        NumerHi = Numerator%High
        NumerLo = Numerator%Low
        IF (Denominator%High == 0_kInt64) THEN
            DenomLZ = LEADZ(Denominator%Low) + 64
        ELSE
            DenomLZ = LEADZ(Denominator%High)
        END IF
        IF (Numerator%High == 0_kInt64) THEN
            NumerLZ = LEADZ(Numerator%Low) + 64
        ELSE
            NumerLZ = LEADZ(Numerator%High)
        END IF
        Shift = DenomLZ - NumerLZ
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
            ! Quotient = SHIFTL(Quotient, 1)
            QuotHi = IOR(SHIFTL(QuotHi, 1), SHIFTR(QuotLo, 63))
            QuotLo = SHIFTL(QuotLo, 1)

            ! if (dividend >= divisor)
            IF (CompareUnsigned_I128(NumerHi, NumerLo, DenomHi, DenomLo) >= 0) THEN
                ! Dividend = Dividend - Divisor
                IF (IEOR(NumerLo, MinInt64) < IEOR(DenomLo, MinInt64)) THEN
                    NumerHi = NumerHi - DenomHi - 1_kInt64
                    NumerLo = NumerLo - DenomLo
                ELSE
                    NumerHi = NumerHi - DenomHi
                    NumerLo = NumerLo - DenomLo
                END IF

                ! quotient = quotient | 1
                QuotLo = IOR(QuotLo, 1_kInt64)
            END IF

            ! Divisor = SHIFTR(Divisor, 1)
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

    !******************************************************************************

    SUBROUTINE FastDivision(Dividend, Divisor, Quotient, Remainder)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: Dividend
        tSInt128, INTENT(IN)   :: Divisor
        tSInt128, INTENT(OUT)  :: Quotient
        tSInt128, INTENT(OUT)  :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32    :: NLZ

    !** FLOW

        IF (Divisor%High == 0_kInt64) THEN
            IF (CompareUnsigned(Dividend%High, Divisor%Low) < 0) THEN
                CALL Divide_128By64(Dividend%High, Dividend%Low, Divisor%Low, &
                                    Quotient%Low, Remainder%Low)
                Quotient%High  = 0_kInt64
                Remainder%High = 0_kInt64
            ELSE
                BLOCK
                    tUInt64       :: NumHi, NumLo
                    NumHi = UMOD(Dividend%High, Divisor%Low)
                    NumLo = Dividend%Low
                    CALL Divide_128By64(NumHi, NumLo, Divisor%Low, Quotient%Low, Remainder%Low)
                END BLOCK
                Quotient%High  = (Dividend%High.UDIV.Divisor%Low)
                Remainder%High = 0_kInt64
            END IF
        ELSE
            BLOCK
                tUInt64       :: V1Hi, U1Hi, U1Lo
                ! execution
                NLZ = LEADZ(Divisor%High)
                ! v1 = divisor << n
                IF (NLZ < 64) THEN
                    V1Hi = IOR(SHIFTL(Divisor%High, NLZ), SHIFTR(Divisor%Low, 64 - NLZ))
                ELSE
                    V1Hi = SHIFTL(Divisor%Low, NLZ - 64)
                END IF
                ! u1 = dividend >>> 1
                U1Lo = IOR(SHIFTR(Dividend%Low, 1), SHIFTL(SHIFTL(Dividend%High, 1), 62))
                U1Hi = SHIFTR(Dividend%High, 1)
                CALL Divide_128By64(U1Hi, U1Lo, V1Hi, Quotient%Low, Remainder%Low)
                Quotient%High = 0_kInt64
                ! q1 = q1 >>> (63 - n)
                Quotient = SHIFTR(Quotient, 63-NLZ)
                ! if (q1 /= 0)
                IF (Quotient /= ZeroI128) THEN
                    ! q1--
                    CALL Decrement(Quotient)
                END IF
                ! r = dividend - q1 * divisor
                Remainder = Dividend - (Quotient * Divisor)
                IF (Compare_I128(Remainder%High, Remainder%Low, &
                                 Divisor%High, Divisor%Low) >= 0) THEN
                    ! quotient++
                    CALL Increment(Quotient)
                    Remainder = Remainder - Divisor
                END IF
            END BLOCK
        END IF

        RETURN

    END SUBROUTINE FastDivision

    !******************************************************************************

    SUBROUTINE Divide_128By64(DvdHi, DvdLo, Denom, QLow, RLow)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: DvdHi, DvdLo
        tUInt64, INTENT(IN)   :: Denom
        tUInt64, INTENT(OUT)  :: QLow
        tUInt64, INTENT(OUT)  :: RLow

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER    :: Mask = ToInt64(Z'00000000FFFFFFFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32       :: Shift
        tUInt64       :: High, Low, Divisor
        tUInt64       :: LowHigh, LowLow
        tUInt64       :: RHat, UHat
        tUInt64       :: DivisorHigh, DivisorLow
        tUInt64       :: QuotientHigh, QuotientLow

    !** FLOW

        ! initialize
        Divisor = Denom
        High  = DvdHi
        Low   = DvdLo
        Shift = LEADZ(Divisor)

        IF (Shift /= 0) THEN
            Divisor = SHIFTL(Divisor, Shift)
            High = SHIFTL(High, Shift)
            High = IOR(High, SHIFTR(Low, 64 - Shift))
            Low = SHIFTL(Low, Shift)
        END IF

        DivisorHigh = SHIFTR(Divisor, 32)
        DivisorLow = IAND(Divisor, Mask)
        LowHigh = SHIFTR(Low, 32)
        LowLow = IAND(Low, Mask)

        ! Compute High quotient digit.
        QuotientHigh = (High.UDIV.DivisorHigh)
        RHat = UMOD(High, DivisorHigh)

        ! qhat >>> 32 == qhat > base
        DO WHILE ((SHIFTR(QuotientHigh, 32) /= 0_kInt64).OR.   &
                  (CompareUnsigned(QuotientHigh * DivisorLow, &
                                   IOR(SHIFTL(RHat, 32), LowHigh)) > 0))
            QuotientHigh = QuotientHigh - 1_kInt64
            RHat = RHat + DivisorHigh
            IF (SHIFTR(RHat, 32) /= 0_kInt64) EXIT
        END DO

        UHat = IOR(SHIFTL(High, 32), LowHigh) - QuotientHigh * Divisor

        ! Compute Low quotient digit.
        QuotientLow = (UHat.UDIV.DivisorHigh)
        RHat = UMOD(UHat, DivisorHigh)

        DO WHILE ((SHIFTR(QuotientLow, 32) /= 0_kInt64).OR.   &
                  (CompareUnsigned(QuotientLow * DivisorLow, &
                                   IOR(SHIFTL(RHat, 32), LowLow)) > 0))
            QuotientLow = QuotientLow - 1
            RHat = RHat + DivisorHigh
            IF (SHIFTR(RHat, 32) /= 0_kInt64) EXIT
        END DO

        QLow = IOR(SHIFTL(QuotientHigh, 32), QuotientLow)
        RLow = SHIFTR(IOR(SHIFTL(UHat, 32), LowLow) - QuotientLow * Divisor, Shift)

        RETURN

    END SUBROUTINE Divide_128By64

    !******************************************************************************

    FUNCTION CompareUnsigned_I128(LhsHi, LhsLo, RhsHi, RhsLo) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compare LHS and RHS where both numbers are treated as unsigned.
        ! - return -1 if LHS < RHS
        ! - return  0 if LHS == RHS
        ! - return +1 if LHS > RHS

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo, RhsHi, RhsLo
        tSInt32               :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: ULHS, URHS

    !** FLOW

        ! Flag = CompareUnsigned(LhsHi, RhsHi)
        ULHS = IEOR(LhsHi, MinInt64)
        URHS = IEOR(RhsHi, MinInt64)
        IF (ULHS < URHS) THEN
            Flag = -1
        ELSEIF (ULHS > URHS) THEN
            Flag = +1
        ELSE
            Flag = 0
        END IF

        IF (Flag == 0) THEN
            ! Flag = CompareUnsigned(LhsLo, RhsLo)
            ULHS = IEOR(LhsLo, MinInt64)
            URHS = IEOR(RhsLo, MinInt64)
            IF (ULHS < URHS) THEN
                Flag = -1
            ELSEIF (ULHS > URHS) THEN
                Flag = +1
            ELSE
                Flag = 0
            END IF
        END IF

        RETURN

    END FUNCTION CompareUnsigned_I128

    !******************************************************************************

    FUNCTION Compare_I128(LhsHi, LhsLo, RhsHi, RhsLo) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To compare LHS and RHS where low components are treated as unsigned
        ! but high components are treated as signed.
        ! - return -1 if LHS < RHS
        ! - return  0 if LHS == RHS
        ! - return +1 if LHS > RHS

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo, RhsHi, RhsLo
        tSInt32               :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: ULHS, URHS

    !** FLOW

        ! Flag = Compare(LhsHi, RhsHi)
        IF (LhsHi < RhsHi) THEN
            Flag = -1
        ELSEIF (LhsHi > RhsHi) THEN
            Flag = +1
        ELSE
            Flag = 0
        END IF

        IF (Flag == 0) THEN
            ! Flag = CompareUnsigned(LhsLo, RhsLo)
            ULHS = IEOR(LhsLo, MinInt64)
            URHS = IEOR(RhsLo, MinInt64)
            IF (ULHS < URHS) THEN
                Flag = -1
            ELSEIF (ULHS > URHS) THEN
                Flag = +1
            ELSE
                Flag = 0
            END IF
        END IF

        RETURN

    END FUNCTION Compare_I128

    !******************************************************************************

END SUBROUTINE I128_DivMod

!******************************************************************************

MODULE SUBROUTINE I128_DivMod_Xp(Dividend, Divisor, Algo, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two SInt128 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: Dividend
    tSInt128, INTENT(IN)   :: Divisor
    tSInt32,  INTENT(IN)   :: Algo
    tSInt128, INTENT(OUT)  :: Quotient
    tSInt128, INTENT(OUT)  :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128   :: URemainder, UQuotient

!** FLOW

    SELECT CASE (Algo)
    CASE (1:6)
        IF ((Dividend == MinI128).AND.(Divisor == -OneI128)) THEN
            ! not applicable for unsigned binary on two's complement
            CALL Handle_ErrLevel('I128_DivMod_Xp', ModName, ErrSevere, &
                              'Dividend = MinI128 and Divisor = -1')
            RETURN
        END IF

        CALL UDivModXp(UABS(Dividend), UABS(Divisor), Algo, UQuotient, URemainder)

        IF ((Dividend%High < 0_kInt64) .NEQV. (Divisor%High < 0_kInt64)) UQuotient = -UQuotient
        IF (Dividend%High < 0_kInt64) URemainder = -URemainder

        Quotient%High = BitCastToSigned(UQuotient%High)
        Quotient%Low  = UQuotient%Low
        Remainder%High = BitCastToSigned(URemainder%High)
        Remainder%Low  = URemainder%Low
    CASE DEFAULT
        CALL I128_DivMod(Dividend, Divisor, Quotient, Remainder)
    END SELECT

    RETURN

END SUBROUTINE I128_DivMod_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                     STRING CONVERSIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION I128_ToDecString_Huldra(I128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a decimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: I128
    tCharAlloc             :: Str

!** SUBROUTINE PARAMETER DECLARATIONS:
    tChar, PARAMETER :: NumStr(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32         :: BufLen, Top, I, J
    tCharLen(41)    :: Buffer
    tSInt128        :: Copy
    tSInt64         :: Tmp
    tSInt64         :: Indx
    tLogical        :: Negative

!** FLOW

    IF (I128 == ZeroI128) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = 41
    FORALL (I=1:BufLen) Buffer(I:I) = '0'
    Top  = BufLen
    Negative = IsNegative(I128)
    IF (Negative) THEN
        Copy = -I128
    ELSE
        Copy = I128
    END IF
    DO
        J = Top
        Tmp = ToStringDivide(Copy)
        DO WHILE (Tmp > 0_kInt64)
            Indx = MOD(Tmp, 10_kInt64)
            Buffer(Top:Top) = NumStr(Indx)
            Top = Top - 1
            Tmp = Tmp / 10_kInt64
        END DO
        IF ((Copy%High == 0).AND.(Copy%Low == 0)) THEN
            EXIT
        ELSE
            Top = J - 13
        END IF
    END DO
    IF (Negative) THEN
        Buffer(Top:Top) = '-'
        Str = Buffer(Top:BufLen)
    ELSE
        Str = Buffer(Top+1:BufLen)
    END IF

    RETURN

    CONTAINS

    FUNCTION ToStringDivide(I128) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the number by 10**13 and return the remainder

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(INOUT)    :: I128
        tSInt64                    :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,  PARAMETER :: Pow2  = SHIFTL(1, 13)
        tSInt64,  PARAMETER :: Pow5  = 1220703125_kInt64
        tSInt64,  PARAMETER :: Pow10 = Pow2*Pow5
        tLogical, PARAMETER :: Positive = FalseVal
        tLogical, PARAMETER :: AsUnsigned = TrueVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt128   :: Rem, Quot, Numer, Pow10_128
        tSInt64    :: Q, R, Mod2

    !** FLOW

        Q = I128%High / Pow5
        R = I128%High - Q*Pow5
        I128%High = SHIFTR(Q, 13)

        Numer%High = R
        Numer%Low  = I128%Low
        Mod2 = IAND(I128%Low, Pow2 - 1_kInt64)

        CALL I128_DivMod(Numer, SInt128(0_kInt64, Pow5), Quot, Rem)
        I128%Low = IOR(SHIFTL(Q, 51), SHIFTR(Quot%Low, 13))

        ! Applies the Chinese Rem Theorem.
        ! -67*5^13 + 9983778*2^13 = 1
        Pow10_128 = SInt128(0_kInt64, Pow10)
        Rem = SMOD((Rem - SMOD(Pow5*(Mod2 - Rem), Pow10_128)*67), Pow10_128)
        IF (Rem%High < 0_kInt64) Rem = Rem + Pow10
        Remainder = Rem%Low

        RETURN

    END FUNCTION ToStringDivide

    !**************************************************************************

    FUNCTION SMOD(Dividend, Divisor) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division of two UInt128 objects (Dividend / Divisor)
        ! and return the remainder
        ! note: although the input and output objects are of UInt128 type,
        !       they are all treated as if they are signed 128-bit integers

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: Dividend
        tSInt128, INTENT(IN)   :: Divisor
        tSInt128               :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt128   :: Quotient

    !** FLOW

        CALL I128_DivMod(Dividend, Divisor, Quotient, Remainder)

        RETURN

    END FUNCTION SMOD

    !******************************************************************************

END FUNCTION I128_ToDecString_Huldra

!******************************************************************************

MODULE FUNCTION I128_ToDecString_Xp(I128, Algo) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a decimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: I128
    tSInt32,  INTENT(IN)   :: Algo
    tCharAlloc             :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128   :: U128

!** FLOW

    SELECT CASE (Algo)
    CASE (1:13)
        IF (I128 == MinI128) THEN
            Str  = '-170141183460469231731687303715884105728'
        ELSEIF (IsNegative(I128)) THEN
            U128 = ToU128(-I128)
            Str  = '-' // ToDecStrXp(U128, Algo)
        ELSE
            U128 = ToU128(I128)
            Str  = ToDecStrXp(U128, Algo)
        END IF
    CASE DEFAULT
        Str = I128_ToDecString_Huldra(I128)
    END SELECT

    RETURN
    CONTAINS

END FUNCTION I128_ToDecString_Xp

!******************************************************************************

MODULE FUNCTION I128_FromDecString_Xp(cStr, Algo, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an unsigned 128-bit integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tUInt32,              INTENT(IN)    :: Algo
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt128                            :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        Number = Parse_I128_V1(cStr, ErrFlag, ErrMsg)
    CASE (2)
        Number = Parse_I128_V2(cStr, ErrFlag, ErrMsg)
    CASE (3)
        Number = Parse_I128_V3(cStr, ErrFlag, ErrMsg)
    CASE (4)
        Number = Parse_I128_V4(cStr, ErrFlag, ErrMsg)
    CASE (5)
        Number = Parse_I128_V5(cStr, ErrFlag, ErrMsg)
    END SELECT

    RETURN

    CONTAINS

    FUNCTION Parse_I128_V1(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to a signed 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tSInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,  PARAMETER    :: A0           = IACHAR('0')
        tSInt32,  PARAMETER    :: A4           = IACHAR('4')
        tSInt32,  PARAMETER    :: A9           = IACHAR('9')
        tSInt32,  PARAMETER    :: MaxDigitI32  = 10
        tSInt32,  PARAMETER    :: MaxDigitI64  = 19
        tSInt32,  PARAMETER    :: MaxDigitI128 = 39
        tSInt128, PARAMETER    :: SInt128ase     = TenI128
        tCharParam             :: MaxStr       = '170141183460469231731687303715884105727'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart
        tCharLen(1), POINTER    :: CurChr
        tLogical                :: NegSign
        tLogical                :: Overflow
        tCharAlloc, TARGET      :: CurStr
        tLogical                :: ErrorFlag  ! true if input is not invalid
        tCharAlloc              :: ErrorMsg   ! message if input is not invalid
        tSInt32                 :: I32Val
        tSInt64                 :: I64Val

    !** FLOW

        ! get valid string length by removing the trailing space(s)
        StrLen = LEN_TRIM(cStr)
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal

        ! check whether there are spaces in front of the number
        ! (only allow space(s) in front of the number but no spaces inside it)
        Indx = 1
        IF (cStr(Indx:Indx) == ' ') THEN
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= ' ') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for sign
        NegSign = FalseVal
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') NegSign = TrueVal
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = 0_kInt64
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                ! only zero digits encountered
                Number = 0_kInt64
                RETURN
            END IF
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I32Val
                ELSE
                    Number = I32Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I64Val
                ELSE
                    Number = I64Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        END IF

        ! compute value of the input string
        IStart   = 0
        NumDigit = 0
        CurChr => cStr(Indx:Indx)
        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
            IStart = Indx
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                Number = Number*SInt128ase + SInt128(0_kInt64, ToInt64(IACHAR(CurChr)-A0))
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI128
                    RETURN
                END IF
            END DO
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitI128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitI128) THEN
            ! value might be in the applicable range
            IF (IsNegative(Number)) THEN
                ! overflow likely occurs
                Overflow = TrueVal
                IF ((NegSign).AND.(Number == MinI128)) THEN
                    ! actually not overflow
                    CurStr = '-' // cStr(IStart:StrLen)
                    IF (ToDecString(MinI128) == CurStr) THEN
                        Overflow = FalseVal
                        NegSign = FalseVal
                    END IF
                END IF
            ELSE
                ! positive value so check overflow
                CurStr = cStr(IStart:StrLen)
                Overflow = FalseVal
                DO Indx = 1, MaxDigitI128
                    CurChr => CurStr(Indx:Indx)
                    IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                        EXIT
                    ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                        Overflow = TrueVal
                        EXIT
                    END IF
                END DO
            END IF
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (NegSign) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI128
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI128
            END IF
        ELSE
            IF (NegSign) Number = -Number
        END IF

        RETURN

    END FUNCTION Parse_I128_V1

    !**************************************************************************

    FUNCTION Parse_I128_V2(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to a signed 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tSInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,  PARAMETER    :: A0           = IACHAR('0')
        tSInt32,  PARAMETER    :: A4           = IACHAR('4')
        tSInt32,  PARAMETER    :: A9           = IACHAR('9')
        tSInt32,  PARAMETER    :: MaxDigitI32  = 10
        tSInt32,  PARAMETER    :: MaxDigitI64  = 19
        tSInt32,  PARAMETER    :: MaxDigitI128 = 39
        tSInt128, PARAMETER    :: SInt128ase     = TenI128
        tCharParam             :: MaxStr       = '170141183460469231731687303715884105727'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart
        tCharLen(1), POINTER    :: CurChr
        tLogical                :: NegSign
        tLogical                :: Overflow
        tCharAlloc, TARGET      :: CurStr
        tLogical                :: ErrorFlag  ! true if input is not invalid
        tCharAlloc              :: ErrorMsg   ! message if input is not invalid
        tSInt32                 :: I32Val
        tSInt64                 :: I64Val

    !** FLOW

        ! get valid string length by removing the trailing space(s)
        StrLen = LEN_TRIM(cStr)
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal

        ! check whether there are spaces in front of the number
        ! (only allow space(s) in front of the number but no spaces inside it)
        Indx = 1
        IF (cStr(Indx:Indx) == ' ') THEN
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= ' ') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for sign
        NegSign = FalseVal
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') NegSign = TrueVal
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = 0_kInt64
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                ! only zero digits encountered
                Number = 0_kInt64
                RETURN
            END IF
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I32Val
                ELSE
                    Number = I32Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I64Val
                ELSE
                    Number = I64Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        END IF

        ! compute value of the input string
        IStart   = 0
        NumDigit = 0
        CurChr => cStr(Indx:Indx)
        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
            IStart = Indx
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                ! Number = Number*10 + (IACHAR(CurChr)-A0)
                CALL Multiply(Number, 10)
                CALL Add(Number, IACHAR(CurChr)-A0)
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI128
                    RETURN
                END IF
            END DO
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitI128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitI128) THEN
            ! value might be in the applicable range
            IF (IsNegative(Number)) THEN
                ! overflow likely occurs
                Overflow = TrueVal
                IF ((NegSign).AND.(Number == MinI128)) THEN
                    ! actually not overflow
                    CurStr = '-' // cStr(IStart:StrLen)
                    IF (ToDecString(MinI128) == CurStr) THEN
                        Overflow = FalseVal
                        NegSign = FalseVal
                    END IF
                END IF
            ELSE
                ! positive value so check overflow
                CurStr = cStr(IStart:StrLen)
                Overflow = FalseVal
                DO Indx = 1, MaxDigitI128
                    CurChr => CurStr(Indx:Indx)
                    IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                        EXIT
                    ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                        Overflow = TrueVal
                        EXIT
                    END IF
                END DO
            END IF
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (NegSign) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI128
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI128
            END IF
        ELSE
            IF (NegSign) Number = -Number
        END IF

        RETURN

    END FUNCTION Parse_I128_V2

    !**************************************************************************

    FUNCTION Parse_I128_V3(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to a signed 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tSInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,  PARAMETER    :: A0           = IACHAR('0')
        tSInt32,  PARAMETER    :: A4           = IACHAR('4')
        tSInt32,  PARAMETER    :: A9           = IACHAR('9')
        tSInt32,  PARAMETER    :: MaxDigitI32  = 10
        tSInt32,  PARAMETER    :: MaxDigitI64  = 19
        tSInt32,  PARAMETER    :: MaxDigitI128 = 39
        tSInt128, PARAMETER    :: SInt128ase     = TenI128
        tCharParam             :: MaxStr       = '170141183460469231731687303715884105727'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart
        tCharLen(1), POINTER    :: CurChr
        tLogical                :: NegSign
        tLogical                :: Overflow
        tCharAlloc, TARGET      :: CurStr
        tLogical                :: ErrorFlag  ! true if input is not invalid
        tCharAlloc              :: ErrorMsg   ! message if input is not invalid
        tSInt32                 :: I32Val
        tSInt64                 :: I64Val

    !** FLOW

        ! get valid string length by removing the trailing space(s)
        StrLen = LEN_TRIM(cStr)
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal

        ! check whether there are spaces in front of the number
        ! (only allow space(s) in front of the number but no spaces inside it)
        Indx = 1
        IF (cStr(Indx:Indx) == ' ') THEN
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= ' ') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for sign
        NegSign = FalseVal
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') NegSign = TrueVal
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = 0_kInt64
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                ! only zero digits encountered
                Number = 0_kInt64
                RETURN
            END IF
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I32Val
                ELSE
                    Number = I32Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I64Val
                ELSE
                    Number = I64Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        END IF

        ! compute value of the input string
        IStart   = 0
        NumDigit = 0
        CurChr => cStr(Indx:Indx)
        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
            IStart = Indx
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                ! Number = Number*10 + (IACHAR(CurChr)-A0)
                CALL MulAddI32(Number, 10, IACHAR(CurChr)-A0)
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI128
                    RETURN
                END IF
            END DO
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitI128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitI128) THEN
            ! value might be in the applicable range
            IF (IsNegative(Number)) THEN
                ! overflow likely occurs
                Overflow = TrueVal
                IF ((NegSign).AND.(Number == MinI128)) THEN
                    ! actually not overflow
                    CurStr = '-' // cStr(IStart:StrLen)
                    IF (ToDecString(MinI128) == CurStr) THEN
                        Overflow = FalseVal
                        NegSign = FalseVal
                    END IF
                END IF
            ELSE
                ! positive value so check overflow
                CurStr = cStr(IStart:StrLen)
                Overflow = FalseVal
                DO Indx = 1, MaxDigitI128
                    CurChr => CurStr(Indx:Indx)
                    IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                        EXIT
                    ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                        Overflow = TrueVal
                        EXIT
                    END IF
                END DO
            END IF
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (NegSign) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI128
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI128
            END IF
        ELSE
            IF (NegSign) Number = -Number
        END IF

        RETURN

    END FUNCTION Parse_I128_V3

    !**************************************************************************

    SUBROUTINE MulAddI32(I128, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply the given number by 'Mul' and then add 'Add' to it.
        ! (i.e. to set  I128 = I128*Mul + Add)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(INOUT)    :: I128
        tSInt32,  INTENT(IN)       :: Mul
        tSInt32,  INTENT(IN)       :: Add

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: X_Lo, Y_Lo, Y_Hi
        tUInt64       :: ProductHi, MulLo

    !** FLOW

        ! perform multiplication
        X_Lo = IAND(ToInt64(Mul), Mask32)
        Y_Lo = IAND(I128%Low, Mask32)
        Y_Hi = SHIFTR(I128%Low, 32)
        ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)
        I128%High = I128%High*Mul + ProductHi
        MulLo     = I128%Low*Mul

        ! perform addition
        I128%Low = MulLo + Add
        IF (IEOR(I128%Low, MinInt64) < IEOR(MulLo, MinInt64)) I128%High = I128%High + 1_kInt64

        RETURN

    END SUBROUTINE MulAddI32

    !******************************************************************************

    FUNCTION Parse_I128_V4(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to a signed 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tSInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,  PARAMETER    :: A0           = IACHAR('0')
        tSInt32,  PARAMETER    :: A4           = IACHAR('4')
        tSInt32,  PARAMETER    :: A9           = IACHAR('9')
        tSInt32,  PARAMETER    :: MaxDigitI32  = 10
        tSInt32,  PARAMETER    :: MaxDigitI64  = 19
        tSInt32,  PARAMETER    :: MaxDigitI128 = 39
        tSInt128, PARAMETER    :: SInt128ase     = TenI128
        tCharParam             :: MaxStr       = '170141183460469231731687303715884105727'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart, IndxP7
        tCharLen(1), POINTER    :: CurChr
        tLogical                :: NegSign
        tLogical                :: Overflow
        tCharAlloc, TARGET      :: CurStr
        tLogical                :: ErrorFlag  ! true if input is not invalid
        tCharAlloc              :: ErrorMsg   ! message if input is not invalid
        tSInt32                 :: I32Val
        tSInt64                 :: I64Val
        tCharLen(8)             :: wStr
        tUInt64                 :: wVal
        EQUIVALENCE(wStr, wVal)

    !** FLOW

        ! get valid string length by removing the trailing space(s)
        StrLen = LEN_TRIM(cStr)
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal

        ! check whether there are spaces in front of the number
        ! (only allow space(s) in front of the number but no spaces inside it)
        Indx = 1
        IF (cStr(Indx:Indx) == ' ') THEN
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= ' ') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for sign
        NegSign = FalseVal
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') NegSign = TrueVal
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = 0_kInt64
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                ! only zero digits encountered
                Number = 0_kInt64
                RETURN
            END IF
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I32Val
                ELSE
                    Number = I32Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I64Val
                ELSE
                    Number = I64Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        END IF

        ! compute value of the input string
        IStart   = 0
        NumDigit = 0
        CurChr => cStr(Indx:Indx)
        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
            IStart = Indx
            IndxP7 = Indx + 7
            DO WHILE (IndxP7 <= StrLen)
                wStr = cStr(Indx:IndxP7)
                IF (Is8Digits(WVal)) THEN
                    ! process 8 digits at once
                    ! Number = Number*100000000_kInt64 + Parse8Digits(wVal)
                    CALL MulAddI64(Number, 100000000_kInt64, Parse8Digits(wVal))
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI128
                    RETURN
                END IF
                Indx = Indx + 8
                IndxP7 = Indx + 7
            END DO
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                DO
                    ! compute the value without checking if it will overflow
                    ! we will check it after we process all the characters if valid
                    ! Number = Number*10 + (IACHAR(CurChr)-A0)
                    CALL MulAddI32(Number, 10, (IACHAR(CurChr)-A0))
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT
                    CurChr => cStr(Indx:Indx)
                    IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                        Number = MinI128
                        RETURN
                    END IF
                END DO
            END IF
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitI128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitI128) THEN
            ! value might be in the applicable range
            IF (IsNegative(Number)) THEN
                ! overflow likely occurs
                Overflow = TrueVal
                IF ((NegSign).AND.(Number == MinI128)) THEN
                    ! actually not overflow
                    CurStr = '-' // cStr(IStart:StrLen)
                    IF (ToDecString(MinI128) == CurStr) THEN
                        Overflow = FalseVal
                        NegSign = FalseVal
                    END IF
                END IF
            ELSE
                ! positive value so check overflow
                CurStr = cStr(IStart:StrLen)
                Overflow = FalseVal
                DO Indx = 1, MaxDigitI128
                    CurChr => CurStr(Indx:Indx)
                    IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                        EXIT
                    ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                        Overflow = TrueVal
                        EXIT
                    END IF
                END DO
            END IF
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (NegSign) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI128
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI128
            END IF
        ELSE
            IF (NegSign) Number = -Number
        END IF

        RETURN

    END FUNCTION Parse_I128_V4

    !**************************************************************************

    FUNCTION Parse8Digits(InVal) RESULT(OutVal)

        !DIR$ ATTRIBUTES FORCEINLINE :: Parse8Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To parse eight digits immediately.

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: InVal
        tUInt64               :: OutVal

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER    :: K1 = ToInt64(Z'0F0F0F0F0F0F0F0F')
        tUInt64, PARAMETER    :: K2 = ToInt64(Z'00FF00FF00FF00FF')
        tUInt64, PARAMETER    :: K3 = ToInt64(Z'0000FFFF0000FFFF')
        tUInt64, PARAMETER    :: M1 = 2561_kInt64
        tUInt64, PARAMETER    :: M2 = 6553601_kInt64
        tUInt64, PARAMETER    :: M3 = 42949672960001_kInt64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)

        RETURN

    END FUNCTION Parse8Digits

    !******************************************************************************

    FUNCTION Is8Digits(InVal) RESULT(Flag)

        !DIR$ ATTRIBUTES FORCEINLINE :: Is8Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether we can process eight digits immediately

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: InVal
        tLogical               :: Flag

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER    :: C1 = ToInt64(Z'F0F0F0F0F0F0F0F0')
        tUInt64, PARAMETER    :: C2 = ToInt64(Z'3333333333333333')
        tUInt64, PARAMETER    :: C3 = ToInt64(Z'0606060606060606')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        Flag = IOR(IAND(InVal, C1), SHIFTR(IAND((InVal + C3), C1), 4)) ==  C2

        RETURN

    END FUNCTION Is8Digits

    !******************************************************************************

    SUBROUTINE MulAddI64(I128, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply the given number by 'Mul' and then add 'Add' to it.
        ! (i.e. to set  I128 = I128*Mul + Add)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(INOUT)    :: I128
        tSInt64,  INTENT(IN)       :: Mul
        tSInt64,  INTENT(IN)       :: Add

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: X_Lo, X_Hi, Y_Lo, Y_Hi
        tUInt64       :: Hi_Lo, Cross, ProductHi, MulLo

    !** FLOW

        ! perform multiplication
        X_Lo = IAND(Mul, Mask32)
        Y_Lo = IAND(I128%Low, Mask32)
        Y_Hi = SHIFTR(I128%Low, 32)
        ProductHi = SHIFTR(SHIFTR(X_Lo*Y_Lo, 32) + X_Lo*Y_Hi, 32)
        I128%High = I128%High*Mul + ProductHi
        MulLo     = I128%Low*Mul

        ! perform addition
        I128%Low = MulLo + Add
        IF (IEOR(I128%Low, MinInt64) < IEOR(MulLo, MinInt64)) I128%High = I128%High + 1_kInt64

        RETURN

    END SUBROUTINE MulAddI64

    !******************************************************************************

    FUNCTION Parse_I128_V5(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to a signed 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tSInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,  PARAMETER    :: A0           = IACHAR('0')
        tSInt32,  PARAMETER    :: A4           = IACHAR('4')
        tSInt32,  PARAMETER    :: A9           = IACHAR('9')
        tSInt32,  PARAMETER    :: MaxDigitI32  = 10
        tSInt32,  PARAMETER    :: MaxDigitI64  = 19
        tSInt32,  PARAMETER    :: MaxDigitI128 = 39
        tSInt128, PARAMETER    :: SInt128ase     = TenI128
        tCharParam             :: MaxStr       = '170141183460469231731687303715884105727'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart, IndxP7
        tCharLen(1), POINTER    :: CurChr
        tLogical                :: NegSign
        tLogical                :: Overflow
        tCharAlloc, TARGET      :: CurStr
        tLogical                :: ErrorFlag  ! true if input is not invalid
        tCharAlloc              :: ErrorMsg   ! message if input is not invalid
        tSInt32                 :: I32Val
        tSInt64                 :: I64Val
        tCharLen(8)             :: wStr
        tUInt64                 :: wVal
        EQUIVALENCE(wStr, wVal)

    !** FLOW

        ! get valid string length by removing the trailing space(s)
        StrLen = LEN_TRIM(cStr)
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal

        ! check whether there are spaces in front of the number
        ! (only allow space(s) in front of the number but no spaces inside it)
        Indx = 1
        IF (cStr(Indx:Indx) == ' ') THEN
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= ' ') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for sign
        NegSign = FalseVal
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') NegSign = TrueVal
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = 0_kInt64
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                ! only zero digits encountered
                Number = 0_kInt64
                RETURN
            END IF
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I32Val
                ELSE
                    Number = I32Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinI128
            ELSE
                IF (NegSign) THEN
                    Number = -I64Val
                ELSE
                    Number = I64Val
                END IF
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        END IF

        ! compute value of the input string
        IStart   = 0
        NumDigit = 0
        CurChr => cStr(Indx:Indx)
        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
            IStart = Indx
            IndxP7 = Indx + 7
            DO WHILE (IndxP7 <= StrLen)
                wStr = cStr(Indx:IndxP7)
                IF (Is8Digits(WVal)) THEN
                    ! process 8 digits at once
                    ! Number = Number*100000000 + Parse8Digits(wVal)
                    CALL Multiply(Number, 100000000)
                    CALL Add(Number, Parse8Digits(wVal))
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI128
                    RETURN
                END IF
                Indx = Indx + 8
                IndxP7 = Indx + 7
            END DO
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                DO
                    ! compute the value without checking if it will overflow
                    ! we will check it after we process all the characters if valid
                    ! Number = Number*10 + (IACHAR(CurChr)-A0)
                    CALL Multiply(Number, 10)
                    CALL Add(Number, (IACHAR(CurChr)-A0))
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT
                    CurChr => cStr(Indx:Indx)
                    IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                        Number = MinI128
                        RETURN
                    END IF
                END DO
            END IF
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitI128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitI128) THEN
            ! value might be in the applicable range
            IF (IsNegative(Number)) THEN
                ! overflow likely occurs
                Overflow = TrueVal
                IF ((NegSign).AND.(Number == MinI128)) THEN
                    ! actually not overflow
                    CurStr = '-' // cStr(IStart:StrLen)
                    IF (ToDecString(MinI128) == CurStr) THEN
                        Overflow = FalseVal
                        NegSign = FalseVal
                    END IF
                END IF
            ELSE
                ! positive value so check overflow
                CurStr = cStr(IStart:StrLen)
                Overflow = FalseVal
                DO Indx = 1, MaxDigitI128
                    CurChr => CurStr(Indx:Indx)
                    IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                        EXIT
                    ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                        Overflow = TrueVal
                        EXIT
                    END IF
                END DO
            END IF
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (NegSign) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI128
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI128
            END IF
        ELSE
            IF (NegSign) Number = -Number
        END IF

        RETURN

    END FUNCTION Parse_I128_V5

    !**************************************************************************

END FUNCTION I128_FromDecString_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                       REAL CONVERSIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION R32_To_I128_Xp(R32, Algo) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 32-bit floating point number to an unsigned 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle, INTENT(IN)    :: R32
    tUInt32, INTENT(IN)    :: Algo
    tSInt128               :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        I128 = R32_To_I128_A1(R32)
    CASE (2)
        I128 = R32_To_I128_A2(R32)
    CASE (3)
        I128 = R32_To_I128_A3(R32)
    END SELECT

    RETURN
    CONTAINS

    FUNCTION R32_To_I128_A1(R32) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 32-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle, INTENT(IN) :: R32
        tSInt128            :: I128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER :: Mask = ToInt32(Z'000000FF')       ! 255
        tUInt32, PARAMETER :: C1   = SHIFTL(1, 23)              ! 2**23
        tUInt32, PARAMETER :: C2   = C1 - 1                     ! 2**23 - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt32         :: IBits
        tSingle         :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

    ! rounding behavior is towards zero.
    ! undefined behavior if R32 is NaN or cannot fit into I128.
    IF (.NOT.IEEE_IS_FINITE (R32)) THEN
        CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R32 is NOT finite.')
        RETURN
    ELSEIF (R32 < -2.0_kSingle**127) THEN
        CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R32 < I128Min.')
        RETURN
    ELSEIF (R32 >= 2.0_kSingle**127) THEN
        CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                          'Undefined behavior: R32 > I128Max.')
        RETURN
    END IF

    ! get absolute value and transfer bits from real to integer
    RBits = ABS(R32)
    ! determine exponent bits
    Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
    ! determine significand bits
    IBits = IOR(IAND(IBits, C2), C1)
    ! convert to SInt128
    I128 = IBits
    ! add exponent bits
    I128 = ISHFT(I128, Exp)
    ! add sign bit
    IF (R32 < 0.0_kSingle) THEN
       !  I128 = -I128
        I128%High = NOT(I128%High)
        IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
        I128%Low = NOT(I128%Low) + 1_kInt64
    END IF

    RETURN

    END FUNCTION R32_To_I128_A1

    !******************************************************************************

    FUNCTION R32_To_I128_A2(R32) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 32-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle, INTENT(IN) :: R32
        tSInt128            :: I128

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128       :: U128

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R32 is NaN or cannot fit into I128.
        IF (.NOT.IEEE_IS_FINITE (R32)) THEN
            CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R32 is NOT finite.')
            RETURN
        ELSEIF (R32 < -2.0_kSingle**127) THEN
            CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R32 < I128Min.')
            RETURN
        ELSEIF (R32 >= 2.0_kSingle**127) THEN
            CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R32 > I128Max.')
            RETURN
        END IF

        ! We must convert the absolute value and then negate as needed, because
        ! floating point types are typically sign-magnitude. Otherwise, the
        ! difference between the High and low 64 bits when interpreted as two's
        ! complement overwhelms the precision of the mantissa.
        IF (R32 < 0.0_kSingle) THEN
            U128 = -UInt128(-R32)
        ELSE
            U128 = UInt128(R32)
        END IF
        I128%High = BitCastToSigned(U128%High)
        I128%Low  = U128%Low

        RETURN

    END FUNCTION R32_To_I128_A2

    !******************************************************************************

    FUNCTION R32_To_I128_A3(R32) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 32-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle, INTENT(IN) :: R32
        tSInt128            :: I128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER :: Mask = ToInt32(Z'000000FF')       ! 255
        tUInt32, PARAMETER :: C1   = SHIFTL(1, 23)              ! 2**23
        tUInt32, PARAMETER :: C2   = C1 - 1                     ! 2**23 - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt32         :: IBits
        tSingle         :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R32 is NaN or cannot fit into I128.
        IF (.NOT.IEEE_IS_FINITE (R32)) THEN
            CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R32 is NOT finite.')
            RETURN
        ELSEIF (R32 < -2.0_kSingle**127) THEN
            CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R32 < I128Min.')
            RETURN
        ELSEIF (R32 >= 2.0_kSingle**127) THEN
            CALL Handle_ErrLevel('R32_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R32 > I128Max.')
            RETURN
        END IF

        ! get absolute value and transfer bits from real to integer
        RBits = ABS(R32)
        ! determine exponent bits
        Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
        ! determine significand bits
        IBits = IOR(IAND(IBits, C2), C1)
        ! convert to SInt128
        ! I128 = IBits
        ! add exponent bits
        ! I128 = ISHFT(I128, Exp)
        IF (Exp < 0) THEN
            ! I128 = SHIFTR(I128, -Exp)
            Exp = -Exp
            IF (Exp >= 64) THEN
                I128 = SInt128(0_kInt64, 0_kInt64)
            ELSE
                I128 = SInt128(0_kInt64, SHIFTR(ToInt64(IBits), Exp))
            END IF
        ELSE
            ! I128 = SHIFTL(I128, Exp)
            IF (Exp >= 128) THEN
                I128 = SInt128(0_kInt64, 0_kInt64)
            ELSEIF (Exp >= 64) THEN
                I128 = SInt128(SHIFTL(ToInt64(IBits), Exp - 64), 0_kInt64)
            ELSE
                I128 = SInt128(SHIFTR(ToInt64(IBits), 64 - Exp), SHIFTL(ToInt64(IBits), Exp))
            END IF
        END IF
        ! add sign bit
        IF (R32 < 0.0_kSingle) THEN
           !  I128 = -I128
            I128%High = NOT(I128%High)
            IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
            I128%Low = NOT(I128%Low) + 1_kInt64
        END IF

        RETURN

    END FUNCTION R32_To_I128_A3

    !******************************************************************************

END FUNCTION R32_To_I128_Xp

!******************************************************************************

MODULE FUNCTION R64_To_I128_Xp(R64, Algo) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 64-bit floating point number to an unsigned 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN)    :: R64
    tUInt32, INTENT(IN)    :: Algo
    tSInt128               :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        I128 = R64_To_I128_A1(R64)
    CASE (2)
        I128 = R64_To_I128_A2(R64)
    CASE (3)
        I128 = R64_To_I128_A3(R64)
    END SELECT

    RETURN
    CONTAINS

    FUNCTION R64_To_I128_A1(R64) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 64-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: R64
        tSInt128            :: I128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64,   PARAMETER  :: Mask = ToInt64(Z'00000000000007FF')   ! 2047
        tUInt64,   PARAMETER  :: C1   = SHIFTL(1_kInt64, 52)           ! 2**52
        tUInt64,   PARAMETER  :: C2   = C1 - 1_kInt64                  ! 2**52 - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt64         :: IBits
        tDouble         :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R64 is NaN or cannot fit into I128.
        IF (.NOT.IEEE_IS_FINITE (R64)) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 is NOT finite.')
            RETURN
        ELSEIF (R64 < -2.0_kDouble**127) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 < I128Min.')
            RETURN
        ELSEIF (R64 >= 2.0_kDouble**127) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 > I128Max.')
            RETURN
        END IF

        ! get absolute value and transfer bits from real to integer
        RBits = ABS(R64)
        ! determine exponent bits
        Exp = ToInt32(IAND(SHIFTR(IBits, 52), Mask)) - 1075   ! 1075 = 1023 + 52
        ! determine significand bits
        IBits = IOR(IAND(IBits, C2), C1)
        ! convert to SInt128
        I128 = IBits
        ! add exponent bits
        I128 = ISHFT(I128, Exp)
        ! add sign bit
        IF (R64 < 0.0_kDouble) THEN
           !  I128 = -I128
            I128%High = NOT(I128%High)
            IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
            I128%Low = NOT(I128%Low) + 1_kInt64
        END IF

        RETURN

    END FUNCTION R64_To_I128_A1

    !******************************************************************************

    FUNCTION R64_To_I128_A2(R64) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 64-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: R64
        tSInt128            :: I128

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128       :: U128

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R64 is NaN or cannot fit into I128.
        IF (.NOT.IEEE_IS_FINITE (R64)) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 is NOT finite.')
            RETURN
        ELSEIF (R64 < -2.0_kDouble**127) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 < I128Min.')
            RETURN
        ELSEIF (R64 >= 2.0_kDouble**127) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 > I128Max.')
            RETURN
        END IF

        ! We must convert the absolute value and then negate as needed, because
        ! floating point types are typically sign-magnitude. Otherwise, the
        ! difference between the High and low 64 bits when interpreted as two's
        ! complement overwhelms the precision of the mantissa.
        IF (R64 < 0.0_kDouble) THEN
            U128 = -UInt128(-R64)
        ELSE
            U128 = UInt128(R64)
        END IF
        I128%High = BitCastToSigned(U128%High)
        I128%Low  = U128%Low

        RETURN

    END FUNCTION R64_To_I128_A2

    !******************************************************************************

    FUNCTION R64_To_I128_A3(R64) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 64-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: R64
        tSInt128            :: I128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64,   PARAMETER  :: Mask = ToInt64(Z'00000000000007FF')   ! 2047
        tUInt64,   PARAMETER  :: C1   = SHIFTL(1_kInt64, 52)           ! 2**52
        tUInt64,   PARAMETER  :: C2   = C1 - 1_kInt64                  ! 2**52 - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt64         :: IBits
        tDouble         :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R64 is NaN or cannot fit into I128.
        IF (.NOT.IEEE_IS_FINITE (R64)) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 is NOT finite.')
            RETURN
        ELSEIF (R64 < -2.0_kDouble**127) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 < I128Min.')
            RETURN
        ELSEIF (R64 >= 2.0_kDouble**127) THEN
            CALL Handle_ErrLevel('R64_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R64 > I128Max.')
            RETURN
        END IF

        ! get absolute value and transfer bits from real to integer
        RBits = ABS(R64)
        ! determine exponent bits
        Exp = ToInt32(IAND(SHIFTR(IBits, 52), Mask)) - 1075   ! 1075 = 1023 + 52
        ! determine significand bits
        IBits = IOR(IAND(IBits, C2), C1)
        ! convert to SInt128
        ! I128 = SInt128(0_kInt64, IBits)
        ! add exponent bits
        ! I128 = ISHFT(I128, Exp)
        IF (Exp < 0) THEN
            ! I128 = SHIFTR(I128, -Exp)
            Exp = -Exp
            IF (Exp >= 64) THEN
                I128 = SInt128(0_kInt64, 0_kInt64)
            ELSE
                I128 = SInt128(0_kInt64, SHIFTR(IBits, Exp))
            END IF
        ELSE
            ! I128 = SHIFTL(I128, Exp)
            IF (Exp >= 128) THEN
                I128 = SInt128(0_kInt64, 0_kInt64)
            ELSEIF (Exp >= 64) THEN
                I128 = SInt128(SHIFTL(IBits, Exp - 64), 0_kInt64)
            ELSE
                I128 = SInt128(SHIFTR(IBits, 64 - Exp), SHIFTL(IBits, Exp))
            END IF
        END IF
        ! add sign bit
        IF (R64 < 0.0_kDouble) THEN
           !  I128 = -I128
            I128%High = NOT(I128%High)
            IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
            I128%Low = NOT(I128%Low) + 1_kInt64
        END IF

        RETURN

    END FUNCTION R64_To_I128_A3

    !******************************************************************************

END FUNCTION R64_To_I128_Xp

!******************************************************************************

MODULE FUNCTION R128_To_I128_Xp(R128, Algo) RESULT(I128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tQuad,   INTENT(IN)    :: R128
    tUInt32, INTENT(IN)    :: Algo
    tSInt128               :: I128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        I128 = R128_To_I128_A1(R128)
    CASE (2)
        I128 = R128_To_I128_A2(R128)
    CASE (3)
        I128 = R128_To_I128_A3(R128)
    END SELECT

    RETURN
    CONTAINS

    FUNCTION R128_To_I128_A1(R128) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tQuad, INTENT(IN)   :: R128
        tSInt128            :: I128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER :: Mask  = ToInt32(Z'00007FFF')                  ! 32767
        tUInt64, PARAMETER :: C1(2) = [ 0_kInt64, 281474976710656_kInt64]   ! 2**112 = SHIFTL(1, 112)
        tUInt64, PARAMETER :: C2(2) = [-1_kInt64, 281474976710655_kInt64]   ! 2**112 -1 = SHIFTL(1, 112) - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt64         :: IBits(2)
        tQuad           :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R128 is NaN or cannot fit into I128.
        IF (.NOT.IEEE_IS_FINITE (R128)) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 is NOT finite.')
            RETURN
        ELSEIF (R128 < -2.0_kQuad**127) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 < I128Min.')
            RETURN
        ELSEIF (R128 >= 2.0_kQuad**127) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 > I128Max.')
            RETURN
        END IF

        ! get absolute value and transfer bits from real to integer
        RBits = ABS(R128)
        ! determine exponent bits
        ! 48 = 112-64; 16495 = 16383 + 112
        Exp = IAND(ToInt32(SHIFTR(IBits(2), 48)), Mask) - 16495
        ! determine significand bits and convert to SInt128
        I128%Low  = IOR(IAND(IBits(1), C2(1)), C1(1))
        I128%High = IOR(IAND(IBits(2), C2(2)), C1(2))
        ! add exponent bits
        I128 = ISHFT(I128, Exp)
        ! add sign bit
        IF (R128 < 0.0_kQuad) THEN
           !  I128 = -I128
            I128%High = NOT(I128%High)
            IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
            I128%Low = NOT(I128%Low) + 1_kInt64
        END IF

        RETURN

    END FUNCTION R128_To_I128_A1

    !******************************************************************************

    FUNCTION R128_To_I128_A2(R128) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tQuad, INTENT(IN)   :: R128
        tSInt128            :: I128

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128       :: U128

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R128 is NaN or cannot fit into I128.
        IF (.NOT.IEEE_IS_FINITE (R128)) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 is NOT finite.')
            RETURN
        ELSEIF (R128 < -2.0_kQuad**127) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 < I128Min.')
            RETURN
        ELSEIF (R128 >= 2.0_kQuad**127) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 > I128Max.')
            RETURN
        END IF

        ! We must convert the absolute value and then negate as needed, because
        ! floating point types are typically sign-magnitude. Otherwise, the
        ! difference between the High and low 64 bits when interpreted as two's
        ! complement overwhelms the precision of the mantissa.
        IF (R128 < 0.0_kQuad) THEN
            U128 = -UInt128(-R128)
        ELSE
            U128 = UInt128(R128)
        END IF
        I128%High = BitCastToSigned(U128%High)
        I128%Low  = U128%Low

        RETURN

    END FUNCTION R128_To_I128_A2

    !******************************************************************************

    FUNCTION R128_To_I128_A3(R128) RESULT(I128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tQuad, INTENT(IN)   :: R128
        tSInt128            :: I128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER :: Mask  = ToInt32(Z'00007FFF')                  ! 32767
        tUInt64, PARAMETER :: C1(2) = [ 0_kInt64, 281474976710656_kInt64]   ! 2**112 = SHIFTL(1, 112)
        tUInt64, PARAMETER :: C2(2) = [-1_kInt64, 281474976710655_kInt64]   ! 2**112 -1 = SHIFTL(1, 112) - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt64         :: IBits(2)
        tQuad           :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R128 is NaN or cannot fit into I128.
        IF (.NOT.IEEE_IS_FINITE (R128)) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 is NOT finite.')
            RETURN
        ELSEIF (R128 < -2.0_kQuad**127) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 < I128Min.')
            RETURN
        ELSEIF (R128 >= 2.0_kQuad**127) THEN
            CALL Handle_ErrLevel('R128_To_I128', ModName, ErrSevere, &
                              'Undefined behavior: R128 > I128Max.')
            RETURN
        END IF

        ! get absolute value and transfer bits from real to integer
        RBits = ABS(R128)
        ! determine exponent bits
        ! 48 = 112-64; 16495 = 16383 + 112
        Exp = IAND(ToInt32(SHIFTR(IBits(2), 48)), Mask) - 16495
        ! determine significand bits and convert to SInt128
        I128%Low  = IOR(IAND(IBits(1), C2(1)), C1(1))
        I128%High = IOR(IAND(IBits(2), C2(2)), C1(2))
        ! add exponent bits
        ! I128 = ISHFT(I128, Exp)
        IF (Exp < 0) THEN
            I128 = SHIFTR(I128, -Exp)
        ELSE
            I128 = SHIFTL(I128, Exp)
        END IF
         ! add sign bit
        IF (R128 < 0.0_kQuad) THEN
           !  I128 = -I128
            I128%High = NOT(I128%High)
            IF (I128%Low == 0_kInt64) I128%High = I128%High + 1_kInt64
            I128%Low = NOT(I128%Low) + 1_kInt64
        END IF

        RETURN

    END FUNCTION R128_To_I128_A3

    !******************************************************************************

END FUNCTION R128_To_I128_Xp

!******************************************************************************

MODULE FUNCTION R32_From_I128_Xp(I128, Algo) RESULT(R32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a 32-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: I128
    tSInt32,  INTENT(IN)   :: Algo
    tSingle                :: R32

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSingle, PARAMETER  :: TwoPow64 = 2.0_kSingle**64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        R32 = R32_From_I128_A1(I128)
    CASE (2)
        R32 = R32_From_I128_A2(I128)
    END SELECT

    RETURN

    CONTAINS

    FUNCTION R32_From_I128_A1(I128) RESULT(R32)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 32-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: I128
        tSingle                :: R32

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSingle, PARAMETER :: TwoPow64 = 2.0_kSingle**64
        tUInt32, PARAMETER :: TwoPow23 = SHIFTL(1, 23)
        tUInt32, PARAMETER :: Mask     = ToInt32(Z'000000FF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: Negative
        tUInt64     :: High, Low
        tSInt32     :: S, Exp
        tUInt32     :: IBits
        tSingle     :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! get sign flag and absolute values of components
        Negative = (I128%High < 0_kInt64)
        IF (Negative) THEN
            High = NOT(I128%High)
            IF (I128%Low == 0_kInt64) High = High + 1_kInt64
            Low  = NOT(I128%Low) + 1_kInt64
        ELSE
            High = I128%High
            Low  = I128%Low
        END IF

        IF (High == 0_kInt64) THEN
            R32 = U64_To_R32(Low)
            IF (IsNegative(I128)) R32 = -R32
            RETURN
        END IF

        S = LEADZ(High)
        ! Mask out the 24 MSBits
        ! Also, the leading bit is implicit so cancel it out to get the significand
        IF (S <= 40) THEN
            IBits = IEOR(ToInt32(SHIFTR(High, 40-S)), TwoPow23)
        ELSE
            ! S-40 == additional bits we need
            IBits = IEOR(ToInt32(IOR(SHIFTL(High, S-40), SHIFTR(Low, 104-S))), TwoPow23)
        END IF
        ! get the binary exponent
        Exp = IAND(254-S, Mask)         ! 254 = 64 + 64 + 127 - 1

        ! Add the exponent
        IBits = IOR(IBits, SHIFTL(Exp, 23))

        ! transfer output (RBits mapped to IBits using equivalence)
        R32 = RBits

        ! check and add sign if needed
        IF (Negative) R32 = -R32

        RETURN

    END FUNCTION R32_From_I128_A1

    !******************************************************************************

    FUNCTION R32_From_I128_A2(I128) RESULT(R32)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 32-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: I128
        tSingle                :: R32

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt128   :: PosI

    !** FLOW

        PosI = ABS(I128)
        R32 = U64_To_R32(PosI%Low) + REAL(PosI%High, KIND=kSingle)*TwoPow64
        IF (IsNegative(I128)) R32 = -R32

        RETURN

    END FUNCTION R32_From_I128_A2

    !******************************************************************************

    FUNCTION U64_To_R32(LongVal) RESULT(SingleVal)

    !DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R32

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 32-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: LongVal      ! integer number treated as unsigned one
        tSingle             :: SingleVal    ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kInt64) THEN
            SingleVal = REAL(LongVal, KIND=kSingle)
        ELSE
            SingleVal = TwoPow64 + REAL(LongVal, KIND=kSingle)
        END IF

        RETURN

    END FUNCTION U64_To_R32

    !******************************************************************************

END FUNCTION R32_From_I128_Xp

!******************************************************************************

MODULE FUNCTION R64_From_I128_Xp(I128, Algo) RESULT(R64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a 64-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: I128
    tSInt32,  INTENT(IN)   :: Algo
    tDouble                :: R64

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: TwoPow64 = 2.0_kDouble**64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        R64 = R64_From_I128_A1(I128)
    CASE (2)
        R64 = R64_From_I128_A2(I128)
    END SELECT

    RETURN

    CONTAINS

    FUNCTION R64_From_I128_A1(I128) RESULT(R64)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 64-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: I128
        tDouble                :: R64

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER :: TwoPow52 = SHIFTL(1_kInt64, 52)
        tUInt32, PARAMETER :: Mask     = ToInt32(Z'000007FF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical    :: Negative
        tUInt64     :: High, Low
        tSInt32     :: S
        tUInt64     :: IBits
        tSInt64     :: Exp
        tDouble     :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! get sign flag and absolute values of components
        Negative = (I128%High < 0_kInt64)
        IF (Negative) THEN
            High = NOT(I128%High)
            IF (I128%Low == 0_kInt64) High = High + 1_kInt64
            Low  = NOT(I128%Low) + 1_kInt64
        ELSE
            High = I128%High
            Low  = I128%Low
        END IF

        IF (High == 0_kInt64) THEN
            R64 = U64_To_R64(Low)
            IF (Negative) R64 = -R64
            RETURN
        END IF

        S = LEADZ(High)
        ! Mask out the 53 MSBits
        ! Also, the leading bit is implicit so cancel it out to get the significand
        IF (S <= 11) THEN
            IBits = IEOR(SHIFTR(High, 11-S), TwoPow52)
        ELSE
            ! S-11 == additional bits we need
            IBits = IEOR(IOR(SHIFTL(High, S-11), SHIFTR(Low, 75-S)), TwoPow52)
        END IF
        ! get the binary exponent
        Exp = ToInt64(IAND(1150-S, Mask))        ! 1150 = 64 + 64 + 1023 - 1

        ! Add the exponent
        IBits = IOR(IBits, SHIFTL(Exp, 52))

        ! transfer output (RBits mapped to IBits using equivalence)
        R64 = RBits

        ! check and add sign if needed
        IF (Negative) R64 = -R64

        RETURN

    END FUNCTION R64_From_I128_A1

    !******************************************************************************

    FUNCTION R64_From_I128_A2(I128) RESULT(R64)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 64-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: I128
        tDouble                :: R64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt128   :: PosI

    !** FLOW

        PosI = ABS(I128)
        R64 = U64_To_R64(PosI%Low) + REAL(PosI%High, KIND=kDouble)*TwoPow64
        IF (IsNegative(I128)) R64 = -R64

        RETURN

    END FUNCTION R64_From_I128_A2

    !******************************************************************************

    FUNCTION U64_To_R64(LongVal) RESULT(DoubleVal)

    !DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R64

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 64-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: LongVal      ! integer number treated as unsigned one
        tDouble             :: DoubleVal    ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kInt64) THEN
            DoubleVal = REAL(LongVal, KIND=kDouble)
        ELSE
            DoubleVal = TwoPow64 + REAL(LongVal, KIND=kDouble)
        END IF

        RETURN

    END FUNCTION U64_To_R64

    !******************************************************************************

END FUNCTION R64_From_I128_Xp

!******************************************************************************

MODULE FUNCTION R128_From_I128_Xp(I128, Algo) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a 128-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)   :: I128
    tSInt32,  INTENT(IN)   :: Algo
    tQuad                  :: R128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tQuad, PARAMETER    :: TwoPow64 = 2.0_kQuad**64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        R128 = R128_From_I128_A1(I128)
    CASE (2)
        R128 = R128_From_I128_A2(I128)
    END SELECT

    RETURN

    CONTAINS

    FUNCTION R128_From_I128_A1(I128) RESULT(R128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 128-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: I128
        tQuad                  :: R128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER :: TwoPow112(2) = [0_kInt64, 281474976710656_kInt64]  ! SHIFTL(1, 112)
        tUInt32, PARAMETER :: Mask         = ToInt32(Z'00007FFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLogical        :: Negative
        tUInt64         :: High, Low
        tSInt32         :: S, Shift
        tSInt64         :: Exp
        tUInt64         :: IBits(2)
        tQuad           :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! get sign flag and absolute values of components
        Negative = (I128%High < 0_kInt64)
        IF (Negative) THEN
            High = NOT(I128%High)
            IF (I128%Low == 0_kInt64) High = High + 1_kInt64
            Low  = NOT(I128%Low) + 1_kInt64
        ELSE
            High = I128%High
            Low  = I128%Low
        END IF

        IF (High == 0_kInt64) THEN
            R128 = U64_To_R128(Low)
            IF (Negative) R128 = -R128
            RETURN
        END IF

        S = LEADZ(High)
        IF (S >= 15) THEN
            R128 = U64_To_R128(Low) + REAL(High, KIND=kQuad)*TwoPow64
            IF (Negative) R128 = -R128
            RETURN
        END IF

        ! Mask out the 113 MSBits
        Shift = 15 - S
        IBits(2) = SHIFTR(High, Shift)
        IBits(1) = IOR(SHIFTR(Low, Shift), SHIFTL(High, 64-Shift))

        ! get the binary exponent
        Exp = ToInt64(IAND(16510-S, Mask))   ! 16510 = 64 + 64 + 16383 - 1

        ! The leading bit is implicit, cancel it out to get the significand
        ! and also add the exponent
        IBits(1) = IEOR(IOR(SHIFTR(Low, Shift), SHIFTL(High, 64-Shift)), TwoPow112(1))
        IBits(2) = IOR(IEOR(SHIFTR(High, Shift), TwoPow112(2)), SHIFTL(Exp, 48))    ! 48 = 112 - 64

        ! transfer output (RBits mapped to IBits using equivalence)
        ! For big-endian machine, this one is likely wrong so we must
        ! swap IBits(1) and IBits(2) before the assigment.
        !   Tmp = IBits(1); IBits(1) = IBits(2); IBits(2) = Tmp
        R128 = RBits

        ! check and add sign if needed
        IF (Negative) R128 = -R128

        RETURN

    END FUNCTION R128_From_I128_A1

    !**************************************************************************

    FUNCTION R128_From_I128_A2(I128) RESULT(R128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 128-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt128, INTENT(IN)   :: I128
        tQuad                  :: R128

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt128   :: PosI

    !** FLOW

        PosI = ABS(I128)
        R128 = U64_To_R128(PosI%Low) + REAL(PosI%High, KIND=kQuad)*TwoPow64
        IF (IsNegative(I128)) R128 = -R128

        RETURN

    END FUNCTION R128_From_I128_A2

    !******************************************************************************

    FUNCTION U64_To_R128(LongVal) RESULT(QuadVal)

    !DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R128

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 128-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: LongVal  ! integer number treated as unsigned one
        tQuad               :: QuadVal  ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kInt64) THEN
            QuadVal = REAL(LongVal, KIND=kQuad)
        ELSE
            QuadVal = TwoPow64 + REAL(LongVal, KIND=kQuad)
        END IF

        RETURN

    END FUNCTION U64_To_R128

    !**************************************************************************

END FUNCTION R128_From_I128_Xp

!******************************************************************************

END SUBMODULE SubBase_I128_Experimental

!******************************************************************************
