
SUBMODULE (ModBase_UInt128) SubBase_U128_Experimental

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for experimental routines of the
!   <a href="../module/modbase_uint128.html">UInt128</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tUInt128        TYPE(UInt128)
#define     tSInt128        TYPE(SInt128)

!** MODULE PARAMETERS:
    tSInt64,  PARAMETER    :: MinInt64 = ToInt64(Z'8000000000000000')    ! -9223372036854775808

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

MODULE FUNCTION U128_Addition_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition:  OutVal = LhsVal + RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: LhsVal
    tUInt128, INTENT(IN)   :: RhsVal
    tSInt32,  INTENT(IN)   :: Algo
    tUInt128               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        CALL UAdd_A1(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (2)
        CALL UAdd_A2(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (3)
        CALL UAdd_A3(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (4)
        CALL UAdd_A4(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
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

    PURE SUBROUTINE UAdd_A1(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: UAdd_A1

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition:  OutVal = LhsVal + RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo + RhsLo
        IF (IEOR(OutLo, MinInt64) < IEOR(LhsLo, MinInt64)) THEN
            OutHi = LhsHi + RhsHi + 1_kInt64
        ELSE
            OutHi = LhsHi + RhsHi
        END IF

        RETURN

    END SUBROUTINE UAdd_A1

    !******************************************************************************

    PURE SUBROUTINE UAdd_A2(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: UAdd_A2

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition:  OutVal = LhsVal + RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: ResHi, ResLo

    !** FLOW

        ResHi = LhsHi + RhsHi
        ResLo = LhsLo + RhsLo
        CALL AddResult(ResHi, ResLo, LhsLo, OutHi, OutLo)

        RETURN

    END SUBROUTINE UAdd_A2

    !******************************************************************************

    PURE SUBROUTINE AddResult(ResHi, ResLo, LhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: AddResult

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check for carry and add one to the result if needed

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: ResHi, ResLo
        tUInt64, INTENT(IN)   :: LhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

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

    PURE SUBROUTINE UAdd_A3(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: UAdd_A3

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition:  OutVal = LhsVal + RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: Carry

    !** FLOW

        CALL AddU64(LhsLo, RhsLo, 0_kInt64, OutLo, Carry)
        CALL AddU64(LhsHi, RhsHi, Carry, OutHi)

        RETURN

    END SUBROUTINE UAdd_A3

    !**************************************************************************

    PURE SUBROUTINE UAdd_A4(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: UAdd_A4

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition:  OutVal = LhsVal + RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo + RhsLo
        OutHi = LhsHi + RhsHi
        IF (OutLo .ULT. LhsLo) OutHi = OutHi + 1_kInt64

        RETURN

    END SUBROUTINE UAdd_A4

    !**************************************************************************

END FUNCTION U128_Addition_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  SUBTRACTION OPERATIONS                  ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION U128_Subtraction_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction:  OutVal = LhsVal - RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: LhsVal
    tUInt128, INTENT(IN)   :: RhsVal
    tSInt32,  INTENT(IN)   :: Algo
    tUInt128               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        CALL USub_A1(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (2)
        CALL USub_A2(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (3)
        CALL USub_A3(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (4)
        CALL USub_A4(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
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

    PURE SUBROUTINE USub_A1(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: USub_A1

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction:  OutVal = LhsVal - RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo - RhsLo
        IF (IEOR(LhsLo, MinInt64) < IEOR(RhsLo, MinInt64)) THEN
            OutHi = LhsHi - RhsHi - 1_kInt64
        ELSE
            OutHi = LhsHi - RhsHi
        END IF

        RETURN

    END SUBROUTINE USub_A1

    !******************************************************************************

    PURE SUBROUTINE USub_A2(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: USub_A2

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction:  OutVal = LhsVal - RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: ResHi, ResLo

    !** FLOW

        ResHi = LhsHi - RhsHi
        ResLo = LhsLo - RhsLo
        CALL SubResult(ResHi, ResLo, LhsLo, RhsLo, OutHi, OutLo)

        RETURN

    END SUBROUTINE USub_A2

    !******************************************************************************

    PURE SUBROUTINE SubResult(ResHi, ResLo, LhsLo, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: SubResult

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check for carry and add one to the result if needed

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: ResHi, ResLo
        tUInt64, INTENT(IN)   :: LhsLo, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

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

    PURE SUBROUTINE USub_A3(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: USub_A3

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction:  OutVal = LhsVal - RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: Borrow

    !** FLOW

        CALL SubU64(LhsLo, RhsLo, 0_kInt64, OutLo, Borrow)
        CALL SubU64(LhsHi, RhsHi, Borrow, OutHi)

        RETURN

    END SUBROUTINE USub_A3

    !**************************************************************************

    PURE SUBROUTINE USub_A4(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: USub_A4

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction:  OutVal = LhsVal - RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo - RhsLo
        OutHi = LhsHi - RhsHi
        IF (OutLo .UGT. LhsLo) OutHi = OutHi - 1_kInt64

        RETURN

    END SUBROUTINE USub_A4

    !**************************************************************************

END FUNCTION U128_Subtraction_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 MULTIPLICATION OPERATIONS                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION U128_Multiplication_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = LhsVal * RhsVal

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: LhsVal
    tUInt128, INTENT(IN)   :: RhsVal
    tSInt32,  INTENT(IN)   :: Algo
    tUInt128               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        CALL UMult_A1(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (2)
        CALL UMult_A2(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (3)
        CALL UMult_A3(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    CASE (4)
        CALL UMult_A4(LhsVal%High, LhsVal%Low, RhsVal%High, RhsVal%Low, OutVal%High, OutVal%Low)
    END SELECT

    RETURN

    CONTAINS

    PURE SUBROUTINE UMult_A1(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: UMult_A1

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication:  OutVal = LhsVal * RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: X_Lo, X_Hi, Y_Lo, Y_Hi
        tUInt64       :: Lo_Lo, Hi_Lo, Cross

    !** FLOW

        X_Lo = IAND(LhsLo, Mask32)
        X_Hi = SHIFTR(LhsLo, 32)
        Y_Lo = IAND(RhsLo, Mask32)
        Y_Hi = SHIFTR(RhsLo, 32)
        Lo_Lo = X_Lo*Y_Lo
        Hi_Lo = X_Hi*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
        OutHi = LhsLo*RhsHi +  LhsHi*RhsLo + &
                        SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
        OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))

        RETURN

    END SUBROUTINE UMult_A1

    !******************************************************************************

    PURE SUBROUTINE UMult_A2(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: UMult_A2

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication:  OutVal = LhsVal * RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: Carry, Low

    !** FLOW

        CALL UMul128(LhsLo, RhsLo, Carry, Low)
        OutHi = LhsLo*RhsHi +  LhsHi*RhsLo + Carry
        OutLo = Low

        RETURN

    END SUBROUTINE UMult_A2

    !******************************************************************************

    PURE SUBROUTINE UMult_A3(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: UMult_A3

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication:  OutVal = LhsVal * RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: X_Lo, X_Hi, Y_Lo, Y_Hi
        tUInt64       :: Lo_Lo, Hi_Lo, Cross

    !** FLOW

        X_Lo = IAND(LhsLo, Mask32)
        Y_Lo = IAND(RhsLo, Mask32)
        X_Hi = SHIFTR(LhsLo, 32)
        Y_Hi = SHIFTR(RhsLo, 32)
        Lo_Lo = X_Lo*Y_Lo
        IF ((LhsHi /= 0_kInt64).AND.(RhsHi /= 0_kInt64)) THEN
            IF (X_Hi == 0_kInt64) THEN
                Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
                OutHi = LhsLo*RhsHi +  LhsHi*RhsLo + SHIFTR(Cross, 32)
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            ELSEIF (Y_Hi == 0_kInt64) THEN
                Cross = SHIFTR(Lo_Lo, 32) + X_Hi*Y_Lo
                OutHi = LhsLo*RhsHi +  LhsHi*RhsLo + SHIFTR(Cross, 32)
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            ELSE
                Hi_Lo = X_Hi*Y_Lo
                Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
                OutHi = LhsLo*RhsHi +  LhsHi*RhsLo + &
                                SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            END IF
        ELSEIF ((LhsHi == 0_kInt64).AND.(RhsHi == 0_kInt64)) THEN
            IF (X_Hi == 0_kInt64) THEN
                Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
                OutHi = SHIFTR(Cross, 32)
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            ELSEIF (Y_Hi == 0_kInt64) THEN
                Cross = SHIFTR(Lo_Lo, 32) + X_Hi*Y_Lo
                OutHi = SHIFTR(Cross, 32)
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            ELSE
                Hi_Lo = X_Hi*Y_Lo
                Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
                OutHi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            END IF
        ELSEIF (RhsHi == 0_kInt64) THEN
            IF (X_Hi == 0_kInt64) THEN
                Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
                OutHi = LhsHi*RhsLo + SHIFTR(Cross, 32)
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            ELSEIF (Y_Hi == 0_kInt64) THEN
                Cross = SHIFTR(Lo_Lo, 32) + X_Hi*Y_Lo
                OutHi = LhsHi*RhsLo + SHIFTR(Cross, 32)
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            ELSE
                Hi_Lo = X_Hi*Y_Lo
                Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
                OutHi = LhsHi*RhsLo + SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            END IF
        ELSE
            IF (X_Hi == 0_kInt64) THEN
                Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
                OutHi = LhsLo*RhsHi + SHIFTR(Cross, 32)
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            ELSEIF (Y_Hi == 0_kInt64) THEN
                Cross = SHIFTR(Lo_Lo, 32) + X_Hi*Y_Lo
                OutHi = LhsLo*RhsHi + SHIFTR(Cross, 32)
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            ELSE
                Hi_Lo = X_Hi*Y_Lo
                Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
                OutHi = LhsLo*RhsHi + SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
                OutLo  = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
            END IF
        END IF

        RETURN

    END SUBROUTINE UMult_A3

    !******************************************************************************

    PURE SUBROUTINE UMult_A4(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !DIR$ ATTRIBUTES INLINE :: UMult_A4

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication:  OutVal = LhsVal * RhsVal

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LhsHi, LhsLo
        tUInt64, INTENT(IN)   :: RhsHi, RhsLo
        tUInt64, INTENT(OUT)  :: OutHi, OutLo

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

    END SUBROUTINE UMult_A4

    !******************************************************************************

END FUNCTION U128_Multiplication_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++             DIVISION/MODULATION OPERATIONS               ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE U128_DivMod_Absl(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two UInt128 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: Dividend
    tUInt128, INTENT(IN)   :: Divisor
    tUInt128, INTENT(OUT)  :: Quotient
    tUInt128, INTENT(OUT)  :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32    :: Shift, I
    tUInt128   :: Numerator, Denominator

!** FLOW

    IF (Divisor == ZeroU128) THEN
        Quotient  = ZeroU128
        Remainder = ZeroU128
        CALL Handle_ErrLevel('U128_DivMod', ModName, ErrSevere, 'The divisor must not be zero.')
        RETURN
    END IF

    IF (Divisor .UGT. Dividend) THEN
        Quotient  = ZeroU128
        Remainder = Dividend
        RETURN
    ELSEIF (Divisor == Dividend) THEN
        Quotient  = OneU128
        Remainder = ZeroU128
        RETURN
    END IF

    ! initialize
    Numerator   = Dividend
    Denominator = Divisor
    Quotient    = ZeroU128

    ! Left aligns the MSB of the denominator and the dividend.
    Shift = Fls128(Dividend) - Fls128(Denominator)
    Denominator = SHIFTL(Denominator, Shift)

    ! Uses shift-subtract algorithm to divide dividend by denominator.
    ! The remainder will be left in dividend.
    DO I = 0, Shift
        Quotient = SHIFTL(Quotient, 1)
        IF (Numerator .UGE. Denominator) THEN
            Numerator = Numerator - Denominator
            Quotient  = IOR(Quotient, OneU128)
        END IF
        Denominator = SHIFTR(Denominator, 1)
    END DO
    Remainder = Numerator

    RETURN

CONTAINS

    FUNCTION Fls128(U128) RESULT(Num)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tSInt32                :: Num

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (U128%High /= 0_kInt64) THEN
            Num = 127 - LEADZ(U128%High)
        ELSE
            Num = 63 - LEADZ(U128%Low)
        END IF

        RETURN

    END FUNCTION Fls128

    !**************************************************************************

END SUBROUTINE U128_DivMod_Absl

!******************************************************************************

SUBROUTINE U128_DivMod_Flang(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two UInt128 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: Dividend
    tUInt128, INTENT(IN)   :: Divisor
    tUInt128, INTENT(OUT)  :: Quotient
    tUInt128, INTENT(OUT)  :: Remainder

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER    :: TopBit  = SHIFTL(1_kInt64, 63)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32    :: J
    tUInt128   :: Bits, Numerator

!** FLOW

    IF (Divisor == ZeroU128) THEN
        Quotient  = ZeroU128
        Remainder = ZeroU128
       CALL Handle_ErrLevel('U128_DivMod_Flang', ModName, ErrSevere, &
                         'The divisor must not be zero.')
        RETURN
    ELSEIF (Divisor .UGT. Dividend) THEN
        Quotient  = ZeroU128
        Remainder = Dividend
        RETURN
    ELSEIF (Divisor == Dividend) THEN
        Quotient  = OneU128
        Remainder = ZeroU128
        RETURN
    END IF

    ! initialize
    J = LEADZ(Dividend)
    Bits = SHIFTL(Dividend, J)
    Numerator = ZeroU128
    Quotient  = ZeroU128

    DO WHILE (J < 128)
        Numerator = SHIFTL(Numerator, 1)
        IF (IAND(Bits%High, TopBit) /= 0_kInt64) Numerator%Low = IOR(Numerator%Low, 1_kInt64)
        Bits = SHIFTL(Bits, 1)
        Quotient = SHIFTL(Quotient, 1)
        IF (Numerator .UGE. Divisor) THEN
            Quotient = Quotient + OneU128
            Numerator = Numerator - Divisor
        END IF
        J = J + 1
    END DO
    Remainder = Numerator

    RETURN

END SUBROUTINE U128_DivMod_Flang

!******************************************************************************

SUBROUTINE U128_DivMod_Go(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two UInt128 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: Dividend
    tUInt128, INTENT(IN)   :: Divisor
    tUInt128, INTENT(OUT)  :: Quotient
    tUInt128, INTENT(OUT)  :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Divisor%High == 0_kInt64) THEN
        IF (IEOR(Dividend%High, MinInt64) < IEOR(Divisor%Low, MinInt64)) THEN
            CALL DivideU128ByU64(Dividend%High, Dividend%Low, Divisor%Low, &
                                 Quotient%Low, Remainder%Low)
            Quotient%High  = 0_kInt64
            Remainder%High = 0_kInt64
        ELSE
            CALL DivideU128ByU64(0_kInt64, Dividend%High, Divisor%Low, &
                                 Quotient%High, Remainder%High)
            CALL DivideU128ByU64(Remainder%High, Dividend%Low, Divisor%Low, &
                                 Quotient%Low, Remainder%Low)
            Remainder%High = 0_kInt64
        END IF
    ELSE
        CALL U128_DivMod_Java(Dividend, Divisor, Quotient, Remainder)
    END IF

    RETURN

CONTAINS

    SUBROUTINE DivideU128ByU64(DividendHi, DividendLo, Divisor, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division of 128-bit unsigned integer by 64-bit unsigned integer
        ! and return quotient and remainder as 64-bit unsigned integers
        ! This routine is only applicable for cases where the divisor is greater
        !   than the upper halft of the dividend (i.e. Divisor .UGT. DividendHi).
        ! Note: This routine is based on 'divlu' of Hacker's delight and its derivative
        !       (division routine in Fast 128-bit math library for Java)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: DividendHi, DividendLo
        tUInt64, INTENT(IN)   :: Divisor
        tUInt64, INTENT(OUT)  :: Quotient
        tUInt64, INTENT(OUT)  :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER    :: Mask = ToInt64(Z'00000000FFFFFFFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32       :: Shift
        tUInt64       :: NHi, NLo         ! N -> numerator
        tUInt64       :: LoHi, LoLo
        tUInt64       :: RHat, UHat
        tUInt64       :: Denom, DHi, DLo  ! D -> denominator
        tUInt64       :: QHi, QLo

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
                  (IEOR(QHi * DLo, MinInt64) > IEOR(IOR(SHIFTL(RHat, 32), LoHi), MinInt64)))
            QHi = QHi - 1_kInt64
            RHat = RHat + DHi
            IF (SHIFTR(RHat, 32) /= 0_kInt64) EXIT
        END DO

        UHat = IOR(SHIFTL(NHi, 32), LoHi) - QHi * Denom

        ! Compute NLo quotient digit.
        CALL UDivMod(UHat, DHi, QLo, RHat)

        DO WHILE ((SHIFTR(QLo, 32) /= 0_kInt64).OR.   &
                  (IEOR(QLo * DLo, MinInt64) > IEOR(IOR(SHIFTL(RHat, 32), LoLo), MinInt64)))
            QLo = QLo - 1
            RHat = RHat + DHi
            IF (SHIFTR(RHat, 32) /= 0_kInt64) EXIT
        END DO

        Quotient  = IOR(SHIFTL(QHi, 32), QLo)
        Remainder = SHIFTR(IOR(SHIFTL(UHat, 32), LoLo) - QLo * Denom, Shift)

        RETURN

    END SUBROUTINE DivideU128ByU64

    !******************************************************************************

END SUBROUTINE U128_DivMod_Go

!******************************************************************************

MODULE SUBROUTINE U128_DivMod_Xp(Dividend, Divisor, Algo, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two UInt128 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: Dividend
    tUInt128, INTENT(IN)   :: Divisor
    tSInt32,  INTENT(IN)   :: Algo
    tUInt128, INTENT(OUT)  :: Quotient
    tUInt128, INTENT(OUT)  :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        CALL U128_DivMod_Java(Dividend, Divisor, Quotient, Remainder)
    CASE (2)
        CALL U128_DivMod_Intx(Dividend, Divisor, Quotient, Remainder)
    CASE (3)
        CALL U128_DivMod_Absl(Dividend, Divisor, Quotient, Remainder)
    CASE (4)
        CALL U128_DivMod_Go(Dividend, Divisor, Quotient, Remainder)
    CASE (5)
        CALL U128_DivMod_Flang(Dividend, Divisor, Quotient, Remainder)
    CASE (6)
        ! mixed version of Intx and Java
        ! IF ((Dividend%High < 0_kInt64).AND.(Divisor%High < 0_kInt64)) THEN
        IF (Dividend%High < 0_kInt64) THEN
            CALL U128_DivMod_Intx(Dividend, Divisor, Quotient, Remainder)
        ELSE
            CALL U128_DivMod_Java(Dividend, Divisor, Quotient, Remainder)
        END IF
    END SELECT

    RETURN

END SUBROUTINE U128_DivMod_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                     STRING CONVERSIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION U128_ToDecString_Huldra(U128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a decimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: U128
    tCharAlloc             :: Str


!** SUBROUTINE PARAMETER DECLARATIONS:
    tChar, PARAMETER :: NumStr(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32         :: BufLen, Top, I, J
    tCharLen(41)    :: Buffer
    tUInt128        :: Copy
    tUInt64         :: Tmp
    tUInt64         :: Indx

!** FLOW

    IF (U128 == ZeroU128) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = 41
    FORALL (I=1:BufLen) Buffer(I:I) = '0'
    Top  = BufLen
    Copy = U128
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
    Str = Buffer(Top+1:BufLen)

    RETURN

    CONTAINS

    FUNCTION ToStringDivide(U128) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the number by 10**13 and return the remainder

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(INOUT) :: U128
        tUInt64                 :: Remainder

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32,  PARAMETER :: Pow2  = SHIFTL(1, 13)
        tUInt64,  PARAMETER :: Pow5  = 1220703125_kInt64
        tUInt64,  PARAMETER :: Pow10 = Pow2*Pow5
        tLogical, PARAMETER :: Positive = FalseVal
        tLogical, PARAMETER :: AsUnsigned = TrueVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128   :: Rem, Quot, Numer, Pow10_128
        tUInt64    :: Q, R, Mod2

    !** FLOW

        CALL UDivMod(U128%High, Pow5, Q, R)
        U128%High = SHIFTR(Q, 13)

        Numer%High = R
        Numer%Low  = U128%Low
        Mod2 = IAND(U128%Low, Pow2 - 1_kInt64)

        CALL UDivMod(Numer, UInt128(Pow5, AsUnsigned), Quot, Rem)
        U128%Low = IOR(SHIFTL(Q, 51), SHIFTR(Quot%Low, 13))

        ! Applies the Chinese Rem Theorem.
        ! -67*5^13 + 9983778*2^13 = 1
        Pow10_128 = UInt128(0_kInt64, Pow10)
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
        tUInt128, INTENT(IN)   :: Dividend
        tUInt128, INTENT(IN)   :: Divisor
        tUInt128               :: Remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128   :: PosN, PosD
        tUInt128   :: PosR, PosQ

    !** FLOW

        ! get unsigned absolute of SInt128
        IF (Dividend%High < 0_kInt64) THEN
            PosN%High = NOT(Dividend%High)
            IF (Dividend%Low == 0_kInt64) PosN%High = PosN%High + 1_kInt64
            PosN%Low = NOT(Dividend%Low) + 1_kInt64
        ELSE
            PosN%High = Dividend%High
            PosN%Low  = Dividend%Low
        END IF
        IF (Divisor%High < 0_kInt64) THEN
            PosD%High = NOT(Divisor%High)
            IF (Divisor%Low == 0_kInt64) PosD%High = PosD%High + 1_kInt64
            PosD%Low = NOT(Divisor%Low) + 1_kInt64
        ELSE
            PosD%High = Divisor%High
            PosD%Low  = Divisor%Low
        END IF

        ! perform unsigned division
!        CALL UDivMod(PosN, PosD, PosQ, PosR)
        IF (PosN%High < 0_kInt64) THEN
            CALL U128_DivMod_Intx(PosN, PosD, PosQ, PosR)
        ELSE
            CALL U128_DivMod_Java(PosN, PosD, PosQ, PosR)
        END IF

        IF (Dividend%High < 0_kInt64) THEN
            ! Remainder = -PosR
            Remainder%High = NOT(PosR%High)
            IF (PosR%Low == 0_kInt64) Remainder%High = Remainder%High + 1_kInt64
            Remainder%Low = NOT(PosR%Low) + 1_kInt64
        ELSE
            ! Remainder = PosR
            Remainder%High = PosR%High
            Remainder%Low  = PosR%Low
        END IF

        RETURN

    END FUNCTION SMOD

    !******************************************************************************

END FUNCTION U128_ToDecString_Huldra

!******************************************************************************

FUNCTION U128_ToDecString_DivFast(U128) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a decimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tCharAlloc              :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32         :: BufLen, Top, I, J
    tCharLen(41)    :: Buffer
    tUInt128        :: Copy
    tSInt64         :: Tmp

!** FLOW

    IF (U128 == ZeroU128) THEN
        Str = '0'
        RETURN
    END IF

    BufLen = 41
    FORALL (I=1:BufLen) Buffer(I:I) = '0'
    Top  = BufLen
    Copy = U128
    DO
        J = Top
        Tmp = DivModBy10Pow18(Copy)
        CALL Write_1_To_18_Digits(Tmp, Buffer, Top)
        IF ((Copy%High == 0).AND.(Copy%Low == 0)) THEN
            EXIT
        ELSE
            Top = J - 18
        END IF
    END DO
    Str = Buffer(Top+1:BufLen)

    RETURN

    CONTAINS

    SUBROUTINE Write_1_To_18_Digits(Number, cStr, Indx)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an integer number to character string

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32,   INTENT(INOUT)    :: Indx     ! current index to the character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER  :: MaxLen = 19
        tSInt64, PARAMETER  :: Div1E8 = 100000000_kInt64
        ! multiplier and shift for 19 digits and divisor of 1.0E8
        tSInt64, PARAMETER  :: M90 = ToInt64(Z'ABCC77118461CEFD')
        tSInt32, PARAMETER  :: S90 = 90 - 64
        ! multiplier for 11 digits and divisor of 1.0E8
        tSInt64, PARAMETER  :: M64 = ToInt64(Z'0000002AF31DC462')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(MaxLen)    :: wStr
        tSInt64             :: PosNum
        tSInt64             :: NxtNum, RemNum
        tSInt32             :: Start

    !** FLOW

        ! start the conversion
        IF (Number >= 1000000000_kInt64) THEN
            ! compute NxtNum = Number/100000000
            NxtNum = SHIFTR(UMul128_Upper64(Number, M90), S90)
            ! compute RemNum = MOD(Number, 100000000)
            RemNum = Number - NxtNum*Div1E8
            ! convert the remainder to a working string
            CALL Write_8_Digits(ToInt32(RemNum), wStr(12:19))

            PosNum = NxtNum
            IF (PosNum > Div1E8) THEN
                ! compute NxtNum = PosNum/100000000
                NxtNum = UMul128_Upper64(PosNum, M64)
                ! compute RemNum = MOD(PosNum, 100000000)
                RemNum = PosNum - NxtNum*Div1E8
                ! convert the remainder to a working string
                CALL Write_8_Digits(ToInt32(RemNum), wStr(4:11))

                ! convert the rest
                IF (NxtNum < 10) THEN
                    wStr(3:3) = Char2Digits(NxtNum)(2:2)
                    Start = 3
                ELSEIF (NxtNum < 100) THEN
                    wStr(2:3) = Char2Digits(NxtNum)
                    Start = 2
                ELSE
                    wStr(1:3) = Char4Digits(NxtNum)(2:4)
                    Start = 1
                END IF
            ELSE
                ! convert the rest
                Start = 3 + Write_1_to_8_Digits(ToInt32(PosNum), wStr(4:11))
            END IF
            ! transfer to output string
            DO I = MaxLen, Start, -1
                cStr(Indx:Indx) = wStr(I:I)
                Indx = Indx - 1
            END DO
        ELSE
            CALL Write_1_To_9_Digits(ToInt32(Number), cStr, Indx)
        END IF

        RETURN

    END SUBROUTINE Write_1_To_18_Digits

    !******************************************************************************

    SUBROUTINE Write_1_To_9_Digits(Number, cStr, Indx)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_1_To_9_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an integer number to character string

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32,   INTENT(INOUT)    :: Indx     ! current index to the character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32, PARAMETER :: MaxLen = 10
        tSInt32, PARAMETER :: ShiftPos = 45
        tSInt64, PARAMETER :: Multiplier = ToInt64(Z'00000000D1B71759')
        tSInt32, PARAMETER :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(MaxLen)    :: wStr     ! working string
        tSInt32             :: PosNum   ! positive number (working number)
        tSInt32             :: NxtNum   ! next round of positive number
        tSInt32             :: RemNum   ! remainder number
        tSInt32             :: Start, Finish

    !** FLOW

        ! start the conversion
        IF (Number >= 10000) THEN
            ! compute the next round of working number
            NxtNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))   ! NxtNum = Number/10000
            ! compute the remainder
            RemNum = Number - NxtNum*Divisor                        ! RemNum = MOD(Number, 10000)
            ! convert the remainder to a working string
            wStr(7:10) = Char4Digits(RemNum)
            Finish = 10
            PosNum = NxtNum
            IF (PosNum < 10000) THEN
                IF (PosNum < 100) THEN
                    wStr(5:6) = Char2Digits(PosNum)
                    Start  = 5
                    IF (wStr(Start:Start) == '0') Start = 6
                ELSE
                    wStr(3:6) = Char4Digits(PosNum)
                    Start  = 3
                    IF (wStr(Start:Start) == '0') Start = 4
                END IF
            ELSE
                ! compute the next round of working number
                NxtNum = ToInt32(SHIFTR(PosNum*Multiplier, ShiftPos))   ! NxtNum = PosNum/10000
                ! compute the remainder
                RemNum = PosNum - NxtNum*Divisor                        ! RemNum = MOD(PosNum, 10000)
                ! convert the remainder to a working string
                wStr(3:6) = Char4Digits(RemNum)
                IF (NxtNum > 0) THEN
                    IF (NxtNum < 10) THEN
                        wStr(2:2) = Char2Digits(NxtNum)(2:2)
                        Start = 2
                    ELSE
                        wStr(1:2) = Char2Digits(NxtNum)
                        Start = 1
                    END IF
                ELSE
                    Start = 3
                END IF
            END IF
        ELSE
            Start  = 1
            IF (Number < 100) THEN
                wStr(1:2) = Char2Digits(Number)
                Finish = 2
                IF (wStr(Start:Start) == '0') Start = 2
            ELSE
                wStr(1:4) = Char4Digits(Number)
                Finish = 4
                IF (wStr(Start:Start) == '0') Start = 2
            END IF
        END IF

        ! transfer to output string
        DO I = Finish, Start, -1
            cStr(Indx:Indx) = wStr(I:I)
            Indx = Indx - 1
        END DO

        RETURN

    END SUBROUTINE Write_1_To_9_Digits

    !******************************************************************************

    SUBROUTINE Write_8_Digits(Number, cStr)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        tSInt32, PARAMETER  :: ShiftPos = 45
        tSInt64, PARAMETER  :: Multiplier = ToInt64(Z'00000000D1B71759')
        tSInt32, PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: NxtNum, RemNum

    !** FLOW

        ! compute NxtNum = PosNum/10000
        NxtNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))
        ! compute RemNum = MOD(PosNum, 10000)
        RemNum = Number - NxtNum*Divisor
        ! convert the remainder to a working string
        cStr(5:8) = Char4Digits(RemNum)
        ! convert the rest
        cStr(1:4) = Char4Digits(NxtNum)

        RETURN

    END SUBROUTINE Write_8_Digits

    !**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

!DIR$ ATTRIBUTES FORCEINLINE :: Write_1_to_8_Digits

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: Start

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! multiplier and shift for 4-byte integer and divisor of 10000
        tSInt32, PARAMETER  :: ShiftPos = 45
        tSInt64, PARAMETER  :: Multiplier = ToInt64(Z'00000000D1B71759')
        tSInt32, PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: NxtNum, RemNum

    !** FLOW

        IF (Number < 10000) THEN
            IF (Number < 100) THEN
                cStr(7:8) = Char2Digits(Number)
                Start = 7
                IF (cStr(Start:Start) == '0') Start = 8
            ELSE
                cStr(5:8) = Char4Digits(Number)
                Start = 5
                IF (cStr(Start:Start) == '0') Start = 6
            END IF
        ELSE
            ! compute NxtNum = Number/10000
            NxtNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))
            ! compute RemNum = MOD(Number, 10000)
            RemNum = Number - NxtNum*Divisor
            ! convert the remainder to a working string
            cStr(5:8) = Char4Digits(RemNum)
            IF (NxtNum < 100) THEN
                cStr(3:4) = Char2Digits(NxtNum)
                Start = 3
                IF (cStr(Start:Start) == '0') Start = 4
            ELSE
                cStr(1:4) = Char4Digits(NxtNum)
                Start  = 1
                IF (cStr(Start:Start) == '0') Start = 2
            END IF
        END IF

        RETURN

    END FUNCTION Write_1_to_8_Digits

    !**************************************************************************

    FUNCTION DivModBy10Pow18(DivQuot) RESULT(Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division DivQuot / Divisor where the Divisor is equal to 10**18

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(INOUT) :: DivQuot      ! on entry, the dividend
                                                ! on exit, the quotient
        tUInt64                 :: Remainder    ! the remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32, PARAMETER  :: LSh   = 4                            ! = LEADZ(Divisor)
        tUInt64, PARAMETER  :: Denom = ToInt64(Z'DE0B6B3A76400000') ! = SHIFTL(Divisor, LSh)
        tUInt64, PARAMETER  :: V     = ToInt64(Z'2725DD1D243ABA0E') ! = Reciprocal_2By1(Denom)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: RSh
        tUInt64         :: NumerHi, NumerLo, DenomHi, DenomLo
        tUInt64         :: NumerEx, RshMask
        tUInt64         :: R1, R2, LHS, RHS

    !** FLOW

        IF (DivQuot%High == 0_kInt64) THEN
            IF ((DivQuot%Low > 0_kInt64).AND.(DivQuot%Low < 1000000000000000000_kInt64)) THEN
                Remainder   = DivQuot%Low
                DivQuot%Low = 0_kInt64
                RETURN
            END IF
        END IF
        RSh = 64 - LSh
        RShMask = -1_kInt64
        NumerLo = SHIFTL(DivQuot%Low, LSh)
        NumerHi = IOR(SHIFTL(DivQuot%High, LSh), IAND(SHIFTR(DivQuot%Low, RSh), RShMask))
        NumerEx = IAND(SHIFTR(DivQuot%High, RSh), RShMask)

        CALL UDivRem_2By1(NumerEx, NumerHi, Denom, V, DivQuot%High, R1)
        CALL UDivRem_2By1(R1, NumerLo, Denom, V, DivQuot%Low, R2)
        Remainder = SHIFTR(R2, LSh)

        RETURN

    END FUNCTION DivModBy10Pow18

    !**************************************************************************

    SUBROUTINE UDivRem_2By1(UHi, ULo, D, V, Q, R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To performm 128-bit integer division by 64-bit integer

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

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
        IF (IEOR(NewLo, MinInt64) < IEOR(QLo, MinInt64)) THEN
            QHi = QHi + UHi + 1_kInt64
        ELSE
            QHi = QHi + UHi
        END IF
        QLo = NewLo

        QHi = QHi + 1_kInt64

        R = ULo - QHi*D

        IF (IEOR(R, MinInt64) > IEOR(QLo, MinInt64)) THEN
            QHi = QHi - 1_kInt64
            R = R + D
        END IF

        IF (IEOR(R, MinInt64) >= IEOR(D, MinInt64)) THEN
            QHi = QHi + 1_kInt64
            R = R - D
        END IF
        Q = QHi

        RETURN

    END SUBROUTINE UDivRem_2By1

    !**************************************************************************

END FUNCTION U128_ToDecString_DivFast

!******************************************************************************

MODULE FUNCTION U128_ToDecString_Xp(U128, Algo) RESULT(Str)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a decimal string

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: U128
    tSInt32,  INTENT(IN)   :: Algo
    tCharAlloc             :: Str

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        Str = U128_ToDecString_Huldra(U128)
    CASE (2:7)
        Str = U128_ToDecString_Absl(U128, Algo-1)
    CASE (8:13)
        Str = U128_ToDecString_AbslMod(U128, Algo-7)
    CASE (14)
        Str = U128_ToDecString_DivFast(U128)
    END SELECT

    RETURN
    CONTAINS

    FUNCTION U128_ToDecString_Absl(U128, DivAlgo) RESULT(Str)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a decimal string

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tSInt32,  INTENT(IN)   :: DivAlgo  ! between 1 - 5
        tCharAlloc             :: Str

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128   :: High, Low, Mid, HiMid

    !** FLOW

        ! Now piece together the uint128 representation from three chunks of the
        ! original value, each less than "div" and therefore representable as a
        ! uint64_t.
        CALL UDivModXp(U128,  DivBase10, DivAlgo, HiMid, Low)
        CALL UDivModXp(HiMid, DivBase10, DivAlgo, High,  Mid)
        IF (High%Low /= 0_kInt64) THEN
            BLOCK
                tCharAlloc      :: LowDec, MidDec
                tCharLen(19)    :: LowStr, MidStr
                LowStr = '0000000000000000000'
                LowDec = ToDecStrUnsigned(Low%Low)
                IF (LEN(LowDec) > 0) LowStr(20-LEN(LowDec):19) = LowDec
                MidStr = '0000000000000000000'
                MidDec = ToDecStrUnsigned(Mid%Low)
                IF (LEN(MidDec) > 0) MidStr(20-LEN(MidDec):19) = MidDec
                Str = ToDecStrUnsigned(High%Low) // MidStr // LowStr
            END BLOCK
        ELSEIF (Mid%Low /= 0_kInt64) THEN
            BLOCK
                tCharAlloc      :: LowDec
                tCharLen(19)    :: LowStr
                LowStr = '0000000000000000000'
                LowDec = ToDecStrUnsigned(Low%Low)
                IF (LEN(LowDec) > 0) LowStr(20-LEN(LowDec):19) = LowDec
                Str = ToDecStrUnsigned(Mid%Low) // LowStr
            END BLOCK
        ELSE
            Str = ToDecStrUnsigned(Low%Low)
        END IF

        RETURN

    END FUNCTION U128_ToDecString_Absl

    !**************************************************************************

    FUNCTION U128_ToDecString_AbslMod(U128, DivAlgo) RESULT(Str)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a decimal string

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tSInt32,  INTENT(IN)   :: DivAlgo  ! between 1 - 5
        tCharAlloc             :: Str

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tChar,   PARAMETER    :: NumStr(1:9)  = ['1', '2', '3', '4', '5', '6', '7', '8', '9']
        tUInt64, PARAMETER    :: DivBase10_UL = IEOR(ToInt64(Z'8AC7230489E80000'), MinInt64)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt128   :: High, Low, Mid, HiMid

    !** FLOW

        ! Now piece together the uint128 representation from three chunks of the
        ! original value, each less than "div" and therefore representable as a
        ! uint64_t.
        CALL UDivModXp(U128,  DivBase10, DivAlgo, HiMid, Low)
        IF ((HiMid%High == 0_kInt64).AND.(IEOR(HiMid%Low, MinInt64) < DivBase10_UL)) THEN
            IF (HiMid%Low /= 0_kInt64) THEN
                BLOCK
                    tCharAlloc      :: LowDec, MidDec
                    tUInt32        :: Start
                    tCharLen(38)    :: OutStr
                    OutStr = '00000000000000000000000000000000000000'
                    LowDec = ToDecStrUnsigned(Low%Low)
                    MidDec = ToDecStrUnsigned(HiMid%Low)
                    Start = 20-LEN(MidDec)
                    OutStr(39-LEN(LowDec):38) = LowDec
                    OutStr(Start:19) = MidDec
                    Str = OutStr(Start:38)
                END BLOCK
            ELSE
                Str = ToDecStrUnsigned(Low%Low)
            END IF
        ELSE
            CALL UDivModXp(HiMid, DivBase10, DivAlgo, High,  Mid)
            IF (High%Low /= 0_kInt64) THEN
                BLOCK
                    tCharAlloc      :: LowDec, MidDec
                    tCharLen(39)    :: OutStr
                    OutStr = '000000000000000000000000000000000000000'
                    LowDec = ToDecStrUnsigned(Low%Low)
                    MidDec = ToDecStrUnsigned(Mid%Low)
                    OutStr(40-LEN(LowDec):39) = LowDec
                    OutStr(21-LEN(MidDec):20) = MidDec
                    OutStr(1:1) = NumStr(High%Low)
                    Str = OutStr(1:39)
                END BLOCK
            ELSEIF (Mid%Low /= 0_kInt64) THEN
                BLOCK
                    tCharAlloc      :: LowDec, MidDec
                    tUInt32        :: Start
                    tCharLen(38)    :: OutStr
                    OutStr = '00000000000000000000000000000000000000'
                    LowDec = ToDecStrUnsigned(Low%Low)
                    MidDec = ToDecStrUnsigned(Mid%Low)
                    Start = 20-LEN(MidDec)
                    OutStr(39-LEN(LowDec):38) = LowDec
                    OutStr(Start:19) = MidDec
                    Str = OutStr(Start:38)
                END BLOCK
            ELSE
                Str = ToDecStrUnsigned(Low%Low)
            END IF
        END IF

        RETURN

    END FUNCTION U128_ToDecString_AbslMod

    !******************************************************************************

END FUNCTION U128_ToDecString_Xp

!******************************************************************************

MODULE FUNCTION U128_FromDecString_Xp(cStr, Algo, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an unsigned 128-bit integer value

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tSInt32,              INTENT(IN)    :: Algo
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tUInt128                            :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        Number = Parse_U128_V1(cStr, ErrFlag, ErrMsg)
    CASE (2)
        Number = Parse_U128_V2(cStr, ErrFlag, ErrMsg)
    CASE (3)
        Number = Parse_U128_V3(cStr, ErrFlag, ErrMsg)
    CASE (4)
        Number = Parse_U128_V4(cStr, ErrFlag, ErrMsg)
    CASE (5)
        Number = Parse_U128_V5(cStr, ErrFlag, ErrMsg)
    END SELECT

    RETURN

    CONTAINS

    FUNCTION Parse_U128_V1(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to an unsigned 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tUInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,  PARAMETER    :: A0           = IACHAR('0')
        tSInt32,  PARAMETER    :: A4           = IACHAR('4')
        tSInt32,  PARAMETER    :: A9           = IACHAR('9')
        tSInt32,  PARAMETER    :: MaxDigitI32  = 10
        tSInt32,  PARAMETER    :: MaxDigitI64  = 19
        tSInt32,  PARAMETER    :: MaxDigitU128 = 39
        tUInt128, PARAMETER    :: U128Base     = TenU128
        tCharParam             :: MaxStr       = '340282366920938463463374607431768211455'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart
        tCharLen(1), POINTER    :: CurChr
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
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for sign
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a minus sign is not allowed for an unsigned integer.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = ZeroU128
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            ! if only zero digits encountered, return
            IF (Indx > StrLen) RETURN
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I32Val)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I64Val)
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
                Number = Number*U128Base + UInt128(0_kInt64, ToInt64(IACHAR(CurChr)-A0))
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinU128
                    RETURN
                END IF
            END DO
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitU128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitU128) THEN
            ! value might be in the applicable range so check overflow
            CurStr = cStr(IStart:StrLen)
            Overflow = FalseVal
            DO Indx = 1, MaxDigitU128
                CurChr => CurStr(Indx:Indx)
                IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                    EXIT
                ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                    Overflow = TrueVal
                    EXIT
                END IF
            END DO
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is too large.'
            Number = MaxU128
        END IF

        RETURN

    END FUNCTION Parse_U128_V1

    !******************************************************************************

    FUNCTION Parse_U128_V2(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to an unsigned 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tUInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,      PARAMETER    :: A0           = IACHAR('0')
        tSInt32,      PARAMETER    :: A4           = IACHAR('4')
        tSInt32,      PARAMETER    :: A9           = IACHAR('9')
        tSInt32,      PARAMETER    :: MaxDigitI32  = 10
        tSInt32,      PARAMETER    :: MaxDigitI64  = 19
        tSInt32,      PARAMETER    :: MaxDigitU128 = 39
        tCharParam                 :: MaxStr       = '340282366920938463463374607431768211455'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart
        tCharLen(1), POINTER    :: CurChr
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
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for sign
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a minus sign is not allowed for an unsigned integer.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = ZeroU128
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            ! if only zero digits encountered, return
            IF (Indx > StrLen) RETURN
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I32Val)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I64Val)
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
                    Number = MinU128
                    RETURN
                END IF
            END DO
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitU128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitU128) THEN
            ! value might be in the applicable range so check overflow
            CurStr = cStr(IStart:StrLen)
            Overflow = FalseVal
            DO Indx = 1, MaxDigitU128
                CurChr => CurStr(Indx:Indx)
                IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                    EXIT
                ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                    Overflow = TrueVal
                    EXIT
                END IF
            END DO
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is too large.'
            Number = MaxU128
        END IF

        RETURN

    END FUNCTION Parse_U128_V2

    !******************************************************************************

    FUNCTION Parse_U128_V3(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to an unsigned 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tUInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,      PARAMETER    :: A0           = IACHAR('0')
        tSInt32,      PARAMETER    :: A4           = IACHAR('4')
        tSInt32,      PARAMETER    :: A9           = IACHAR('9')
        tSInt32,      PARAMETER    :: MaxDigitI32  = 10
        tSInt32,      PARAMETER    :: MaxDigitI64  = 19
        tSInt32,      PARAMETER    :: MaxDigitU128 = 39
        tCharParam                 :: MaxStr       = '340282366920938463463374607431768211455'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart
        tCharLen(1), POINTER    :: CurChr
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
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for sign
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a minus sign is not allowed for an unsigned integer.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = ZeroU128
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            ! if only zero digits encountered, return
            IF (Indx > StrLen) RETURN
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I32Val)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I64Val)
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
                CALL MulAddU32(Number, 10, (IACHAR(CurChr)-A0))
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinU128
                    RETURN
                END IF
            END DO
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitU128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitU128) THEN
            ! value might be in the applicable range so check overflow
            CurStr = cStr(IStart:StrLen)
            Overflow = FalseVal
            DO Indx = 1, MaxDigitU128
                CurChr => CurStr(Indx:Indx)
                IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                    EXIT
                ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                    Overflow = TrueVal
                    EXIT
                END IF
            END DO
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is too large.'
            Number = MaxU128
        END IF

        RETURN

    END FUNCTION Parse_U128_V3

    !******************************************************************************

    SUBROUTINE MulAddU32(U128, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply the given number by 'Mul' and then add 'Add' to it.
        ! (i.e. to set  U128 = U128*Mul + Add)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(INOUT)    :: U128
        tUInt32,  INTENT(IN)       :: Mul
        tUInt32,  INTENT(IN)       :: Add

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: X_Lo, Y_Lo, Y_Hi
        tUInt64       :: Lo_Lo, Cross, MulLo

    !** FLOW

        ! perform multiplication
        X_Lo = IAND(ToInt64(Mul), Mask32)
        Y_Lo = IAND(U128%Low, Mask32)
        Y_Hi = SHIFTR(U128%Low, 32)
        Lo_Lo = X_Lo*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
        MulLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
        U128%High = U128%High*Mul + SHIFTR(Cross, 32)

        ! perform addition
        U128%Low = MulLo + Add
        IF (IEOR(U128%Low, MinInt64) < IEOR(MulLo, MinInt64)) U128%High = U128%High + 1_kInt64

        RETURN

    END SUBROUTINE MulAddU32

    !******************************************************************************

    FUNCTION Parse_U128_V4(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to an unsigned 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tUInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,      PARAMETER    :: A0           = IACHAR('0')
        tSInt32,      PARAMETER    :: A4           = IACHAR('4')
        tSInt32,      PARAMETER    :: A9           = IACHAR('9')
        tSInt32,      PARAMETER    :: MaxDigitI32  = 10
        tSInt32,      PARAMETER    :: MaxDigitI64  = 19
        tSInt32,      PARAMETER    :: MaxDigitU128 = 39
        tCharParam                 :: MaxStr       = '340282366920938463463374607431768211455'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart, IndxP7
        tCharLen(1), POINTER    :: CurChr
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
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for sign
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a minus sign is not allowed for an unsigned integer.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = ZeroU128
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            ! if only zero digits encountered, return
            IF (Indx > StrLen) RETURN
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I32Val)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I64Val)
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
                    CALL MulAddU64(Number, 100000000_kInt64, Parse8Digits(wVal))
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinU128
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
                    CALL MulAddU32(Number, 10, (IACHAR(CurChr)-A0))
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT
                    CurChr => cStr(Indx:Indx)
                    IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                        Number = MinU128
                        RETURN
                    END IF
                END DO
            END IF
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitU128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitU128) THEN
            ! value might be in the applicable range so check overflow
            CurStr = cStr(IStart:StrLen)
            Overflow = FalseVal
            DO Indx = 1, MaxDigitU128
                CurChr => CurStr(Indx:Indx)
                IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                    EXIT
                ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                    Overflow = TrueVal
                    EXIT
                END IF
            END DO
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is too large.'
            Number = MaxU128
        END IF

        RETURN

    END FUNCTION Parse_U128_V4

    !******************************************************************************

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
        tLogical              :: Flag

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

    SUBROUTINE MulAddU64(U128, Mul, Add)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply the given number by 'Mul' and then add 'Add' to it.
        ! (i.e. to set  U128 = U128*Mul + Add)

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(INOUT)    :: U128
        tUInt64,  INTENT(IN)       :: Mul
        tUInt64,  INTENT(IN)       :: Add

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64       :: X_Lo, Y_Lo, Y_Hi
        tUInt64       :: Lo_Lo, Cross, MulLo

    !** FLOW

        ! perform multiplication
        X_Lo = IAND(Mul, Mask32)
        Y_Lo = IAND(U128%Low, Mask32)
        Y_Hi = SHIFTR(U128%Low, 32)
        Lo_Lo = X_Lo*Y_Lo
        Cross = SHIFTR(Lo_Lo, 32) + X_Lo*Y_Hi
        MulLo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, Mask32))
        U128%High = U128%High*Mul + SHIFTR(Cross, 32)

        ! perform addition
        U128%Low = MulLo + Add
        IF (IEOR(U128%Low, MinInt64) < IEOR(MulLo, MinInt64)) U128%High = U128%High + 1_kInt64

        RETURN

    END SUBROUTINE MulAddU64

    !******************************************************************************

    FUNCTION Parse_U128_V5(cStr, ErrFlag, ErrMsg) RESULT(Number)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a decimal string to an unsigned 128-bit integer value

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
        tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
        tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
        tUInt128                            :: Number   ! number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSInt32,      PARAMETER    :: A0           = IACHAR('0')
        tSInt32,      PARAMETER    :: A4           = IACHAR('4')
        tSInt32,      PARAMETER    :: A9           = IACHAR('9')
        tSInt32,      PARAMETER    :: MaxDigitI32  = 10
        tSInt32,      PARAMETER    :: MaxDigitI64  = 19
        tSInt32,      PARAMETER    :: MaxDigitU128 = 39
        tCharParam                 :: MaxStr       = '340282366920938463463374607431768211455'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32                 :: Indx, StrLen, DigitLen
        tSInt32                 :: NumDigit
        tSInt32                 :: IStart, IndxP7
        tCharLen(1), POINTER    :: CurChr
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
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for sign
        CurChr => cStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            IF (CurChr == '-') THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a minus sign is not allowed for an unsigned integer.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            Indx = Indx + 1
            IF (Indx > StrLen) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
            ! check whether the following character is a digit or not
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                ! current character is not a digit
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinU128
                RETURN
            END IF
        END IF

        ! check for leading zero(s)
        Number = ZeroU128
        IF (cStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (cStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            ! if only zero digits encountered, return
            IF (Indx > StrLen) RETURN
        END IF

        ! compute the length of digits
        DigitLen = StrLen - Indx + 1

        ! return quickly if possible
        IF (DigitLen < MaxDigitI32) THEN
            I32Val = I32_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I32Val)
            END IF
            IF (PRESENT(ErrMsg))  ErrMsg  = ErrorMsg
            IF (PRESENT(ErrFlag)) ErrFlag = ErrorFlag
            RETURN
        ELSEIF (DigitLen < MaxDigitI64) THEN
            I64Val = I64_FromChar(cStr(Indx:StrLen), ErrorFlag, ErrorMsg)
            IF (ErrorFlag) THEN
                Number = MinU128
            ELSE
                Number = UInt128(I64Val)
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
                    CALL Multiply(Number, 100000000)
                    CALL Add(Number, Parse8Digits(wVal))
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinU128
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
                    CALL Add(Number, IACHAR(CurChr)-A0)
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT
                    CurChr => cStr(Indx:Indx)
                    IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                        Number = MinU128
                        RETURN
                    END IF
                END DO
            END IF
            NumDigit = Indx - IStart
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinU128
            RETURN
        END IF

        ! now, we have a valid string so check if the value is in the applicable range
        IF (NumDigit < MaxDigitU128) THEN
            ! value is in the applicable range
            Overflow = FalseVal
        ELSEIF (NumDigit == MaxDigitU128) THEN
            ! value might be in the applicable range so check overflow
            CurStr = cStr(IStart:StrLen)
            Overflow = FalseVal
            DO Indx = 1, MaxDigitU128
                CurChr => CurStr(Indx:Indx)
                IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                    EXIT
                ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                    Overflow = TrueVal
                    EXIT
                END IF
            END DO
        ELSE
            ! value is out of the applicable range
            Overflow = TrueVal
        END IF
        IF (Overflow) THEN
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is too large.'
            Number = MaxU128
        END IF

        RETURN

    END FUNCTION Parse_U128_V5

    !******************************************************************************

END FUNCTION U128_FromDecString_Xp

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                       REAL CONVERSIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE FUNCTION R32_To_U128_Xp(R32, Algo) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 32-bit floating point number to an unsigned 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle,  INTENT(IN)    :: R32
    tSInt32,  INTENT(IN)    :: Algo
    tUInt128                :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSingle, PARAMETER  :: TwoPow64 = 2.0_kSingle**64
    tSingle, PARAMETER  :: I64Max   = REAL(MaxI64, KIND=kSingle)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        U128 = R32_To_U128_A1(R32)
    CASE (2)
        U128 = R32_To_U128_A2(R32)
    CASE (3)
        U128 = R32_To_U128_A3(R32)
    END SELECT

    RETURN
    CONTAINS

    FUNCTION R32_To_U128_A1(R32) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 32-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle, INTENT(IN) :: R32
        tUInt128            :: U128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER :: Mask = ToInt32(Z'000000FF')    ! 255
        tUInt32, PARAMETER :: C1   = SHIFTL(1, 23)             ! 2**23
        tUInt32, PARAMETER :: C2   = C1 - 1                    ! 2**23 - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt32         :: IBits
        tSingle         :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R32 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R32)) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 is NOT finite.')
            RETURN
        ELSEIF (R32 <= -1.0_kSingle) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 < U128Min.')
            RETURN
        ELSEIF (R32 >= 2.0_kSingle**128) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 > U128Max.')
            RETURN
        ELSEIF (R32 < 0.0_kSingle) THEN
            U128 = ZeroU128
            RETURN
        END IF

        ! transfer bits from real to integer
        RBits = R32
        ! determine exponent bits
        Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
        ! determine significand bits
        IBits = IOR(IAND(IBits, C2), C1)
        ! convert to UInt128
        U128 = UInt128(0_kInt64, ToInt64(IBits))
        ! add exponent bits
        U128 = ISHFT(U128, Exp)

        RETURN

    END FUNCTION R32_To_U128_A1

    !******************************************************************************

    FUNCTION R32_To_U128_A2(R32) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 32-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle, INTENT(IN) :: R32
        tUInt128            :: U128

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R32 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R32)) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 is NOT finite.')
            RETURN
        ELSEIF (R32 <= -1.0_kSingle) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 < U128Min.')
            RETURN
        ELSEIF (R32 >= 2.0_kSingle**128) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 > U128Max.')
            RETURN
        END IF

        IF (R32 >= TwoPow64) THEN
            U128%High = R32_To_U64(R32*(2.0_kSingle**(-64)))
            U128%Low  = R32_To_U64(R32 - U64_To_R32(U128%High)*TwoPow64)
        ELSE
            U128%High = 0_kInt64
            U128%Low  = R32_To_U64(R32)
        END IF

        RETURN

    END FUNCTION R32_To_U128_A2

    !******************************************************************************

    FUNCTION R32_To_U128_A3(R32) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 32-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle, INTENT(IN) :: R32
        tUInt128            :: U128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER :: Mask = ToInt32(Z'000000FF')    ! 255
        tUInt32, PARAMETER :: C1   = SHIFTL(1, 23)             ! 2**23
        tUInt32, PARAMETER :: C2   = C1 - 1                    ! 2**23 - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt32         :: IBits
        tSingle         :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R32 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R32)) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 is NOT finite.')
            RETURN
        ELSEIF (R32 <= -1.0_kSingle) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 < U128Min.')
            RETURN
        ELSEIF (R32 >= 2.0_kSingle**128) THEN
            CALL Handle_ErrLevel('R32_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R32 > U128Max.')
            RETURN
        ELSEIF (R32 < 0.0_kSingle) THEN
            U128 = ZeroU128
            RETURN
        END IF

        ! transfer bits from real to integer
        RBits = R32
        ! determine exponent bits
        Exp = IAND(SHIFTR(IBits, 23), Mask) - 150   ! 150 = 127 + 23
        ! determine significand bits
        IBits = IOR(IAND(IBits, C2), C1)
        ! convert to UInt128
        ! U128 = UInt128(0_kInt64, ToInt64(IBits))
        ! add exponent bits
        ! U128 = ISHFT(U128, Exp)
        IF (Exp < 0) THEN
            ! U128 = SHIFTR(U128, -Exp)
            Exp = -Exp
            IF (Exp >= 64) THEN
                U128 = UInt128(0_kInt64, 0_kInt64)
            ELSE
                U128 = UInt128(0_kInt64, SHIFTR(ToInt64(IBits), Exp))
            END IF
        ELSE
            ! U128 = SHIFTL(U128, Exp)
            IF (Exp >= 128) THEN
                U128 = UInt128(0_kInt64, 0_kInt64)
            ELSEIF (Exp >= 64) THEN
                U128 = UInt128(SHIFTL(ToInt64(IBits), Exp - 64), 0_kInt64)
            ELSE
                U128 = UInt128(SHIFTR(ToInt64(IBits), 64 - Exp), SHIFTL(ToInt64(IBits), Exp))
            END IF
        END IF

        RETURN

    END FUNCTION R32_To_U128_A3

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

    FUNCTION R32_To_U64(SingleVal) RESULT(LongVal)

    !DIR$ ATTRIBUTES FORCEINLINE :: R32_To_U64

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 32-bit floating point number to an unsigned 64-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSingle, INTENT(IN) :: SingleVal    ! floating point number
        tUInt64             :: LongVal      ! integer number treated as unsigned one

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (SingleVal >= I64Max) THEN
            LongVal  = -ToInt64(TwoPow64 - AINT(SingleVal))
        ELSE
            LongVal  = ToInt64(SingleVal)
        END IF

        RETURN

    END FUNCTION R32_To_U64

    !******************************************************************************

END FUNCTION R32_To_U128_Xp

!******************************************************************************

MODULE FUNCTION R64_To_U128_Xp(R64, Algo) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 64-bit floating point number to an unsigned 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble, INTENT(IN)     :: R64
    tSInt32, INTENT(IN)     :: Algo
    tUInt128                :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: TwoPow64 = 2.0_kDouble**64
    tDouble, PARAMETER  :: I64Max   = REAL(MaxI64, KIND=kDouble)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        U128 = R64_To_U128_A1(R64)
    CASE (2)
        U128 = R64_To_U128_A2(R64)
    CASE (3)
        U128 = R64_To_U128_A3(R64)
    END SELECT

    RETURN
    CONTAINS

    FUNCTION R64_To_U128_A1(R64) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 64-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: R64
        tUInt128            :: U128

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
        ! undefined behavior if R64 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R64)) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 is NOT finite.')
            RETURN
        ELSEIF (R64 <= -1.0_kDouble) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 < U128Min.')
            RETURN
        ELSEIF (R64 >= 2.0_kDouble**128) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 > U128Max.')
            RETURN
        ELSEIF (R64 < 0.0_kDouble) THEN
            U128 = ZeroU128
            RETURN
        END IF

        ! transfer bits from real to integer
        RBits = R64
        ! determine exponent bits
        Exp = ToInt32(IAND(SHIFTR(IBits, 52), Mask)) - 1075   ! 1075 = 1023 + 52
        ! determine significand bits
        IBits = IOR(IAND(IBits, C2), C1)
        ! convert to UInt128
        U128 = UInt128(0_kInt64, IBits)
        ! add exponent bits
        U128 = ISHFT(U128, Exp)

        RETURN

    END FUNCTION R64_To_U128_A1

    !******************************************************************************

    FUNCTION R64_To_U128_A2(R64) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 64-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: R64
        tUInt128            :: U128

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R64 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R64)) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 is NOT finite.')
            RETURN
        ELSEIF (R64 <= -1.0_kDouble) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 < U128Min.')
            RETURN
        ELSEIF (R64 >= 2.0_kDouble**128) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 > U128Max.')
            RETURN
        END IF

        IF (R64 >= TwoPow64) THEN
            U128%High = R64_To_U64(R64*(2.0_kDouble**(-64)))
            U128%Low  = R64_To_U64(R64 - U64_To_R64(U128%High)*TwoPow64)
        ELSE
            U128%High = 0_kInt64
            U128%Low  = R64_To_U64(R64)
        END IF

        RETURN

    END FUNCTION R64_To_U128_A2

    !******************************************************************************

    FUNCTION R64_To_U128_A3(R64) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 64-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: R64
        tUInt128            :: U128

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
        ! undefined behavior if R64 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R64)) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 is NOT finite.')
            RETURN
        ELSEIF (R64 <= -1.0_kDouble) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 < U128Min.')
            RETURN
        ELSEIF (R64 >= 2.0_kDouble**128) THEN
            CALL Handle_ErrLevel('R64_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R64 > U128Max.')
            RETURN
        ELSEIF (R64 < 0.0_kDouble) THEN
            U128 = ZeroU128
            RETURN
        END IF

        ! transfer bits from real to integer
        RBits = R64
        ! determine exponent bits
        Exp = ToInt32(IAND(SHIFTR(IBits, 52), Mask)) - 1075   ! 1075 = 1023 + 52
        ! determine significand bits
        IBits = IOR(IAND(IBits, C2), C1)
        ! convert to UInt128
        ! U128 = UInt128(0_kInt64, IBits)
        ! add exponent bits
        ! U128 = ISHFT(U128, Exp)
        IF (Exp < 0) THEN
            ! U128 = SHIFTR(U128, -Exp)
            Exp = -Exp
            IF (Exp >= 64) THEN
                U128 = UInt128(0_kInt64, 0_kInt64)
            ELSE
                U128 = UInt128(0_kInt64, SHIFTR(IBits, Exp))
            END IF
        ELSE
            ! U128 = SHIFTL(U128, Exp)
            IF (Exp >= 128) THEN
                U128 = UInt128(0_kInt64, 0_kInt64)
            ELSEIF (Exp >= 64) THEN
                U128 = UInt128(SHIFTL(IBits, Exp - 64), 0_kInt64)
            ELSE
                U128 = UInt128(SHIFTR(IBits, 64 - Exp), SHIFTL(IBits, Exp))
            END IF
        END IF

        RETURN

    END FUNCTION R64_To_U128_A3

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

    FUNCTION R64_To_U64(DoubleVal) RESULT(LongVal)

    !DIR$ ATTRIBUTES FORCEINLINE :: R64_To_U64

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 64-bit floating point number to an unsigned 64-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tDouble, INTENT(IN) :: DoubleVal    ! floating point number
        tUInt64             :: LongVal      ! integer number treated as unsigned one

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (DoubleVal >= I64Max) THEN
            LongVal  = -ToInt64(TwoPow64 - AINT(DoubleVal))
        ELSE
            LongVal  = ToInt64(DoubleVal)
        END IF

        RETURN

    END FUNCTION R64_To_U64

    !******************************************************************************

END FUNCTION R64_To_U128_Xp

!******************************************************************************

MODULE FUNCTION R128_To_U128_Xp(R128, Algo) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tQuad,    INTENT(IN)    :: R128
    tSInt32,  INTENT(IN)    :: Algo
    tUInt128                :: U128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tQuad, PARAMETER  :: TwoPow64 = 2.0_kQuad**64
    tQuad, PARAMETER  :: I64Max   = REAL(MaxI64, KIND=kQuad)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        U128 = R128_To_U128_A1(R128)
    CASE (2)
        U128 = R128_To_U128_A2(R128)
    CASE (3)
        U128 = R128_To_U128_A3(R128)
    END SELECT

    RETURN
    CONTAINS

    FUNCTION R128_To_U128_A1(R128) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tQuad, INTENT(IN)   :: R128
        tUInt128            :: U128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER :: Mask  = ToInt32(Z'00007FFF')               ! 32767
        tUInt64, PARAMETER :: C1(2) = [ 0_kInt64, 281474976710656_kInt64]    ! 2**112 = SHIFTL(1, 112)
        tUInt64, PARAMETER :: C2(2) = [-1_kInt64, 281474976710655_kInt64]    ! 2**112 -1 = SHIFTL(1, 112) - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt64         :: IBits(2)
        tQuad           :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R128 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R128)) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 is NOT finite.')
            RETURN
        ELSEIF (R128 <= -1.0_kQuad) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 < U128Min.')
            RETURN
        ELSEIF (R128 >= 2.0_kQuad**128) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 > U128Max.')
            RETURN
        ELSEIF (R128 < 0.0_kQuad) THEN
            U128 = ZeroU128
            RETURN
        END IF

        ! transfer bits from real to integer
        RBits = R128
        ! determine exponent bits (48 = 112-64, 16495 = 16383 + 112)
        Exp = IAND(ToInt32(SHIFTR(IBits(2), 48)), Mask) - 16495
        ! determine significand bits and convert to UInt128
        U128 = UInt128(IOR(IAND(IBits(2), C2(2)), C1(2)), IOR(IAND(IBits(1), C2(1)), C1(1)))
        ! add exponent bits
        U128 = ISHFT(U128, Exp)

        RETURN

    END FUNCTION R128_To_U128_A1

    !******************************************************************************

    FUNCTION R128_To_U128_A2(R128) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tQuad, INTENT(IN)   :: R128
        tUInt128            :: U128

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R128 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R128)) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 is NOT finite.')
            RETURN
        ELSEIF (R128 <= -1.0_kQuad) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 < U128Min.')
            RETURN
        ELSEIF (R128 >= 2.0_kQuad**128) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 > U128Max.')
            RETURN
        END IF

        IF (R128 >= TwoPow64) THEN
            U128%High = R128_To_U64(R128*(2.0_kQuad**(-64)))
            U128%Low  = R128_To_U64(R128 - U64_To_R128(U128%High)*TwoPow64)
        ELSE
            U128%High = 0_kInt64
            U128%Low  = R128_To_U64(R128)
        END IF

        RETURN

    END FUNCTION R128_To_U128_A2

    !******************************************************************************

    FUNCTION R128_To_U128_A3(R128) RESULT(U128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 128-bit floating point number to an unsigned 128-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tQuad, INTENT(IN)   :: R128
        tUInt128            :: U128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt32, PARAMETER :: Mask  = ToInt32(Z'00007FFF')               ! 32767
        tUInt64, PARAMETER :: C1(2) = [ 0_kInt64, 281474976710656_kInt64]    ! 2**112 = SHIFTL(1, 112)
        tUInt64, PARAMETER :: C2(2) = [-1_kInt64, 281474976710655_kInt64]    ! 2**112 -1 = SHIFTL(1, 112) - 1

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: Exp
        tUInt64         :: ExpL
        tUInt64         :: IBits(2)
        tQuad           :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        ! rounding behavior is towards zero.
        ! undefined behavior if R128 is NaN or cannot fit into U128.
        IF (.NOT.IEEE_IS_FINITE (R128)) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 is NOT finite.')
            RETURN
        ELSEIF (R128 <= -1.0_kQuad) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 < U128Min.')
            RETURN
        ELSEIF (R128 >= 2.0_kQuad**128) THEN
            CALL Handle_ErrLevel('R128_To_U128', ModName, ErrSevere, &
                              'Undefined behavior: R128 > U128Max.')
            RETURN
        ELSEIF (R128 < 0.0_kQuad) THEN
            U128 = ZeroU128
            RETURN
        END IF

        ! transfer bits from real to integer
        RBits = R128
        ! determine exponent bits
        ExpL = SHIFTR(IBits(2), 48)                 ! 48 = 112-64
        Exp = IAND(ToInt32(ExpL), Mask) - 16495   ! 16495 = 16383 + 112
        ! determine significand bits and convert to UInt128
        U128%Low  = IOR(IAND(IBits(1), C2(1)), C1(1))
        U128%High = IOR(IAND(IBits(2), C2(2)), C1(2))
        ! add exponent bits
        ! U128 = ISHFT(U128, Exp)
        IF (Exp < 0) THEN
            U128 = SHIFTR(U128, -Exp)
        ELSE
            U128 = SHIFTL(U128, Exp)
        END IF

        RETURN

    END FUNCTION R128_To_U128_A3

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

    !******************************************************************************

    FUNCTION R128_To_U64(QuadVal) RESULT(LongVal)

    !DIR$ ATTRIBUTES FORCEINLINE :: R128_To_U64

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert a 128-bit floating point number to an unsigned 64-bit integer number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tQuad, INTENT(IN)   :: QuadVal  ! floating point number
        tUInt64             :: LongVal  ! integer number treated as unsigned one

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (QuadVal >= I64Max) THEN
            LongVal  = -ToInt64(TwoPow64 - AINT(QuadVal))
        ELSE
            LongVal  = ToInt64(QuadVal)
        END IF

        RETURN

    END FUNCTION R128_To_U64

    !******************************************************************************

END FUNCTION R128_To_U128_Xp

!******************************************************************************

MODULE FUNCTION R32_From_U128_Xp(U128, Algo) RESULT(R32)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a 32-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: U128
    tSInt32,  INTENT(IN)   :: Algo
    tSingle                :: R32

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSingle, PARAMETER  :: TwoPow64 = 2.0_kSingle**64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        R32 = R32_From_U128_A1(U128)
    CASE (2)
        R32 = R32_From_U128_A2(U128)
    END SELECT

    RETURN

    CONTAINS

    FUNCTION R32_From_U128_A1(U128) RESULT(R32)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 32-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tSingle                :: R32

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tSingle, PARAMETER :: TwoPow64 = 2.0_kSingle**64
        tUInt32, PARAMETER :: TwoPow23 = SHIFTL(1, 23)
        tUInt32, PARAMETER :: Mask     = ToInt32(Z'000000FF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: S, Exp
        tUInt32     :: IBits
        tSingle     :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        IF (U128%High == 0_kInt64) THEN
            ! convert directly and return quickly
            R32 = U64_To_R32(U128%Low)
            RETURN
        END IF

        S = LEADZ(U128%High)
        ! Mask out the 24 MSBits
        ! Also, the leading bit is implicit so cancel it out to get the significand
        IF (S <= 40) THEN
            IBits = IEOR(ToInt32(SHIFTR(U128%High, 40-S)), TwoPow23)
        ELSE
            ! S-40 == additional bits we need
            IBits = IEOR(ToInt32(IOR(SHIFTL(U128%High, S-40), SHIFTR(U128%Low, 104-S))), TwoPow23)
        END IF
        ! get the binary exponent
        Exp = IAND(254-S, Mask)         ! 254 = 64 + 64 + 127 - 1

        ! Add the exponent
        IBits = IOR(IBits, SHIFTL(Exp, 23))

        ! transfer output (RBits mapped to IBits using equivalence)
        R32 = RBits

        RETURN

    END FUNCTION R32_From_U128_A1

    !******************************************************************************

    FUNCTION R32_From_U128_A2(U128) RESULT(R32)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 32-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tSingle                :: R32

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        R32 = U64_To_R32(U128%Low) + U64_To_R32(U128%High)*TwoPow64

        RETURN

    END FUNCTION R32_From_U128_A2

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

END FUNCTION R32_From_U128_Xp

!******************************************************************************

MODULE FUNCTION R64_From_U128_Xp(U128, Algo) RESULT(R64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a 64-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: U128
    tSInt32,  INTENT(IN)   :: Algo
    tDouble                :: R64

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER  :: TwoPow64 = 2.0_kDouble**64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        R64 = R64_From_U128_A1(U128)
    CASE (2)
        R64 = R64_From_U128_A2(U128)
    END SELECT

    RETURN

    CONTAINS

    FUNCTION R64_From_U128_A1(U128) RESULT(R64)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 64-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tDouble                :: R64

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER :: TwoPow52 = SHIFTL(1_kInt64, 52)
        tUInt32, PARAMETER :: Mask     = ToInt32(Z'000007FF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: S
        tSInt64     :: Exp
        tUInt64     :: IBits
        tDouble     :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        IF (U128%High == 0_kInt64) THEN
            R64 = U64_To_R64(U128%Low)
            RETURN
        END IF

        S = LEADZ(U128%High)
        ! Mask out the 53 MSBits
        ! Also, the leading bit is implicit so cancel it out to get the significand
        IF (S <= 11) THEN
            IBits = IEOR(SHIFTR(U128%High, 11-S), TwoPow52)
        ELSE
            ! S-11 == additional bits we need
            IBits = IEOR(IOR(SHIFTL(U128%High, S-11), SHIFTR(U128%Low, 75-S)), TwoPow52)
        END IF
        ! get the binary exponent
        Exp = ToInt64(IAND(1150-S, Mask))        ! 1150 = 64 + 64 + 1023 - 1

        ! Add the exponent
        IBits = IOR(IBits, SHIFTL(Exp, 52))

        ! transfer output (RBits mapped to IBits using equivalence)
        R64 = RBits

        RETURN

    END FUNCTION R64_From_U128_A1

    !******************************************************************************

    FUNCTION R64_From_U128_A2(U128) RESULT(R64)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 64-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tDouble                :: R64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        R64 = U64_To_R64(U128%Low) + U64_To_R64(U128%High)*TwoPow64

        RETURN

    END FUNCTION R64_From_U128_A2

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

END FUNCTION R64_From_U128_Xp

!******************************************************************************

MODULE FUNCTION R128_From_U128_Xp(U128, Algo) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert an unsigned 128-bit integer number to a 128-bit floating point number

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: U128
    tSInt32,  INTENT(IN)   :: Algo
    tQuad                  :: R128

!** SUBROUTINE PARAMETER DECLARATIONS:
    tQuad, PARAMETER    :: TwoPow64 = 2.0_kQuad**64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algo)
    CASE (1)
        R128 = R128_From_U128_A1(U128)
    CASE (2)
        R128 = R128_From_U128_A2(U128)
    END SELECT

    RETURN

    CONTAINS

    FUNCTION R128_From_U128_A1(U128) RESULT(R128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 128-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tQuad                  :: R128

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tUInt64, PARAMETER :: TwoPow112(2) = [0_kInt64, 281474976710656_kInt64]  ! SHIFTL(1, 112)
        tUInt32, PARAMETER :: Mask         = ToInt32(Z'00007FFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: S, Shift
        tSInt64         :: Exp
        tUInt64         :: IBits(2)
        tQuad           :: RBits
        EQUIVALENCE(IBits, RBits)

    !** FLOW

        IF (U128%High == 0_kInt64) THEN
            R128 = U64_To_R128(U128%Low)
            RETURN
        END IF

        S = LEADZ(U128%High)
        IF (S >= 15) THEN
            R128 = U64_To_R128(U128%Low) + U64_To_R128(U128%High)*TwoPow64
            RETURN
        END IF

        ! Mask out the 113 MSBits
        Shift = 15 - S
        IBits(2) = SHIFTR(U128%High, Shift)
        IBits(1) = IOR(SHIFTR(U128%Low, Shift), SHIFTL(U128%High, 64-Shift))

        ! get the binary exponent
        Exp = ToInt64(IAND(16510-S, Mask))   ! 16510 = 64 + 64 + 16383 - 1

        ! The leading bit is implicit, cancel it out to get the significand
        ! and also add the exponent
        IBits(1) = IEOR(IOR(SHIFTR(U128%Low, Shift), SHIFTL(U128%High, 64-Shift)), TwoPow112(1))
        IBits(2) = IOR(IEOR(SHIFTR(U128%High, Shift), TwoPow112(2)), SHIFTL(Exp, 48))    ! 48 = 112 - 64

        ! transfer output (RBits mapped to IBits using equivalence)
        ! For big-endian machine, this one is likely wrong so we must
        ! swap IBits(1) and IBits(2) before the assigment.
        !   Tmp = IBits(1); IBits(1) = IBits(2); IBits(2) = Tmp
        R128 = RBits

        RETURN

    END FUNCTION R128_From_U128_A1

    !**************************************************************************

    FUNCTION R128_From_U128_A2(U128) RESULT(R128)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 128-bit integer number to a 128-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)   :: U128
        tQuad                  :: R128

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        R128 = U64_To_R128(U128%Low) + U64_To_R128(U128%High)*TwoPow64

        RETURN

    END FUNCTION R128_From_U128_A2

    !******************************************************************************

    FUNCTION U64_To_R128(LongVal) RESULT(QuadVal)

    !DIR$ ATTRIBUTES FORCEINLINE :: U64_To_R128

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 128-bit floating point number

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)   :: LongVal  ! integer number treated as unsigned one
        tQuad                 :: QuadVal  ! floating point number

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

END FUNCTION R128_From_U128_Xp

!******************************************************************************

END SUBMODULE SubBase_U128_Experimental

!******************************************************************************
