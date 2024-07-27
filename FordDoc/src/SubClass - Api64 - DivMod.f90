
SUBMODULE (Class_ApInt64 : SubClass_Api64_Arithmetic) SubClass_Api64_DivMod

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to division
!   operations of the <a href="../module/class_apint64.html">ApInt64</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName  = 'SubClass_Api64_DivMod'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE SUBROUTINES OR FUNCTIONS:

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    DIVISION OPERATIONS                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE ApInt64_Over_I32(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64),     INTENT(INOUT)   :: This
    tInteger,           INTENT(IN)      :: Other
    tInteger, OPTIONAL, INTENT(OUT)     :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Rem

!** FLOW

    IF (This%IsZero()) THEN
        IF (PRESENT(Remainder)) Remainder = 0
        RETURN
    ELSEIF (Other == 0) THEN
        This = ZeroApInt64()
        IF (PRESENT(Remainder)) Remainder = 0
        CALL Handle_ErrLevel('ApInt64_Over_I32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    IF (PRESENT(Remainder)) THEN
        IF (Other < 0) THEN
            This%Sign = -This%Sign
            Remainder = -This%Sign*UDiv(This, -Other)
        ELSE
            Remainder = This%Sign*UDiv(This, Other)
        END IF
    ELSE
        IF (Other < 0) THEN
            This%Sign = -This%Sign
            Rem = -This%Sign*UDiv(This, -Other)
        ELSE
            Rem = This%Sign*UDiv(This, Other)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt64_Over_I32

!******************************************************************************

MODULE SUBROUTINE ApInt64_Over_I64(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64),  INTENT(INOUT)  :: This
    tLong,           INTENT(IN)     :: Other
    tLong, OPTIONAL, INTENT(OUT)    :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Rem

!** FLOW

    IF (This%IsZero()) THEN
        IF (PRESENT(Remainder)) Remainder = 0_kLong
        RETURN
    ELSEIF (Other == 0_kLong) THEN
        This = ZeroApInt64()
        IF (PRESENT(Remainder)) Remainder = 0_kLong
        CALL Handle_ErrLevel('ApInt64_Over_I32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    IF (PRESENT(Remainder)) THEN
        IF (Other < 0) THEN
            This%Sign = -This%Sign
            Remainder = -This%Sign*UDiv(This, -Other)
        ELSE
            Remainder = This%Sign*UDiv(This, Other)
        END IF
    ELSE
        IF (Other < 0) THEN
            This%Sign = -This%Sign
            Rem = -This%Sign*UDiv(This, -Other)
        ELSE
            Rem = This%Sign*UDiv(This, Other)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt64_Over_I64

!******************************************************************************

MODULE SUBROUTINE ApInt64_Over_I128(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64),          INTENT(INOUT)  :: This
    TYPE(SInt128),           INTENT(IN)     :: Other
    TYPE(SInt128), OPTIONAL, INTENT(OUT)    :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(ApInt64)   :: Rem

!** FLOW

    IF (PRESENT(Remainder)) THEN
        CALL This%Divide(ApInt64(Other), Rem)
        Remainder = ToI128(Rem)
    ELSE
        CALL This%Divide(ApInt64(Other))
    END IF

    RETURN

END SUBROUTINE ApInt64_Over_I128

!******************************************************************************

MODULE SUBROUTINE ApInt64_Over_ApInt64(This, Other, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64),          INTENT(INOUT)  :: This
    TYPE(ApInt64),           INTENT(IN)     :: Other
    TYPE(ApInt64), OPTIONAL, INTENT(OUT)    :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: Tmp
    tLong               :: Rem
    tLong, ALLOCATABLE  :: Dividend(:)

!** FLOW

    IF (This%IsZero()) THEN
        IF (PRESENT(Remainder)) Remainder = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(Other)) THEN
        This = ZeroApInt64()
        IF (PRESENT(Remainder)) Remainder = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_Over_ApInt64', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Other%Length == 1_kIndex) THEN
        Rem = UDiv(This, Other%Digit(0))
        IF (PRESENT(Remainder)) Remainder = ApInt64(This%Sign, Rem)
        This%Sign = This%Sign*Other%Sign
        RETURN
    END IF
    Tmp = CompareAbs(This, Other)
    IF (Tmp < 0) THEN
        IF (PRESENT(Remainder)) Remainder = This
        This = ZeroApInt64()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(This, This%Sign*Other%Sign, 1)
        IF (PRESENT(Remainder)) Remainder = ZeroApInt64()
        RETURN
    END IF

    ! preprocess
    CALL MemAlloc(Dividend, This%Length, StartID=0_kIndex)
    Dividend(0:This%Length-1) = This%Digit(0:This%Length-1)
    CALL MemAlloc(This%Digit, This%Length-Other%Length+1_kIndex, StartID=0_kIndex)

    IF (PRESENT(Remainder)) THEN
        ! allocate output storage
        CALL MemAlloc(Remainder%Digit, Other%Length, StartID=0_kIndex)
        ! perform division
        CALL DivCore_ApInt64(Dividend, This%Length, Other%Digit, Other%Length, This%Digit, &
                             Remainder%Digit)
        ! postprocess remainder
        Remainder%Sign = This%Sign
        Remainder%Length = Other%Length
        DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
            Remainder%Length = Remainder%Length - 1_kIndex
        END DO
    ELSE
        ! perform division
        CALL DivCore_ApInt64(Dividend, This%Length, Other%Digit, Other%Length, This%Digit)
    END IF

    ! postprocess quotient
    This%Length = SIZE(This%Digit, KIND=kIndex)
    DO WHILE ((This%Length > 1_kIndex).AND.(This%Digit(This%Length-1_kIndex) == 0))
        This%Length = This%Length - 1_kIndex
    END DO
    This%Sign = This%Sign*Other%Sign

    RETURN

END SUBROUTINE ApInt64_Over_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Divide_I32(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    tInteger,      INTENT(IN)   :: Divisor
    TYPE(ApInt64)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Quotient = MakeCopy(Dividend)
    CALL Quotient%Divide(Divisor)

    RETURN

END FUNCTION ApInt64_Divide_I32

!******************************************************************************

MODULE FUNCTION ApInt64_Divide_I64(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    tLong,         INTENT(IN)   :: Divisor
    TYPE(ApInt64)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Quotient = MakeCopy(Dividend)
    CALL Quotient%Divide(Divisor)

    RETURN

END FUNCTION ApInt64_Divide_I64

!******************************************************************************

MODULE FUNCTION ApInt64_Divide_I128(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    TYPE(SInt128), INTENT(IN)   :: Divisor
    TYPE(ApInt64)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Quotient = MakeCopy(Dividend)
    CALL Quotient%Divide(Divisor)

    RETURN

END FUNCTION ApInt64_Divide_I128

!******************************************************************************

MODULE FUNCTION ApInt64_Divide_ApInt64(Dividend, Divisor) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division: Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    TYPE(ApInt64), INTENT(IN)   :: Divisor
    TYPE(ApInt64)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp
    tLong       :: Rem

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Quotient = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_Divide_ApInt64', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Quotient = MakeCopy(Dividend)
        Quotient%Sign = Dividend%Sign*Divisor%Sign
        Rem = UDiv(Quotient, Divisor%Digit(0))
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Quotient = ZeroApInt64()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(Quotient, Dividend%Sign*Divisor%Sign, 1)
        RETURN
    END IF

    ! preprocess
    CALL MemAlloc(Quotient%Digit, Dividend%Length-Divisor%Length+1_kIndex, StartID=0_kIndex)

    ! perform division
    CALL DivCore_ApInt64(Dividend%Digit, Dividend%Length, Divisor%Digit, &
                         Divisor%Length, Quotient%Digit)

    ! postprocess
    Quotient%Length = SIZE(Quotient%Digit, KIND=kIndex)
    DO WHILE ((Quotient%Length > 1_kIndex).AND.(Quotient%Digit(Quotient%Length-1_kIndex) == 0))
        Quotient%Length = Quotient%Length - 1_kIndex
    END DO
    Quotient%Sign = Dividend%Sign*Divisor%Sign

    RETURN

END FUNCTION ApInt64_Divide_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Mod_I32(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation:  Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    tInteger,      INTENT(IN)   :: Divisor
    TYPE(ApInt64)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: Rem
    TYPE(ApInt64)   :: Numer

!** FLOW

    Numer = MakeCopy(Dividend)
    CALL Numer%Divide(Divisor, Rem)
    Remainder = Rem

    RETURN

END FUNCTION ApInt64_Mod_I32

!******************************************************************************

MODULE FUNCTION ApInt64_Mod_I64(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation:  Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    tLong,         INTENT(IN)   :: Divisor
    TYPE(ApInt64)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: Rem
    TYPE(ApInt64)   :: Numer

!** FLOW

    Numer = MakeCopy(Dividend)
    CALL Numer%Divide(Divisor, Rem)
    Remainder = Rem

    RETURN

END FUNCTION ApInt64_Mod_I64

!******************************************************************************

MODULE FUNCTION ApInt64_Mod_I128(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation:  Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    TYPE(SInt128), INTENT(IN)   :: Divisor
    TYPE(ApInt64)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)   :: Rem
    TYPE(ApInt64)   :: Numer

!** FLOW

    Numer = MakeCopy(Dividend)
    CALL Numer%Divide(Divisor, Rem)
    Remainder = Rem

    RETURN

END FUNCTION ApInt64_Mod_I128

!******************************************************************************

MODULE FUNCTION ApInt64_Mod_ApInt64(Dividend, Divisor) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation: Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    TYPE(ApInt64), INTENT(IN)   :: Divisor
    TYPE(ApInt64)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp

!** FLOW

    IF (IsZero(Dividend)) THEN
        Remainder = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Remainder = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_Mod_ApInt64', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Remainder = MakeCopy(Dividend)
        CALL URem(Remainder, Divisor%Digit(0))
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Remainder = MakeCopy(Dividend)
        RETURN
    END IF
    IF (Tmp == 0) THEN
        Remainder = ZeroApInt64()
        RETURN
    END IF

    ! allocate storage
    CALL MemAlloc(Remainder%Digit, Divisor%Length, StartID=0_kIndex)

    ! perform division
    CALL DivCore_ApInt64(Dividend%Digit, Dividend%Length, Divisor%Digit, Divisor%Length, &
                         R=Remainder%Digit)

    ! postprocess
    Remainder%Sign = Dividend%Sign
    Remainder%Length = Divisor%Length
    DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
        Remainder%Length = Remainder%Length - 1_kIndex
    END DO

    RETURN

END FUNCTION ApInt64_Mod_ApInt64

!******************************************************************************

MODULE SUBROUTINE ApInt64_DivMod_I32(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor
    ! return both quotient and remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Dividend
    tInteger,       INTENT(IN)  :: Divisor
    TYPE(ApInt64),  INTENT(OUT) :: Quotient
    TYPE(ApInt64),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient  = ZeroApInt64()
        Remainder = ZeroApInt64()
        RETURN
    ELSEIF (Divisor == 0) THEN
        Quotient  = ZeroApInt64()
        Remainder = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_DivMod_I32', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    Quotient = MakeCopy(Dividend)
    IF (Divisor < 0) THEN
        Quotient%Sign = -Quotient%Sign
        Remainder = -Quotient%Sign*UDiv(Quotient, -Divisor)
    ELSE
        Remainder = Quotient%Sign*UDiv(Quotient, Divisor)
    END IF

    RETURN

END SUBROUTINE ApInt64_DivMod_I32

!******************************************************************************

MODULE SUBROUTINE ApInt64_DivMod_I64(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor and
    ! return both quotient and remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Dividend
    tLong,          INTENT(IN)  :: Divisor
    TYPE(ApInt64),  INTENT(OUT) :: Quotient
    TYPE(ApInt64),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient  = ZeroApInt64()
        Remainder = ZeroApInt64()
        RETURN
    ELSEIF (Divisor == 0_kLong) THEN
        Quotient  = ZeroApInt64()
        Remainder = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_DivMod_I64', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    Quotient = MakeCopy(Dividend)
    IF (Divisor < 0) THEN
        Quotient%Sign = -Quotient%Sign
        Remainder = -Quotient%Sign*UDiv(Quotient, -Divisor)
    ELSE
        Remainder = Quotient%Sign*UDiv(Quotient, Divisor)
    END IF

    RETURN

END SUBROUTINE ApInt64_DivMod_I64

!******************************************************************************

MODULE SUBROUTINE ApInt64_DivMod_I128(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  Quotient = Dividend / Divisor and
    ! return both quotient and remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Dividend
    TYPE(SInt128),  INTENT(IN)  :: Divisor
    TYPE(ApInt64),  INTENT(OUT) :: Quotient
    TYPE(ApInt64),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Dividend%DivMod(ApInt64(Divisor), Quotient, Remainder)

    RETURN

END SUBROUTINE ApInt64_DivMod_I128

!******************************************************************************

MODULE SUBROUTINE ApInt64_DivMod_ApInt64(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two ApInt64 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Dividend
    TYPE(ApInt64),  INTENT(IN)  :: Divisor
    TYPE(ApInt64),  INTENT(OUT) :: Quotient
    TYPE(ApInt64),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient  = ZeroApInt64()
        Remainder = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Quotient  = ZeroApInt64()
        Remainder = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_DivMod_ApInt64', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Quotient = MakeCopy(Dividend)
        Quotient%Sign = Dividend%Sign*Divisor%Sign
        Remainder = ApInt64(Dividend%Sign, UDiv(Quotient, Divisor%Digit(0)))
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Remainder = Dividend
        Quotient  = ZeroApInt64()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(Quotient, Dividend%Sign*Divisor%Sign, 1)
        Remainder = ZeroApInt64()
        RETURN
    END IF

    ! allocate storages
    CALL MemAlloc(Quotient%Digit, Dividend%Length-Divisor%Length+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Remainder%Digit, Divisor%Length, StartID=0_kIndex)

    ! perform division
    CALL DivCore_ApInt64(Dividend%Digit, Dividend%Length, Divisor%Digit, Divisor%Length, &
                         Quotient%Digit, Remainder%Digit)

    ! postprocess remainder
    Remainder%Sign = Dividend%Sign
    Remainder%Length = Divisor%Length
    DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
        Remainder%Length = Remainder%Length - 1_kIndex
    END DO

    ! postprocess quotient
    Quotient%Length = SIZE(Quotient%Digit, KIND=kIndex)
    DO WHILE ((Quotient%Length > 1_kIndex).AND.(Quotient%Digit(Quotient%Length-1_kIndex) == 0))
        Quotient%Length = Quotient%Length - 1_kIndex
    END DO
    Quotient%Sign = Dividend%Sign*Divisor%Sign

    RETURN

END SUBROUTINE ApInt64_DivMod_ApInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    SUPPORTING ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE DivCore_ApInt64(Dvd, MM, Dvs, NN, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the first magnitude Dvd(0:M-1)) by the second magnitude Dvs[0:N-1)
    ! and stores the resulting quotient in Q and remainder in R.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: MM           ! The length of the first array
    tLong,           INTENT(IN)     :: Dvd(0:MM-1)  ! The first magnitude array (the dividend)
    tIndex,          INTENT(IN)     :: NN           ! The length of the second array
    tLong,           INTENT(IN)     :: Dvs(0:NN-1)  ! The second magnitude array (divisor)
    tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)        ! The quotient array
    tLong, OPTIONAL, INTENT(OUT)    :: R(0:)        ! The remainder array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: RHi, RLo     ! The remainder
    tIndex          :: M, N
    tLong           :: U(0:MM)      ! working dividend and remainder
    tLong           :: V(0:NN-1)    ! working divisor
    tIndex          :: I
    tInteger        :: S

!** FLOW

    ! Initialize
    M = MM
    N = NN
    U(0:M-1) = Dvd(0:M-1)
    U(M)     = 0_kLong
    V(0:N-1) = Dvs(0:N-1)

    ! perform normalization
    S = LEADZ(V(N-1))
    IF (S > 0) THEN
        DO I = N-1, 1, -1
            V(I) = IOR(SHIFTL(V(I), S), SHIFTR(V(I-1), 64-S))
        END DO
        V(0) = SHIFTL(V(0), S)

        U(M) = SHIFTR(U(M-1), 64-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF

    IF (N == 1) THEN
        CALL UDivRemBy1(U, M+1, V(0), RLo)
        IF (PRESENT(Q)) Q(0:) = U(0:)
        IF (PRESENT(R)) R(0) = SHIFTR(RLo, S)
        RETURN
    END IF

    IF (N == 2) THEN
        CALL UDivRemBy2(U, M+1, V(1), V(0), RHi, RLo)
        IF (PRESENT(Q)) Q(0:) = U(0:)
        IF (PRESENT(R)) THEN
            R(0) = IOR(SHIFTR(RLo, S), SHIFTL(RHi, 64-S))
            R(1) = SHIFTR(RHi, S)
        END IF
        RETURN
    END IF

    ! Skip the highest word of numerator if not significant
    IF ((U(M) /= 0_kLong) .OR. (U(M-1) >= V(N-1))) M = M + 1

    CALL UDivRemKnuth(U, M, V, N, Q)

    IF (PRESENT(R)) THEN
        IF (S > 0) THEN
            DO I = 0, N-2
                R(I) = IOR(SHIFTR(U(I), S), SHIFTL(U(I+1), 64-S))
            END DO
        ELSE
            R(0:N-2) = U(0:N-2)
        END IF
        R(N-1) = SHIFTR(U(N-1), S)
    END IF

    RETURN

    CONTAINS

    FUNCTION AddArrayU64(X, Y, Length) RESULT(Carry)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition: X = X + Y.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(INOUT)   :: X(0:)
        tLong,  INTENT(IN)      :: Y(0:)
        tIndex, INTENT(IN)      :: Length
        tLong                   :: Carry

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I
        tLong       :: S

    !** FLOW

        IF (Length < 2) THEN
            Carry = 0_kLong
            CALL Handle_ErrLevel('AddArrayU64', SubName, ErrSevere, 'Length must be at least 2.')
            RETURN
        END IF

        Carry = 0_kLong
        DO I = 0, Length-1
            CALL AddU64(X(I), Y(I), Carry, S)
            X(I) = S
        END DO

        RETURN

    END FUNCTION AddArrayU64

    !**************************************************************************

    FUNCTION SubMulArrayU64(X, Y, Length, Multiplier) RESULT(Borrow)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtract and multiply: X = X - Multiplier * Y.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(INOUT)   :: X(0:)
        tLong,  INTENT(IN)      :: Y(0:)
        tIndex, INTENT(IN)      :: Length
        tLong,  INTENT(IN)      :: Multiplier
        tLong                   :: Borrow

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: I
        tLong       :: PHi, PLo, S, Carry

    !** FLOW

        IF (Length < 1) THEN
            Borrow = 0_kLong
            CALL Handle_ErrLevel('SubMulArrayU64', SubName, ErrSevere, 'Length must be at least 1.')
            RETURN
        END IF

        Borrow = 0_kLong
        DO I = 0, Length-1
            S = X(I) - Borrow
            CALL UMul128(Y(I), Multiplier, PHi, PLo)
            IF (X(I) .ULT. S) THEN
                Borrow = PHi + 1_kLong
            ELSE
                Borrow = PHi
            END IF
            X(I) = S - PLo
            IF (S .ULT. X(I)) Borrow = Borrow + 1_kLong
        END DO

        RETURN

    END FUNCTION SubMulArrayU64

    !**************************************************************************

    SUBROUTINE UDivRemKnuth(U, ULen, D, DLen, Q)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division using Knuth algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,            INTENT(INOUT) :: U(0:)
        tIndex,           INTENT(IN)    :: ULen
        tLong,            INTENT(IN)    :: D(0:)
        tIndex,           INTENT(IN)    :: DLen
        tLong,  OPTIONAL, INTENT(OUT)   :: Q(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: DHi, DLo, Reciprocal
        tLong       :: U2, U1, U0
        tLong       :: QHat, RHi, RLo
        tLong       :: Overflow, Carry, Borrow
        tIndex      :: J

    !** FLOW

        IF (DLen < 3) THEN
            CALL Handle_ErrLevel('UDivRemKnuth', SubName, ErrSevere, 'DLen must be at least 3.')
            RETURN
        END IF
        IF (ULen < DLen) THEN
            CALL Handle_ErrLevel('UDivRemKnuth', SubName, ErrSevere, &
                              'ULen must be greater than or equal to DLen.')
            RETURN
        END IF

        IF (PRESENT(Q)) Q = 0_kLong
        ! set divisor
        DHi = D(DLen - 1)
        DLo = D(DLen - 2)
        Reciprocal = Reciprocal_3By2(DHi, DLo)
        DO J = ULen-DLen-1, 0, -1
            U2 = U(J + DLen)
            U1 = U(J + DLen - 1)
            U0 = U(J + DLen - 2)
            IF ((U2 == DHi).AND.(U1 == DLo)) THEN
                ! division overflows
                QHat = NOT(0_kLong)
                U(J + DLen) = U2 - SubMulArrayU64(U(J:), D, DLen, QHat)
            ELSE
                CALL UDivRem_3By2(U2, U1, U0, DHi, DLo, Reciprocal, QHat, RHi, RLo)
                Overflow = SubMulArrayU64(U(J:), D, DLen-2, QHat)
                Borrow = 0_kLong
                CALL SubU64(RLo, Overflow, Borrow, U(J + DLen - 2))
                Carry = 0_kLong
                CALL SubU64(RHi, Borrow, Carry, U(J + DLen - 1))
                IF (Carry /= 0_kLong) THEN
                    QHat = QHat - 1_kLong
                    U(J + DLen - 1) = U(J + DLen - 1) + DHi + AddArrayU64(U(J:), D, DLen - 1)
                END IF
            END IF
            ! Store quotient digit
            IF (PRESENT(Q)) Q(J) = QHat
        END DO

        RETURN

    END SUBROUTINE UDivRemKnuth

    !**************************************************************************

END SUBROUTINE DivCore_ApInt64

!******************************************************************************

FUNCTION Reciprocal_3By2(DHi, DLo) RESULT(R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the reciprocal (2**128 - 1) / D - 2**64 for normalized D
    ! based on Algorithm 2 from "Improved division by invariant integers".

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: DHi, DLo
    tLong               :: R

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: V, P, THi, TLo

!** FLOW

    V = Reciprocal_2By1(DHi)
    P = DHi * V
    P = P + DLo
    IF (IEOR(P, MinI64) < IEOR(DLo, MinI64)) THEN
        V = V - 1_kLong
        IF (IEOR(P, MinI64) >= IEOR(DHi, MinI64)) THEN
            V = V - 1_kLong
            P = P - DHi
        END IF
        P = P - DHi
    END IF

    CALL UMul128(V, DLo, THi, TLo)

    P = P + THi
    IF (IEOR(P, MinI64) < IEOR(THi, MinI64)) THEN
        V = V - 1_kLong
        IF (IEOR(P, MinI64) >= IEOR(DHi, MinI64)) THEN
            IF ((IEOR(P,   MinI64) >  IEOR(DHi, MinI64)).OR.&
                (IEOR(TLo, MinI64) >= IEOR(DLo, MinI64))) V = V - 1_kLong
        END IF
    END IF
    R = V

    RETURN

END FUNCTION Reciprocal_3By2

!******************************************************************************

SUBROUTINE UDivRem_3By2(U2, U1, U0, DHi, DLo, V, Q, RHi, RLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform 128-bit integer division by 64-bit integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: U2, U1, U0, DHi, DLo, V
    tLong, INTENT(OUT)  :: Q, RHi, RLo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: QHi, QLo, NewLo, R1
    tLong       :: THi, TLo, SHi, SLo
    tLogical    :: Flag

!** FLOW

    ! Q128 = V*U2
    CALL UMul128(V, U2, QHi, QLo)

    ! Q128 = Q128 + UInt128(U2, U1)
    NewLo = QLo + U1
    IF (IEOR(NewLo, MinI64) < IEOR(QLo, MinI64)) THEN
        QHi = QHi + U2 + 1_kLong
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
        SHi = R1 - THi - 1_kLong
    ELSE
        SHi = R1 - THi
    END IF
    RLo  = SLo - DLo
    IF (IEOR(SLo, MinI64) < IEOR(DLo, MinI64)) THEN
        RHi = SHi - DHi - 1_kLong
    ELSE
        RHi = SHi - DHi
    END IF

    R1 = RHi

    QHi = QHi + 1_kLong

    IF (R1 .UGE. QLo) THEN
        QHi = QHi - 1_kLong
        ! R128 = R128 + D128
        NewLo = RLo + DLo
        IF (IEOR(NewLo, MinI64) < IEOR(RLo, MinI64)) THEN
            RHi = RHi + DHi + 1_kLong
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
        QHi = QHi + 1_kLong
        ! R128 = R128 - D128
        NewLo = RLo - DLo
        IF (IEOR(RLo, MinI64) < IEOR(DLo, MinI64)) THEN
            RHi = RHi - DHi - 1_kLong
        ELSE
            RHi = RHi - DHi
        END IF
        RLo = NewLo
    END IF

    Q = QHi

    RETURN

END SUBROUTINE UDivRem_3By2

!******************************************************************************

SUBROUTINE UDivRemBy2(U, ULen, DHi, DLo, RHi, RLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform arbitrary-precision unsigned integer division by 128-bit unsigned integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)      :: ULen         ! length of U
    tLong,  INTENT(INOUT)   :: U(0:ULen-1)  ! on entry, the dividend
                                            ! on exit, the quotient
    tLong,  INTENT(IN)      :: DHi, DLo     ! the divisor (128-bit integer)
    tLong,  INTENT(OUT)     :: RHi, RLo     ! the remainder (128-bit integer)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Reciprocal
    tLong       :: U2, U1, U0
    tIndex      :: I

!** FLOW

    IF (ULen < 3) THEN
        CALL Handle_ErrLevel('UDivRemBy1', SubName, ErrSevere, 'ULen must be at least 3.')
        RETURN
    END IF

    Reciprocal = Reciprocal_3By2(DHi, DLo)

    U2 = U(ULen - 1)
    U1 = U(ULen - 2)        ! Set the 2 top words as remainder.
    U(ULen - 1) = 0_kLong
    U(ULen - 2) = 0_kLong   ! Reset these words being a part of the resulting quotient.

    I = ULen - 3
    DO
        U0 = U(I)
        CALL UDivRem_3By2(U2, U1, U0, DHi, DLo, Reciprocal, U(I), RHi, RLo)
        U2 = RHi
        U1 = RLo
        IF (I == 0) EXIT
        I = I - 1
    END DO

    RETURN

END SUBROUTINE UDivRemBy2

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                   ALTERNATIVE ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION UDiv_SInt128(Big, U64) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the ApInt64 by the specified integer and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)
    tLong                           :: Rem  ! The remainder (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (U64 < 0) THEN
        Rem = UDiv_Safe(Big, U64)
    ELSE
        Rem = UDiv_Unsafe(Big, U64)
    END IF

    RETURN

    CONTAINS

    FUNCTION UDiv_Unsafe(Big, Div) RESULT(Rem)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the ApInt64 by the specified integer and return the remainder
        ! Assume the divisor is greater than zero

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: Big
        tLong,         INTENT(IN)       :: Div  ! The divisor (treated as unsigned)
        tLong                           :: Rem  ! The remainder (treated as unsigned)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        !TYPE(SInt128)   :: DL, RL
        TYPE(SInt128)   :: D128, Q128, R128
        tIndex          :: I

    !** FLOW

        R128 = ZeroI128
        DO I = Big%Length-1_kIndex, 0_kIndex, -1_kIndex
            D128 = ShiftL64(R128) + SInt128(Big%Digit(I), Positive)
            CALL DivMod(D128, Div, Q128, R128)
            Big%Digit(I) = ToU64(Q128)
        END DO
        IF ((Big%Digit(Big%Length-1_kIndex) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
            Big%Length = Big%Length - 1_kIndex
        END IF
        IF ((Big%Length == 1_kIndex).AND.(Big%Digit(0) == 0_kLong)) Big%Sign = 1
        Rem = ToU64(R128)

        RETURN

    END FUNCTION UDiv_Unsafe

    !**************************************************************************

    FUNCTION UDiv_Safe(Big, Div) RESULT(Rem)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the ApInt64 by the specified integer and return the remainder
        ! Assume the divisor is less than zero

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: Big
        tLong,         INTENT(IN)       :: Div  ! The divisor (treated as unsigned)
        tLong                           :: Rem  ! The remainder (treated as unsigned)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(SInt128)   :: HBit, HBM1, RL, DL, HQ, HRL, Q, RLS
        tIndex          :: I

    !** FLOW

        HBit = SInt128(ToLong(Z'8000000000000000'), 0_kLong)
        HBM1 = HBit - OneI128
        DL = SInt128(Div, Positive)
        HQ = HBM1/DL
        IF ((HQ*DL + DL) == HBit) CALL Increment(HQ)
        HRL = HBit - HQ*DL
        RL = ZeroI128
        DO I = Big%Length-1_kIndex, 0_kIndex, -1_kIndex
            RL = ShiftL64(RL) + SInt128(Big%Digit(I), Positive)
            RLS = ShiftA64Up(RL, 127)
            Q = IAND(HQ, RLS) + (IAND(RL, HBM1) + IAND(HRL, RLS))/DL
            RL = RL - Q*DL
            Big%Digit(I) = ToU64(Q)
        END DO
        IF ((Big%Digit(Big%Length-1_kIndex) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
            Big%Length = Big%Length - 1_kIndex
        END IF
        IF ((Big%Length == 1_kIndex).AND.(Big%Digit(0) == 0_kLong)) Big%Sign = 1
        Rem = ToU64(RL)

        RETURN

    END FUNCTION UDiv_Safe

    !**************************************************************************

END FUNCTION UDiv_SInt128

!******************************************************************************

SUBROUTINE URem_SInt128(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulo of the ApInt64 with the specified integer
    ! (i.e. set Big to MOD(Big, U64)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    ! na

!** FLOW

    IF (U64 < 0) THEN
        CALL URem_Safe(Big, U64)
    ELSE
        CALL URem_Unsafe(Big, U64)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE URem_Unsafe(Big, Div)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform modulo of the ApInt64 with the specified integer
        ! Assume the divisor is greater than zero

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: Big
        tLong,         INTENT(IN)       :: Div  ! The divisor (treated as unsigned)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(SInt128)   :: RL
        tIndex          :: I

    !** FLOW

        RL = ZeroI128
        DO I = Big%Length-1_kIndex, 0_kIndex, -1_kIndex
            RL = MOD(ShiftL64(RL) + SInt128(Big%Digit(I), Positive), Div)
        END DO
        Big%Length = 1_kIndex
        Big%Digit(0) = ToU64(RL)
        IF (Big%Digit(0) == 0_kLong) Big%Sign = 1

        RETURN

    END SUBROUTINE URem_Unsafe

    !**************************************************************************

    SUBROUTINE URem_Safe(Big, Div)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform modulo of the ApInt64 with the specified integer
        ! Assume the divisor is less than zero

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: Big
        tLong,         INTENT(IN)       :: Div  ! The divisor (treated as unsigned)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(SInt128)   :: HBit, HBM1, DL, HRem, Rem
        tIndex          :: I

    !** FLOW

        HBit = SInt128(ToLong(Z'8000000000000000'), 0_kLong)
        HBM1 = HBit - OneI128
        DL = SInt128(Div, Positive)
        ! Precompute hrem = (1 << 63) % d
        ! I.e. the remainder caused by the highest bit.
        HRem = MOD(HBM1, DL)
        CALL Increment(HRem)
        IF (HRem == DL) HRem = ZeroI128
        Rem = 0_kLong
        DO I = Big%Length-1_kIndex, 0_kIndex, -1_kIndex
            Rem = ShiftL64(Rem) + SInt128(Big%Digit(I), Positive)
            ! Calculate rem %= d.
            ! Do this by calculating the lower 127 bits and highest bit separately.
            ! The highest bit remainder only gets added if it's set.
            Rem = MOD(IAND(Rem, HBM1) + IAND(HRem, ShiftA64Up(Rem, 127)), DL)
            ! The addition is safe and cannot overflow.
            ! Because hrem < 2^64 and there's at least one zero bit in [126,64] if bit 127 is set.
        END DO
        Big%Length = 1_kIndex
        Big%Digit(0) = ToU64(Rem)
        IF (Big%Digit(0) == 0_kLong) Big%Sign = 1

        RETURN

    END SUBROUTINE URem_Safe

    !**************************************************************************

END SUBROUTINE URem_SInt128

!******************************************************************************

FUNCTION UDiv_UInt128(Big, U64) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the ApInt64 by the specified integer and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)
    tLong                           :: Rem  ! The remainder (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: D128, Q128, R128
    tLong           :: Q64, R64
    tIndex          :: I

!** FLOW

    CALL UDivMod(Big%Digit(Big%Length-1), U64, Q64, R64)
    Big%Digit(Big%Length-1) = Q64
    D128 = R64
    IF (Big%Length > 1_kIndex) THEN
        DO I = Big%Length-2, 0, -1
            D128 = ShiftL64(D128) + Big%Digit(I)
            CALL UDivMod(D128, U64, Q128, R128)
            Big%Digit(I) = Q128
            D128 = R128
        END DO
    END IF
    IF ((Big%Digit(Big%Length-1_kIndex) == 0_kLong).AND.(Big%Length > 1_kIndex)) THEN
        Big%Length = Big%Length - 1_kIndex
    END IF
    IF ((Big%Length == 1_kIndex).AND.(Big%Digit(0) == 0_kLong)) Big%Sign = 1
    Rem = D128

    RETURN

END FUNCTION UDiv_UInt128

!******************************************************************************

SUBROUTINE URem_UInt128(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulo of the ApInt64 with the specified integer
    ! (i.e. set Big to MOD(Big, U64)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: R128
    tIndex          :: I

!** FLOW

    R128 = UMOD(Big%Digit(Big%Length-1), U64)
    IF (Big%Length > 1_kIndex) THEN
        DO I = Big%Length-2_kIndex, 0_kIndex, -1_kIndex
            R128 = UMOD(ShiftL64(R128) + Big%Digit(I), U64)
        END DO
    END IF
    Big%Length = 1_kIndex
    Big%Digit(0) = R128
    IF (Big%Digit(0) == 0_kLong) Big%Sign = 1

    RETURN

END SUBROUTINE URem_UInt128

!******************************************************************************

SUBROUTINE DivCore_IntX(Dvd, M, Dvs, N, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the first magnitude Dvd(0:M-1)) by the second magnitude Dvs[0:N-1)
    ! and stores the resulting quotient in Q and remainder in R.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: M            ! The length of the first array
    tLong,           INTENT(IN)     :: Dvd(0:M-1)   ! The first magnitude array (the dividend)
    tIndex,          INTENT(IN)     :: N            ! The length of the second array
    tLong,           INTENT(IN)     :: Dvs(0:N-1)   ! The second magnitude array (divisor)
    tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)        ! The quotient array
    tLong, OPTIONAL, INTENT(OUT)    :: R(0:)        ! The remainder array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: U(0:M)      ! working dividend and remainder
    tLong           :: V(0:N-1)    ! working divisor
    tLong           :: R0, R1
    tIndex          :: I
    tInteger        :: S

!** FLOW

    ! Initialize
    U(0:M-1) = Dvd(0:M-1)
    U(M)     = 0_kLong
    V(0:N-1) = Dvs(0:N-1)

    ! perform normalization
    S = LEADZ(V(N-1))
    IF (S > 0) THEN
        DO I = N-1, 1, -1
            V(I) = IOR(SHIFTL(V(I), S), SHIFTR(V(I-1), 64-S))
        END DO
        V(0) = SHIFTL(V(0), S)

        U(M) = SHIFTR(U(M-1), 64-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF

    IF (N == 1) THEN
        CALL UDivRemBy1(U, M+1, V(0), R0)
        IF (PRESENT(Q)) Q(0:) = U(0:)
        IF (PRESENT(R)) R(0) = SHIFTR(R0, S)
        RETURN
    END IF

    IF (N == 2) THEN
        CALL UDivRemBy2(U, M+1, V(1), V(0), R1, R0)
        IF (PRESENT(Q)) Q(0:) = U(0:)
        IF (PRESENT(R)) THEN
            R(0) = IOR(SHIFTR(R0, S), SHIFTL(R1, 64-S))
            R(1) = SHIFTR(R1, S)
        END IF
        RETURN
    END IF

    CALL UDivRemKnuth(U, M, V, N, Q)

    IF (PRESENT(R)) THEN
        IF (S > 0) THEN
            DO I = 0, N-1
                R(I) = IOR(SHIFTR(U(I), S), SHIFTL(U(I+1), 64-S))
            END DO
        ELSE
            R(0:N-1) = U(0:N-1)
        END IF
    END IF

    RETURN

    CONTAINS

    SUBROUTINE UDivRemKnuth(U, ULen, D, DLen, Q)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division using Knuth algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,           INTENT(INOUT)  :: U(0:)
        tIndex,          INTENT(IN)     :: ULen
        tLong,           INTENT(IN)     :: D(0:)
        tIndex,          INTENT(IN)     :: DLen
        tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong           :: DHi, DLo, Reciprocal
        tLong           :: U2, U1, U0
        tLong           :: QHat, RHat, PHi, PLo
        TYPE(UInt128)   :: RHat128, S
        tLong           :: Overflow, Carry, Borrow
        tIndex          :: J, I

    !** FLOW

        DHi = D(DLen - 1)
        DLo = D(DLen - 2)
        Reciprocal = Reciprocal_2By1(DHi)
        DO J = ULen-DLen, 0, -1
            U2 = U(J + DLen)
            U1 = U(J + DLen - 1)
            U0 = U(J + DLen - 2)
            IF (U2 .UGE. DHi) THEN
                ! Will overflow
                QHat = NOT(0_kLong)
                RHat128 = UInt128(U2, U1) - UInt128(DHi, 0_kLong)
                RHat128 = RHat128 + DHi
                ! Adjustment
                CALL UMul128(QHat, DLo, PHi, PLo)
                IF ((RHat128%High == 0_kLong).AND. &
                    (UInt128(PHi, PLo).UGT.UInt128(RHat128%Low, U0))) THEN
                    QHat = QHat - 1_kLong
                END IF
            ELSE
                CALL UDivRem_2By1(U2, U1, DHi, Reciprocal, QHat, RHat)
                RHat128 = UInt128(0_kLong, RHat)
                CALL UMul128(QHat, DLo, PHi, PLo)
                IF (UInt128(PHi, PLo).UGT.UInt128(RHat, U0)) THEN
                    QHat = QHat - 1_kLong
                    RHat128 = RHat128 + DHi
                    ! Adjustment
                    CALL UMul128(QHat, DLo, PHi, PLo)
                    IF ((RHat128%High == 0_kLong).AND. &
                        (UInt128(PHi, PLo).UGT.UInt128(RHat128%Low, U0))) THEN
                        QHat = QHat - 1_kLong
                    END IF
                END IF
            END IF
            ! Multiply and subtract
            Borrow = 0_kLong
            DO I = 0, DLen-1
                CALL UMul128(QHat, D(I), PHi, PLo)
                S = (UInt128(0_kLong, U(I+J)) - Borrow) - PLo
                U(I+J) = S%Low
                Borrow = PHi - S%High
            END DO
            U(DLen+J) = U2 - Borrow
            IF (U2 .ULT. Borrow) THEN
                ! Too much subtracted, add back
                QHat = QHat - 1_kLong
                Carry = 0_kLong
                DO I = 0, DLen-1
                    S = (UInt128(0_kLong, U(I+J)) + D(I)) + Carry
                    U(I+J) = S%Low
                    Carry  = S%High
                END DO
                U(DLen+J) = U(DLen+J) + Carry
            END IF
            ! Store quotient digit
            IF (PRESENT(Q)) Q(J) = QHat
        END DO

        RETURN

    END SUBROUTINE UDivRemKnuth

    !**************************************************************************

END SUBROUTINE DivCore_IntX

!******************************************************************************

SUBROUTINE DivCore_Hybrid(Dvd, M, Dvs, N, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the first magnitude Dvd(0:M-1)) by the second magnitude Dvs[0:N-1)
    ! and stores the resulting quotient in Q and remainder in R.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: M            ! The length of the first array
    tLong,           INTENT(IN)     :: Dvd(0:M-1)   ! The first magnitude array (the dividend)
    tIndex,          INTENT(IN)     :: N            ! The length of the second array
    tLong,           INTENT(IN)     :: Dvs(0:N-1)   ! The second magnitude array (divisor)
    tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)        ! The quotient array
    tLong, OPTIONAL, INTENT(OUT)    :: R(0:)        ! The remainder array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: U(0:M)      ! working dividend and remainder
    tLong           :: V(0:N-1)    ! working divisor
    tLong           :: R0, R1
    tIndex          :: I
    tInteger        :: S

!** FLOW

    ! Initialize
    U(0:M-1) = Dvd(0:M-1)
    U(M)     = 0_kLong
    V(0:N-1) = Dvs(0:N-1)

    ! perform normalization
    S = LEADZ(V(N-1))
    IF (S > 0) THEN
        DO I = N-1, 1, -1
            V(I) = IOR(SHIFTL(V(I), S), SHIFTR(V(I-1), 64-S))
        END DO
        V(0) = SHIFTL(V(0), S)

        U(M) = SHIFTR(U(M-1), 64-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF

    IF (N == 1) THEN
        CALL UDivRemBy1(U, M+1, V(0), R0)
        IF (PRESENT(Q)) Q(0:) = U(0:)
        IF (PRESENT(R)) R(0) = SHIFTR(R0, S)
        RETURN
    END IF

    IF (N == 2) THEN
        CALL UDivRemBy2(U, M+1, V(1), V(0), R1, R0)
        IF (PRESENT(Q)) Q(0:) = U(0:)
        IF (PRESENT(R)) THEN
            R(0) = IOR(SHIFTR(R0, S), SHIFTL(R1, 64-S))
            R(1) = SHIFTR(R1, S)
        END IF
        RETURN
    END IF

    CALL UDivRemKnuth(U, M, V, N, Q)

    IF (PRESENT(R)) THEN
        IF (S > 0) THEN
            DO I = 0, N-1
                R(I) = IOR(SHIFTR(U(I), S), SHIFTL(U(I+1), 64-S))
            END DO
        ELSE
            R(0:N-1) = U(0:N-1)
        END IF
    END IF

    RETURN
    CONTAINS

    SUBROUTINE UDivRemKnuth(U, M, V, N, Q)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division using Knuth algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,           INTENT(INOUT)  :: U(0:)
        tIndex,          INTENT(IN)     :: M
        tLong,           INTENT(IN)     :: V(0:)
        tIndex,          INTENT(IN)     :: N
        tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tLong, PARAMETER    :: MaskLo = MaxU64

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: QHat_Hi, QHat_Lo     ! estimated quotient digit
        tLong       :: RHat_Hi, RHat_Lo     ! a remainder
        tLong       :: PHi, PLo             ! product of two digits
        tIndex      :: I, J
        tInteger    :: S
        tLong       :: THi, TLo
        tLong       :: KHi, KLo
        tLong       :: DHLo, DLLo
        tLong       :: QXHi, QXLo
        tLong       :: XHi, XLo
        tLong       :: TmpLo, TmpHi, Tmp
        tLong       :: Reciprocal, Rem

    !** FLOW

        DHLo = V(N-1)
        DLLo = V(N-2)
        Reciprocal = Reciprocal_2By1(DHLo)

        ! Main loop
        DO J = M-N, 0, -1

            ! +++ Compute estimate QHat of Q(J). +++
            KHi = U(J+N)
            KLo = U(J+N-1)
            CALL UDivRem_2By1(SHIFTR(KHi, 1), IOR(SHIFTR(KLo, 1), SHIFTL(SHIFTL(KHi, 1), 62)), &
                              DHLo, Reciprocal, QXLo, Rem)
            QHat_Hi = SHIFTR(QXLo, 63)
            QHat_Lo = SHIFTL(QXLo, 1)
            CALL I128_SubMul(KHi, KLo, QHat_Hi, QHat_Lo, DHLo, THi, TLo)
            IF (THi /= 0_kLong) THEN
                CALL I128_Increment(QHat_Hi, QHat_Lo)
                CALL I128_SubMul(KHi, KLo, QHat_Hi, QHat_Lo, DHLo, RHat_Hi, RHat_Lo)
            ELSE
                IF (IEOR(TLo, MinI64) < IEOR(DHLo, MinI64)) THEN
                    RHat_Hi = THi
                    RHat_Lo = TLo
                ELSE
                    CALL I128_Increment(QHat_Hi, QHat_Lo)
                    CALL I128_SubMul(KHi, KLo, QHat_Hi, QHat_Lo, DHLo, RHat_Hi, RHat_Lo)
                END IF
            END IF

            ! +++ Unsigned comparison. +++
            TLo = U(J+N-2)
            DO
                IF (QHat_Hi == 0_kLong) THEN
                    CALL I128_Multiply_U64(QHat_Hi, QHat_Lo, DLLo, TmpHi, TmpLo)
                    IF (ULE(TmpHi, TmpLo, RHat_Lo, TLo)) EXIT
                END IF
                CALL I128_Decrement(QHat_Hi, QHat_Lo)
                CALL I128_Add_U64(RHat_Hi, RHat_Lo, DHLo)
                IF (RHat_Hi /= 0_kLong) EXIT
            END DO

            ! +++ Multiply and subtract. +++
            KHi = 0_kLong
            KLo = 0_kLong
            DO I = 0, N-1
                CALL I128_Multiply_U64(QHat_Hi, QHat_Lo, V(I), PHi, PLo)
                CALL I128_DoubleSub(U(I+J), KHi, KLo, IAND(PLo, MaskLo), THi, TLo)
                U(I+J) = TLo
                CALL I128_Subtract(PHi, SHIFTA(THi, 63), THi, KHi, KLo)
            END DO
            CALL I128_Subtract(U(J+N), KHi, KLo, THi, TLo)
            U(J+N) = TLo

            ! +++ Store quotient digit. If we subtracted too much, add back. +++
            IF (THi < 0_kLong) THEN
                QHat_Lo = QHat_Lo - 1_kLong
                KLo = 0_kLong
                DO I = 0, N-1
                    CALL I128_DoubleAdd(U(I+J), V(I), KLo, THi, TLo)
                    U(I+J) = TLo
                    KLo = THi
                END DO
                U(J+N) = U(J+N) + KLo
            END IF
            IF (PRESENT(Q)) Q(J) = QHat_Lo
        END DO

        RETURN

    END SUBROUTINE UDivRemKnuth

    !**************************************************************************

END SUBROUTINE DivCore_Hybrid

!******************************************************************************

SUBROUTINE DivCore_Div2By1(Dvd, M, Dvs, N, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the first magnitude Dvd(0:M-1)) by the second magnitude Dvs[0:N-1)
    ! and stores the resulting quotient in Q and remainder in R.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: M            ! The length of the first array
    tLong,           INTENT(IN)     :: Dvd(0:M-1)   ! The first magnitude array (the dividend)
    tIndex,          INTENT(IN)     :: N            ! The length of the second array
    tLong,           INTENT(IN)     :: Dvs(0:N-1)   ! The second magnitude array (divisor)
    tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)        ! The quotient array
    tLong, OPTIONAL, INTENT(OUT)    :: R(0:)        ! The remainder array

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MaskHi = MinU64
    tLong, PARAMETER    :: MaskLo = MaxU64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: U(0:M)               ! working dividend and remainder
    tLong       :: V(0:N-1)             ! working divisor
    tLong       :: QHat_Hi, QHat_Lo     ! estimated quotient digit
    tLong       :: RHat_Hi, RHat_Lo     ! a remainder
    tLong       :: PHi, PLo             ! product of two digits
    tIndex      :: I, J
    tInteger    :: S
    tLong       :: THi, TLo
    tLong       :: KHi, KLo
    tLong       :: DHLo, DLLo
    tLong       :: QXLo
    tLong       :: TmpLo, TmpHi
    tLong       :: Reciprocal, Rem

!** FLOW

    ! Initialize
    U(0:M-1) = Dvd(0:M-1)
    U(M)     = 0_kLong
    V(0:N-1) = Dvs(0:N-1)

    ! Normalize by shifting v left just enough so that
    ! its high-order bit is on, and shift u left the
    ! same amount.  We may have to append a high-order
    ! digit on the dividend; we do that unconditionally.
    S = LEADZ(V(N-1))
    IF (S > 0) THEN
        DO I = N-1, 1, -1
            V(I) = IOR(SHIFTL(V(I), S), SHIFTR(V(I-1), 64-S))
        END DO
        V(0) = SHIFTL(V(0), S)

        U(M) = SHIFTR(U(M-1), 64-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF

    DHLo = V(N-1)
    DLLo = V(N-2)
    Reciprocal = Reciprocal_2By1(DHLo)

    ! Main loop
    DO J = M-N, 0, -1

        ! +++ Compute estimate QHat of Q(J). +++
        KHi = U(J+N)
        KLo = U(J+N-1)
        CALL UDivRem_2By1(SHIFTR(KHi, 1), IOR(SHIFTR(KLo, 1), SHIFTL(SHIFTL(KHi, 1), 62)), &
                            DHLo, Reciprocal, QXLo, Rem)
        QHat_Hi = SHIFTR(QXLo, 63)
        QHat_Lo = SHIFTL(QXLo, 1)
        CALL I128_SubMul(KHi, KLo, QHat_Hi, QHat_Lo, DHLo, THi, TLo)
        IF (THi /= 0_kLong) THEN
            CALL I128_Increment(QHat_Hi, QHat_Lo)
            CALL I128_SubMul(KHi, KLo, QHat_Hi, QHat_Lo, DHLo, RHat_Hi, RHat_Lo)
        ELSE
            IF (IEOR(TLo, MinI64) < IEOR(DHLo, MinI64)) THEN
                RHat_Hi = THi
                RHat_Lo = TLo
            ELSE
                CALL I128_Increment(QHat_Hi, QHat_Lo)
                CALL I128_SubMul(KHi, KLo, QHat_Hi, QHat_Lo, DHLo, RHat_Hi, RHat_Lo)
            END IF
        END IF

        ! +++ Unsigned comparison. +++
        TLo = U(J+N-2)
        DO
            IF (QHat_Hi == 0_kLong) THEN
                CALL I128_Multiply_U64(QHat_Hi, QHat_Lo, DLLo, TmpHi, TmpLo)
                IF (ULE(TmpHi, TmpLo, RHat_Lo, TLo)) EXIT
            END IF
            CALL I128_Decrement(QHat_Hi, QHat_Lo)
            CALL I128_Add_U64(RHat_Hi, RHat_Lo, DHLo)
            IF (RHat_Hi /= 0_kLong) EXIT
        END DO

        ! +++ Multiply and subtract. +++
        KHi = 0_kLong
        KLo = 0_kLong
        DO I = 0, N-1
            CALL I128_Multiply_U64(QHat_Hi, QHat_Lo, V(I), PHi, PLo)
            CALL I128_DoubleSub(U(I+J), KHi, KLo, IAND(PLo, MaskLo), THi, TLo)
            U(I+J) = TLo
            CALL I128_Subtract(PHi, SHIFTA(THi, 63), THi, KHi, KLo)
        END DO
        CALL I128_Subtract(U(J+N), KHi, KLo, THi, TLo)
        U(J+N) = TLo

        ! +++ Store quotient digit. If we subtracted too much, add back. +++
        IF (PRESENT(Q)) Q(J) = QHat_Lo
        IF (THi < 0_kLong) THEN
            IF (PRESENT(Q)) Q(J) = Q(J) - 1_kLong
            KLo = 0_kLong
            DO I = 0, N-1
                CALL I128_DoubleAdd(U(I+J), V(I), KLo, THi, TLo)
                U(I+J) = TLo
                KLo = THi
            END DO
            U(J+N) = U(J+N) + KLo
        END IF
    END DO

    IF (PRESENT(R)) THEN
        IF (S > 0) THEN
            ! Unnormalize U().
            DO I = 0, M-1
                U(I) = IOR(SHIFTR(U(I), S), SHIFTL(U(I+1), 64-S))
            END DO
            U(M) = SHIFTR(U(M), S)
        END IF
        R(0:N-1) = U(0:N-1)
    END IF

    RETURN

END SUBROUTINE DivCore_Div2By1

!**************************************************************************

SUBROUTINE DivCore_Inline(Dvd, M, Dvs, N, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the first magnitude Dvd(0:M-1)) by the second magnitude Dvs[0:N-1)
    ! and stores the resulting quotient in Q and remainder in R.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: M            ! The length of the first array
    tLong,           INTENT(IN)     :: Dvd(0:M-1)   ! The first magnitude array (the dividend)
    tIndex,          INTENT(IN)     :: N            ! The length of the second array
    tLong,           INTENT(IN)     :: Dvs(0:N-1)   ! The second magnitude array (divisor)
    tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)        ! The quotient array
    tLong, OPTIONAL, INTENT(OUT)    :: R(0:)        ! The remainder array

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MaskHi = MinU64
    tLong, PARAMETER    :: MaskLo = MaxU64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: U(0:M)               ! working dividend and remainder
    tLong       :: V(0:N-1)             ! working divisor
    tLong       :: QHat_Hi, QHat_Lo     ! estimated quotient digit
    tLong       :: RHat_Hi, RHat_Lo     ! a remainder
    tLong       :: PHi, PLo             ! product of two digits
    tIndex      :: I, J
    tInteger    :: S
    tLong       :: THi, TLo
    tLong       :: KHi, KLo
    tLong       :: DHLo, DLLo
    tLong       :: QXLo
    tLong       :: XHi, XLo
    tLong       :: TmpLo, TmpHi, Tmp

!** FLOW

    ! *** Hacker's Delight's implementation of Knuth's Algorithm D ***

    ! Initialize
    U(0:M-1) = Dvd(0:M-1)
    U(M)     = 0_kLong
    V(0:N-1) = Dvs(0:N-1)

    ! Normalize by shifting v left just enough so that
    ! its high-order bit is on, and shift u left the
    ! same amount.  We may have to append a high-order
    ! digit on the dividend; we do that unconditionally.

    S = LEADZ(V(N-1))
    IF (S > 0) THEN
        DO I = N-1, 1, -1
            V(I) = IOR(SHIFTL(V(I), S), SHIFTR(V(I-1), 64-S))
        END DO
        V(0) = SHIFTL(V(0), S)

        U(M) = SHIFTR(U(M-1), 64-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF

    DHLo = V(N-1)
    DLLo = V(N-2)

    ! Main loop
    DO J = M-N, 0, -1

        ! +++ Compute estimate QHat of Q(J). +++

        ! K = U(J+N)*B + SInt128(U(J+N-1), Positive)
        KHi = U(J+N)
        KLo = U(J+N-1)

        ! QHat = SHIFTL(SHIFTR(K, 1)/DH, 1)
        XHi = SHIFTR(KHi, 1)
        XLo = IOR(SHIFTR(KLo, 1), SHIFTL(SHIFTL(KHi, 1), 62))
        QXLo = I128_DivideBy_U64(XHi, XLo, DHLo)
        QHat_Hi = SHIFTR(QXLo, 63)
        QHat_Lo = SHIFTL(QXLo, 1)

        ! T = K - QHat*DH
        CALL I128_SubMul(KHi, KLo, QHat_Hi, QHat_Lo, DHLo, THi, TLo)

        IF (UGE(THi, TLo, 0_kLong, DHLo)) CALL I128_Increment(QHat_Hi, QHat_Lo)

        ! RHat = K - QHat*DH
        CALL I128_SubMul(KHi, KLo, QHat_Hi, QHat_Lo, DHLo, RHat_Hi, RHat_Lo)

        ! +++ Unsigned comparison. +++

        ! DO WHILE ((ToU128(QHat) .UGE. ToU128(B)).OR.           &
        !           (ToU128(I128_Multiply_U64(QHat, DLLo)) .UGT. &
        !            UInt128(GetLowI64(RHat), U(J+N-2))))
        CALL I128_Multiply_U64(QHat_Hi, QHat_Lo, DLLo, TmpHi, TmpLo)
        DO WHILE (UGE_Base(QHat_Hi, QHat_Lo) .OR. UGT(TmpHi, TmpLo, RHat_Lo, U(J+N-2)))

            ! QHat = QHat - OneI128
            CALL I128_Decrement(QHat_Hi, QHat_Lo)

            ! RHat = RHat + DH
            CALL I128_Add_Self(RHat_Hi, RHat_Lo, DHLo)

            ! IF (ToU128(RHat) .UGE. ToU128(B)) EXIT
            IF (UGE_Base(RHat_Hi, RHat_Lo)) EXIT

            CALL I128_Multiply_U64(QHat_Hi, QHat_Lo, DLLo, TmpHi, TmpLo)
        END DO

        ! +++ Multiply and subtract. +++

        ! K = ZeroI128
        KHi = 0_kLong
        KLo = 0_kLong
        DO I = 0, N-1

            ! P = QHat*SInt128(V(I), Positive)
            CALL I128_Multiply_U64(QHat_Hi, QHat_Lo, V(I), PHi, PLo)

            ! T = SInt128(U(I+J), Positive) - K - IAND(P, MASK)
            CALL I128_Add_Self(KHi, KLo, IAND(PLo, MaskLo))
            CALL I128_Sub(0_kLong, U(I+J), KHi, KLo, THi, TLo)

            U(I+J) = TLo

            ! K = SHIFTR(P, 64) - SHIFTA(T, 64)
            KLo = PHi - THi
            KHi = -SHIFTA(THi, 63)
            IF (IEOR(PHi, MinI64) < IEOR(THi, MinI64)) KHi = KHi - 1_kLong
        END DO

        ! T = SInt128(U(J+N), Positive) - K
        CALL I128_Sub(0_kLong, U(J+N), KHi, KLo, THi, TLo)
        U(J+N) = TLo

        ! +++ Store quotient digit. If we subtracted too much, add back. +++
        IF (PRESENT(Q)) Q(J) = QHat_Lo
        IF (THi < 0_kLong) THEN
            IF (PRESENT(Q)) Q(J) = Q(J) - 1_kLong
            ! K = ZeroI128
            KLo = 0_kLong
            DO I = 0, N-1

                ! T = SInt128(U(I+J), Positive) + SInt128(V(I), Positive) + K
                Tmp = U(I+J) + V(I)
                IF (IEOR(Tmp, MinI64) < IEOR(V(I), MinI64)) THEN
                    THi = 1_kLong
                ELSE
                    THi = 0_kLong
                END IF
                TLo = Tmp + KLo
                IF (IEOR(TLo, MinI64) < IEOR(Tmp, MinI64)) THi = THi + 1_kLong

                U(I+J) = TLo

                ! K = SHIFTR(T, 64)
                KLo = THi
            END DO
            ! U(J+N) = U(J+N) + ToU64(K)
            U(J+N) = U(J+N) + KLo
        END IF
    END DO

    IF (PRESENT(R)) THEN
        IF (S > 0) THEN
            ! Unnormalize U().
            DO I = 0, M-1
                U(I) = IOR(SHIFTR(U(I), S), SHIFTL(U(I+1), 64-S))
            END DO
            U(M) = SHIFTR(U(M), S)
        END IF
        R(0:N-1) = U(0:N-1)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE I128_Add_Self(LhsHi, LhsLo, RhsLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform addition of two 128-bit signed integers (Lhs + Rhs)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: LhsHi, LhsLo
        tLong, INTENT(IN)       :: RhsLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong   :: NewLo

    !** FLOW

        NewLo = LhsLo + RhsLo
        IF (IEOR(NewLo, MinI64) < IEOR(LhsLo, MinI64)) LhsHi = LhsHi + 1_kLong
        LhsLo = NewLo

        RETURN

    END SUBROUTINE I128_Add_Self

    !**************************************************************************

    SUBROUTINE I128_Sub(LhsHi, LhsLo, RhsHi, RhsLo, OutHi, OutLo)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform subtraction of two 128-bit signed integers (Lhs - Rhs)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LhsHi, LhsLo, RhsHi, RhsLo
        tLong, INTENT(OUT)  :: OutHi, OutLo

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutLo = LhsLo - RhsLo
        OutHi = LhsHi - RhsHi
        IF (IEOR(LhsLo, MinI64) < IEOR(RhsLo, MinI64)) OutHi = OutHi - 1_kLong

        RETURN

    END SUBROUTINE I128_Sub

    !**************************************************************************

    FUNCTION I128_DivideBy_U64(DvdHi, DvdLo, Denom) RESULT(Quotient)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To divide the 128-bit (un)signed integer by the 64-bit unsigned integer

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: DvdHi, DvdLo, Denom
        tLong               :: Quotient

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tLong, PARAMETER    :: Mask = ToLong(Z'00000000FFFFFFFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger    :: Shift
        tLong       :: High, Low, Divisor
        tLong       :: LowHigh, LowLow
        tLong       :: RHat, UHat
        tLong       :: DivisorHigh, DivisorLow
        tLong       :: QuotientHigh, QuotientLow

    !** FLOW

        ! initialize
        Divisor = Denom
        High  = DvdHi
        Low   = DvdLo
        Shift = LEADZ(Divisor)

        IF (Shift /= 0) THEN
            Divisor = SHIFTL(Divisor, Shift)
            High = IOR(SHIFTL(High, Shift), SHIFTR(Low, 64 - Shift))
            Low = SHIFTL(Low, Shift)
        END IF

        DivisorHigh = SHIFTR(Divisor, 32)
        DivisorLow = IAND(Divisor, Mask)
        LowHigh = SHIFTR(Low, 32)
        LowLow = IAND(Low, Mask)

        ! Compute High quotient digit.
        CALL UDivMod(High, DivisorHigh, QuotientHigh, RHat)

        ! qhat >>> 32 == qhat > base
        DO WHILE ((SHIFTR(QuotientHigh, 32) /= 0_kLong).OR.     &
                  (IEOR(QuotientHigh * DivisorLow, MinI64) >  &
                   IEOR(IOR(SHIFTL(RHat, 32), LowHigh), MinI64)))
            QuotientHigh = QuotientHigh - 1_kLong
            RHat = RHat + DivisorHigh
            IF (SHIFTR(RHat, 32) /= 0_kLong) EXIT
        END DO

        UHat = IOR(SHIFTL(High, 32), LowHigh) - QuotientHigh * Divisor

        ! Compute Low quotient digit.
        CALL UDivMod(UHat, DivisorHigh, QuotientLow, RHat)

        DO WHILE ((SHIFTR(QuotientLow, 32) /= 0_kLong).OR.     &
                  (IEOR(QuotientLow * DivisorLow, MinI64) >  &
                   IEOR(IOR(SHIFTL(RHat, 32), LowLow), MinI64)))
            QuotientLow = QuotientLow - 1
            RHat = RHat + DivisorHigh
            IF (SHIFTR(RHat, 32) /= 0_kLong) EXIT
        END DO

        Quotient = IOR(SHIFTL(QuotientHigh, 32), QuotientLow)

        RETURN

    END FUNCTION I128_DivideBy_U64

    !**************************************************************************

    FUNCTION UGT(LhsHi, LhsLo, RhsHi, RhsLo) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the LHS Uint128 object is greater than the RHS Uint128 object

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LhsHi, LhsLo, RhsHi, RhsLo
        tLogical            :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LhsHi == RhsHi) THEN
            Flag = (IEOR(LhsLo, MinI64) > IEOR(RhsLo, MinI64))
        ELSE
            Flag = (IEOR(LhsHi, MinI64) > IEOR(RhsHi, MinI64))
        END IF

        RETURN

    END FUNCTION UGT

    !**************************************************************************

    FUNCTION UGE(LhsHi, LhsLo, RhsHi, RhsLo) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the LHS Uint128 object is greater than
        ! or equal to the RHS Uint128 object

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: LhsHi, LhsLo, RhsHi, RhsLo
        tLogical            :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LhsHi == RhsHi) THEN
            Flag = (IEOR(LhsLo, MinI64) >= IEOR(RhsLo, MinI64))
        ELSE
            Flag = (IEOR(LhsHi, MinI64) >= IEOR(RhsHi, MinI64))
        END IF

        RETURN

    END FUNCTION UGE

    !**************************************************************************

    FUNCTION UGE_Base(Hi, Lo) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given Uint128 object is greater than
        ! or equal to the 'Base'

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(IN)   :: Hi, Lo
        tLogical            :: Flag

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (Hi == 0_kLong) THEN
            Flag = FalseVal
        ELSE
            Flag = TrueVal
        END IF

        RETURN

    END FUNCTION UGE_Base

    !**************************************************************************

END SUBROUTINE DivCore_Inline

!**************************************************************************

SUBROUTINE DivCore_Sint128(Dvd, M, Dvs, N, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the first magnitude Dvd(0:M-1)) by the second magnitude Dvs[0:N-1)
    ! and stores the resulting quotient in Q and remainder in R.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: M            ! The length of the first array
    tLong,           INTENT(IN)     :: Dvd(0:M-1)   ! The first magnitude array (the dividend)
    tIndex,          INTENT(IN)     :: N            ! The length of the second array
    tLong,           INTENT(IN)     :: Dvs(0:N-1)   ! The second magnitude array (divisor)
    tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)        ! The quotient array
    tLong, OPTIONAL, INTENT(OUT)    :: R(0:)        ! The remainder array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong           :: U(0:M)   ! working dividend and remainder
    tLong           :: V(0:N-1) ! working divisor
    TYPE(SInt128)   :: QHat     ! estimated quotient digit
    TYPE(SInt128)   :: RHat     ! a remainder
    TYPE(SInt128)   :: P        ! product of two digits
    TYPE(SInt128)   :: B        ! Number base (64 bits)
    tIndex          :: I, J
    tInteger        :: S
    TYPE(SInt128)   :: T, K, DH, DL, MASK

!** FLOW

    ! *** Hacker's Delight's implementation of Knuth's Algorithm D ***

    ! Initialize
    MASK = SInt128(MinU64, MaxU64)
    B = SInt128(1_kLong, 0_kLong)
    U(0:M-1) = Dvd(0:M-1)
    U(M)     = 0_kLong
    V(0:N-1) = Dvs(0:N-1)

    ! Normalize by shifting v left just enough so that
    ! its high-order bit is on, and shift u left the
    ! same amount.  We may have to append a high-order
    ! digit on the dividend; we do that unconditionally.

    S = LEADZ(V(N-1))
    IF (S > 0) THEN
        DO I = N-1, 1, -1
            V(I) = IOR(SHIFTL(V(I), S), SHIFTR(V(I-1), 64-S))
        END DO
        V(0) = SHIFTL(V(0), S)

        U(M) = SHIFTR(U(M-1), 64-S)
        DO I = M-1, 1, -1
            U(I) = IOR(SHIFTL(U(I), S), SHIFTR(U(I-1), 64-S))
        END DO
        U(0) = SHIFTL(U(0), S)
    END IF

    DH = SInt128(V(N-1), Positive)
    DL = SInt128(V(N-2), Positive)

    ! Main loop
    DO J = M-N, 0, -1

        ! Compute estimate QHat of Q(J).
        K = U(J+N)*B + SInt128(U(J+N-1), Positive)
        QHat = ShiftLOnce(ShiftRONce(K)/DH)
        T = K - QHat*DH
        IF (ToU128(T) .UGE. ToU128(DH)) THEN
            CALL Increment(QHat)
            RHat = K - QHat*DH
        ELSE
            RHat = T
        END IF

        ! Unsigned comparison.
        DO WHILE ((ToU128(QHat) .UGE. ToU128(B)).OR. &
                    (ToU128(QHat*DL) .UGT. ToU128(B*RHat+SInt128(U(J+N-2), Positive))))
            CALL Decrement(QHat)
            RHat = RHat + DH
            IF (ToU128(RHat) .UGE. ToU128(B)) EXIT
        END DO

        ! Multiply and subtract.
        K = ZeroI128
        DO I = 0, N-1
            P = QHat*SInt128(V(I), Positive)
            T = SInt128(U(I+J), Positive) - K - IAND(P, MASK)
            U(I+J) = ToU64(T)
            K = ShiftR64(P) - ShiftA64(T)
        END DO
        T = SInt128(U(J+N), Positive) - K
        U(J+N) = ToU64(T)

        ! Store quotient digit. If we subtracted too much, add back.
        IF (PRESENT(Q)) Q(J) = ToU64(QHat)
        IF (T < ZeroI128) THEN
            IF (PRESENT(Q)) Q(J) = Q(J) - 1_kLong
            K = ZeroI128
            DO I = 0, N-1
                T = SInt128(U(I+J), Positive) + SInt128(V(I), Positive) + K
                U(I+J) = ToU64(T)
                K = ShiftR64(T)
            END DO
            U(J+N) = U(J+N) + ToU64(K)
        END IF
    END DO

    IF (PRESENT(R)) THEN
        IF (S > 0) THEN
            ! Unnormalize U().
            DO I = 0, M-1
                U(I) = IOR(SHIFTR(U(I), S), SHIFTL(U(I+1), 64-S))
            END DO
            U(M) = SHIFTR(U(M), S)
        END IF
        R(0:N-1) = U(0:N-1)
    END IF

    RETURN

END SUBROUTINE DivCore_Sint128

!**************************************************************************

SUBROUTINE I128_DoubleSub(FirstLo, SecondHi, SecondLo, ThirdLo, OutHi, OutLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform double subtraction (First - Second - Third)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: FirstLo, SecondHi, SecondLo, ThirdLo
    tLong, INTENT(OUT)  :: OutHi, OutLo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong   :: NewLo, Borrow1, Borrow2

!** FLOW

    NewLo = FirstLo - SecondLo
    OutLo = NewLo - ThirdLo
    Borrow1 = SHIFTR(IOR(IAND(NOT(FirstLo), SecondLo), &
                            IAND(NOT(IEOR(FirstLo, SecondLo)), NewLo)), 63)
    Borrow2 = SHIFTR(IOR(IAND(NOT(NewLo), ThirdLo), &
                            IAND(NOT(IEOR(NewLo, ThirdLo)), OutLo)), 63)
    OutHi = -(SecondHi + Borrow1 + Borrow2)

    RETURN

END SUBROUTINE I128_DoubleSub

!******************************************************************************

SUBROUTINE I128_DoubleAdd(FirstLo, SecondLo, ThirdLo, OutHi, OutLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform double addition (First + Second + Third)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: FirstLo, SecondLo, ThirdLo
    tLong, INTENT(OUT)  :: OutHi, OutLo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong   :: Sum, Carry

!** FLOW

    Sum = FirstLo + SecondLo
    Carry = SHIFTR(IOR(IAND(FirstLo, SecondLo), IAND(IOR(FirstLo, SecondLo), NOT(Sum))), 63)
    OutLo = Sum + ThirdLo
    OutHi = Carry + SHIFTR(IOR(IAND(Sum, ThirdLo), IAND(IOR(Sum, ThirdLo), NOT(Carry))), 63)

    RETURN

END SUBROUTINE I128_DoubleAdd

!******************************************************************************

SUBROUTINE I128_Add_U64(LhsHi, LhsLo, RhsLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform addition of two 128-bit signed integers (Lhs + Rhs)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: LhsHi, LhsLo
    tLong, INTENT(IN)       :: RhsLo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong   :: NewLo

!** FLOW

    NewLo = LhsLo + RhsLo
    IF (IEOR(NewLo, MinI64) < IEOR(LhsLo, MinI64)) LhsHi = LhsHi + 1_kLong
    LhsLo = NewLo

    RETURN

END SUBROUTINE I128_Add_U64

!******************************************************************************

SUBROUTINE I128_Increment(Hi, Lo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increment the 128-bit signed integer by 1

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: Hi, Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Lo = Lo + 1_kLong
    IF (Lo == 0_kLong) Hi = Hi + 1_kLong

    RETURN

END SUBROUTINE I128_Increment

!******************************************************************************

SUBROUTINE I128_Subtract(LhsLo, RhsHi, RhsLo, OutHi, OutLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform subtraction of two 128-bit signed integers (Lhs - Rhs)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: LhsLo, RhsHi, RhsLo
    tLong, INTENT(OUT)  :: OutHi, OutLo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Borrow

!** FLOW

    OutLo = LhsLo - RhsLo
    Borrow = SHIFTR(IOR(IAND(NOT(LhsLo), RhsLo), IAND(NOT(IEOR(LhsLo, RhsLo)), OutLo)), 63)
    OutHi = -(RhsHi + Borrow)

    RETURN

END SUBROUTINE I128_Subtract

!******************************************************************************

SUBROUTINE I128_Decrement(Hi, Lo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To decrement the 128-bit signed integer by 1

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: Hi, Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Lo == 0_kLong) Hi = Hi - 1_kLong
    Lo = Lo - 1_kLong

    RETURN

END SUBROUTINE I128_Decrement

!******************************************************************************

SUBROUTINE I128_Multiply_U64(InHi, InLo, U64, OutHi, OutLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the 128-bit (un)signed integer by the 64-bit unsigned integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: InHi, InLo, U64
    tLong, INTENT(OUT)  :: OutHi, OutLo

!** SUBROUTINE PARAMETERS DECLARATIONS:
    tLong, PARAMETER    :: Mask32 = ToLong(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: X_Lo, X_Hi, Y_Lo, Y_Hi
    tLong       :: Hi_Lo, Cross

!** FLOW

    ! "UMul128_Upper64"
    X_Lo = IAND(InLo, Mask32)
    X_Hi = SHIFTR(InLo, 32)
    Y_Lo = IAND(U64, Mask32)
    Y_Hi = SHIFTR(U64, 32)
    Hi_Lo = X_Hi*Y_Lo
    Cross = SHIFTR(X_Lo*Y_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi

    OutHi = InHi*U64 + SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
    OutLo = InLo*U64

    RETURN

END SUBROUTINE I128_Multiply_U64

!******************************************************************************

SUBROUTINE I128_SubMul(LhsHi, LhsLo, InHi, InLo, U64, OutHi, OutLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the 128-bit (un)signed integer by the 64-bit unsigned integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: LhsHi, LhsLo
    tLong, INTENT(IN)   :: InHi, InLo, U64
    tLong, INTENT(OUT)  :: OutHi, OutLo

!** SUBROUTINE PARAMETERS DECLARATIONS:
    tLong, PARAMETER    :: Mask32 = ToLong(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: X_Lo, X_Hi, Y_Lo, Y_Hi
    tLong       :: Hi_Lo, Cross, ProductLo
    tLong       :: Borrow

!** FLOW

    ! "UMul128_Upper64"
    X_Lo = IAND(InLo, Mask32)
    X_Hi = SHIFTR(InLo, 32)
    Y_Lo = IAND(U64, Mask32)
    Y_Hi = SHIFTR(U64, 32)
    Hi_Lo = X_Hi*Y_Lo
    Cross = SHIFTR(X_Lo*Y_Lo, 32) + IAND(Hi_Lo, Mask32) + X_Lo*Y_Hi
    ProductLo = InLo*U64

    OutLo = LhsLo - ProductLo
    Borrow = SHIFTR(IOR(IAND(NOT(LhsLo), ProductLo), &
                        IAND(NOT(IEOR(LhsLo, ProductLo)), OutLo)), 63)
    OutHi = LhsHi - (InHi*U64 + SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi) - Borrow

    RETURN

END SUBROUTINE I128_SubMul

!******************************************************************************

FUNCTION ULE(LhsHi, LhsLo, RhsHi, RhsLo) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the LHS Uint128 object is greater than
    ! or equal to the RHS Uint128 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: LhsHi, LhsLo, RhsHi, RhsLo
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LhsHi == RhsHi) THEN
        Flag = (IEOR(LhsLo, MinI64) <= IEOR(RhsLo, MinI64))
    ELSE
        Flag = (IEOR(LhsHi, MinI64) <= IEOR(RhsHi, MinI64))
    END IF

    RETURN

END FUNCTION ULE

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  EXPERIMENTAL ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION UDiv_Xp(Big, U64, Algorithm) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the ApInt64 by the specified integer and return the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)
    tLong                           :: Rem  ! The remainder (treated as unsigned)
    tInteger,      INTENT(IN)       :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        Rem = UDiv_SInt128(Big, U64)
    CASE (2)
        Rem = UDiv_UInt128(Big, U64)
    CASE DEFAULT
        Rem = UDiv_U64(Big, U64)
    END SELECT

    RETURN

END FUNCTION UDiv_Xp

!******************************************************************************

SUBROUTINE URem_Xp(Big, U64, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulo of the ApInt64 with the specified integer
    ! (i.e. set Big to MOD(Big, U64)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The divisor (treated as unsigned)
    tInteger,      INTENT(IN)       :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        CALL URem_SInt128(Big, U64)
    CASE (2)
        CALL URem_UInt128(Big, U64)
    CASE DEFAULT
        CALL URem_U64(Big, U64)
    END SELECT

    RETURN

END SUBROUTINE URem_Xp

!******************************************************************************

SUBROUTINE DivCore_Xp(Dvd, M, Dvs, N, Algorithm, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To divide the first magnitude Dvd(0:M-1)) by the second magnitude Dvs[0:N-1)
    ! and stores the resulting quotient in Q and remainder in R.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,          INTENT(IN)     :: M            ! The length of the first array
    tLong,           INTENT(IN)     :: Dvd(0:M-1)   ! The first magnitude array (the dividend)
    tIndex,          INTENT(IN)     :: N            ! The length of the second array
    tLong,           INTENT(IN)     :: Dvs(0:N-1)   ! The second magnitude array (divisor)
    tInteger,        INTENT(IN)     :: Algorithm
    tLong, OPTIONAL, INTENT(OUT)    :: Q(0:)        ! The quotient array
    tLong, OPTIONAL, INTENT(OUT)    :: R(0:)        ! The remainder array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        CALL DivCore_Sint128(Dvd, M, Dvs, N, Q, R)
    CASE (2)
        CALL DivCore_Inline(Dvd, M, Dvs, N, Q, R)
    CASE (3)
        CALL DivCore_Div2By1(Dvd, M, Dvs, N, Q, R)
    CASE (4)
        CALL DivCore_Hybrid(Dvd, M, Dvs, N, Q, R)
    CASE (5)
        CALL DivCore_IntX(Dvd, M, Dvs, N, Q, R)
    CASE DEFAULT
        CALL DivCore_ApInt64(Dvd, M, Dvs, N, Q, R)
    END SELECT

    RETURN

END SUBROUTINE DivCore_Xp

!******************************************************************************

MODULE SUBROUTINE ApInt64_Over_Xp(This, Other, Algorithm, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division:  This = This / Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64),          INTENT(INOUT)  :: This
    TYPE(ApInt64),           INTENT(IN)     :: Other
    tInteger,                INTENT(IN)     :: Algorithm
    TYPE(ApInt64), OPTIONAL, INTENT(OUT)    :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: Tmp
    tLong               :: Rem
    tLong, ALLOCATABLE  :: Dividend(:)

!** FLOW

    IF (This%IsZero()) THEN
        IF (PRESENT(Remainder)) Remainder = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(Other)) THEN
        This = ZeroApInt64()
        IF (PRESENT(Remainder)) Remainder = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_Over_Xp', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Other%Length == 1_kIndex) THEN
        Rem = UDiv_Xp(This, Other%Digit(0), Algorithm)
        IF (PRESENT(Remainder)) Remainder = ApInt64(This%Sign, Rem)
        This%Sign = This%Sign*Other%Sign
        RETURN
    END IF
    Tmp = CompareAbs(This, Other)
    IF (Tmp < 0) THEN
        IF (PRESENT(Remainder)) Remainder = This
        This = ZeroApInt64()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(This, This%Sign*Other%Sign, 1)
        IF (PRESENT(Remainder)) Remainder = ZeroApInt64()
        RETURN
    END IF

    ! preprocess
    CALL MemAlloc(Dividend, This%Length, StartID=0_kIndex)
    Dividend(0:This%Length-1) = This%Digit(0:This%Length-1)
    CALL MemAlloc(This%Digit, This%Length-Other%Length+1_kIndex, StartID=0_kIndex)

    IF (PRESENT(Remainder)) THEN
        ! allocate output storage
        CALL MemAlloc(Remainder%Digit, Other%Length, StartID=0_kIndex)
        ! perform division
        CALL DivCore_Xp(Dividend, This%Length, Other%Digit, Other%Length, Algorithm, &
                        This%Digit, Remainder%Digit)
        ! postprocess remainder
        Remainder%Sign = This%Sign
        Remainder%Length = Other%Length
        DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
            Remainder%Length = Remainder%Length - 1_kIndex
        END DO
    ELSE
        ! perform division
        CALL DivCore_Xp(Dividend, This%Length, Other%Digit, Other%Length, Algorithm, This%Digit)
    END IF

    ! postprocess quotient
    This%Length = SIZE(This%Digit, KIND=kIndex)
    DO WHILE ((This%Length > 1_kIndex).AND.(This%Digit(This%Length-1_kIndex) == 0))
        This%Length = This%Length - 1_kIndex
    END DO
    This%Sign = This%Sign*Other%Sign

    RETURN

END SUBROUTINE ApInt64_Over_Xp

!******************************************************************************

MODULE FUNCTION ApInt64_Divide_Xp(Dividend, Divisor, Algorithm) RESULT(Quotient)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division: Quotient = Dividend / Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    TYPE(ApInt64), INTENT(IN)   :: Divisor
    tInteger,      INTENT(IN)   :: Algorithm
    TYPE(ApInt64)               :: Quotient

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp
    tLong       :: Rem

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Quotient = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_Divide_Xp', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Quotient = MakeCopy(Dividend)
        Quotient%Sign = Dividend%Sign*Divisor%Sign
        Rem = UDiv_Xp(Quotient, Divisor%Digit(0), Algorithm)
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Quotient = ZeroApInt64()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(Quotient, Dividend%Sign*Divisor%Sign, 1)
        RETURN
    END IF

    ! preprocess
    CALL MemAlloc(Quotient%Digit, Dividend%Length-Divisor%Length+1_kIndex, StartID=0_kIndex)

    ! perform division
    CALL DivCore_Xp(Dividend%Digit, Dividend%Length, Divisor%Digit, Divisor%Length, &
                    Algorithm, Quotient%Digit)

    ! postprocess
    Quotient%Length = SIZE(Quotient%Digit, KIND=kIndex)
    DO WHILE ((Quotient%Length > 1_kIndex).AND.(Quotient%Digit(Quotient%Length-1_kIndex) == 0))
        Quotient%Length = Quotient%Length - 1_kIndex
    END DO
    Quotient%Sign = Dividend%Sign*Divisor%Sign

    RETURN

END FUNCTION ApInt64_Divide_Xp

!******************************************************************************

MODULE FUNCTION ApInt64_Mod_Xp(Dividend, Divisor, Algorithm) RESULT(Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform modulation: Remainder = Dividend Mod Divisor

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Dividend
    TYPE(ApInt64), INTENT(IN)   :: Divisor
    tInteger,      INTENT(IN)   :: Algorithm
    TYPE(ApInt64)               :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp

!** FLOW

    IF (IsZero(Dividend)) THEN
        Remainder = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Remainder = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_Mod_Xp', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Remainder = MakeCopy(Dividend)
        CALL URem_Xp(Remainder, Divisor%Digit(0), Algorithm)
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Remainder = MakeCopy(Dividend)
        RETURN
    END IF
    IF (Tmp == 0) THEN
        Remainder = ZeroApInt64()
        RETURN
    END IF

    ! allocate storage
    CALL MemAlloc(Remainder%Digit, Divisor%Length, StartID=0_kIndex)

    ! perform division
    CALL DivCore_Xp(Dividend%Digit, Dividend%Length, Divisor%Digit, Divisor%Length, &
                    Algorithm, R=Remainder%Digit)

    ! postprocess
    Remainder%Sign = Dividend%Sign
    Remainder%Length = Divisor%Length
    DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
        Remainder%Length = Remainder%Length - 1_kIndex
    END DO

    RETURN

END FUNCTION ApInt64_Mod_Xp

!******************************************************************************

MODULE SUBROUTINE ApInt64_DivMod_Xp(Dividend, Divisor, Algorithm, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division of two ApInt64 objects (Dividend / Divisor)
    ! and return both the quotient and the remainder

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(IN)  :: Dividend
    TYPE(ApInt64),  INTENT(IN)  :: Divisor
    tInteger,       INTENT(IN)  :: Algorithm
    TYPE(ApInt64),  INTENT(OUT) :: Quotient
    TYPE(ApInt64),  INTENT(OUT) :: Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Tmp

!** FLOW

    IF (IsZero(Dividend)) THEN
        Quotient  = ZeroApInt64()
        Remainder = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(Divisor)) THEN
        Quotient  = ZeroApInt64()
        Remainder = ZeroApInt64()
        CALL Handle_ErrLevel('ApInt64_DivMod_Xp', SubName, ErrSevere, 'Division by zero.')
        RETURN
    END IF

    ! check and return quickly if possible
    IF (Divisor%Length == 1) THEN
        Quotient = MakeCopy(Dividend)
        Quotient%Sign = Dividend%Sign*Divisor%Sign
        Remainder = ApInt64(Dividend%Sign, UDiv_Xp(Quotient, Divisor%Digit(0), Algorithm))
        RETURN
    END IF
    Tmp = CompareAbs(Dividend, Divisor)
    IF (Tmp < 0) THEN
        Remainder = Dividend
        Quotient  = ZeroApInt64()
        RETURN
    END IF
    IF (Tmp == 0) THEN
        CALL AssignUnsigned(Quotient, Dividend%Sign*Divisor%Sign, 1)
        Remainder = ZeroApInt64()
        RETURN
    END IF

    ! allocate storages
    CALL MemAlloc(Quotient%Digit, Dividend%Length-Divisor%Length+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Remainder%Digit, Divisor%Length, StartID=0_kIndex)

    ! perform division
    CALL DivCore_Xp(Dividend%Digit, Dividend%Length, Divisor%Digit, Divisor%Length, &
                    Algorithm, Quotient%Digit, Remainder%Digit)

    ! postprocess remainder
    Remainder%Sign = Dividend%Sign
    Remainder%Length = Divisor%Length
    DO WHILE (Remainder%Digit(Remainder%Length-1_kIndex) == 0)
        Remainder%Length = Remainder%Length - 1_kIndex
    END DO

    ! postprocess quotient
    Quotient%Length = SIZE(Quotient%Digit, KIND=kIndex)
    DO WHILE ((Quotient%Length > 1_kIndex).AND.(Quotient%Digit(Quotient%Length-1_kIndex) == 0))
        Quotient%Length = Quotient%Length - 1_kIndex
    END DO
    Quotient%Sign = Dividend%Sign*Divisor%Sign

    RETURN

END SUBROUTINE ApInt64_DivMod_Xp

!******************************************************************************

END SUBMODULE SubClass_Api64_DivMod

!******************************************************************************
