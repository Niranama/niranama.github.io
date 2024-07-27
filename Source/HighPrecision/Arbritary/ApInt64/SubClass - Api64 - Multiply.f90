
SUBMODULE (Class_ApInt64 : SubClass_Api64_Arithmetic) SubClass_Api64_MulSqr

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to multiplication
!   operations of the <a href="../module/class_apint64.html">ApInt64</a> type.

!** USE STATEMENTS:
    ! na

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    tCharStar, PARAMETER    :: SubName  = 'SubClass_Api64_MulSqr'

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE SUBROUTINES OR FUNCTIONS:

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                 MULTIPLICATION OPERATIONS                ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MODULE SUBROUTINE ApInt64_Times_I32(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    tInteger,       INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (This%IsZero()) RETURN

    IF (Other < 0) THEN
        This%Sign = -This%Sign
        CALL UMul(This, -Other)
    ELSE
        CALL UMul(This, Other)
    END IF

    RETURN

END SUBROUTINE ApInt64_Times_I32

!******************************************************************************

MODULE SUBROUTINE ApInt64_Times_I64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    tLong,          INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (This%IsZero()) RETURN

    IF (Other < 0) THEN
        This%Sign = -This%Sign
        CALL UMul(This, -Other)
    ELSE
        CALL UMul(This, Other)
    END IF

    RETURN

END SUBROUTINE ApInt64_Times_I64

!******************************************************************************

MODULE SUBROUTINE ApInt64_Times_I128(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    TYPE(SInt128),  INTENT(IN)      :: Other

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL This%Multiply(ApInt64(Other))

    RETURN

END SUBROUTINE ApInt64_Times_I128

!******************************************************************************

MODULE SUBROUTINE ApInt64_Times_ApInt64(This, Other)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  This = This * Other

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt64), INTENT(INOUT)   :: This
    TYPE(ApInt64),  INTENT(IN)      :: Other

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold, use Karatsuba algorithm
    tIndex, PARAMETER   :: Karatsuba_Threshold = 64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Sign
    tLong       :: Digit

!** FLOW

    ! check and returun quickly if possible
    IF (This%IsZero()) THEN
        RETURN
    ELSEIF (IsZero(Other)) THEN
        This = ZeroApInt64()
        RETURN
    ELSEIF ((This%Length <= 1_kIndex).OR.(Other%Length <= 1_kIndex)) THEN
        IF (Other%Length == 1) THEN
            This%Sign = This%Sign*Other%Sign
            CALL UMul(This, Other%Digit(0))
        ELSEIF (This%Length == 1) THEN
            Sign  = This%Sign*Other%Sign
            Digit = This%Digit(0)
            This  = MakeCopy(Other, Other%Length+1_kIndex)
            This%Sign = Sign
            CALL UMul(This, Digit)
        END IF
    ELSEIF ((This%Length <= Karatsuba_Threshold).OR.(Other%Length <= Karatsuba_Threshold)) THEN
        ! perform multiplication using grade-school algorithm
        CALL Multiply_Small(This, Other)
    ELSE
        ! perform multiplication using Karatsuba algorithm
        CALL Multiply_Big(This, Other)
    END IF

    RETURN
    CONTAINS

    SUBROUTINE Multiply_Small(This, Other)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply This by Other using a quadratic algorithm
        ! which is often suitable for smaller numbers.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT) :: This
        TYPE(ApInt64), INTENT(IN)    :: Other

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! if length is greater than the threshold and This == Other, use 'Squaring' algorithm
        tIndex, PARAMETER   :: Square_Threshold = 16

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: ThisDig(0:This%Length-1)
        tIndex      :: ThisLen

    !** FLOW

        ! perform multiplication
        This%Sign = This%Sign*Other%Sign
        ThisLen   = This%Length
        ThisDig(0:ThisLen-1) = This%Digit(0:ThisLen-1)
        IF (SIZE(This%Digit, KIND=kIndex) < ThisLen+Other%Length) THEN
            CALL MemAlloc(This%Digit, ThisLen+Other%Length, StartID=0_kIndex)
        END IF
        IF (This == Other) THEN
            IF (ThisLen <= Square_Threshold) THEN
                CALL Multiply_Basic(ThisDig, ThisLen, ThisDig, ThisLen, This%Digit)
            ELSE
                CALL Square_Basic_NoAlloc(ThisDig, ThisLen, This%Digit)
            END IF
        ELSE
            IF (ThisLen < Other%Length) THEN
                CALL Multiply_Basic(ThisDig, ThisLen, Other%Digit, Other%Length, This%Digit)
            ELSE
                CALL Multiply_Basic(Other%Digit, Other%Length, ThisDig, ThisLen, This%Digit)
            END IF
        END IF
        This%Length = ThisLen+Other%Length
        IF (This%Digit(This%Length-1) == 0_kLong) This%Length = This%Length - 1_kIndex

        RETURN

    END SUBROUTINE Multiply_Small

    !******************************************************************************

    SUBROUTINE Multiply_Big(This, Other)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To multiply This by Other using the Karatsuba algorithm.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64),         INTENT(INOUT)    :: This
        TYPE(ApInt64), TARGET, INTENT(IN)       :: Other

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong, ALLOCATABLE  :: XDigit(:)
        tLong, POINTER      :: YDigit(:)
        tIndex              :: I, MaxLen
        tLogical            :: FreeY

    !** FLOW

        ! process input
        MaxLen = MAX(This%Length, Other%Length)
        CALL MemAlloc(XDigit, MaxLen, StartID=0_kIndex)
        XDigit(0:This%Length-1) = This%Digit(0:This%Length-1)
        IF (SIZE(Other%Digit, KIND=kIndex) < MaxLen) THEN
            CALL MemAlloc(YDigit, MaxLen, StartID=0_kIndex)
            YDigit(0:Other%Length-1) = Other%Digit(0:Other%Length-1)
            FreeY = TrueVal
        ELSE
            YDigit => Other%Digit
            FreeY = FalseVal
        END IF
        IF (This%Length  < MaxLen) XDigit(This%Length:MaxLen - 1)  = 0_kLong
        IF (Other%Length < MaxLen) YDigit(Other%Length:MaxLen - 1) = 0_kLong
        ! perform multiplication
        IF (SIZE(This%Digit, KIND=kIndex) < SHIFTL(MaxLen, 1)) THEN
            CALL MemAlloc(This%Digit, SHIFTL(MaxLen, 1), StartID=0_kIndex)
        END IF
        CALL Multiply_Karatsuba_NoAlloc(XDigit, YDigit, 0_kIndex, MaxLen, This%Digit)
        ! process result
        This%Length = This%Length + Other%Length
        DO WHILE (This%Digit(This%Length-1_kIndex) == 0_kLong)
            This%Length = This%Length - 1_kIndex
        END DO
        This%Sign = This%Sign*Other%Sign
        ! free working variables
        CALL MemFree(XDigit)
        IF (FreeY) THEN
            CALL MemFree(YDigit)
        ELSE
            NULLIFY(YDigit)
        END IF

        RETURN

    END SUBROUTINE Multiply_Big

    !******************************************************************************

END SUBROUTINE ApInt64_Times_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Multiply_I32(Big, I32) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = Big * I32

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Multiply(I32)

    RETURN

END FUNCTION ApInt64_Multiply_I32

!******************************************************************************

MODULE FUNCTION I32_Multiply_ApInt64(I32, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = I32 * Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger,      INTENT(IN)   :: I32
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Multiply(I32)

    RETURN

END FUNCTION I32_Multiply_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Multiply_I64(Big, I64) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = Big * I64

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Multiply(I64)

    RETURN

END FUNCTION ApInt64_Multiply_I64

!******************************************************************************

MODULE FUNCTION I64_Multiply_ApInt64(I64, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = I64 * Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,         INTENT(IN)   :: I64
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+1_kIndex)
    CALL OutVal%Multiply(I64)

    RETURN

END FUNCTION I64_Multiply_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Multiply_I128(Big, I128) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = Big * I128

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+2_kIndex)
    CALL OutVal%Multiply(I128)

    RETURN

END FUNCTION ApInt64_Multiply_I128

!******************************************************************************

MODULE FUNCTION I128_Multiply_ApInt64(I128, Big) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication:  OutVal = I128 * Big

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(SInt128), INTENT(IN)   :: I128
    TYPE(ApInt64), INTENT(IN)   :: Big
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(Big, Big%Length+2_kIndex)
    CALL OutVal%Multiply(I128)

    RETURN

END FUNCTION I128_Multiply_ApInt64

!******************************************************************************

MODULE FUNCTION ApInt64_Multiply_ApInt64(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication: OutVal = LhsVal * RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LhsVal
    TYPE(ApInt64), INTENT(IN)   :: RhsVal
    TYPE(ApInt64)               :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is greater than the threshold, use Karatsuba algorithm
    tIndex, PARAMETER   :: Karatsuba_Threshold = 64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check and returun quickly if possible
    IF (IsZero(LhsVal)) THEN
        OutVal = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(RhsVal)) THEN
        OutVal = ZeroApInt64()
        RETURN
    END IF

    IF ((LhsVal%Length <= 1_kIndex).OR.(RhsVal%Length <= 1_kIndex)) THEN
        IF (RhsVal%Length == 1) THEN
            OutVal%Length = LhsVal%Length
            CALL MemAlloc(OutVal%Digit, LhsVal%Length+RhsVal%Length, StartID=0_kIndex)
            OutVal%Digit(0:OutVal%Length-1) = LhsVal%Digit(0:OutVal%Length-1)
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            CALL UMul(OutVal, RhsVal%Digit(0))
        ELSEIF (LhsVal%Length == 1) THEN
            OutVal%Length = RhsVal%Length
            CALL MemAlloc(OutVal%Digit, LhsVal%Length+RhsVal%Length, StartID=0_kIndex)
            OutVal%Digit(0:OutVal%Length-1) = RhsVal%Digit(0:OutVal%Length-1)
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            CALL UMul(OutVal, LhsVal%Digit(0))
        END IF
    ELSEIF ((LhsVal%Length <= Karatsuba_Threshold).OR.(RhsVal%Length <= Karatsuba_Threshold)) THEN
        ! perform multiplication using grade-school algorithm
        OutVal = Multiply_Small(LhsVal, RhsVal)
    ELSE
        ! perform multiplication using Karatsuba algorithm
        OutVal = Multiply_Big(LhsVal, RhsVal)
    END IF

    RETURN
    CONTAINS

    FUNCTION Multiply_Small(LhsVal, RhsVal) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal, using grade-school algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)   :: LhsVal
        TYPE(ApInt64), INTENT(IN)   :: RhsVal
        TYPE(ApInt64)               :: OutVal

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! if length is greater than the threshold and LhsVal == RhsVal, use 'Squaring' algorithm
        tIndex, PARAMETER   :: Square_Threshold = 16

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: OutSize

    !** FLOW

        IF (LhsVal == RhsVal) THEN
            ! allocate storage of output
            OutSize = SHIFTL(LhsVal%Length, 1)
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length <= Square_Threshold) THEN
                CALL Multiply_Basic(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                                    OutVal%Digit)
            ELSE
                CALL Square_Basic_NoAlloc(LhsVal%Digit, LhsVal%Length, OutVal%Digit)
            END IF
            ! set sign and length
            OutVal%Sign = 1
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        ELSE
            ! allocate storage of output
            OutSize = LhsVal%Length+RhsVal%Length
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length < RhsVal%Length) THEN
                CALL Multiply_Basic(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                                    OutVal%Digit)
            ELSE
                CALL Multiply_Basic(RhsVal%Digit, RhsVal%Length, LhsVal%Digit, LhsVal%Length, &
                                    OutVal%Digit)
            END IF
            ! set sign and length
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        END IF

        RETURN

    END FUNCTION Multiply_Small

    !**************************************************************************

    FUNCTION Multiply_Big(LhsVal, RhsVal) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal, using Karatsuba algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), TARGET, INTENT(IN)   :: LhsVal
        TYPE(ApInt64), TARGET, INTENT(IN)   :: RhsVal
        TYPE(ApInt64)                       :: OutVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong, POINTER  :: XDigit(:)
        tLong, POINTER  :: YDigit(:)
        tIndex          :: MaxLen
        tLogical        :: FreeX, FreeY

    !** FLOW

        ! process input
        MaxLen = MAX(LhsVal%Length, RhsVal%Length)
        IF (SIZE(LhsVal%Digit, KIND=kIndex) < MaxLen) THEN
            CALL MemAlloc(XDigit, MaxLen, StartID=0_kIndex)
            XDigit(0:LhsVal%Length-1) = LhsVal%Digit(0:LhsVal%Length-1)
            FreeX = TrueVal
        ELSE
            XDigit => LhsVal%Digit
            FreeX = FalseVal
        END IF
        IF (SIZE(RhsVal%Digit, KIND=kIndex) < MaxLen) THEN
            CALL MemAlloc(YDigit, MaxLen, StartID=0_kIndex)
            YDigit(0:RhsVal%Length-1) = RhsVal%Digit(0:RhsVal%Length-1)
            FreeY = TrueVal
        ELSE
            YDigit => RhsVal%Digit
            FreeY = FalseVal
        END IF
        IF (LhsVal%Length < MaxLen) XDigit(LhsVal%Length:MaxLen - 1) = 0_kLong
        IF (RhsVal%Length < MaxLen) YDigit(RhsVal%Length:MaxLen - 1) = 0_kLong
        ! perform multiplication
!        CALL Multiply_Karatsuba_Alloc(XDigit, YDigit, 0_kIndex, MaxLen, OutVal%Digit)
        CALL Multiply_Karatsuba_InLine(XDigit, YDigit, 0_kIndex, MaxLen, OutVal%Digit)
        ! process result
        OutVal%Length = LhsVal%Length + RhsVal%Length
        DO WHILE (OutVal%Digit(OutVal%Length-1_kIndex) == 0_kLong)
            OutVal%Length = OutVal%Length - 1_kIndex
        END DO
        OutVal%Sign = LhsVal%Sign*RhsVal%Sign
        ! free pointers
        IF (FreeX) THEN
            CALL MemFree(XDigit)
        ELSE
            NULLIFY(XDigit)
        END IF
        IF (FreeY) THEN
            CALL MemFree(YDigit)
        ELSE
            NULLIFY(YDigit)
        END IF

        RETURN

    END FUNCTION Multiply_Big

    !**************************************************************************

END FUNCTION ApInt64_Multiply_ApInt64

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                    SUPPORTING ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Multiply_Karatsuba_NoAlloc(X, Y, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,  INTENT(IN)  :: X(0:)        ! The first magnitude array
    tLong,  INTENT(IN)  :: Y(0:)        ! The second magnitude array
    tIndex, INTENT(IN)  :: Off          ! The offset, where the first element is residing
    tIndex, INTENT(IN)  :: N            ! The length of each of the two partial arrays
    tLong,  INTENT(OUT) :: Z(0:2*N-1)   ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: B, C
    tLong, ALLOCATABLE  :: X2(:), Y2(:)
    tLong, ALLOCATABLE  :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN  ! Basecase
        CALL Multiply_Basic(X(Off:), N, Y(Off:), N, Z)
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Multiply_Karatsuba_Alloc(X, Y, Off+B, N-B, Z2)
    CALL Multiply_Karatsuba_Alloc(X, Y, Off, B, Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Y2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0_kLong
    Y2 = 0_kLong
    ! set X2 and Y2
    CALL AddHiLoParts(X, N, B, Off, X2)
    CALL AddHiLoParts(Y, N, B, Off, Y2)

    C = 0_kIndex
    IF ((X2(N-B) /= 0_kLong).OR.(Y2(N-B) /= 0_kLong)) C = 1_kIndex
    CALL Multiply_Karatsuba_Alloc(X2, Y2, 0_kIndex, N-B+C, Z1)

    ! set Z
    CALL AddThreeParts(Z0, Z1, Z2, N, B, Z)

    RETURN

END SUBROUTINE Multiply_Karatsuba_NoAlloc

!******************************************************************************

RECURSIVE SUBROUTINE Multiply_Karatsuba_Alloc(X, Y, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,              INTENT(IN)  :: X(0:)    ! The first magnitude array
    tLong,              INTENT(IN)  :: Y(0:)    ! The second magnitude array
    tIndex,             INTENT(IN)  :: Off      ! The offset, where the first element is residing
    tIndex,             INTENT(IN)  :: N        ! The length of each of the two partial arrays
    tLong, ALLOCATABLE, INTENT(OUT) :: Z(:)     ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: B, C
    tLong, ALLOCATABLE  :: X2(:), Y2(:)
    tLong, ALLOCATABLE  :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN  ! Basecase
        CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
        Z = 0_kLong
        CALL Multiply_Basic(X(Off:), N, Y(Off:), N, Z(0:))
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Multiply_Karatsuba_Alloc(X, Y, Off+B, N-B, Z2)
    CALL Multiply_Karatsuba_Alloc(X, Y, Off, B, Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Y2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0_kLong
    Y2 = 0_kLong
    ! set X2 and Y2
    CALL AddHiLoParts(X, N, B, Off, X2)
    CALL AddHiLoParts(Y, N, B, Off, Y2)

    C = 0_kIndex
    IF ((X2(N-B) /= 0_kLong).OR.(Y2(N-B) /= 0_kLong)) C = 1_kIndex
    CALL Multiply_Karatsuba_Alloc(X2, Y2, 0_kIndex, N-B+C, Z1)

    CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
    ! set Z
    CALL AddThreeParts(Z0, Z1, Z2, N, B, Z)

    RETURN

END SUBROUTINE Multiply_Karatsuba_Alloc

!******************************************************************************

RECURSIVE SUBROUTINE Multiply_Karatsuba_InLine(X, Y, Off, N, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,              INTENT(IN)  :: X(0:)    ! The first magnitude array
    tLong,              INTENT(IN)  :: Y(0:)    ! The second magnitude array
    tIndex,             INTENT(IN)  :: Off      ! The offset, where the first element is residing
    tIndex,             INTENT(IN)  :: N        ! The length of each of the two partial arrays
    tLong, ALLOCATABLE, INTENT(OUT) :: Z(:)     ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! if length is NOT greater than the threshold, use basic algorithm
    tIndex, PARAMETER   :: Basic_Threshold = 32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong               :: CHi, CLo, OutLo, InLo
    tIndex              :: I, B, C, OffB
    tLong, ALLOCATABLE  :: X2(:), Y2(:)
    tLong, ALLOCATABLE  :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= Basic_Threshold) THEN  ! Basecase
        ALLOCATE(Z(0:2*N-1))
        Z = 0_kLong
        CALL Multiply_Basic(X(Off:), N, Y(Off:), N, Z(0:))
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL Multiply_Karatsuba_InLine(X, Y, Off+B, N-B, Z2)
    CALL Multiply_Karatsuba_InLine(X, Y, Off, B, Z0)

    ALLOCATE(X2(0:2*(N-B+1)-1))
    ALLOCATE(Y2(0:2*(N-B+1)-1))
    X2 = 0_kLong
    Y2 = 0_kLong
    ! set X2 and Y2
    OffB = Off+B
    X2(0) = X(OffB) + X(Off)
    CHi = SHIFTR(IOR(IAND(X(OffB), X(Off)), IAND(IOR(X(OffB), X(Off)), NOT(X2(0)))), 63)
    CLo = CHi
    DO I = 1, B-1
        InLo = X(OffB+I)
        OutLo = CLo + InLo
        CHi = SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        InLo = X(Off+I)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        X2(I) = OutLo
        CLo = CHi
    END DO
    IF (IAND(N, 1_kIndex) /= 0_kIndex) X2(B) = X(OffB+B)
    IF (CLo /= 0_kLong) THEN
        X2(B) = X2(B) + 1_kLong
        IF (X2(B) == 0_kLong) X2(B+1) = X2(B+1) + 1_kLong
    END IF
    Y2(0) = Y(OffB) + Y(Off)
    CHi = SHIFTR(IOR(IAND(Y(OffB), Y(Off)), IAND(IOR(Y(OffB), Y(Off)), NOT(Y2(0)))), 63)
    CLo = CHi
    DO I = 1, B-1
        InLo = Y(OffB+I)
        OutLo = CLo + InLo
        CHi = SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        InLo = Y(Off+I)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        Y2(I) = OutLo
        CLo = CHi
    END DO
    IF (IAND(N, 1_kIndex) /= 0_kIndex) Y2(B) = Y(OffB+B)
    IF (CLo /= 0_kLong) THEN
        Y2(B) = Y2(B) + 1_kLong
        IF (Y2(B) == 0_kLong) Y2(B+1) = Y2(B+1) + 1_kLong
    END IF

    C = 0_kIndex
    IF ((X2(N-B) /= 0_kLong).OR.(Y2(N-B) /= 0_kLong)) C = 1_kIndex
    CALL Multiply_Karatsuba_InLine(X2, Y2, 0_kIndex, N-B+C, Z1)

    ALLOCATE(Z(0:2*N-1))

    C = 2*B
    ! Add Z0
    Z(0:C-1)   = Z0(0:C-1)
    ! Add Z2
    Z(C:2*N-1) = Z2(0:2*(N-B)-1)
    ! Add Z1
    CLo = Z(B) + Z1(0)
    CHi = SHIFTR(IOR(IAND(Z(B), Z1(0)), IAND(IOR(Z(B), Z1(0)), NOT(CLo))), 63)
    OutLo = CLo - Z2(0)
    CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z2(0)), IAND(NOT(IEOR(CLo, Z2(0))), OutLo)), 63)
    CLo = OutLo
    OutLo = CLo - Z0(0)
    CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z0(0)), IAND(NOT(IEOR(CLo, Z0(0))), OutLo)), 63)
    Z(B) = OutLo
    CLo = CHi
    CHi = SHIFTA(CHi, 63)
    I = 1
    DO WHILE (I < C)
        InLo = Z(I+B)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo + Z1(I)
        CHi = CHi + SHIFTR(IOR(IAND(CLo, Z1(I)), IAND(IOR(CLo, Z1(I)), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo - Z2(I)
        CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z2(I)), IAND(NOT(IEOR(CLo, Z2(I))), OutLo)), 63)
        CLo = OutLo
        OutLo = CLo - Z0(I)
        CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z0(I)), IAND(NOT(IEOR(CLo, Z0(I))), OutLo)), 63)
        Z(I+B) = OutLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    DO WHILE (I < 2*(N-B))
        InLo = Z(I+B)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo + Z1(I)
        CHi = CHi + SHIFTR(IOR(IAND(CLo, Z1(I)), IAND(IOR(CLo, Z1(I)), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo - Z2(I)
        CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z2(I)), IAND(NOT(IEOR(CLo, Z2(I))), OutLo)), 63)
        Z(I+B) = OutLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    DO WHILE (I < SIZE(Z1, KIND=kIndex))
        InLo = Z(I+B)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo + Z1(I)
        CHi = CHi + SHIFTR(IOR(IAND(CLo, Z1(I)), IAND(IOR(CLo, Z1(I)), NOT(OutLo))), 63)
        Z(I+B) = OutLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    IF ((CHi /= 0_kLong).OR.(CLo /= 0_kLong)) THEN
        Z(I+B) = Z(I+B) + 1_kLong
        DO WHILE (Z(I+B) == 0_kLong)
            I = I + 1
            Z(I+B) = Z(I+B) + 1_kLong
        END DO
    END IF

    RETURN

END SUBROUTINE Multiply_Karatsuba_InLine

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                   ALTERNATIVE ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE UMul_UMul128(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the ApInt64 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The multiplier (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: High, Low
    tLong       :: Carry
    tIndex      :: I

!** FLOW

    IF (U64 == 0_kLong) THEN
        Big = ZeroApInt64()
        RETURN
    END IF

    Carry = 0_kLong
    DO I = 0_kIndex, Big%Length-1_kIndex
        CALL UMul128(Big%Digit(I), U64, High, Low)
        Big%Digit(I) = Low + Carry
        IF (Big%Digit(I) .ULT. Low) THEN
            Carry = High + 1_kLong
        ELSE
            Carry = High
        END IF
    END DO
    IF (Carry /= 0_kLong) THEN
        IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
            CALL MemResize(Big%Digit, Big%Length*2_kIndex)
        END IF
        Big%Digit(Big%Length) = Carry
        Big%Length = Big%Length + 1_kIndex
    END IF

    RETURN

END SUBROUTINE UMul_UMul128

!******************************************************************************

SUBROUTINE MulB_UMul128(U, ULen, V, VLen, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two magnitude arrays and return the result using grade-school algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: ULen             ! The length of the first array
    tLong,  INTENT(IN)  :: U(0:ULen-1)      ! The first magnitude array
    tIndex, INTENT(IN)  :: VLen             ! The length of the second array
    tLong,  INTENT(IN)  :: V(0:VLen-1)      ! The second magnitude array
    tLong,  INTENT(OUT) :: R(0:ULen+VLen-1) ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: Carry64, ProductHi, ProductLo, Sum
    tIndex      :: I, J

!** FLOW

    Carry64 = 0_kLong
    DO J = 0, VLen-1
        ! compute 128-bit result of multiplication of two 64-bit unsigned integers
        CALL UMul128(U(0), V(J), ProductHi, ProductLo)
        R(J) = ProductLo + Carry64
        IF (IEOR(R(J), MinI64) < IEOR(ProductLo, MinI64)) THEN
            Carry64 = ProductHi + 1_kLong
        ELSE
            Carry64 = ProductHi
        END IF
    END DO
    R(VLen) = Carry64
    DO I = 1, ULen-1
        Carry64 = 0_kLong
        DO J = 0, VLen-1
            ! compute 128-bit result of multiplication of two 64-bit unsigned integers
            CALL UMul128(U(I), V(J), ProductHi, ProductLo)

            Sum = ProductLo + R(I+J)
            IF (IEOR(Sum, MinI64) < IEOR(ProductLo, MinI64)) ProductHi = ProductHi + 1_kLong
            R(I+J) = Sum + Carry64
            IF (IEOR(R(I+J), MinI64) < IEOR(Sum, MinI64)) THEN
                Carry64 = ProductHi + 1_kLong
            ELSE
                Carry64 = ProductHi
            END IF
        END DO
        R(I+VLen) = Carry64
    END DO

    RETURN

END SUBROUTINE MulB_UMul128

!******************************************************************************

SUBROUTINE UMul_SInt128(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the BigI64 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The multiplier (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)   :: Carry, Multiplier
    tIndex          :: I

!** FLOW

    IF (U64 == 0_kLong) THEN
        Big = ZeroApInt64()
        RETURN
    END IF

    Carry = ZeroI128
    Multiplier = SInt128(U64, Positive)
    DO I = 0_kIndex, Big%Length-1_kIndex
        Carry = SInt128(Big%Digit(I), Positive)*Multiplier + Carry
        Big%Digit(I) = ToU64(Carry)
        Carry = ShiftR64(Carry)
    END DO
    IF (Carry /= ZeroI128) THEN
        IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
            CALL MemResize(Big%Digit, Big%Length*2_kIndex)
        END IF
        Big%Digit(Big%Length) = ToU64(Carry)
        Big%Length = Big%Length + 1_kIndex
    END IF

    RETURN

END SUBROUTINE UMul_SInt128

!******************************************************************************

SUBROUTINE MulB_SInt128(U, ULen, V, VLen, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two magnitude arrays and return the result using grade-school algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: ULen             ! The length of the first array
    tLong,  INTENT(IN)  :: U(0:ULen-1)      ! The first magnitude array
    tIndex, INTENT(IN)  :: VLen             ! The length of the second array
    tLong,  INTENT(IN)  :: V(0:VLen-1)      ! The second magnitude array
    tLong,  INTENT(OUT) :: R(0:ULen+VLen-1) ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)   :: Carry, Tmp, UI
    tIndex          :: I, J

!** FLOW

    Carry = ZeroI128
    UI = SInt128(U(0), Positive)
    DO J = 0, VLen-1
        Tmp = SInt128(V(J), Positive)*UI + Carry
        R(J) = ToU64(Tmp)
        Carry = ShiftR64(Tmp)
    END DO
    R(VLen) = ToU64(Carry)
    DO I = 1, ULen-1
        Carry = ZeroI128
        UI = SInt128(U(I), Positive)
        DO J = 0, VLen-1
            Tmp = SInt128(V(J), Positive)*UI + SInt128(R(I+J), Positive) + Carry
            R(I+J) = ToU64(Tmp)
            Carry = ShiftR64(Tmp)
        END DO
        R(I+VLen) = ToU64(Carry)
    END DO

    RETURN

END SUBROUTINE MulB_SInt128

!******************************************************************************

SUBROUTINE UMul_UInt128(Big, U64)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the BigI64 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The multiplier (treated as unsigned)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: Carry
    tIndex          :: I

!** FLOW

    IF (U64 == 0_kLong) THEN
        Big = ZeroApInt64()
        RETURN
    END IF

    Carry = ZeroU128
    DO I = 0_kIndex, Big%Length-1_kIndex
        Carry = UInt128(0_kLong, Big%Digit(I))*U64 + Carry
        Big%Digit(I) = Carry
        Carry = ShiftR64(Carry)
    END DO
    IF (Carry /= ZeroU128) THEN
        IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
            CALL MemResize(Big%Digit, Big%Length*2_kIndex)
        END IF
        Big%Digit(Big%Length) = Carry
        Big%Length = Big%Length + 1_kIndex
    END IF

    RETURN

END SUBROUTINE UMul_UInt128

!******************************************************************************

SUBROUTINE MulB_UInt128(U, ULen, V, VLen, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two magnitude arrays and return the result using grade-school algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex, INTENT(IN)  :: ULen             ! The length of the first array
    tLong,  INTENT(IN)  :: U(0:ULen-1)      ! The first magnitude array
    tIndex, INTENT(IN)  :: VLen             ! The length of the second array
    tLong,  INTENT(IN)  :: V(0:VLen-1)      ! The second magnitude array
    tLong,  INTENT(OUT) :: R(0:ULen+VLen-1) ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: Carry, Tmp
    tIndex          :: I, J

!** FLOW

    Carry = ZeroU128
    DO J = 0, VLen-1
        Tmp = UInt128(0_kLong, V(J))*U(0) + Carry
        R(J) = Tmp
        Carry = ShiftR64(Tmp)
    END DO
    R(VLen) = Carry
    DO I = 1, ULen-1
        Carry = ZeroU128
        DO J = 0, VLen-1
            Tmp = UInt128(0_kLong, V(J))*U(I) + UInt128(0_kLong, R(I+J)) + Carry
            R(I+J) = Tmp
            Carry = ShiftR64(Tmp)
        END DO
        R(I+VLen) = Carry
    END DO

    RETURN

END SUBROUTINE MulB_UInt128

!******************************************************************************

RECURSIVE FUNCTION MulK_JDK_I(X, Y, BaseCut) RESULT(Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication (Z = X*Y) using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: X
    TYPE(ApInt64), INTENT(IN)   :: Y
    TYPE(ApInt64)               :: Z
    tIndex,        INTENT(IN)   :: BaseCut  ! threshold to perform basic algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(ApInt64)   :: XLo, XHi, YLo, YHi
    TYPE(ApInt64)   :: ZLo, ZMd, ZHi
    tIndex          :: Half, Full
    tInteger        :: Shift
    tIndex          :: BasicThreshold

!** FLOW

    Full = MAX(X%Length, Y%Length)
    BasicThreshold = BaseCut
    IF (BasicThreshold < 16) BasicThreshold = 16

    ! if length is small, use grade-school algorithm
    IF (Full <= BasicThreshold) THEN
        Z = Multiply_Small(X, Y)
        RETURN
    END IF

    ! otherwise, use Karatsuba algorithm
    Half = SHIFTR(Full, 1) + 1
    XLo = ApInt64(X%Sign, Half, X%Digit(0:Half-1))
    XHi = ApInt64(X%Sign, Full-Half, X%Digit(Half:Full-1))
    YLo = ApInt64(Y%Sign, Half, Y%Digit(0:Half-1))
    YHi = ApInt64(Y%Sign, Full-Half, Y%Digit(Half:Full-1))

    ZHi = MulK_JDK_I(XHi, YHi, BasicThreshold)
    ZLo = MulK_JDK_I(XLo, YLo, BasicThreshold)
    ZMd = MulK_JDK_I((XHi + XLo), (YHi + YLo), BasicThreshold) - (ZHi + ZLo)

    Shift = ToInteger(Half)
    Z = LShiftLarge(LShiftLarge(ZHi, Shift) + ZMd, Shift) + ZLo
    IF (X%Sign /= Y%Sign) Z%Sign = -Z%Sign

    RETURN

    CONTAINS

    FUNCTION Multiply_Small(LhsVal, RhsVal) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal, using grade-school algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)   :: LhsVal
        TYPE(ApInt64), INTENT(IN)   :: RhsVal
        TYPE(ApInt64)               :: OutVal

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! if length is greater than the threshold and This == Other, use 'Squaring' algorithm
        tIndex, PARAMETER   :: Square_Threshold = 16

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: OutSize

    !** FLOW

        IF (LhsVal == RhsVal) THEN
            ! allocate storage of output
            OutSize = SHIFTL(LhsVal%Length, 1)
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length <= Square_Threshold) THEN
                CALL Multiply_Basic(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                                    OutVal%Digit)
            ELSE
                CALL Square_Basic_NoAlloc(LhsVal%Digit, LhsVal%Length, OutVal%Digit)
            END IF
            ! set sign and length
            OutVal%Sign = 1
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        ELSE
            ! allocate storage of output
            OutSize = LhsVal%Length+RhsVal%Length
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length < RhsVal%Length) THEN
                CALL Multiply_Basic(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                                    OutVal%Digit)
            ELSE
                CALL Multiply_Basic(RhsVal%Digit, RhsVal%Length, LhsVal%Digit, LhsVal%Length, &
                                    OutVal%Digit)
            END IF
            ! set sign and length
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        END IF

        RETURN

    END FUNCTION Multiply_Small

    !**************************************************************************

    FUNCTION LShiftLarge(InVal, Shift) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift the ApInt64 number left by 64*shift, i.e. moves each
        ! digit shift positions to the left.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)   :: InVal
        tInteger,      INTENT(IN)   :: Shift    ! The amount to shift
        TYPE(ApInt64)               :: OutVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Index

    !** FLOW

        OutVal = MakeCopy(InVal, InVal%Length + Shift)
        DO Index = OutVal%Length-1, 0, -1
            OutVal%Digit(Shift+Index) = OutVal%Digit(Index)
        END DO
        OutVal%Digit(0:Shift-1) = 0_kLong
        OutVal%Length = OutVal%Length + Shift

        RETURN

    END FUNCTION LShiftLarge

    !**************************************************************************

END FUNCTION MulK_JDK_I

!******************************************************************************

RECURSIVE FUNCTION MulK_JDK_II(X, Y, BaseCut) RESULT(Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication (Z = X*Y) using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: X
    TYPE(ApInt64), INTENT(IN)   :: Y
    TYPE(ApInt64)               :: Z
    tIndex,        INTENT(IN)   :: BaseCut  ! threshold to perform basic algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(ApInt64)   :: XLo, XHi, YLo, YHi
    TYPE(ApInt64)   :: ZLo, ZMd, ZHi
    tIndex          :: Half, Full
    tInteger        :: Shift
    tIndex          :: BasicThreshold

!** FLOW

    Full = MAX(X%Length, Y%Length)
    BasicThreshold = BaseCut
    IF (BasicThreshold < 16) BasicThreshold = 16

    ! if length is small, use grade-school algorithm
    IF (Full <= BasicThreshold) THEN
        Z = Multiply_Small(X, Y)
        RETURN
    END IF

    ! otherwise, use Karatsuba algorithm
    Half = SHIFTR(Full, 1) + 1
    XLo = ApInt64(X%Sign, Half, X%Digit(0:Half-1))
    XHi = ApInt64(X%Sign, Full-Half, X%Digit(Half:Full-1))
    YLo = ApInt64(Y%Sign, Half, Y%Digit(0:Half-1))
    YHi = ApInt64(Y%Sign, Full-Half, Y%Digit(Half:Full-1))

    ZHi = MulK_JDK_II(XHi, YHi, BasicThreshold)
    ZLo = MulK_JDK_II(XLo, YLo, BasicThreshold)
    CALL XHi%Add(XLo)
    CALL YHi%Add(YLo)
    ZMd = MulK_JDK_II(XHi, YHi, BasicThreshold)
    CALL ZMd%Subtract(ZHi)
    CALL ZMd%Subtract(ZLo)

    Shift = ToInteger(Half)
    Z = MakeCopy(ZHi, 2*Full)
    CALL LShiftLarge(Z, Shift)
    CALL Z%Add(ZMd)
    CALL LShiftLarge(Z, Shift)
    CALL Z%Add(ZLo)
    IF (X%Sign /= Y%Sign) Z%Sign = -Z%Sign

    RETURN

    CONTAINS

    FUNCTION Multiply_Small(LhsVal, RhsVal) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal, using grade-school algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)   :: LhsVal
        TYPE(ApInt64), INTENT(IN)   :: RhsVal
        TYPE(ApInt64)               :: OutVal

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! if length is greater than the threshold and This == Other, use 'Squaring' algorithm
        tIndex, PARAMETER   :: Square_Threshold = 16

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: OutSize

    !** FLOW

        IF (LhsVal == RhsVal) THEN
            ! allocate storage of output
            OutSize = SHIFTL(LhsVal%Length, 1)
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length <= Square_Threshold) THEN
                CALL Multiply_Basic(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                                    OutVal%Digit)
            ELSE
                CALL Square_Basic_NoAlloc(LhsVal%Digit, LhsVal%Length, OutVal%Digit)
            END IF
            ! set sign and length
            OutVal%Sign = 1
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        ELSE
            ! allocate storage of output
            OutSize = LhsVal%Length+RhsVal%Length
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length < RhsVal%Length) THEN
                CALL Multiply_Basic(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                                    OutVal%Digit)
            ELSE
                CALL Multiply_Basic(RhsVal%Digit, RhsVal%Length, LhsVal%Digit, LhsVal%Length, &
                                    OutVal%Digit)
            END IF
            ! set sign and length
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        END IF

        RETURN

    END FUNCTION Multiply_Small

    !**************************************************************************

    SUBROUTINE LShiftLarge(Val, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift the ApInt64 number left by 64*shift, i.e. moves each
        ! digit shift positions to the left.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(INOUT)    :: Val
        tInteger,      INTENT(IN)       :: Shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: Index

    !** FLOW

        DO Index = Val%Length-1, 0, -1
            Val%Digit(Shift+Index) = Val%Digit(Index)
        END DO
        Val%Digit(0:Shift-1) = 0_kLong
        Val%Length = Val%Length + Shift

        RETURN

    END SUBROUTINE LShiftLarge

    !**************************************************************************

END FUNCTION MulK_JDK_II

!******************************************************************************

RECURSIVE SUBROUTINE KMul_SInt128(X, Y, Off, N, BaseCut, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,              INTENT(IN)  :: X(0:)    ! The first magnitude array
    tLong,              INTENT(IN)  :: Y(0:)    ! The second magnitude array
    tIndex,             INTENT(IN)  :: Off      ! The offset, where the first element is residing
    tIndex,             INTENT(IN)  :: N        ! The length of each of the two partial arrays
    tIndex,             INTENT(IN)  :: BaseCut  ! threshold to perform basic algorithm
    tLong, ALLOCATABLE, INTENT(OUT) :: Z(:)     ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(SInt128)       :: Carry
    tIndex              :: I, B, C
    tLong, ALLOCATABLE  :: X2(:), Y2(:)
    tLong, ALLOCATABLE  :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= BaseCut) THEN   ! Basecase
        CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
        Z = 0_kLong
        CALL Multiply_Basic(X(Off:), N, Y(Off:), N, Z(0:))
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL KMul_SInt128(X, Y, Off+B, N-B, BaseCut, Z2)
    CALL KMul_SInt128(X, Y, Off, B, BaseCut, Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Y2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0_kLong
    Y2 = 0_kLong
    ! set X2 and Y2
    CALL Add_HiLo_Parts(X, N, B, Off, X2)
    CALL Add_HiLo_Parts(Y, N, B, Off, Y2)

    C = 0_kIndex
    IF ((X2(N-B) /= 0_kLong).OR.(Y2(N-B) /= 0_kLong)) C = 1_kIndex
    CALL KMul_SInt128(X2, Y2, 0_kIndex, N-B+C, BaseCut, Z1)

    CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
    Z = 0_kLong

    C = 2*B
    ! Add Z0
    Z(0:C-1)   = Z0(0:C-1)
    ! Add Z2
    Z(C:2*N-1) = Z2(0:2*(N-B)-1)
    ! Add Z1
    Carry = ZeroI128
    I = 0
    DO WHILE (I < C)
        Carry  = SInt128(Z(I+B), Positive) + SInt128(Z1(I), Positive) - &
                 SInt128(Z2(I), Positive) - SInt128(Z0(I), Positive) + Carry
        Z(I+B) = ToU64(Carry)
        Carry  = ShiftA64(Carry)
        I = I + 1
    END DO
    DO WHILE (I < 2*(N-B))
        Carry  = SInt128(Z(I+B), Positive) + SInt128(Z1(I), Positive) - &
                 SInt128(Z2(I), Positive) + Carry
        Z(I+B) = ToU64(Carry)
        Carry  = ShiftA64(Carry)
        I = I + 1
    END DO
    DO WHILE (I < SIZE(Z1, KIND=kIndex))
        Carry  = SInt128(Z(I+B), Positive) + SInt128(Z1(I), Positive) + Carry
        Z(I+B) = ToU64(Carry)
        Carry  = ShiftA64(Carry)
        I = I + 1
    END DO
    IF (.NOT.IsZero(Carry)) THEN
        Z(I+B) = Z(I+B) + 1_kLong
        DO WHILE (Z(I+B) == 0_kLong)
            I = I + 1
            Z(I+B) = Z(I+B) + 1_kLong
        END DO
    END IF

    RETURN
    CONTAINS

    SUBROUTINE Add_HiLo_Parts(X1, L, K, Offset, X2)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,  INTENT(IN)  :: X1(0:)
        tIndex, INTENT(IN)  :: L
        tIndex, INTENT(IN)  :: K
        tIndex, INTENT(IN)  :: Offset
        tLong,  INTENT(OUT) :: X2(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        TYPE(SInt128)   :: Carry
        tIndex          :: I

    !** FLOW

        Carry = ZeroI128
        DO I = 0, K-1
            Carry = SInt128(X1(Offset+K+I), Positive) + SInt128(X1(Offset+I), Positive) + Carry
            X2(I) = ToU64(Carry)
            Carry = ShiftR64(Carry)
        END DO
        IF (IAND(L, 1_kIndex) /= 0_kIndex) X2(K) = X1(Offset+K+K)
        IF (.NOT.IsZero(Carry)) THEN
            X2(K) = X2(K) + 1_kLong
            IF (X2(K) == 0_kLong) X2(K+1) = X2(K+1) + 1_kLong
        END IF

        RETURN

    END SUBROUTINE Add_HiLo_Parts

    !**************************************************************************

END SUBROUTINE KMul_SInt128

!******************************************************************************

RECURSIVE SUBROUTINE KMul_Alloc(X, Y, Off, N, BaseCut, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,              INTENT(IN)  :: X(0:)    ! The first magnitude array
    tLong,              INTENT(IN)  :: Y(0:)    ! The second magnitude array
    tIndex,             INTENT(IN)  :: Off      ! The offset, where the first element is residing
    tIndex,             INTENT(IN)  :: N        ! The length of each of the two partial arrays
    tIndex,             INTENT(IN)  :: BaseCut  ! threshold to perform basic algorithm
    tLong, ALLOCATABLE, INTENT(OUT) :: Z(:)     ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex              :: B, C
    tLong, ALLOCATABLE  :: X2(:), Y2(:)
    tLong, ALLOCATABLE  :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= BaseCut) THEN   ! Base case
        CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
        Z = 0_kLong
        CALL Multiply_Basic(X(Off:), N, Y(Off:), N, Z(0:))
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL KMul_Alloc(X, Y, Off+B, N-B, BaseCut, Z2)
    CALL KMul_Alloc(X, Y, Off, B, BaseCut, Z0)

    CALL MemAlloc(X2, N-B+1_kIndex, StartID=0_kIndex)
    CALL MemAlloc(Y2, N-B+1_kIndex, StartID=0_kIndex)
    X2 = 0_kLong
    Y2 = 0_kLong
    ! set X2 and Y2
    CALL AddHiLoParts(X, N, B, Off, X2)
    CALL AddHiLoParts(Y, N, B, Off, Y2)

    C = 0_kIndex
    IF ((X2(N-B) /= 0_kLong).OR.(Y2(N-B) /= 0_kLong)) C = 1_kIndex
    CALL KMul_Alloc(X2, Y2, 0_kIndex, N-B+C, BaseCut, Z1)

    CALL MemAlloc(Z, 2_kIndex*N, StartID=0_kIndex)
    ! set Z
    CALL AddThreeParts(Z0, Z1, Z2, N, B, Z)

    RETURN

END SUBROUTINE KMul_Alloc

!******************************************************************************

RECURSIVE SUBROUTINE KMul_InLine(X, Y, Off, N, BaseCut, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,              INTENT(IN)  :: X(0:)    ! The first magnitude array
    tLong,              INTENT(IN)  :: Y(0:)    ! The second magnitude array
    tIndex,             INTENT(IN)  :: Off      ! The offset, where the first element is residing
    tIndex,             INTENT(IN)  :: N        ! The length of each of the two partial arrays
    tIndex,             INTENT(IN)  :: BaseCut  ! threshold to perform basic algorithm
    tLong, ALLOCATABLE, INTENT(OUT) :: Z(:)     ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong               :: CHi, CLo, OutLo, InLo
    tIndex              :: I, B, C, OffB
    tLong, ALLOCATABLE  :: X2(:), Y2(:)
    tLong, ALLOCATABLE  :: Z0(:), Z1(:), Z2(:)

!** FLOW

    IF (N <= BaseCut) THEN   ! Base case
        ALLOCATE(Z(0:2*N-1))
        Z = 0_kLong
        CALL Multiply_Basic(X(Off:), N, Y(Off:), N, Z(0:))
        RETURN
    END IF

    B = SHIFTR(N, 1)
    CALL KMul_InLine(X, Y, Off+B, N-B, BaseCut, Z2)
    CALL KMul_InLine(X, Y, Off, B, BaseCut, Z0)

    ALLOCATE(X2(0:2*(N-B+1)-1))
    ALLOCATE(Y2(0:2*(N-B+1)-1))
    X2 = 0_kLong
    Y2 = 0_kLong
    ! set X2 and Y2
    OffB = Off+B
    X2(0) = X(OffB) + X(Off)
    CHi = SHIFTR(IOR(IAND(X(OffB), X(Off)), IAND(IOR(X(OffB), X(Off)), NOT(X2(0)))), 63)
    CLo = CHi
    DO I = 1, B-1
        InLo = X(OffB+I)
        OutLo = CLo + InLo
        CHi = SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        InLo = X(Off+I)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        X2(I) = OutLo
        CLo = CHi
    END DO
    IF (IAND(N, 1_kIndex) /= 0_kIndex) X2(B) = X(OffB+B)
    IF (CLo /= 0_kLong) THEN
        X2(B) = X2(B) + 1_kLong
        IF (X2(B) == 0_kLong) X2(B+1) = X2(B+1) + 1_kLong
    END IF
    Y2(0) = Y(OffB) + Y(Off)
    CHi = SHIFTR(IOR(IAND(Y(OffB), Y(Off)), IAND(IOR(Y(OffB), Y(Off)), NOT(Y2(0)))), 63)
    CLo = CHi
    DO I = 1, B-1
        InLo = Y(OffB+I)
        OutLo = CLo + InLo
        CHi = SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        InLo = Y(Off+I)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        Y2(I) = OutLo
        CLo = CHi
    END DO
    IF (IAND(N, 1_kIndex) /= 0_kIndex) Y2(B) = Y(OffB+B)
    IF (CLo /= 0_kLong) THEN
        Y2(B) = Y2(B) + 1_kLong
        IF (Y2(B) == 0_kLong) Y2(B+1) = Y2(B+1) + 1_kLong
    END IF

    C = 0_kIndex
    IF ((X2(N-B) /= 0_kLong).OR.(Y2(N-B) /= 0_kLong)) C = 1_kIndex
    CALL KMul_InLine(X2, Y2, 0_kIndex, N-B+C, BaseCut, Z1)

    ALLOCATE(Z(0:2*N-1))

    C = 2*B
    ! Add Z0
    Z(0:C-1)   = Z0(0:C-1)
    ! Add Z2
    Z(C:2*N-1) = Z2(0:2*(N-B)-1)
    ! Add Z1
    CLo = Z(B) + Z1(0)
    CHi = SHIFTR(IOR(IAND(Z(B), Z1(0)), IAND(IOR(Z(B), Z1(0)), NOT(CLo))), 63)
    OutLo = CLo - Z2(0)
    CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z2(0)), IAND(NOT(IEOR(CLo, Z2(0))), OutLo)), 63)
    CLo = OutLo
    OutLo = CLo - Z0(0)
    CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z0(0)), IAND(NOT(IEOR(CLo, Z0(0))), OutLo)), 63)
    Z(B) = OutLo
    CLo = CHi
    CHi = SHIFTA(CHi, 63)
    I = 1
    DO WHILE (I < C)
        InLo = Z(I+B)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo + Z1(I)
        CHi = CHi + SHIFTR(IOR(IAND(CLo, Z1(I)), IAND(IOR(CLo, Z1(I)), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo - Z2(I)
        CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z2(I)), IAND(NOT(IEOR(CLo, Z2(I))), OutLo)), 63)
        CLo = OutLo
        OutLo = CLo - Z0(I)
        CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z0(I)), IAND(NOT(IEOR(CLo, Z0(I))), OutLo)), 63)
        Z(I+B) = OutLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    DO WHILE (I < 2*(N-B))
        InLo = Z(I+B)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo + Z1(I)
        CHi = CHi + SHIFTR(IOR(IAND(CLo, Z1(I)), IAND(IOR(CLo, Z1(I)), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo - Z2(I)
        CHi = CHi - SHIFTR(IOR(IAND(NOT(CLo), Z2(I)), IAND(NOT(IEOR(CLo, Z2(I))), OutLo)), 63)
        Z(I+B) = OutLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    DO WHILE (I < SIZE(Z1, KIND=kIndex))
        InLo = Z(I+B)
        OutLo = CLo + InLo
        CHi = CHi + SHIFTR(IOR(IAND(CLo, InLo), IAND(IOR(CLo, InLo), NOT(OutLo))), 63)
        CLo = OutLo
        OutLo = CLo + Z1(I)
        CHi = CHi + SHIFTR(IOR(IAND(CLo, Z1(I)), IAND(IOR(CLo, Z1(I)), NOT(OutLo))), 63)
        Z(I+B) = OutLo
        CLo = CHi
        CHi = SHIFTA(CHi, 63)
        I = I + 1
    END DO
    IF ((CHi /= 0_kLong).OR.(CLo /= 0_kLong)) THEN
        Z(I+B) = Z(I+B) + 1_kLong
        DO WHILE (Z(I+B) == 0_kLong)
            I = I + 1
            Z(I+B) = Z(I+B) + 1_kLong
        END DO
    END IF

    RETURN

END SUBROUTINE KMul_InLine

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!++++++++++                  EXPERIMENTAL ROUTINES                   ++++++++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE UMul_Xp(Big, U64, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply the ApInt64 by the specified integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(INOUT)    :: Big
    tLong,         INTENT(IN)       :: U64  ! The multiplier (treated as unsigned)
    tInteger,      INTENT(IN)       :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        CALL UMul_SInt128(Big, U64)
    CASE (2)
        CALL UMul_UInt128(Big, U64)
    CASE (3)
        CALL UMul_UMul128(Big, U64)
    CASE DEFAULT
        CALL UMul_U64(Big, U64)
    END SELECT

    RETURN

END SUBROUTINE UMul_Xp

!******************************************************************************

SUBROUTINE MulB_Xp(U, ULen, V, VLen, R, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two magnitude arrays and return the result using grade-school algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   INTENT(IN)    :: ULen             ! The length of the first array
    tLong,    INTENT(IN)    :: U(0:ULen-1)      ! The first magnitude array
    tIndex,   INTENT(IN)    :: VLen             ! The length of the second array
    tLong,    INTENT(IN)    :: V(0:VLen-1)      ! The second magnitude array
    tLong,    INTENT(OUT)   :: R(0:ULen+VLen-1) ! The result array
    tInteger, INTENT(IN)    :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        CALL MulB_SInt128(U, ULen, V, VLen, R)
    CASE (2)
        CALL MulB_UInt128(U, ULen, V, VLen, R)
    CASE (3)
        CALL MulB_UMul128(U, ULen, V, VLen, R)
    CASE DEFAULT
        CALL Multiply_Basic(U, ULen, V, VLen, R)
    END SELECT

    RETURN

END SUBROUTINE MulB_Xp

!******************************************************************************

SUBROUTINE KMul_Xp(X, Y, Off, N, BaseCut, Z, Algorithm)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply partial magnitude arrays X(Off:Off+N) and Y(Off:Off+N)
    ! and return the result using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong,              INTENT(IN)  :: X(0:)    ! The first magnitude array
    tLong,              INTENT(IN)  :: Y(0:)    ! The second magnitude array
    tIndex,             INTENT(IN)  :: Off      ! The offset, where the first element is residing
    tIndex,             INTENT(IN)  :: N        ! The length of each of the two partial arrays
    tIndex,             INTENT(IN)  :: BaseCut  ! threshold to perform basic algorithm
    tLong, ALLOCATABLE, INTENT(OUT) :: Z(:)     ! The result array
    tInteger,           INTENT(IN)  :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    SELECT CASE (Algorithm)
    CASE (1)
        CALL KMul_SInt128(X, Y, Off, N, BaseCut, Z)
    CASE (2)
        CALL KMul_Alloc(X, Y, Off, N, BaseCut, Z)
    CASE (3)
        CALL KMul_InLine(X, Y, Off, N, BaseCut, Z)
    CASE DEFAULT
        CALL Multiply_Karatsuba_InLine(X, Y, Off, N, Z)
    END SELECT

    RETURN

END SUBROUTINE KMul_Xp

!******************************************************************************

FUNCTION MulK_JDK_Xp(X, Y, BaseCut, Algorithm) RESULT(Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication (Z = X*Y) using the Karatsuba algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: X
    TYPE(ApInt64), INTENT(IN)   :: Y
    TYPE(ApInt64)               :: Z
    tIndex,        INTENT(IN)   :: BaseCut  ! threshold to perform basic algorithm
    tInteger,      INTENT(IN)   :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Algorithm == 1) THEN
        Z = MulK_JDK_I(X, Y, BaseCut)
    ELSE
        Z = MulK_JDK_II(X, Y, BaseCut)
    END IF

    RETURN

END FUNCTION MulK_JDK_Xp

!******************************************************************************

MODULE FUNCTION ApInt64_Multiply_Xp(LhsVal, RhsVal, KThreshold, BaseCut, Algorithm) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication: OutVal = LhsVal * RhsVal

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt64), INTENT(IN)   :: LhsVal
    TYPE(ApInt64), INTENT(IN)   :: RhsVal
    TYPE(ApInt64)               :: OutVal
    tIndex,        INTENT(IN)   :: KThreshold   ! threshold to perform Karatsuba algorithm
    tIndex,        INTENT(IN)   :: BaseCut      ! threshold to perform basic algorithm
    tInteger,      INTENT(IN)   :: Algorithm

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check and return quickly if possible
    IF (IsZero(LhsVal)) THEN
        OutVal = ZeroApInt64()
        RETURN
    ELSEIF (IsZero(RhsVal)) THEN
        OutVal = ZeroApInt64()
        RETURN
    END IF

    IF ((LhsVal%Length <= 1_kIndex).OR.(RhsVal%Length <= 1_kIndex)) THEN
        IF (RhsVal%Length == 1) THEN
            OutVal%Length = LhsVal%Length
            CALL MemAlloc(OutVal%Digit, LhsVal%Length+RhsVal%Length, StartID=0_kIndex)
            OutVal%Digit(0:OutVal%Length-1) = LhsVal%Digit(0:OutVal%Length-1)
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            CALL UMul_Xp(OutVal, RhsVal%Digit(0), Algorithm)
        ELSEIF (LhsVal%Length == 1) THEN
            OutVal%Length = RhsVal%Length
            CALL MemAlloc(OutVal%Digit, LhsVal%Length+RhsVal%Length, StartID=0_kIndex)
            OutVal%Digit(0:OutVal%Length-1) = RhsVal%Digit(0:OutVal%Length-1)
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            CALL UMul_Xp(OutVal, LhsVal%Digit(0), Algorithm)
        END IF
    ELSEIF ((LhsVal%Length <= KThreshold).OR.(RhsVal%Length <= KThreshold)) THEN
        ! perform multiplication using grade-school algorithm
        OutVal = Multiply_Small(LhsVal, RhsVal, Algorithm)
    ELSE
        ! perform multiplication using Karatsuba algorithm
        IF (Algorithm <= 4) THEN
            OutVal = Multiply_Big(LhsVal, RhsVal, BaseCut, Algorithm)
        ELSEIF (Algorithm <= 6) THEN
            OutVal = Multiply_Large(LhsVal, RhsVal, BaseCut, Algorithm-4)
        ELSE
            OutVal = Multiply_Big(LhsVal, RhsVal, BaseCut, Algorithm)
        END IF
    END IF

    RETURN
    CONTAINS

    FUNCTION Multiply_Small(LhsVal, RhsVal, Algorithm) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal, using grade-school algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), INTENT(IN)   :: LhsVal
        TYPE(ApInt64), INTENT(IN)   :: RhsVal
        TYPE(ApInt64)               :: OutVal
        tInteger,      INTENT(IN)   :: Algorithm

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! if length is greater than the threshold and LhsVal == RhsVal, use 'Squaring' algorithm
        tIndex, PARAMETER   :: Square_Threshold = 16

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: OutSize

    !** FLOW

        IF (LhsVal == RhsVal) THEN
            ! allocate storage of output
            OutSize = SHIFTL(LhsVal%Length, 1)
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length <= Square_Threshold) THEN
                CALL MulB_Xp(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                             OutVal%Digit, Algorithm)
            ELSE
                CALL Square_Basic_NoAlloc(LhsVal%Digit, LhsVal%Length, OutVal%Digit)
            END IF
            ! set sign and length
            OutVal%Sign = 1
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        ELSE
            ! allocate storage of output
            OutSize = LhsVal%Length+RhsVal%Length
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length < RhsVal%Length) THEN
                CALL MulB_Xp(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                             OutVal%Digit, Algorithm)
            ELSE
                CALL MulB_Xp(RhsVal%Digit, RhsVal%Length, LhsVal%Digit, LhsVal%Length, &
                             OutVal%Digit, Algorithm)
            END IF
            ! set sign and length
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        END IF

        RETURN

    END FUNCTION Multiply_Small

    !**************************************************************************

    FUNCTION Multiply_Big(LhsVal, RhsVal, BaseCut, Algorithm) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal, using Karatsuba algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), TARGET, INTENT(IN)   :: LhsVal
        TYPE(ApInt64), TARGET, INTENT(IN)   :: RhsVal
        TYPE(ApInt64)                       :: OutVal
        tIndex,                INTENT(IN)   :: BaseCut  ! threshold to perform basic algorithm
        tInteger,              INTENT(IN)   :: Algorithm

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong, POINTER  :: XDigit(:)
        tLong, POINTER  :: YDigit(:)
        tIndex          :: MaxLen
        tLogical        :: FreeX, FreeY

    !** FLOW

        ! process input
        MaxLen = MAX(LhsVal%Length, RhsVal%Length)
        IF (SIZE(LhsVal%Digit, KIND=kIndex) < MaxLen) THEN
            CALL MemAlloc(XDigit, MaxLen, StartID=0_kIndex)
            XDigit(0:LhsVal%Length-1) = LhsVal%Digit(0:LhsVal%Length-1)
            FreeX = TrueVal
        ELSE
            XDigit => LhsVal%Digit
            FreeX = FalseVal
        END IF
        IF (SIZE(RhsVal%Digit, KIND=kIndex) < MaxLen) THEN
            CALL MemAlloc(YDigit, MaxLen, StartID=0_kIndex)
            YDigit(0:RhsVal%Length-1) = RhsVal%Digit(0:RhsVal%Length-1)
            FreeY = TrueVal
        ELSE
            YDigit => RhsVal%Digit
            FreeY = FalseVal
        END IF
        IF (LhsVal%Length < MaxLen) XDigit(LhsVal%Length:MaxLen - 1) = 0_kLong
        IF (RhsVal%Length < MaxLen) YDigit(RhsVal%Length:MaxLen - 1) = 0_kLong
        ! perform multiplication
        CALL KMul_Xp(XDigit, YDigit, 0_kIndex, MaxLen, BaseCut, OutVal%Digit, Algorithm)
        ! process result
        OutVal%Length = LhsVal%Length + RhsVal%Length
        DO WHILE (OutVal%Digit(OutVal%Length-1_kIndex) == 0_kLong)
            OutVal%Length = OutVal%Length - 1_kIndex
        END DO
        OutVal%Sign = LhsVal%Sign*RhsVal%Sign
        ! free pointers
        IF (FreeX) THEN
            CALL MemFree(XDigit)
        ELSE
            NULLIFY(XDigit)
        END IF
        IF (FreeY) THEN
            CALL MemFree(YDigit)
        ELSE
            NULLIFY(YDigit)
        END IF

        RETURN

    END FUNCTION Multiply_Big

    !**************************************************************************

    FUNCTION Multiply_Large(LhsVal, RhsVal, BaseCut, Algorithm) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal, using Karatsuba algorithm

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt64), TARGET, INTENT(IN)   :: LhsVal
        TYPE(ApInt64), TARGET, INTENT(IN)   :: RhsVal
        TYPE(ApInt64)                       :: OutVal
        tIndex,                INTENT(IN)   :: BaseCut  ! threshold to perform basic algorithm
        tInteger,              INTENT(IN)   :: Algorithm

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: OutSize
        tIndex      :: M1, M2, M3

    !** FLOW

        M1 = ModLen(LhsVal%Length)
        M2 = ModLen(RhsVal%Length)
        M3 = ModLen(LhsVal%Length+RhsVal%Length)
        IF (MinModLen(M1, M2, M3) < 5) THEN
            ! perform multiplication using grade-school algorithm
            ! allocate storage of output
            OutSize = LhsVal%Length+RhsVal%Length
            CALL MemAlloc(OutVal%Digit, OutSize, StartID=0_kIndex)
            ! perform multiplication
            IF (LhsVal%Length < RhsVal%Length) THEN
                CALL Multiply_Basic(LhsVal%Digit, LhsVal%Length, RhsVal%Digit, RhsVal%Length, &
                                    OutVal%Digit)
            ELSE
                CALL Multiply_Basic(RhsVal%Digit, RhsVal%Length, LhsVal%Digit, LhsVal%Length, &
                                    OutVal%Digit)
            END IF
            ! set sign and length
            OutVal%Sign = LhsVal%Sign*RhsVal%Sign
            OutVal%Length = OutSize
            IF (OutVal%Digit(OutVal%Length-1) == 0) OutVal%Length = OutVal%Length - 1_kIndex
        ELSE
            ! perform multiplication using Karatsuba algorithm
            BLOCK
                TYPE(ApInt64)   :: X, Y
                tIndex          :: MaxLen
                ! process input
                MaxLen = MAX(LhsVal%Length, RhsVal%Length)
                X = MakeCopy(LhsVal, MaxLen)
                Y = MakeCopy(RhsVal, MaxLen)
                IF (Y%Length < MaxLen) Y%Digit(Y%Length:MaxLen - 1) = 0_kLong
                IF (X%Length < MaxLen) X%Digit(X%Length:MaxLen - 1) = 0_kLong
                ! perform multiplication
                OutVal = MulK_JDK_Xp(X, Y, BaseCut, Algorithm)
                ! process result
                OutVal%Length = X%Length + Y%Length
                DO WHILE (OutVal%Digit(OutVal%Length-1_kIndex) == 0_kLong)
                    OutVal%Length = OutVal%Length - 1_kIndex
                END DO
                OutVal%Sign = X%Sign*Y%Sign
            END BLOCK
        END IF

        RETURN

    END FUNCTION Multiply_Large

    !**************************************************************************

    FUNCTION ModLen(InVal) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: InVal
        tIndex              :: OutVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (InVal > 32_kIndex) THEN
            OutVal = MOD(InVal, 32_kIndex) / (InVal/32_kIndex)
        ELSE
            OutVal = 1_kIndex
        END IF

        RETURN

    END FUNCTION ModLen

    !**************************************************************************

    FUNCTION MinModLen(In1, In2, In3) RESULT(OutVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform multiplication: OutVal = LhsVal * RhsVal

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tIndex, INTENT(IN)  :: In1, In2, In3
        tIndex              :: OutVal

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        OutVal = MIN(MIN(In1, In2), In3)

        RETURN

    END FUNCTION MinModLen

    !**************************************************************************

END FUNCTION ApInt64_Multiply_Xp

!******************************************************************************

END SUBMODULE SubClass_Api64_MulSqr

!******************************************************************************
