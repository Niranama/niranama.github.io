
SUBMODULE (Class_ApInt32) SubClass_Api32_Bitwise

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to bitwise
!   operations of the <a href="../module/class_apint32.html">ApInt32</a> type.

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** SUBMODULE PARAMETERS:
    ! name of the module
    tCharStar, PARAMETER    :: SubName = 'SubClass_Api32_Bitwise'
    ! parameters for large and small shifts
    tInteger,  PARAMETER    :: LargePos  = 5
    tInteger,  PARAMETER    :: SmallMask = 31
    tInteger,  PARAMETER    :: DigitBits = 32

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE SUBROUTINES OR FUNCTIONS:

MODULE FUNCTION ApInt32_ShiftLeft(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the ApInt32 number left by the specified amount

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    tInteger,      INTENT(IN)   :: ShiftPos ! must be nonnegative
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LargeShift, SmallShift

!** FLOW

    IF (IsZero(InVal)) THEN
        OutVal = ZeroApInt32()
    ELSEIF (ShiftPos > 0) THEN
        LargeShift = SHIFTR(ShiftPos, LargePos)
        SmallShift = IAND(ShiftPos, SmallMask)
        OutVal = MakeCopy(InVal, InVal%Length + LargeShift + 1)
        IF (LargeShift > 0) CALL ShiftLeft_Large(OutVal, LargeShift)
        IF (SmallShift > 0) CALL ShiftLeft_Small(OutVal, SmallShift, LargeShift)
    ELSEIF (ShiftPos == 0) THEN
        OutVal = MakeCopy(InVal)
    ELSE
        CALL Handle_ErrLevel('ApInt32_ShiftLeft', SubName, ErrWarning, &
                   'ShiftPos is negative. Perform right shift instead.')
        LargeShift = SHIFTR(-ShiftPos, LargePos)
        IF (LargeShift >= InVal%Length) THEN
            OutVal = ZeroApInt32()
        ELSE
            SmallShift = IAND(-ShiftPos, SmallMask)
            OutVal = MakeCopy(InVal)
            IF (LargeShift > 0) CALL ShiftRight_Large(OutVal, LargeShift)
            IF (SmallShift > 0) CALL ShiftRight_Small(OutVal, SmallShift)
        END IF
    END IF

    RETURN

END FUNCTION ApInt32_ShiftLeft

!******************************************************************************

MODULE FUNCTION ApInt32_ShiftRight(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the ApInt32 number right by the specified amount

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    tInteger,      INTENT(IN)   :: ShiftPos ! must be nonnegative
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LargeShift, SmallShift

!** FLOW

    IF (IsZero(InVal)) THEN
        OutVal = ZeroApInt32()
    ELSEIF (ShiftPos > 0) THEN
        LargeShift = SHIFTR(ShiftPos, LargePos)
        IF (LargeShift >= InVal%Length) THEN
            OutVal = ZeroApInt32()
        ELSE
            SmallShift = IAND(ShiftPos, SmallMask)
            OutVal = MakeCopy(InVal)
            IF (LargeShift > 0) CALL ShiftRight_Large(OutVal, LargeShift)
            IF (SmallShift > 0) CALL ShiftRight_Small(OutVal, SmallShift)
        END IF
    ELSEIF (ShiftPos == 0) THEN
        OutVal = MakeCopy(InVal)
    ELSE
        CALL Handle_ErrLevel('ApInt32_ShiftRight', SubName, ErrWarning, &
                   'ShiftPos is negative. Perform left shift instead.')
        LargeShift = SHIFTR(-ShiftPos, LargePos)
        SmallShift = IAND(-ShiftPos, SmallMask)
        OutVal = MakeCopy(InVal, InVal%Length + LargeShift + 1)
        IF (LargeShift > 0) CALL ShiftLeft_Large(OutVal, LargeShift)
        IF (SmallShift > 0) CALL ShiftLeft_Small(OutVal, SmallShift, LargeShift)
    END IF

    RETURN

END FUNCTION ApInt32_ShiftRight

!******************************************************************************

MODULE FUNCTION ApInt32_LogicalShift(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift 'Big' left by 'ShiftPos' position if 'ShiftPos' is positive
    ! or to shift 'Big' right by 'ShiftPos' position if 'ShiftPos' is negative

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    tInteger,      INTENT(IN)   :: ShiftPos
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos == 0) THEN
        OutVal = MakeCopy(InVal)
    ELSEIF (ShiftPos > 0) THEN
        OutVal = SHIFTL(InVal, ShiftPos)
    ELSE
        OutVal = SHIFTR(InVal, -ShiftPos)
    END IF

    RETURN

END FUNCTION ApInt32_LogicalShift

!******************************************************************************

MODULE SUBROUTINE ApInt32_LeftShift(Val, ShiftPos)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the ApInt32 number left by the specified amount

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: Val
    tInteger,       INTENT(IN)      :: ShiftPos ! must be nonnegative

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LargeShift, SmallShift

!** FLOW

    IF (Val%IsZero()) THEN
        RETURN
    ELSEIF (ShiftPos > 0) THEN
        LargeShift = SHIFTR(ShiftPos, LargePos)
        SmallShift = IAND(ShiftPos, SmallMask)
        IF (LargeShift > 0) CALL ShiftLeft_Large(Val, LargeShift)
        IF (SmallShift > 0) CALL ShiftLeft_Small(Val, SmallShift, LargeShift)
    ELSEIF (ShiftPos == 0) THEN
        RETURN
    ELSE
        CALL Handle_ErrLevel('ApInt32_ShiftLeft', SubName, ErrWarning, &
                   'ShiftPos is negative. Perform right shift instead.')
        LargeShift = SHIFTR(-ShiftPos, LargePos)
        IF (LargeShift >= Val%Length) THEN
            Val = ZeroApInt32()
        ELSE
            SmallShift = IAND(-ShiftPos, SmallMask)
            IF (LargeShift > 0) CALL ShiftRight_Large(Val, LargeShift)
            IF (SmallShift > 0) CALL ShiftRight_Small(Val, SmallShift)
        END IF
    END IF

    RETURN

END SUBROUTINE ApInt32_LeftShift

!******************************************************************************

MODULE SUBROUTINE ApInt32_RightShift(Val, ShiftPos)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the ApInt32 number right by the specified amount

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ApInt32), INTENT(INOUT)   :: Val
    tInteger,       INTENT(IN)      :: ShiftPos ! must be nonnegative

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LargeShift, SmallShift

!** FLOW

    IF (Val%IsZero()) THEN
        RETURN
    ELSEIF (ShiftPos > 0) THEN
        LargeShift = SHIFTR(ShiftPos, LargePos)
        IF (LargeShift >= Val%Length) THEN
            Val = ZeroApInt32()
        ELSE
            SmallShift = IAND(ShiftPos, SmallMask)
            IF (LargeShift > 0) CALL ShiftRight_Large(Val, LargeShift)
            IF (SmallShift > 0) CALL ShiftRight_Small(Val, SmallShift)
        END IF
    ELSEIF (ShiftPos == 0) THEN
        RETURN
    ELSE
        CALL Handle_ErrLevel('ApInt32_ShiftRight', SubName, ErrWarning, &
                   'ShiftPos is negative. Perform left shift instead.')
        LargeShift = SHIFTR(-ShiftPos, LargePos)
        SmallShift = IAND(-ShiftPos, SmallMask)
        IF (LargeShift > 0) CALL ShiftLeft_Large(Val, LargeShift)
        IF (SmallShift > 0) CALL ShiftLeft_Small(Val, SmallShift, LargeShift)
    END IF

    RETURN

END SUBROUTINE ApInt32_RightShift

!******************************************************************************

SUBROUTINE ShiftLeft_Small(Big, Shift, First)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the ApInt32 number left by the given amount (less than DigitBits) starting
    ! at the given digit, i.e. the first (<len) digits are left untouched.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Shift  ! The amount to shift
    tInteger,      INTENT(IN)       :: First  ! The starting position of the shift

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: DigInLen
    tInteger        :: DigIn(0:Big%Length-1)
    tInteger        :: Nxt
    tIndex          :: I

!** FLOW

    ! get input information
    DigInLen = SIZE(Big%Digit, KIND=kIndex)
    DigIn(0:Big%Length-1) = Big%Digit(0:Big%Length-1)

    ! check if Big%Digit needs to be resized
    IF (SHIFTR(SHIFTL(DigIn(Big%Length-1), Shift), Shift) /= DigIn(Big%Length-1)) THEN
        ! Overflow?
        Big%Length = Big%Length + 1_kIndex
        IF (Big%Length > DigInLen) THEN
            CALL MemResize(Big%Digit, Big%Length + 1_kIndex)
        ELSE
            DigIn(Big%Length-1) = 0
        END IF
    END IF
    IF (Big%Length > DigInLen) THEN
        Nxt = 0
    ELSE
        Nxt = DigIn(Big%Length-1)
    END IF
    DO I = Big%Length-1, First+1, -1
        Big%Digit(I) = IOR(SHIFTL(Nxt, Shift), SHIFTR(DigIn(I-1), DigitBits-Shift))
        Nxt = DigIn(I-1)
    END DO
    Big%Digit(First) = SHIFTL(Nxt, Shift)

    RETURN

END SUBROUTINE ShiftLeft_Small

!******************************************************************************

SUBROUTINE ShiftLeft_Large(Big, Shift)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the ApInt32 number left by DigitBits*shift, i.e. moves each
    ! digit shift positions to the left.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Shift  ! The amount to shift

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF ((Big%Length + Shift) > SIZE(Big%Digit, KIND=kIndex)) THEN
        BLOCK
            tInteger    :: DigIn(0:Big%Length-1)
            DigIn(0:Big%Length-1) = Big%Digit(0:Big%Length-1)
            CALL MemAlloc(Big%Digit, Big%Length+Shift+1, StartID=0_kIndex)
            Big%Digit = 0
            Big%Digit(Shift:Big%Length+Shift-1) = DigIn(0:Big%Length-1)
        END BLOCK
    ELSE
        BLOCK
            tIndex      :: Index
            DO Index = Big%Length-1, 0, -1
                Big%Digit(Shift+Index) = Big%Digit(Index)
            END DO
            Big%Digit(0:Shift-1) = 0
        END BLOCK
    END IF
    Big%Length = Big%Length + Shift

    RETURN

END SUBROUTINE ShiftLeft_Large

!******************************************************************************

SUBROUTINE ShiftRight_Small(Big, Shift)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the ApInt32 number right by the given amount (less than DigitBits).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Shift  ! The amount to shift

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex          :: I
    tInteger        :: Nxt

!** FLOW

    Nxt = Big%Digit(0)
    DO I = 0, Big%Length-2
        Big%Digit(I) = IOR(SHIFTR(Nxt, Shift), SHIFTL(Big%Digit(I+1), DigitBits-Shift))
        Nxt = Big%Digit(I+1)
    END DO
    Big%Digit(Big%Length-1) = SHIFTR(Big%Digit(Big%Length-1), Shift)
    IF ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1_kIndex)) THEN
        Big%Length = Big%Length - 1_kIndex
    END IF

    RETURN

END SUBROUTINE ShiftRight_Small

!******************************************************************************

SUBROUTINE ShiftRight_Large(Big, Shift)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the ApInt32 number right by DigitBits*shift, i.e. moves each
    ! digit shift positions to the right.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    tInteger,      INTENT(IN)       :: Shift  ! The amount to shift

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Index

!** FLOW

    DO Index = 0, Big%Length-Shift-1
        Big%Digit(Index) = Big%Digit(Shift+Index)
    END DO
    Big%Length = Big%Length - Shift

    RETURN

END SUBROUTINE ShiftRight_Large

!******************************************************************************

MODULE FUNCTION ApInt32_Not(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the bitwise logical complement of the ApInt32 object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = MakeCopy(InVal)
    IF (OutVal%Sign > 0) THEN
        OutVal%Sign = -1
        CALL UAddMag(OutVal, 1)
    ELSE
        OutVal%Sign = 1
        CALL USubMag(OutVal, 1)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE UAddMag(Big, U32)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To increase the magnitude of ApInt32 by the specified integer

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT)    :: Big
        tInteger,      INTENT(IN)       :: U32  ! The amount of the increase (treated as unsigned)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: Tmp

    !** FLOW

        Tmp = ToUnsignedLong(Big%Digit(0)) + ToUnsignedLong(U32)
        Big%Digit(0) = ToInteger(Tmp)
        IF (SHIFTR(Tmp, 32) /= 0_kLong) THEN
            BLOCK
                tIndex    :: I
                I = 1_kIndex
                DO
                    Big%Digit(I) = Big%Digit(I) + 1
                    IF (.NOT.((I < Big%Length).AND.(Big%Digit(I) == 0))) EXIT
                    I = I + 1_kIndex
                END DO
                IF (I == Big%Length) THEN
                    IF (Big%Length == SIZE(Big%Digit, KIND=kIndex)) THEN
                        CALL MemResize(Big%Digit, Big%Length*2_kIndex)
                    END IF
                    Big%Digit(Big%Length) = 1
                    Big%Length = Big%Length + 1
                END IF
            END BLOCK
        END IF

        RETURN

    END SUBROUTINE UAddMag

    !**************************************************************************

    SUBROUTINE USubMag(Big, U32)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To decrease the magnitude of ApInt32 by the specified integer.
        ! If U32 > the magnitude of ApInt32, behavior is undefined.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        TYPE(ApInt32), INTENT(INOUT)    :: Big
        tInteger,      INTENT(IN)       :: U32  ! The amount of the decrease (treated as unsigned)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: Dif

    !** FLOW

        Dif = ToUnsignedLong(Big%Digit(0)) - ToUnsignedLong(U32)
        Big%Digit(0) = ToInteger(Dif)
        IF (SHIFTA(Dif, 32) /= 0_kLong) THEN
            BLOCK
                tIndex    :: I
                I = 1_kIndex
                DO WHILE (Big%Digit(I) == 0)
                    Big%Digit(I) = Big%Digit(I) - 1
                    I = I + 1_kIndex
                END DO
                Big%Digit(I) = Big%Digit(I) - 1
                IF ((Big%Digit(I) == 0).AND.(I+1_kIndex == Big%Length)) THEN
                    Big%Length = Big%Length - 1
                END IF
            END BLOCK
        END IF

        RETURN

    END SUBROUTINE USubMag

    !**************************************************************************

END FUNCTION ApInt32_Not

!******************************************************************************

MODULE FUNCTION ApInt32_Ior(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform an inclusive OR on corresponding bits of the ApInt32 objects

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LhsVal
    TYPE(ApInt32), INTENT(IN)   :: RhsVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(LhsVal)) THEN
        OutVal = MakeCopy(RhsVal)
    ELSEIF (IsZero(RhsVal)) THEN
        OutVal = MakeCopy(LhsVal)
    ELSE
        OutVal = MakeCopy(LhsVal, MAX(LhsVal%Length, RhsVal%Length)+2_kIndex)
        CALL Bitwise_Or(OutVal, RhsVal)
    END IF

    RETURN

END FUNCTION ApInt32_Ior

!******************************************************************************

SUBROUTINE Bitwise_Or(Big, Mask)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a bitwise OR on corresponding bits of the ApInt32 objects

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    TYPE(ApInt32), INTENT(IN)       :: Mask

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B
    tIndex      :: I, J, MLen, Index

!** FLOW

    IF (Big%Sign > 0) THEN
        IF (Mask%SIgn > 0) THEN
            IF (Mask%Length > Big%Length) THEN
                IF (Mask%Length > SIZE(Big%Digit, KIND=kIndex))  THEN
                    CALL MemResize(Big%Digit, Mask%Length+1)
                END IF
                COPY_ARRAY_ZERO_BASED(Mask%Digit, Big%Length, Big%Digit, Big%Length, Mask%Length-Big%Length)
                DO I = 0, Big%Length-1
                    Big%Digit(I) = IOR(Big%Digit(I), Mask%Digit(I))
                END DO
                Big%Length = Mask%Length
            ELSE
                DO I = 0, Mask%Length-1
                    Big%Digit(I) = IOR(Big%Digit(I), Mask%Digit(I))
                END DO
            END IF
        ELSE
            IF (Mask%Length > SIZE(Big%Digit, KIND=kIndex))  THEN
                CALL MemResize(Big%Digit, Mask%Length+1)
            END IF
            IF (Mask%Length > Big%Length) THEN
                COPY_ARRAY_ZERO_BASED(Mask%Digit, Big%Length, Big%Digit, Big%Length, Mask%Length-Big%Length)
            END IF
            MLen = MIN(Mask%Length, Big%Length)
            A = Big%Digit(0)
            B = Mask%Digit(0)
            J = 1
            DO WHILE ((IOR(A, B) == 0).AND.(J < MLen))
                A = Big%Digit(J)
                B = Mask%Digit(J)
                J = J + 1
            END DO
            IF (A /= 0 .AND. B == 0) THEN
                Big%Digit(J-1) = -A
                DO WHILE (Mask%Digit(J) == 0)
                    Big%Digit(J) = IEOR(Big%Digit(J), -1)
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = NOT(IOR(Big%Digit(J), -Mask%Digit(J)))
                ELSE    ! Mask%Digit(J)  ==  Big%Digit(J)
                    Big%Digit(J) = NOT(-Big%Digit(J))
                END IF
                J = J + 1
            ELSEIF (A == 0) THEN    ! .AND. (B /= 0 || J == MLen)
                Big%Digit(J-1) = B
                DO WHILE ((J < MLen).AND.(Big%Digit(J) == 0))
                    Big%Digit(J) = Mask%Digit(J)
                    J = J + 1
                END DO
            ELSE    ! A /= 0 .AND. B /= 0
                Big%Digit(J-1) = -IOR(A, -B)
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IAND(NOT(Big%Digit(J)), Mask%Digit(J))   ! ~(Big%Digit(J)|~Mask%Digit(J))
                J = J + 1
            END DO
            Big%Sign = -1
            Big%Length = Mask%Length
            DO WHILE (Big%Digit(Big%Length-1) == 0)
                Big%Length = Big%Length - 1
            END DO
        END IF
    ELSE
        MLen = MIN(Mask%Length, Big%Length)
        A = Big%Digit(0)
        B = Mask%Digit(0)
        J = 1
        DO WHILE ((IOR(A, B) == 0).AND.(J < MLen))
            A = Big%Digit(J)
            B = Mask%Digit(J)
            J = J + 1
        END DO
        IF (Mask%SIgn > 0) THEN
            IF (A /= 0 .AND. B == 0) THEN
                DO WHILE ((J < MLen).AND.(Mask%Digit(J) == 0))
                    J = J + 1
                END DO
            ELSEIF (A == 0) THEN    ! .AND. (B /= 0 || J == MLen)
                Big%Digit(J-1) = -B
                DO WHILE ((J < MLen).AND.(Big%Digit(J) == 0))
                    Big%Digit(J) = NOT(Mask%Digit(J))
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = NOT(IOR(-Big%Digit(J), Mask%Digit(J)))
                ELSE
                    DO WHILE (Big%Digit(J) == 0)
                        Big%Digit(J) = -1
                        J = J + 1
                    END DO
                    Big%Digit(J) = NOT(-Big%Digit(J))
                END IF
                J = J + 1
            ELSE    ! A /= 0 .AND. B /= 0
                Big%Digit(J-1) = -IOR(-A, B)
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IAND(Big%Digit(J), NOT(Mask%Digit(J)))   ! ~(~Big%Digit(J)|Mask%Digit(J))
                J = J + 1
            END DO
        ELSE
            IF (A /= 0 .AND. B == 0) THEN
                DO WHILE ((J < MLen).AND.(Mask%Digit(J) == 0))
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = NOT(IOR(NOT(Big%Digit(J)), -Mask%Digit(J)))
                END IF
                J = J + 1
            ELSEIF (A == 0) THEN    ! .AND. (B /= 0 || J == MLen)
                Big%Digit(J-1) = B
                DO WHILE ((J < MLen).AND.(Big%Digit(J) == 0))
                    Big%Digit(J) = Mask%Digit(J)
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = NOT(IOR(-Big%Digit(J), NOT(Mask%Digit(J))))
                END IF
                J = J + 1
            ELSE    ! A /= 0 .AND. B /= 0
                Big%Digit(J-1) = -IOR(-A, -B)
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IAND(Big%Digit(J), Mask%Digit(J))    ! ~(~Big%Digit(J)|~Mask%Digit(J))
                J = J + 1
            END DO
            Big%Length = MLen
        END IF
        DO WHILE (Big%Digit(Big%Length-1) == 0)
            Big%Length = Big%Length - 1
        END DO
    END IF
    DO WHILE ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1))
        Big%Length = Big%Length - 1
    END DO

    RETURN

END SUBROUTINE Bitwise_Or

!******************************************************************************

MODULE FUNCTION ApInt32_Iand(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a bitwise AND on corresponding bits of the ApInt32 objects

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LhsVal
    TYPE(ApInt32), INTENT(IN)   :: RhsVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(LhsVal)) THEN
        OutVal = ZeroApInt32()
    ELSEIF (IsZero(RhsVal)) THEN
        OutVal = ZeroApInt32()
    ELSE
        OutVal = MakeCopy(LhsVal, MAX(LhsVal%Length, RhsVal%Length)+2_kIndex)
        CALL Bitwise_And(OutVal, RhsVal)
    END IF

    RETURN

END FUNCTION ApInt32_Iand

!******************************************************************************

SUBROUTINE Bitwise_And(Big, Mask)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a bitwise AND on corresponding bits of the ApInt32 objects

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    TYPE(ApInt32), INTENT(IN)       :: Mask

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B
    tIndex      :: I, J, MLen, BLen, Index

!** FLOW

    IF (Big%Sign > 0) THEN
        IF (Mask%Sign > 0) THEN
            IF (Mask%Length < Big%Length) Big%Length = Mask%Length
            DO I = 0, Big%Length-1
                Big%Digit(I) = IAND(Big%Digit(I), Mask%Digit(I))
            END DO
        ELSE
            MLen = MIN(Big%Length, Mask%Length)
            A = Big%Digit(0)
            B = Mask%Digit(0)
            J = 1
            DO WHILE ((IOR(A, B) == 0).AND.(J < MLen))
                A = Big%Digit(J)
                B = Mask%Digit(J)
                J = J + 1
            END DO
            IF ((A /= 0).AND.(B == 0)) THEN
                Big%Digit(J-1) = 0
                DO WHILE ((J < MLen).AND.(Mask%Digit(J) == 0))
                    Big%Digit(J) = 0
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = IAND(Big%Digit(J), -Mask%Digit(J))
                ELSEIF (J == Big%Length) THEN
                    Big%Length = 1
                END IF
                J = J + 1
            ELSEIF (A == 0) THEN    ! && (B /= 0 || J == MLen)
                DO WHILE ((J < MLen).AND.(Big%Digit(J) == 0))
                    J = J + 1
                END DO
            ELSE
                Big%Digit(J-1) = IAND(Big%Digit(J-1), -B)
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IAND(Big%Digit(J), NOT(Mask%Digit(J)))
                J = J + 1
            END DO
        END IF
        DO WHILE ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1))
            Big%Length = Big%Length - 1
        END DO
    ELSE
        MLen = MIN(Big%Length, Mask%Length)
        IF (Mask%Sign > 0) THEN
            A = Big%Digit(0)
            B = Mask%Digit(0)
            J = 1
            DO WHILE ((IOR(A, B) == 0).AND.(J < MLen))
                A = Big%Digit(J)
                B = Mask%Digit(J)
                J = J + 1
            END DO
            IF ((A /= 0).AND.(B == 0)) THEN
                Big%Digit(J-1) = 0
                DO WHILE ((J < MLen).AND.(Mask%Digit(J) == 0))
                    Big%Digit(J) = 0
                    J = J + 1
                END DO
            ELSEIF (A == 0) THEN    ! && (B /= 0 || J == MLen)
                DO WHILE ((J < MLen).AND.(Big%Digit(J) == 0))
                    J = J + 1
                END DO
                IF (J < MLen) Big%Digit(J) = IAND(-Big%Digit(J), Mask%Digit(J))
                J = J + 1
            ELSE
                Big%Digit(J-1) = IAND(-A, B)
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IAND(NOT(Big%Digit(J)), Mask%Digit(J))
                J = J + 1
            END DO
            IF (Mask%Length > Big%Length) THEN
                IF (Mask%Length > SIZE(BIg%Digit, KIND=kIndex)) THEN
                    CALL MemResIze(BIg%Digit, Mask%Length+2)
                END IF
                COPY_ARRAY_ZERO_BASED(Mask%Digit, Big%Length, Big%Digit, Big%Length, Mask%Length-Big%Length)
            END IF
            Big%Length = Mask%Length
            Big%Sign = 1
            DO WHILE ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1))
                Big%Length = Big%Length - 1
            END DO
        ELSE
            IF (Mask%Length > Big%Length) THEN
                IF (Mask%Length > SIZE(BIg%Digit, KIND=kIndex)) THEN
                    CALL MemResIze(BIg%Digit, Mask%Length+2)
                END IF
                COPY_ARRAY_ZERO_BASED(Mask%Digit, Big%Length, Big%Digit, Big%Length, Mask%Length-Big%Length)
            END IF
            A = Big%Digit(0)
            B = Mask%Digit(0)
            J = 1
            DO WHILE (IOR(A, B) == 0)
                A = Big%Digit(J)
                B = Mask%Digit(J)
                J = J + 1
            END DO
            IF ((A /= 0).AND.(B == 0)) THEN
                Big%Digit(J-1) = 0
                DO WHILE ((J < MLen).AND.(Mask%Digit(J) == 0))
                    Big%Digit(J) = 0
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = -IAND(NOT(Big%Digit(J)), -Mask%Digit(J))
                END IF
                J = J + 1
            ELSEIF (A == 0) THEN    ! && (B /= 0 || J == MLen)
                DO WHILE ((J < MLen).AND.(Big%Digit(J) == 0))
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = -IAND(-Big%Digit(J), NOT(Mask%Digit(J)))
                END IF
                J = J + 1
            ELSE
                Big%Digit(J-1) = -IAND(-A, -B)
            END IF
            IF ((J <= MLen).AND.(Big%Digit(J-1) == 0)) THEN
                IF (J < MLen) THEN
                    Big%Digit(J) = -NOT(IOR(Big%Digit(J), Mask%Digit(J)))
                    J = J + 1
                    DO WHILE ((J < MLen).AND.(Big%Digit(J-1) == 0))
                        Big%Digit(J) = -NOT(IOR(Big%Digit(J), Mask%Digit(J)))
                        J = J + 1
                    END DO
                END IF
                IF ((J == MLen).AND.(Big%Digit(J-1) == 0)) THEN
                    BLen = MAX(Big%Length, Mask%Length)
                    DO WHILE ((J < BLen).AND.(Big%Digit(J) == -1))  ! Mask%Digit(J)==Big%Digit(J)
                        Big%Digit(J) = 0
                        J = J + 1
                    END DO
                    IF (J < BLen) THEN
                        Big%Digit(J) = -NOT(Big%Digit(J))
                    ELSE
                        IF (BLen >= SIZE(BIg%Digit, KIND=kIndex)) THEN
                            CALL MemResIze(BIg%Digit, BLen+2)
                        END IF
                        Big%Digit(BLen) = 1
                        Big%Length = BLen + 1
                        RETURN
                    END IF
                    J = J + 1
                END IF
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IOR(Big%Digit(J), Mask%Digit(J))
                J = J + 1
            END DO
            IF (Mask%Length > Big%Length) Big%Length = Mask%Length
        END IF
    END IF
    DO WHILE ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1))
        Big%Length = Big%Length - 1
    END DO

    RETURN

END SUBROUTINE Bitwise_And

!******************************************************************************

MODULE FUNCTION ApInt32_Ieor(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform an exclusive OR on corresponding bits of the ApInt32 objects

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LhsVal
    TYPE(ApInt32), INTENT(IN)   :: RhsVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(LhsVal)) THEN
        OutVal = MakeCopy(RhsVal)
    ELSEIF (IsZero(RhsVal)) THEN
        OutVal = MakeCopy(LhsVal)
    ELSE
        OutVal = MakeCopy(LhsVal, MAX(LhsVal%Length, RhsVal%Length)+2_kIndex)
        CALL Bitwise_Xor(OutVal, RhsVal)
    END IF

    RETURN

END FUNCTION ApInt32_Ieor

!******************************************************************************

SUBROUTINE Bitwise_Xor(Big, Mask)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a bitwise XOR on corresponding bits of the ApInt32 objects

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    TYPE(ApInt32), INTENT(IN)       :: Mask

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B
    tIndex      :: I, J, MLen, BLen, Index

!** FLOW

    IF (Big%Sign > 0) THEN
        IF (Mask%Length > Big%Length) THEN
            IF (Mask%Length > SIZE(Big%Digit, KIND=kIndex)) THEN
                CALL MemResize(Big%Digit, Mask%Length+2)
            END IF
            COPY_ARRAY_ZERO_BASED(Mask%Digit, Big%Length, Big%Digit, Big%Length, Mask%Length-Big%Length)
        END IF
        MLen = MIN(Big%Length, Mask%Length)
        IF (Mask%Sign > 0) THEN
            DO I = 0, MLen-1
                Big%Digit(I) = IEOR(Big%Digit(I), Mask%Digit(I))
            END DO
        ELSE
            A = Big%Digit(0)
            B = Mask%Digit(0)
            J = 1
            DO WHILE ((IOR(A, B) == 0).AND.(J < MLen))
                A = Big%Digit(J)
                B = Mask%Digit(J)
                J = J + 1
            END DO
            IF ((A /= 0).AND.(B == 0)) THEN
                Big%Digit(J-1) = -A
                DO WHILE (Mask%Digit(J) == 0)
                    Big%Digit(J) = IEOR(Big%Digit(J), -1)
                    J = J + 1
                END DO
                IF (J < Big%Length) THEN
                    Big%Digit(J) = NOT(IEOR(Big%Digit(J), -Mask%Digit(J)))
                ELSE
                    Big%Digit(J) = NOT(-Mask%Digit(J))
                END IF
                J = J + 1
            ELSEIF (A == 0) THEN    ! && (B /= 0 || J == mLen)
                Big%Digit(J-1) = B  ! -(0^-B)
            ELSE    ! A /= 0 && B /= 0
                Big%Digit(J-1) = -IEOR(A, -B)
                DO WHILE ((J < MLen).AND.(Big%Digit(J-1) == 0))
                    Big%Digit(J) = -IEOR(Big%Digit(J), NOT(Mask%Digit(J)))
                    J = J + 1
                END DO
                IF ((J  >= MLen).AND.(Big%Digit(J-1) == 0)) THEN
                    BLOCK
                        tInteger, ALLOCATABLE   :: Tmp(:)
                        IF (J < Big%Length) THEN
                            CALL MemAlloc(Tmp, Big%Length, StartID=0_kIndex)
                            Tmp(0:Big%Length-1) = Big%Digit(0:Big%Length-1)
                        ELSE
                            CALL MemAlloc(Tmp, Mask%Length, StartID=0_kIndex)
                            Tmp(0:Mask%Length-1) = Mask%Digit(0:Mask%Length-1)
                        END IF
                        BLen = MAX(Big%Length, Mask%Length)
                        DO WHILE ((J < BLen).AND.(Tmp(J) == -1))
                            Big%Digit(J) = 0
                            J = J + 1
                        END DO
                        IF (BLen == SIZE(Big%Digit, KIND=kIndex)) THEN
                            CALL MemResize(Big%Digit, BLen+2)   ! Big%Length == BLen
                        END IF
                        IF (J == BLen) THEN
                            Big%Digit(BLen) = 1
                            Big%Length = BLen+1
                        ELSE
                            Big%Digit(J) = -NOT(Tmp(J))
                        END IF
                        J = J + 1
                        CALL MemFree(Tmp)
                    END BLOCK
                END IF
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IEOR(Big%Digit(J), Mask%Digit(J))    ! ~(Big%Digit(J)^~Mask%Digit(J))
                J = J + 1
            END DO
            Big%Sign = -1
        END IF
        IF (Mask%Length > Big%Length) THEN
            Big%Length = Mask%Length
        ELSE
            DO WHILE ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1))
                Big%Length = Big%Length - 1
            END DO
        END IF
    ELSE
        IF (Mask%Length > Big%Length) THEN
            IF (Mask%Length > SIZE(Big%Digit, KIND=kIndex)) THEN
                CALL MemResize(Big%Digit, Mask%Length+2)
            END IF
            COPY_ARRAY_ZERO_BASED(Mask%Digit, Big%Length, Big%Digit, Big%Length, Mask%Length-Big%Length)
        END IF
        MLen = MIN(Big%Length, Mask%Length)
        IF (Mask%Sign > 0) THEN
            A = Big%Digit(0)
            B = Mask%Digit(0)
            J = 1
            DO WHILE ((IOR(A, B) == 0).AND.(J < MLen))
                A = Big%Digit(J)
                B = Mask%Digit(J)
                J = J + 1
            END DO
            IF ((A /= 0).AND.(B == 0)) THEN
                DO WHILE ((J < MLen).AND.(Mask%Digit(J) == 0))
                    J = J + 1
                END DO
            ELSEIF (A == 0) THEN    ! && (B /= 0 || J == mLen)
                Big%Digit(J-1) = -B
                DO WHILE ((J < MLen).AND.(Big%Digit(J) == 0))
                    Big%Digit(J) = NOT(Mask%Digit(J))
                    J = J + 1
                END DO
                DO WHILE ((J < Big%Length).AND.(Big%Digit(J) == 0))
                    Big%Digit(J) = -1
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = NOT(IEOR(-Big%Digit(J), Mask%Digit(J)))
                ELSE
                    Big%Digit(J) = NOT(-Big%Digit(J))
                END IF
                J = J + 1
            ELSE    ! A /= 0 && B /= 0
                Big%Digit(J-1) = -IEOR(-A, B)
                IF (Big%Digit(J-1) == 0) THEN   ! Perform carry.
                    DO WHILE ((Big%Digit(J-1) == 0).AND.(J < MLen))
                        Big%Digit(J) = IEOR(Big%Digit(J), Mask%Digit(J))
                        Big%Digit(J) = Big%Digit(J) + 1
                        J = J + 1
                    END DO
                    BLen = MAX(Big%Length, Mask%Length)
                    DO WHILE ((Big%Digit(J-1) == 0).AND.(J < BLen))
                        Big%Digit(J) = Big%Digit(J) + 1
                        J = J + 1
                    END DO
                    IF (J == SIZE(Big%Digit, KIND=kIndex)) THEN
                        CALL MemResize(Big%Digit, J+2)
                    END IF
                    IF (Big%Digit(J-1) == 0) THEN
                        Big%Digit(J) = 1
                        Big%Length = J + 1
                        RETURN
                    END IF
                END IF
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IEOR(Big%Digit(J), Mask%Digit(J))    ! ~(~Big%Digit(J)^Mask%Digit(J))
                J = J + 1
            END DO
        ELSE
            A = Big%Digit(0)
            B = Mask%Digit(0)
            J = 1
            DO WHILE ((IOR(A, B) == 0).AND.(J < MLen))
                A = Big%Digit(J)
                B = Mask%Digit(J)
                J = J + 1
            END DO
            IF ((A /= 0).AND.(B == 0)) THEN
                Big%Digit(J-1) = -A
                DO WHILE (Mask%Digit(J) == 0)
                    Big%Digit(J) = IEOR(Big%Digit(J), -1)   ! ~Big%Digit(J)
                    J = J + 1
                END DO
                IF (J < Big%Length) THEN
                    Big%Digit(J) = IEOR(NOT(Big%Digit(J)), -Mask%Digit(J))
                ELSE
                    Big%Digit(J) = NOT(-Big%Digit(J))   ! Big%Digit(J) == Mask%Digit(J), ~0^-Mask%Digit(J)
                END IF
                J = J + 1
            ELSEIF (A == 0) THEN    ! && B /= 0
                Big%Digit(J-1) = -B
                DO WHILE ((J < Mask%Length).AND.(Big%Digit(J) == 0))
                    Big%Digit(J) = NOT(Mask%Digit(J))
                    J = J + 1
                END DO
                DO WHILE (Big%Digit(J) == 0)
                    Big%Digit(J) = -1
                    J = J + 1
                END DO
                IF (J < Mask%Length) THEN
                    Big%Digit(J) = IEOR(-Big%Digit(J), NOT(Mask%Digit(J)))
                ELSE
                    Big%Digit(J) = NOT(-Big%Digit(J))   ! -Big%Digit(J)^~0
                END IF
                J = J + 1
            ELSE    ! A /= 0 && B /= 0
                Big%Digit(J-1) = IEOR(-A, -B)
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IEOR(Big%Digit(J), Mask%Digit(J))    ! ~Big%Digit(J)^~Mask%Digit(J)
                J = J + 1
            END DO
            Big%Sign = 1
        END IF
        IF (Mask%Length > Big%Length) THEN
            Big%Length = Mask%Length
        ELSE
            DO WHILE ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1))
                Big%Length = Big%Length - 1
            END DO
        END IF
    END IF
    DO WHILE ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1))
        Big%Length = Big%Length - 1
    END DO

    RETURN

END SUBROUTINE Bitwise_Xor

!******************************************************************************

MODULE FUNCTION ApInt32_IandNot(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a bitwise AND and NOT on corresponding bits of the ApInt32 objects
    ! => OutVal = IAND(LhsVal, NOT(RhsVal))

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: LhsVal
    TYPE(ApInt32), INTENT(IN)   :: RhsVal
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsZero(LhsVal)) THEN
        OutVal = ZeroApInt32()
    ELSEIF (RhsVal == -OneApInt32()) THEN
        OutVal = ZeroApInt32()
    ELSE
        IF (IsZero(RhsVal)) THEN
            OutVal = MakeCopy(LhsVal, MAX(LhsVal%Length, RhsVal%Length)+2_kIndex)
            CALL Bitwise_AndNot(OutVal, ZeroApInt32())
        ELSE
            OutVal = MakeCopy(LhsVal, MAX(LhsVal%Length, RhsVal%Length)+2_kIndex)
            CALL Bitwise_AndNot(OutVal, RhsVal)
        END IF
    END IF

    RETURN

END FUNCTION ApInt32_IandNot

!******************************************************************************

SUBROUTINE Bitwise_AndNot(Big, Mask)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform a bitwise AND and NOT on corresponding bits of the ApInt32 objects
    ! => Big = IAND(Big, NOT(Mask))

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(INOUT)    :: Big
    TYPE(ApInt32), INTENT(IN)       :: Mask

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B
    tIndex      :: I, J, MLen, BLen, Index

!** FLOW

    MLen = MIN(Big%Length, Mask%Length)
    IF (Big%Sign > 0) THEN
        IF (Mask%Sign > 0) THEN
            DO I = 0, MLen-1
                Big%Digit(I) = IAND(Big%Digit(I), NOT(Mask%Digit(I)))
            END DO
        ELSE
            J = 0
            DO WHILE ((J < MLen).AND.(Mask%Digit(J) == 0))
                J = J + 1
            END DO
            IF (J < MLen) THEN
                Big%Digit(J) = IAND(Big%Digit(J), NOT(-Mask%Digit(J)))
                J = J + 1
                DO WHILE (J < MLen)
                    Big%Digit(J) = IAND(Big%Digit(J), Mask%Digit(J))    ! ~~Mask%Digit(J)
                    J = J + 1
                END DO
            END IF
            Big%Length = MLen
        END IF
    ELSE
        IF (Mask%Length > Big%Length) THEN
            IF (Mask%Length > SIZE(Big%Digit, KIND=kIndex)) THEN
                CALL MemResize(Big%Digit, Mask%Length+2)
            END IF
            COPY_ARRAY_ZERO_BASED(Mask%Digit, Big%Length, Big%Digit, Big%Length, Mask%Length-Big%Length)
        END IF
        IF (Mask%Sign > 0) THEN
            J = 0
            DO WHILE (Big%Digit(J) == 0)
                J = J + 1
            END DO
            IF (J < MLen) THEN
                Big%Digit(J) = -IAND(-Big%Digit(J), NOT(Mask%Digit(J)))
                J = J + 1
                DO WHILE ((J < MLen).AND.(Big%Digit(J-1) == 0))
                    Big%Digit(J) = -NOT(IOR(Big%Digit(J), Mask%Digit(J)))   ! -(~Big%Digit(J)&~Mask%Digit(J))
                    J = J + 1
                END DO
                IF ((J == MLen).AND.(Big%Digit(J-1) == 0)) THEN
                    BLen = MAX(Big%Length, Mask%Length)
                    DO WHILE ((J < BLen).AND.(Big%Digit(J) == -1))
                        Big%Digit(J) = 0    ! Mask%Digit(J) == Big%Digit(J)
                        J = J + 1
                    END DO
                    IF (J < BLen) THEN
                        Big%Digit(J) = -NOT(Big%Digit(J))
                    ELSE
                        IF (BLen >= SIZE(Big%Digit, KIND=kIndex)) THEN
                            CALL MemResize(Big%Digit, BLen+2)
                        END IF
                        Big%Digit(BLen) = 1
                        Big%Length = BLen + 1
                        RETURN
                    END IF
                    J = J + 1
                END IF
                DO WHILE (J < MLen)
                    Big%Digit(J) = IOR(Big%Digit(J), Mask%Digit(J)) ! ~(~Big%Digit(J)&~Mask%Digit(J))
                    J = J + 1
                END DO
                IF (Mask%Length > Big%Length) Big%Length = Mask%Length
            END IF
        ELSE
            A = Big%Digit(0)
            B = Mask%Digit(0)
            J = 1
            DO WHILE ((IOR(A, B) == 0).AND.(J < MLen))
                A = Big%Digit(J)
                B = Mask%Digit(J)
                J = J + 1
            END DO
            IF ((A /= 0).AND.(B == 0)) THEN
                Big%Digit(J-1) = -A
                DO WHILE ((J < Mask%Length).AND.(Mask%Digit(J) == 0))
                    Big%Digit(J) = IEOR(Big%Digit(J), -1)
                    J = J + 1
                END DO
                IF (J < Big%Length) THEN
                    Big%Digit(J) = NOT(IOR(Big%Digit(J), -Mask%Digit(J)))   ! ~Big%Digit(J)&~-Mask%Digit(J))
                ELSE
                    Big%Digit(J) = NOT(-Big%Digit(J))   ! Big%Digit(J) == Mask%Digit(J)
                END IF
                J = J + 1
            ELSEIF (A == 0) THEN    ! && (B /= 0 || J == MLen)
                DO WHILE ((J < MLen).AND.(Big%Digit(J) == 0))
                    J = J + 1
                END DO
                IF (J < MLen) THEN
                    Big%Digit(J) = IAND(-Big%Digit(J), Mask%Digit(J))   ! ~~Mask%Digit(J)
                END IF
                J = J + 1
            ELSE
                Big%Digit(J-1) = IAND(-A, NOT(-B))
            END IF
            DO WHILE (J < MLen)
                Big%Digit(J) = IAND(NOT(Big%Digit(J)), Mask%Digit(J))
                J = J + 1
            END DO
            Big%Length = Mask%Length
            Big%Sign = 1
        END IF
    END IF
    DO WHILE ((Big%Digit(Big%Length-1) == 0).AND.(Big%Length > 1))
        Big%Length = Big%Length - 1
    END DO

    RETURN

END SUBROUTINE Bitwise_AndNot

!******************************************************************************

MODULE FUNCTION ApInt32_LeadingZeros(Big) RESULT(NumLZ)

!** PURPOSE OF THIS SUBROUTINE:
    ! To count the number of leading zero bits

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger                    :: NumLZ

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        NumLZ = LEADZ(Big%Digit(Big%Length-1))
    ELSE
        NumLZ = -1
    END IF

    RETURN

END FUNCTION ApInt32_LeadingZeros

!******************************************************************************

MODULE FUNCTION ApInt32_TrailingZeros(Big) RESULT(NumTZ)

!** PURPOSE OF THIS SUBROUTINE:
    ! To count the number of trailing zero bits

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger                    :: NumTZ

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        NumTZ = TRAILZ(Big%Digit(0))
    ELSE
        NumTZ = -1
    END IF

    RETURN

END FUNCTION ApInt32_TrailingZeros

!******************************************************************************

MODULE FUNCTION ApInt32_Count1Bits(Big) RESULT(NumBits)

!** PURPOSE OF THIS SUBROUTINE:
    ! To count the number of 1 bits in the specified input

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger                    :: NumBits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        NumBits = 0
        DO I = 0, Big%Length-1
            NumBits = POPCNT(Big%Digit(I))
        END DO
        IF (Big%Sign < 0) NumBits = NumBits + 1
    ELSE
        NumBits = -1
    END IF

    RETURN

END FUNCTION ApInt32_Count1Bits

!******************************************************************************

MODULE FUNCTION ApInt32_Parity(Big) RESULT(ParNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine the parity of the specified input

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger                    :: ParNum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        ParNum = IAND(POPCNT(Big), 1)
    ELSE
        ParNum = -1
    END IF

    RETURN

END FUNCTION ApInt32_Parity

!******************************************************************************

MODULE FUNCTION ApInt32_SetBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set the bit at the specified position to 1
    ! For more detail, see explanation of elemental intrinsic function 'IBSET'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    tInteger,      INTENT(IN)   :: Pos
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LargeBit, SmallBit, J, K

!** FLOW

    IF (.NOT.ALLOCATED(InVal%Digit)) THEN
        CALL Handle_ErrLevel('ApInt32_SetBit', SubName, ErrSevere, &
                          'Empty Input : Storage of InVal has not yet been allocated.')
        RETURN
    END IF

    OutVal = MakeCopy(InVal)

    LargeBit = SHIFTR(Pos, LargePos)
    SmallBit = IAND(Pos, SmallMask)

    IF (OutVal%Sign > 0) THEN
        IF (LargeBit >= SIZE(OutVal%Digit)) THEN
            CALL MemResize(OutVal%Digit, LargeBit+1_kIndex)
            OutVal%Length = LargeBit+1_kIndex
        ELSEIF (LargeBit >= OutVal%Length) THEN
            DO WHILE (OutVal%Length <= LargeBit)
                OutVal%Digit(OutVal%Length) = 0
                OutVal%Length = OutVal%Length + 1_kIndex
            END DO
        END IF
        OutVal%Digit(LargeBit) = IOR(OutVal%Digit(LargeBit), SHIFTL(1, SmallBit))
    ELSE
        IF (LargeBit >= OutVal%Length) RETURN
        J = 0
        DO WHILE ((J <= LargeBit).AND.(OutVal%Digit(J) == 0))
            J = J + 1
        END DO
        IF (J > LargeBit) THEN
            OutVal%Digit(LargeBit) = SHIFTL(-1, SmallBit)
            DO WHILE (OutVal%Digit(J) == 0)
                OutVal%Digit(J) = -1
                J = J + 1
            END DO
            OutVal%Digit(J) = NOT(-OutVal%Digit(J))
            IF ((J == OutVal%Length-1).AND.(OutVal%Digit(OutVal%Length-1) == 0)) THEN
                OutVal%Length = OutVal%Length - 1_kIndex
            END IF
            RETURN
        END IF
        IF (J < LargeBit) THEN
            OutVal%Digit(LargeBit) = IAND(OutVal%Digit(LargeBit), NOT(SHIFTL(1, SmallBit)))
            DO WHILE (OutVal%Digit(OutVal%Length-1) == 0)
                OutVal%Length = OutVal%Length - 1_kIndex
            END DO
            RETURN
        END IF
        J = TRAILZ(OutVal%Digit(J))
        K = SHIFTL(1, SmallBit)
        IF (K -J > 0) THEN
            OutVal%Digit(LargeBit) = IAND(OutVal%Digit(LargeBit), NOT(K)) ! Unsigned compare.
        ELSE
            OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), IEOR(SHIFTL(J, 1) - 1, (K-1)))
            OutVal%Digit(LargeBit) = IOR(OutVal%Digit(LargeBit), K)
        END IF
    END IF

    RETURN

END FUNCTION ApInt32_SetBit

!******************************************************************************

MODULE FUNCTION ApInt32_ClearBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set the bit at the specified position to 0
    ! For more detail, see explanation of elemental intrinsic function 'IBCLR'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    tInteger,      INTENT(IN)   :: Pos
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LargeBit, SmallBit, J, K

!** FLOW

    IF (.NOT.ALLOCATED(InVal%Digit)) THEN
        CALL Handle_ErrLevel('ApInt32_ClearBit', SubName, ErrSevere, &
                          'Empty Input : Storage of InVal has not yet been allocated.')
        RETURN
    END IF

    OutVal = MakeCopy(InVal)

    LargeBit = SHIFTR(Pos, LargePos)
    SmallBit = IAND(Pos, SmallMask)

    IF (OutVal%Sign > 0) THEN
        IF (LargeBit<OutVal%Length) THEN
            OutVal%Digit(LargeBit) = IAND(OutVal%Digit(LargeBit), NOT(SHIFTL(1, SmallBit)))
            DO WHILE ((OutVal%Digit(OutVal%Length-1) == 0).AND.(OutVal%Length > 1))
                OutVal%Length = OutVal%Length - 1_kIndex
            END DO
        END IF
    ELSE
        IF (LargeBit>=SIZE(OutVal%Digit)) THEN
            CALL MemResize(OutVal%Digit, LargeBit+1_kIndex)
            OutVal%Length = LargeBit + 1_kIndex
            OutVal%Digit(LargeBit) = IOR(OutVal%Digit(LargeBit), SHIFTL(1, SmallBit))
            RETURN
        ELSEIF (LargeBit>=OutVal%Length) THEN
            DO WHILE (OutVal%Length <= LargeBit)
                OutVal%Digit(OutVal%Length) = 0
                OutVal%Length = OutVal%Length + 1_kIndex
            END DO
            OutVal%Digit(LargeBit) = IOR(OutVal%Digit(LargeBit), SHIFTL(1, SmallBit))
            RETURN
        END IF
        J = 0
        DO WHILE ((J <= LargeBit).AND.(OutVal%Digit(J) == 0))
            J = J + 1
        END DO
        IF (J > LargeBit) RETURN
        IF (J < LargeBit) THEN
            OutVal%Digit(LargeBit) = IOR(OutVal%Digit(LargeBit), SHIFTL(1, SmallBit))
            RETURN
        END IF
        J = TRAILZ(OutVal%Digit(J))
        K = SHIFTL(1, SmallBit)
        IF (J-K > 0) RETURN     ! Unsigned compare
        IF (J-K < 0) THEN
            OutVal%Digit(LargeBit) = IOR(OutVal%Digit(LargeBit), K)
            RETURN
        END IF
        J = OutVal%Digit(LargeBit)
        IF (J == IEOR(-1, K-1)) THEN
            OutVal%Digit(LargeBit) = 0
             J = LargeBit+1
            DO WHILE ((J < OutVal%Length).AND.(OutVal%Digit(J) == -1))
                OutVal%Digit(J) = 0
                J = J + 1
            END DO
            IF (J == SIZE(OutVal%Digit)) CALL MemResize(OutVal%Digit, J+2_kIndex)
            IF (J == OutVal%Length) THEN
                OutVal%Digit(OutVal%Length) = 1
                OutVal%Length = OutVal%Length + 1_kIndex
                RETURN
            END IF
            OutVal%Digit(J) = -NOT(OutVal%Digit(J))
        ELSE
            J = TRAILZ(IEOR(J, IEOR(-1, K-1)))
            OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), IOR(J, IEOR(J-1, K-1)))
        END IF
    END IF

    RETURN

END FUNCTION ApInt32_ClearBit

!******************************************************************************

MODULE FUNCTION ApInt32_FlipBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To reverse the bit at the specified position

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: InVal
    tInteger,      INTENT(IN)   :: Pos
    TYPE(ApInt32)               :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LargeBit, SmallBit, J, K

!** FLOW

    IF (.NOT.ALLOCATED(InVal%Digit)) THEN
        CALL Handle_ErrLevel('ApInt32_FlipBit', SubName, ErrSevere, &
                          'Empty Input : Storage of InVal has not yet been allocated.')
        RETURN
    END IF

    OutVal = MakeCopy(InVal)

    LargeBit = SHIFTR(Pos, LargePos)
    SmallBit = IAND(Pos, SmallMask)

    IF (LargeBit >= SIZE(OutVal%Digit)) THEN
        CALL MemResize(OutVal%Digit, LargeBit+1_kIndex)
        OutVal%Length = LargeBit + 1_kIndex
        OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), SHIFTL(1, SmallBit))
    ELSEIF (LargeBit >= OutVal%Length) THEN
        DO WHILE (OutVal%Length <= LargeBit)
            OutVal%Digit(OutVal%Length) = 0
            OutVal%Length = OutVal%Length + 1_kIndex
        END DO
        OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), SHIFTL(1, SmallBit))
    ELSEIF (OutVal%Sign > 0)  THEN
        OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), SHIFTL(1, SmallBit))
    ELSE
        J = 0
        DO WHILE ((J <= LargeBit).AND.(OutVal%Digit(J) == 0))
            J = J + 1
        END DO
        IF (J < LargeBit) THEN
            OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), SHIFTL(1, SmallBit))
        ELSE
            IF (J > LargeBit) THEN          ! TODO: Refactor with setBit?
                OutVal%Digit(LargeBit) = SHIFTL(-1, SmallBit)
                DO WHILE (OutVal%Digit(J) == 0)
                    OutVal%Digit(J) = -1
                    J = J + 1
                END DO
                OutVal%Digit(J) = NOT(-OutVal%Digit(J))
                IF ((J == OutVal%Length-1).AND.(OutVal%Digit(OutVal%Length-1) == 0)) THEN
                    OutVal%Length = OutVal%Length - 1_kIndex
                END IF
                RETURN
            END IF
            J = TRAILZ(OutVal%Digit(J))
            K = SHIFTL(1, SmallBit)
            IF (J-K > 0) THEN
                OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), IEOR(SHIFTL(J, 1) - 1, K-1))
                RETURN
            END IF
            IF (J-K < 0) THEN
                OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), K)
                RETURN
            END IF
            J = OutVal%Digit(LargeBit)
            IF (J == IEOR(-1, K-1)) THEN        ! TODO: Refactor with clearBit?
                OutVal%Digit(LargeBit) = 0
                J = LargeBit + 1
                DO WHILE ((J < OutVal%Length).AND.(OutVal%Digit(J) == -1))
                    OutVal%Digit(J) = 0
                    J = J + 1
                END DO
                IF (J == SIZE(OutVal%Digit)) CALL MemResize(OutVal%Digit, J+2_kIndex)
                IF (J==OutVal%Length) THEN
                    OutVal%Digit(OutVal%Length) = 1
                    OutVal%Length = OutVal%Length + 1_kIndex
                    RETURN
                END IF
                OutVal%Digit(J) = -NOT(OutVal%Digit(J))
            ELSE
                J = TRAILZ(IEOR(J, IEOR(-1, K-1)))
                OutVal%Digit(LargeBit) = IEOR(OutVal%Digit(LargeBit), IOR(J, IEOR(J-1, K-1)))
            END IF
        END IF
    END IF
    DO WHILE ((OutVal%Digit(OutVal%Length-1) == 0).AND.(OutVal%Length > 1))
        OutVal%Length = OutVal%Length - 1_kIndex
    END DO

    RETURN

END FUNCTION ApInt32_FlipBit

!******************************************************************************

MODULE FUNCTION ApInt32_TestBit(Big, Pos) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the bit at the specified position is 0 (False) or 1 (True)
    ! For more detail, see explanation of elemental intrinsic function 'BTEST'

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tInteger,      INTENT(IN)   :: Pos
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: LargeBit, SmallBit, J

!** FLOW

    IF (.NOT.ALLOCATED(Big%Digit)) THEN
        Flag = FalseVal
        CALL Handle_ErrLevel('ApInt32_TestBit', SubName, ErrWarning, &
                          'Empty Input : Storage of InVal has not yet been allocated.')
        RETURN
    END IF

    LargeBit = SHIFTR(Pos, LargePos)
    SmallBit = IAND(Pos, SmallMask)

    IF (LargeBit >= Big%Length) THEN
        Flag = (Big%Sign < 0)
        RETURN
    END IF
    IF (Big%Sign > 0) THEN
        Flag = (IAND(Big%Digit(LargeBit), SHIFTL(1, SmallBit)) /= 0)
        RETURN
    END IF

    J = 0
    DO WHILE ((J <= LargeBit).AND.(Big%Digit(J) == 0))
        J = J + 1
    END DO
    IF (J > LargeBit) THEN
        Flag = FalseVal
    ELSEIF (J < LargeBit) THEN
        Flag = (IAND(Big%Digit(LargeBit), SHIFTL(1, SmallBit)) == 0)
    ELSE
        J = -Big%Digit(LargeBit)
        Flag = (IAND(J, SHIFTL(1, SmallBit)) /= 0)
    END IF

    RETURN

END FUNCTION ApInt32_TestBit

!******************************************************************************

END SUBMODULE SubClass_Api32_Bitwise

!******************************************************************************
