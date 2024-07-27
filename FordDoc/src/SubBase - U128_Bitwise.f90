
SUBMODULE (ModBase_UInt128) SubBase_U128_Bitwise

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to bitwise
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

MODULE FUNCTION U128_ShiftLeftOnce(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical left shift by 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IOR(SHIFTL(InVal%High, 1), SHIFTR(InVal%Low, 63))
    OutVal%Low  = SHIFTL(InVal%Low, 1)

    RETURN

END FUNCTION U128_ShiftLeftOnce

!******************************************************************************

MODULE FUNCTION U128_ShiftRightOnce(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical right shift by 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTR(InVal%High, 1)
    OutVal%Low  = IOR(SHIFTR(InVal%Low, 1), SHIFTL(InVal%High, 63))

    RETURN

END FUNCTION U128_ShiftRightOnce

!******************************************************************************

MODULE FUNCTION U128_ShiftLeft64(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical left shift by 64.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = InVal%Low
    OutVal%Low  = 0_kInt64

    RETURN

END FUNCTION U128_ShiftLeft64

!******************************************************************************

MODULE FUNCTION U128_ShiftRight64(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical right shift by 64.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = 0_kInt64
    OutVal%Low  = InVal%High

    RETURN

END FUNCTION U128_ShiftRight64

!******************************************************************************

MODULE FUNCTION U128_ShiftLeft63Down(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical left shift by 63 or less.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 63
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IOR(SHIFTL(InVal%High, ShiftPos), SHIFTR(InVal%Low, 64 - ShiftPos))
    OutVal%Low  = SHIFTL(InVal%Low, ShiftPos)

    RETURN

END FUNCTION U128_ShiftLeft63Down

!******************************************************************************

MODULE FUNCTION U128_ShiftRight63Down(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical right shift by 63 or less.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 63
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTR(InVal%High, ShiftPos)
    OutVal%Low  = IOR(SHIFTR(InVal%Low, ShiftPos), SHIFTL(InVal%High, 64 - ShiftPos))

    RETURN

END FUNCTION U128_ShiftRight63Down

!******************************************************************************

MODULE FUNCTION U128_ShiftLeft64Up(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical left shift by 64 or more.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: ShiftPos !! 64 <= ShiftPos <= 128
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = SHIFTL(InVal%Low, ShiftPos - 64)
    OutVal%Low  = 0_kInt64

    RETURN

END FUNCTION U128_ShiftLeft64Up

!******************************************************************************

MODULE FUNCTION U128_ShiftRight64Up(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical right shift by 64 or more.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: ShiftPos !! 64 <= ShiftPos <= 128
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = 0_kInt64
    OutVal%Low  = SHIFTR(InVal%High, ShiftPos - 64)

    RETURN

END FUNCTION U128_ShiftRight64Up

!******************************************************************************

MODULE FUNCTION U128_ShiftLogical(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical (left or right) shift of the UInt128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    !> Value must be between -128 and 128. <br>
    ! - Positive, the shift is to the left. <br>
    ! - Negative, the shift is to the right.
    tSInt32,  INTENT(IN)    :: ShiftPos
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos < 0) THEN
        OutVal = SHIFTR(InVal, -ShiftPos)
    ELSE
        OutVal = SHIFTL(InVal, ShiftPos)
    END IF

    RETURN

END FUNCTION U128_ShiftLogical

!******************************************************************************

MODULE FUNCTION U128_ShiftLeft(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical left shift of the UInt128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 128
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos < 0) THEN
        CALL Handle_ErrLevel('U128_ShiftLeft', ModName, ErrSevere, 'ShiftPos must be nonnegative number')
    ELSEIF (ShiftPos == 0) THEN
        OutVal = InVal
    ELSEIF (ShiftPos >= 128) THEN
        OutVal = ZeroU128
    ELSEIF (ShiftPos >= 64) THEN
        OutVal%High = SHIFTL(InVal%Low, ShiftPos - 64)
        OutVal%Low  = 0_kInt64
    ELSE
        OutVal%High = IOR(SHIFTL(InVal%High, ShiftPos), SHIFTR(InVal%Low, 64 - ShiftPos))
        OutVal%Low  = SHIFTL(InVal%Low, ShiftPos)
    END IF

    RETURN

END FUNCTION U128_ShiftLeft

!******************************************************************************

MODULE FUNCTION U128_ShiftRight(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform logical right shift of the UInt128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 128
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ShiftPos < 0) THEN
        CALL Handle_ErrLevel('U128_ShiftRight', ModName, ErrSevere, 'ShiftPos must be nonnegative number')
    ELSEIF (ShiftPos == 0) THEN
        OutVal = InVal
    ELSEIF (ShiftPos >= 128) THEN
        OutVal = ZeroU128
    ELSEIF (ShiftPos >= 64) THEN
        OutVal%High = 0_kInt64
        OutVal%Low  = SHIFTR(InVal%High, ShiftPos - 64)
    ELSE
        OutVal%High = SHIFTR(InVal%High, ShiftPos)
        OutVal%Low  = IOR(SHIFTR(InVal%Low, ShiftPos), SHIFTL(InVal%High, 64 - ShiftPos))
    END IF

    RETURN

END FUNCTION U128_ShiftRight

!******************************************************************************

MODULE FUNCTION U128_Rotate(InVal, ShiftPos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform a circular shift of the rightmost bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    !> Value must be between -128 and 128. <br>
    ! - Positive, the shift is to the left. <br>
    ! - Negative, the shift is to the right.
    tSInt32,  INTENT(IN)    :: ShiftPos
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32    :: LeftShift

!** FLOW

    IF (ShiftPos == 0) THEN
        OutVal = InVal
        RETURN
    ELSEIF (ABS(ShiftPos) == 128) THEN
        OutVal = ZeroU128
        RETURN
    ELSEIF (ABS(ShiftPos) > 128) THEN
        LeftShift = MOD(ShiftPos, 128)
    ELSE
        LeftShift = ShiftPos
    END IF
    IF (LeftShift < 0) LeftShift = 128 + LeftShift
    OutVal = IOR(SHIFTL(InVal, LeftShift), SHIFTR(InVal, 128 - LeftShift))

    RETURN

END FUNCTION U128_Rotate

!******************************************************************************

MODULE FUNCTION U128_Not(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the bitwise logical complement of the UInt128 object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = NOT(InVal%High)
    OutVal%Low  = NOT(InVal%Low)

    RETURN

END FUNCTION U128_Not

!******************************************************************************

MODULE FUNCTION U128_Ior(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform an inclusive OR on corresponding bits of the UInt128 objects.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IOR(LhsVal%High, RhsVal%High)
    OutVal%Low  = IOR(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION U128_Ior

!******************************************************************************

MODULE FUNCTION U128_Iand(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform a logical AND on corresponding bits of the UInt128 objects.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IAND(LhsVal%High, RhsVal%High)
    OutVal%Low  = IAND(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION U128_Iand

!******************************************************************************

MODULE FUNCTION U128_Ieor(LhsVal, RhsVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform an exclusive OR on corresponding bits of the UInt128 objects.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: LhsVal
    tUInt128, INTENT(IN)    :: RhsVal
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal%High = IEOR(LhsVal%High, RhsVal%High)
    OutVal%Low  = IEOR(LhsVal%Low, RhsVal%Low)

    RETURN

END FUNCTION U128_Ieor

!******************************************************************************

MODULE FUNCTION U128_LeadingZeros(U128) RESULT(NumLZ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To count the number of leading zero bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tSInt32                 :: NumLZ

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (U128%High == 0_kInt64) THEN
        NumLZ = LEADZ(U128%Low) + 64
    ELSE
        NumLZ = LEADZ(U128%High)
    END IF

    RETURN

END FUNCTION U128_LeadingZeros

!******************************************************************************

MODULE FUNCTION U128_TrailingZeros(U128) RESULT(NumTZ)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To count the number of trailing zero bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tSInt32                 :: NumTZ

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (U128%Low == 0_kInt64) THEN
        NumTZ = TRAILZ(U128%High) + 64
    ELSE
        NumTZ = TRAILZ(U128%Low)
    END IF

    RETURN

END FUNCTION U128_TrailingZeros

!******************************************************************************

MODULE FUNCTION U128_Count1Bits(U128) RESULT(NumBits)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To count the number of 1 bits in the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tSInt32                 :: NumBits

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    NumBits = POPCNT(U128%Low) + POPCNT(U128%High)

    RETURN

END FUNCTION U128_Count1Bits

!******************************************************************************

MODULE FUNCTION U128_Parity(U128) RESULT(ParNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To determine the parity of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tSInt32                 :: ParNum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! ParNum = IAND(POPCNT(U128), 1)
    ParNum = POPPAR(U128%Low) + POPPAR(U128%High)
    IF (ParNum == 2) ParNum = 0

    RETURN

END FUNCTION U128_Parity

!******************************************************************************

MODULE FUNCTION U128_SetBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the bit at the specified position to 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: Pos
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (NOT_IN_RANGE(Pos, 0, 127)) THEN
        CALL Handle_ErrLevel('U128_SetBit', ModName, ErrSevere, 'Pos must be between 0 and 127.')
        RETURN
    END IF

    IF (Pos < 64) THEN
        OutVal%Low  = IBSET(InVal%Low, Pos)
        OutVal%High = InVal%High
    ELSE
        OutVal%Low  = InVal%Low
        OutVal%High = IBSET(InVal%High, Pos-64)
    END IF

    RETURN

END FUNCTION U128_SetBit

!******************************************************************************

MODULE FUNCTION U128_ClearBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the bit at the specified position to 0.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: Pos
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (NOT_IN_RANGE(Pos, 0, 127)) THEN
        CALL Handle_ErrLevel('U128_ClearBit', ModName, ErrSevere, 'Pos must be between 0 and 127.')
        RETURN
    END IF

    IF (Pos < 64) THEN
        OutVal%Low  = IBCLR(InVal%Low, Pos)
        OutVal%High = InVal%High
    ELSE
        OutVal%Low  = InVal%Low
        OutVal%High = IBCLR(InVal%High, Pos-64)
    END IF

    RETURN

END FUNCTION U128_ClearBit

!******************************************************************************

MODULE FUNCTION U128_FlipBit(InVal, Pos) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reverse the bit at the specified position.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: Pos
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: HiPos

!** FLOW

    IF (NOT_IN_RANGE(Pos, 0, 127)) THEN
        CALL Handle_ErrLevel('U128_FlipBit', ModName, ErrSevere, 'Pos must be between 0 and 127.')
        RETURN
    END IF

    IF (Pos < 64) THEN
        IF (BTEST(InVal%Low, Pos)) THEN
            ! clear bit
            OutVal%Low = IBCLR(InVal%Low, Pos)
        ELSE
            ! set bit
            OutVal%Low = IBSET(InVal%Low, Pos)
        END IF
        OutVal%High = InVal%High
    ELSE
        HiPos = Pos-64
        IF (BTEST(InVal%High, HiPos)) THEN
            ! clear bit
            OutVal%High = IBCLR(InVal%High, HiPos)
        ELSE
            ! set bit
            OutVal%High = IBSET(InVal%High, HiPos)
        END IF
        OutVal%Low = InVal%Low
    END IF

    RETURN

END FUNCTION U128_FlipBit

!******************************************************************************

MODULE FUNCTION U128_TestBit(U128, Pos) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the bit at the specified position is 0 (False) or 1 (True).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: U128
    tSInt32,  INTENT(IN)    :: Pos
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (NOT_IN_RANGE(Pos, 0, 127)) THEN
        CALL Handle_ErrLevel('U128_TestBit', ModName, ErrSevere, 'Pos must be between 0 and 127.')
        RETURN
    END IF

    IF (Pos < 64) THEN
        Flag = BTEST(U128%Low, Pos)
    ELSE
        Flag = BTEST(U128%High, Pos-64)
    END IF

    RETURN

END FUNCTION U128_TestBit

!******************************************************************************

MODULE FUNCTION U128_ExtractBits(InVal, Pos, Len) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To extract a sequence of bits according to the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: Pos
    tSInt32,  INTENT(IN)    :: Len
    tUInt128                :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Len1, Len2, Len3

!** FLOW

    ! first, check input validity
    IF (Len < 0) THEN
        CALL Handle_ErrLevel('U128_ExtractBits', ModName, ErrSevere, 'Len must be nonnegative.')
        RETURN
    ELSEIF (Len == 0) THEN
        OutVal = ZeroU128
        RETURN
    ELSEIF (NOT_IN_RANGE(Pos, 0, 127)) THEN
        CALL Handle_ErrLevel('U128_ExtractBits', ModName, ErrSevere, 'Pos must be between 0 and 127.')
        RETURN
    ELSEIF (Pos + Len > 128) THEN
        CALL Handle_ErrLevel('U128_ExtractBits', ModName, ErrSevere, 'Pos + Len > 128.')
        RETURN
    END IF

    OutVal = ZeroU128
    IF (Pos < 64) THEN
        IF (Pos + Len <= 64) THEN
            ! bit fields are in only lower elements of both input and output
            CALL MVBITS(InVal%Low, Pos, Len, OutVal%Low, 0)
        ELSE
            IF (Len <= 64) THEN
                ! bit fields are in both lower and upper elements of input
                ! but only in lower element of output
                Len1 = 64-Pos
                Len2 = Len - Len1
                CALL MVBITS(InVal%Low,  Pos, Len1, OutVal%Low,    0)
                CALL MVBITS(InVal%High,   0, Len2, OutVal%Low, Len1)
            ELSE
                ! bit fields are in lower and upper elements of both input and output
                Len1 = 64-Pos           ! Input%Low  -> Output%Low
                Len2 = 64-Len1          ! Input%High -> Output%Low
                Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                CALL MVBITS(InVal%Low,   Pos, Len1, OutVal%Low,     0)
                CALL MVBITS(InVal%High,    0, Len2, OutVal%Low,  Len1)
                CALL MVBITS(InVal%High, Len2, Len3, OutVal%High,    0)
            END IF
        END IF
    ELSE
        ! one of the simplest cases where bit fields are in upper element of input
        ! and in lower element of output
        CALL MVBITS(InVal%High, Pos-64, Len, OutVal%Low, 0)
    END IF

    RETURN

END FUNCTION U128_ExtractBits

!******************************************************************************

MODULE SUBROUTINE U128_MoveBits(InVal, InPos, Len, OutVal, OutPos)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy a sequence of bits (a bit field) from one location to another.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: InVal
    tSInt32,  INTENT(IN)    :: InPos
    tSInt32,  INTENT(IN)    :: Len
    tUInt128, INTENT(INOUT) :: OutVal
    tSInt32,  INTENT(IN)    :: OutPos

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32        :: Len1, Len2, Len3

!** FLOW

    ! first, check input validity
    IF (Len < 0) THEN
        CALL Handle_ErrLevel('U128_MoveBits', ModName, ErrSevere, 'Len must be nonnegative.')
        RETURN
    ELSEIF (Len == 0) THEN
        RETURN
    ELSEIF (NOT_IN_RANGE(InPos, 0, 127)) THEN
        CALL Handle_ErrLevel('U128_MoveBits', ModName, ErrSevere, 'InPos must be between 0 and 127.')
        RETURN
    ELSEIF (NOT_IN_RANGE(OutPos, 0, 127)) THEN
        CALL Handle_ErrLevel('U128_MoveBits', ModName, ErrSevere, 'OutPos must be between 0 and 127.')
        RETURN
    ELSEIF (InPos + Len > 128) THEN
        CALL Handle_ErrLevel('U128_MoveBits', ModName, ErrSevere, 'InPos + Len > 128.')
        RETURN
    ELSEIF (OutPos + Len > 128) THEN
        CALL Handle_ErrLevel('U128_MoveBits', ModName, ErrSevere, 'OutPos + Len > 128.')
        RETURN
    END IF

    IF (InPos < 64) THEN
        IF (InPos + Len <= 64) THEN
            IF (OutPos < 64) THEN
                IF (OutPos + Len <= 64) THEN
                    ! one of the simplest cases where bit fields are in lower elements
                    CALL MVBITS(InVal%Low, InPos, Len, OutVal%Low, OutPos)
                ELSE
                    ! bit fields are in lower element of input but in both lower and
                    ! upper elements of output
                    Len1 = 64-OutPos
                    Len2 = Len - Len1
                    CALL MVBITS(InVal%Low, InPos,      Len1, OutVal%Low,  OutPos)
                    CALL MVBITS(InVal%Low, InPos+Len1, Len2, OutVal%High,      0)
                END IF
            ELSE
                ! one of the simplest cases where bit fields are in lower element of input
                ! and upper element of output, respectively
                CALL MVBITS(InVal%Low, InPos, Len, OutVal%High, OutPos-64)
            END IF
        ELSE
            IF (OutPos < 64) THEN
                IF (OutPos + Len <= 64) THEN
                    ! bit fields are in both lower and upper element of input but
                    ! only in lower element of output
                    Len1 = 64-InPos
                    Len2 = Len - Len1
                    CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,      OutPos)
                    CALL MVBITS(InVal%High,     0, Len2, OutVal%Low, OutPos+Len1)
                ELSE
                    ! the most complicated cases where bit fields are in lower
                    ! and upper elements of both input and output
                    IF (InPos == OutPos) THEN
                        Len1 = 64-InPos     ! Input%Low  -> Output%Low
                        Len2 = Len-Len1     ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,  OutPos)
                        CALL MVBITS(InVal%High,     0, Len2, OutVal%High,      0)
                    ELSEIF (InPos < OutPos) THEN
                        Len1 = 64-OutPos        ! Input%Low  -> Output%Low
                        Len2 = 64-(InPos+Len1)  ! Input%Low  -> Output%High
                        Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,      InPos,  Len1, OutVal%Low,  OutPos)
                        CALL MVBITS(InVal%Low,  InPos+Len1, Len2, OutVal%High,      0)
                        CALL MVBITS(InVal%High,          0, Len3, OutVal%High,   Len2)
                    ELSE
                        Len1 = 64-InPos         ! Input%Low  -> Output%Low
                        Len2 = 64-(OutPos+Len1) ! Input%High -> Output%Low
                        Len3 = Len-(Len1+Len2)  ! Input%High -> Output%High
                        CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%Low,       OutPos)
                        CALL MVBITS(InVal%High,     0, Len2, OutVal%Low,  OutPos+Len1)
                        CALL MVBITS(InVal%High,  Len2, Len3, OutVal%High,           0)
                    END IF
                END IF
            ELSE
                ! bit fields are in both lower and upper element of input but
                ! only in upper element of output
                Len1 = 64-InPos
                Len2 = Len - Len1
                CALL MVBITS(InVal%Low,  InPos, Len1, OutVal%High,      OutPos-64)
                CALL MVBITS(InVal%High,     0, Len2, OutVal%High, OutPos-64+Len1)
            END IF
        END IF
    ELSE
        IF (OutPos < 64) THEN
            IF (OutPos + Len <= 64) THEN
                ! one of the simplest cases where bit fields are in upper element of input
                ! and lower element of output, respectively
                CALL MVBITS(InVal%High, InPos-64, Len, OutVal%Low, OutPos)
            ELSE
                ! bit fields are in upper element of input but in both lower and
                ! upper elements of output
                Len1 = 64-OutPos
                Len2 = Len - Len1
                CALL MVBITS(InVal%High, InPos-64,      Len1, OutVal%Low,  OutPos)
                CALL MVBITS(InVal%High, InPos-64+Len1, Len2, OutVal%High,      0)
            END IF
        ELSE
            ! one of the simplest cases where bit fields are in upper elements
            CALL MVBITS(InVal%High, InPos-64, Len, OutVal%High, OutPos-64)
        END IF
    END IF

    RETURN

END SUBROUTINE U128_MoveBits

!******************************************************************************

END SUBMODULE SubBase_U128_Bitwise

!******************************************************************************
