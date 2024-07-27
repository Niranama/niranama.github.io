
! -----------------------------------------------------------------------------
! -----   BigUInt Routines for FastFloat Algorithms                       -----
! -----------------------------------------------------------------------------

FUNCTION Empty_Hi64(Truncated) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return empty value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, INTENT(OUT)   :: Truncated
    tUInt64                 :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Truncated = FalseVal
    Val = 0_kInt64

    RETURN

END FUNCTION Empty_Hi64

!******************************************************************************

FUNCTION UInt64_Hi64_I(R0, Truncated) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return high bit of uint64 value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: R0
    tLogical, INTENT(OUT)   :: Truncated
    tUInt64                 :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ShiftPos

!** FLOW

    Truncated = FalseVal
    ShiftPos = LEADZ(R0)
    Val = SHIFTL(R0, ShiftPos)

    RETURN

END FUNCTION UInt64_Hi64_I

!******************************************************************************

FUNCTION UInt64_Hi64_II(R0, R1, Truncated) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return high bit of uint64 values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: R0, R1
    tLogical, INTENT(OUT)   :: Truncated
    tUInt64                 :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ShiftPos

!** FLOW

    ShiftPos = LEADZ(R0)
    IF (ShiftPos == 0) THEN
        Truncated = R1 /= 0_kInt64
        Val = R0
    ELSE
        Truncated = SHIFTL(R1, ShiftPos) /= 0_kInt64
        Val = IOR(SHIFTL(R0, ShiftPos), SHIFTR(R1, 64-ShiftPos))
    END IF

    RETURN

END FUNCTION UInt64_Hi64_II

!******************************************************************************

#ifdef tFloat_is_tQuad

FUNCTION UInt128_Hi128_I(R0, R1, Truncated) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return high bit of uint64 values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: R0, R1
    tLogical, INTENT(OUT)   :: Truncated
    tUInt128                :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ShiftPos

!** FLOW

    Truncated = FalseVal
    ShiftPos = LEADZ(R0)
    IF (ShiftPos == 0) THEN
        Val = UInt128(R0, R1)
    ELSE
        Val%High = IOR(SHIFTL(R0, ShiftPos), SHIFTR(R1, 64-ShiftPos))
        Val%Low  = SHIFTL(R1, ShiftPos)
    END IF

    RETURN

END FUNCTION UInt128_Hi128_I

!******************************************************************************

FUNCTION UInt128_Hi128_II(R0, R1, R2, Truncated) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return high bit of uint64 values.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: R0, R1, R2
    tLogical, INTENT(OUT)   :: Truncated
    tUInt128                :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ShiftPos

!** FLOW

    ShiftPos = LEADZ(R0)
    IF (ShiftPos == 0) THEN
        Truncated = R2 /= 0_kInt64
        Val = UInt128(R0, R1)
    ELSE
        Truncated = SHIFTL(R2, ShiftPos) /= 0_kInt64
        Val%High = IOR(SHIFTL(R0, ShiftPos), SHIFTR(R1, 64-ShiftPos))
        Val%Low  = IOR(SHIFTL(R1, ShiftPos), SHIFTR(R2, 64-ShiftPos))
    END IF

    RETURN

END FUNCTION UInt128_Hi128_II

!******************************************************************************

#endif

FUNCTION BigUInt_IsEmpty(Big) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether BigUInt is empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    tLogical                    :: Flag     ! true if empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Big%Length == 0

    RETURN

END FUNCTION BigUInt_IsEmpty

!******************************************************************************

FUNCTION BigUInt_IsNonZero(Big, Index) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check if any limbs are non-zero after the given index.
    ! this needs to be done in reverse order, since the index
    ! is relative to the most significant limbs.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big      ! BigUInt object
    tSInt32,        INTENT(IN)  :: Index    ! the specified index
    tLogical                    :: Flag     ! true if the stack is empty

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I, RIndex

!** FLOW

    I = Index
    DO WHILE (I < Big%Length)
        RIndex = Big%Length - I - 1
        IF (Big%Digit(RIndex) /= 0_kInt64) THEN
            Flag = TrueVal
            RETURN
        END IF
        I = I + 1
    END DO
    Flag = FalseVal

    RETURN

END FUNCTION BigUInt_IsNonZero

!******************************************************************************

SUBROUTINE BigUInt_Push(Big, Value)

!** PURPOSE OF THIS SUBROUTINE:
    ! To append the item to the BigUInt

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)  :: Big       ! BigUInt object
    tUInt64,        INTENT(IN)     :: Value    ! item to be appended

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Big%Digit(Big%Length) = Value
    Big%Length = Big%Length + 1

    RETURN

END SUBROUTINE BigUInt_Push

!******************************************************************************

SUBROUTINE BigUInt_Extend(Big, Span)

!** PURPOSE OF THIS SUBROUTINE:
    ! To append a span of items to the stack

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)  :: Big       ! BigUInt object
    tUInt64,        INTENT(IN)     :: Span(0:)  ! span of items to be appended

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: SpanLen

! FLOW

    SpanLen = SIZE(Span)
    Big%Digit(Big%Length:Big%Length+SpanLen-1) = Span(0:SpanLen-1)
    Big%Length = Big%Length + SpanLen

    RETURN

END SUBROUTINE BigUInt_Extend

!******************************************************************************

SUBROUTINE BigUInt_Normalize(Big)

!** PURPOSE OF THIS SUBROUTINE:
    ! To normalize the BigUInt, so most-significant zero digits are removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big  ! BigUInt object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: RIndex

! FLOW

    RIndex = Big%Length - 1
    IF (RIndex >= 0) THEN
        DO WHILE (Big%Digit(RIndex) == 0_kInt64)
            Big%Length = Big%Length - 1
            RIndex = Big%Length - 1
            IF (RIndex < 0) EXIT
        END DO
    END IF

    RETURN

END SUBROUTINE BigUInt_Normalize

!******************************************************************************

FUNCTION ScalarAdd(X, Y, Overflow) RESULT(Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add two small integers, checking for overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(IN)    :: X, Y
    tLogical, INTENT(OUT)   :: Overflow
    tUInt64                 :: Z

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! add value
    Z = X + Y

    ! check overflow
    Overflow = Z .ULT. X

    RETURN

END FUNCTION ScalarAdd

!******************************************************************************

FUNCTION ScalarMul(X, Y, Carry) RESULT(Z_Low)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two small integers, getting both the high and low bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: X, Y
    tUInt64, INTENT(INOUT)  :: Carry
    tUInt64                 :: Z_Low

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Z_Hi
    tLogical    :: Overflow

!** FLOW

    CALL UMul128(X, Y, Z_Hi, Z_Low)

    Z_Low = ScalarAdd(Z_Low, Carry, Overflow)

    IF (Overflow) Z_Hi  = Z_Hi + 1_kInt64    ! cannot overflow
    Carry = Z_Hi

    RETURN

END FUNCTION ScalarMul

!******************************************************************************

SUBROUTINE BigUInt_SmallMul(Big, Y)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply BigUInt by scalar value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big  ! BigUInt object
    tUInt64,        INTENT(IN)      :: Y    ! value to be added

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Index
    tUInt64     :: Carry

!** FLOW

    Carry = 0_kInt64
    DO Index = 0, Big%Length-1
        Big%Digit(Index) = ScalarMul(Big%Digit(Index), Y, Carry)
    END DO
    IF (Carry /= 0_kInt64) CALL Big%Push(Carry)

    RETURN

END SUBROUTINE BigUInt_SmallMul

!******************************************************************************

SUBROUTINE BigUInt_LongMul(Big, Span)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply BigUInt and BigUInt using grade-school multiplication algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big      ! BigUInt object
    tUInt64,        INTENT(IN)      :: Span(0:) ! span of values

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: SpanLen
    tUInt64     :: Z(0:Big%Length+SIZE(Span)-1)

!** FLOW

    SpanLen = SIZE(Span)
    IF (SpanLen /= 0) THEN
        ! perform multiplication
        CALL MultiplyBasic(Big%Digit, Big%Length, Span, SpanLen, Z)
        ! transfer output from the buffer back to the stack
        Big%Length = Big%Length + SpanLen
        Big%Digit(0:Big%Length-1) = Z(0:Big%Length-1)
    END IF

    CALL Big%Normalize()

    RETURN

END SUBROUTINE BigUInt_LongMul

!******************************************************************************

SUBROUTINE BigUInt_From_U64(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To create BigUInt from a unsigned 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)    :: Big
    tUInt64,        INTENT(IN)       :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Big%Push(Val)
    CALL Big%Normalize()

    RETURN

END SUBROUTINE BigUInt_From_U64

!******************************************************************************

FUNCTION BigUInt_Get_Hi64(Big, Truncated) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the high 64 bits from the vector, and if bits were truncated.
    ! this is to get the significant digits for the float.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    tLogical,       INTENT(OUT) :: Truncated
    tUInt64                     :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: RIndex

!** FLOW

    IF (Big%Length == 0) THEN
        Val = Empty_Hi64(Truncated)
    ELSEIF (Big%Length == 1) THEN
        RIndex = Big%Length - 1
        Val = Uint64_Hi64_I(Big%Digit(RIndex), Truncated)
    ELSE
        RIndex = Big%Length - 1
        Val = Uint64_Hi64_II(Big%Digit(RIndex), Big%Digit(RIndex-1), Truncated)
        Truncated = Truncated .OR. Big%IsNonZero(2)
    END IF

    RETURN

END FUNCTION BigUInt_Get_Hi64

!******************************************************************************

#ifdef tFloat_is_tQuad

SUBROUTINE BigUInt_From_U128(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To create BigUInt from a 128-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big
    tUInt128,       INTENT(IN)      :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Big%Push(Val%Low)
    CALL Big%Push(Val%High)
    CALL Big%Normalize()

    RETURN

END SUBROUTINE BigUInt_From_U128

!******************************************************************************

FUNCTION BigUInt_Get_Hi128(Big, Truncated) RESULT(Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the high 128 bits from the vector, and if bits were truncated.
    ! this is to get the significant digits for the float.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    tLogical,       INTENT(OUT) :: Truncated
    tUInt128                    :: Val

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: RIndex

!** FLOW

    IF (Big%Length == 0) THEN
        Val = UInt128(0_kInt64, Empty_Hi64(Truncated))
    ELSEIF (Big%Length == 1) THEN
        RIndex = Big%Length - 1
        Val = UInt128(Uint64_Hi64_I(Big%Digit(RIndex), Truncated), 0_kInt64)
    ELSEIF (Big%Length == 2) THEN
        RIndex = Big%Length - 1
        Val = Uint128_Hi128_I(Big%Digit(RIndex), Big%Digit(RIndex-1), Truncated)
    ELSE
        RIndex = Big%Length - 1
        Val = Uint128_Hi128_II(Big%Digit(RIndex), Big%Digit(RIndex-1), &
                               Big%Digit(RIndex-2), Truncated)
        Truncated = Truncated .OR. Big%IsNonZero(3)
    END IF

    RETURN

END FUNCTION BigUInt_Get_Hi128

!******************************************************************************

#endif

FUNCTION BigUInt_Compare(Big, Other) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compare two big integers, returning the large value.
    ! assumes both are normalized. if the return value is
    ! negative, other is larger, if the return value is
    ! positive, this is larger, otherwise they are equal.
    ! the limbs are stored in little-endian order, so we
    ! must compare the limbs in ever order.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big, Other
    tSInt32                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Index

!** FLOW

    IF (Big%Length > Other%Length) THEN
        Flag = 1
    ELSEIF (Big%Length < Other%Length) THEN
        Flag = -1
    ELSE
        DO Index = Big%Length-1, 0, -1
            ASSOCIATE (XI => Big%Digit(Index), YI => Other%Digit(Index))
                IF (XI .UGT. YI) THEN
                    Flag = 1
                    RETURN
                ELSEIF (XI .ULT. YI) THEN
                    Flag = -1
                    RETURN
                END IF
            END ASSOCIATE
        END DO
        Flag = 0
    END IF

    RETURN

END FUNCTION BigUInt_Compare

!******************************************************************************

SUBROUTINE BigUInt_ShiftL(Big, N)

!** PURPOSE OF THIS SUBROUTINE:
    ! To move the limbs left by `n` bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big
    tSInt32,        INTENT(IN)      :: N

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! these parameters are for DigitBits = 64
    tSInt32, PARAMETER  :: LargePos  = 6
    tSInt32, PARAMETER  :: SmallMask = 63

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: LargeShift, SmallShift

!** FLOW

    LargeShift = SHIFTR(N, LargePos)
    SmallShift = IAND(N, SmallMask)
    IF (LargeShift > 0) CALL BigUInt_ShiftL_Limbs(Big, LargeShift)
    IF (SmallShift > 0) CALL BigUInt_ShiftL_Bits(Big, SmallShift)

    RETURN
    CONTAINS

    SUBROUTINE BigUInt_ShiftL_Bits(Big, N)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift left each limb n bits, carrying over to the new limb
        ! returns true if we were able to shift all the digits.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BigUInt), INTENT(INOUT)   :: Big
        tSInt32,        INTENT(IN)      :: N

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Index, Shl, Shr
        tUInt64     :: XI, Carry, Prev

    !** FLOW

        ! Internally, for each item, we shift left by n, and add the previous
        ! right shifted limb-bits.
        ! For example, we transform (for u8) shifted left 2, to:
        !      b10100100 b01000010
        !      b10 b10010001 b00001000
        ! ASSERT(n /= 0)
        ! ASSERT(n < sizeof(limb) * 8)

        Shl = N
        Shr = DigitBits - Shl
        Prev = 0_kInt64
        DO Index = 0, Big%Length-1
            XI = Big%Digit(Index)
            Big%Digit(Index) = IOR(SHIFTL(XI, Shl), SHIFTR(Prev, Shr))
            Prev = XI
        END DO

        Carry = SHIFTR(Prev, Shr)
        IF (Carry /= 0_kInt64) CALL Big%Push(Carry)

        RETURN

    END SUBROUTINE BigUInt_ShiftL_Bits

    !**************************************************************************

    SUBROUTINE BigUInt_ShiftL_Limbs(Big, N)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To move the limbs left by `n` limbs.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(BigUInt), INTENT(INOUT)   :: Big
        tSInt32,        INTENT(IN)      :: N

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: Buffer(0:Big%Length-1)

    !** FLOW

        IF (.NOT.Big%IsEmpty()) THEN
            ! move limbs by first copy source to buffer
            Buffer(0:Big%Length-1) = Big%Digit(0:Big%Length-1)
            ! then copy from the buffer to the destination
            Big%Digit(N:Big%Length+N-1) = Buffer(0:Big%Length-1)
            ! fill in empty limbs
            Big%Digit(0:N-1) = 0_kInt64
            ! set length
            Big%Length = Big%Length + N
        END IF

        RETURN

    END SUBROUTINE BigUInt_ShiftL_Limbs

    !**************************************************************************

END SUBROUTINE BigUInt_ShiftL

!******************************************************************************

FUNCTION BigUInt_LeadZ(Big) RESULT(N)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the number of leading zeros in the BigUInt.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    tSInt32                     :: N

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Big%IsEmpty()) THEN
        N = 0
    ELSE
        N = LEADZ(Big%Digit(Big%Length - 1))
    END IF

    RETURN

END FUNCTION BigUInt_LeadZ

!******************************************************************************

FUNCTION BigUInt_BitLen(Big) RESULT(N)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the number of bits in the BigUInt.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(IN)  :: Big
    tSInt32                     :: N

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: LZ

!** FLOW

    LZ = Big%LeadZ()
    N  = DigitBits*Big%Length - LZ

    RETURN

END FUNCTION BigUInt_BitLen

!******************************************************************************

SUBROUTINE BigUInt_Add(Big, Y)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add a long number to the BigUInt.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big
    tUInt64,        INTENT(IN)      :: Y

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Index
    tUInt64     :: Carry
    tLogical    :: Overflow

!** FLOW

    Index = 0
    Carry = Y

    DO WHILE ((Carry /= 0_kInt64).AND.(Index < Big%Length))
        Big%Digit(Index) = ScalarAdd(Big%Digit(Index), Carry, Overflow)
        IF (Overflow) THEN
            Carry = 1_kInt64
        ELSE
            Carry = 0_kInt64
        END IF
        Index = Index + 1
    END DO
    IF (Carry /= 0_kInt64) CALL Big%Push(Carry)

    RETURN

END SUBROUTINE BigUInt_Add

!******************************************************************************

SUBROUTINE BigUInt_Pow2(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply as if by 2 raised to a power.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big
    tSInt32,        INTENT(IN)      :: Exp

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Big%ShiftL(Exp)

    RETURN

END SUBROUTINE BigUInt_Pow2

!******************************************************************************

SUBROUTINE BigUInt_Pow5(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply as if by 5 raised to a power.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big
    tSInt32,        INTENT(IN)      :: Exp

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! multiply by a power of 5
    tSInt32, PARAMETER  :: Large_Step = 135
    tSInt32, PARAMETER  :: Small_Step = 27
    tSInt32, PARAMETER  :: Large_Length = 5
    tUInt64, PARAMETER  :: Small_Power_of_5(0:Small_Step) = [       &
        ToInt64(Z'0000000000000001'), ToInt64(Z'0000000000000005'), &
        ToInt64(Z'0000000000000019'), ToInt64(Z'000000000000007D'), &
        ToInt64(Z'0000000000000271'), ToInt64(Z'0000000000000C35'), &
        ToInt64(Z'0000000000003D09'), ToInt64(Z'000000000001312D'), &
        ToInt64(Z'000000000005F5E1'), ToInt64(Z'00000000001DCD65'), &
        ToInt64(Z'00000000009502F9'), ToInt64(Z'0000000002E90EDD'), &
        ToInt64(Z'000000000E8D4A51'), ToInt64(Z'0000000048C27395'), &
        ToInt64(Z'000000016BCC41E9'), ToInt64(Z'000000071AFD498D'), &
        ToInt64(Z'0000002386F26FC1'), ToInt64(Z'000000B1A2BC2EC5'), &
        ToInt64(Z'000003782DACE9D9'), ToInt64(Z'00001158E460913D'), &
        ToInt64(Z'000056BC75E2D631'), ToInt64(Z'0001B1AE4D6E2EF5'), &
        ToInt64(Z'000878678326EAC9'), ToInt64(Z'002A5A058FC295ED'), &
        ToInt64(Z'00D3C21BCECCEDA1'), ToInt64(Z'0422CA8B0A00A425'), &
        ToInt64(Z'14ADF4B7320334B9'), ToInt64(Z'6765C793FA10079D')]
    tUInt64, PARAMETER  :: Max_Native = Small_Power_of_5(Small_Step)    ! 7450580596923828125_kInt64
    tUInt64, PARAMETER  :: Large_Power_of_5(0:Large_Length-1) = [ &
        ToInt64(Z'13A1D71CFF1B172D'), ToInt64(Z'7F682D3DEFA07617'), &
        ToInt64(Z'3F0131E7FF8C90C0'), ToInt64(Z'917B01773FDCB9FE'), &
        ToInt64(Z'2C06B9D16C407A7')]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IExp

!** FLOW

    IExp = Exp

    ! multiply Big by 5**Large_Step
    DO WHILE (IExp >= Large_Step)
        CALL Big%LongMul(Large_Power_of_5)
        IExp = IExp - Large_Step
    END DO

    ! multiply Big by 5**Small_Step
    DO WHILE (IExp >= Small_Step)
        CALL Big%SmallMul(Max_Native)
        IExp = IExp - Small_Step
    END DO

    ! multiply Big by 5**IExp
    IF (IExp /= 0) CALL Big%SmallMul(Small_Power_of_5(IExp))

    RETURN

END SUBROUTINE BigUInt_Pow5

!******************************************************************************

SUBROUTINE BigUInt_Pow10(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply as if by 10 raised to a power.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BigUInt), INTENT(INOUT)   :: Big
    tSInt32,        INTENT(IN)      :: Exp

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL Big%Pow5(Exp)
    CALL Big%Pow2(Exp)

    RETURN

END SUBROUTINE BigUInt_Pow10

! -----------------------------------------------------------------------------
! -----   BigUInt Routines for YY Algorithms                              -----
! -----------------------------------------------------------------------------

SUBROUTINE BigInt_Add_U64(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To evaluate 'Big = Big + Val'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), INTENT(INOUT)    :: Big  ! a big number (can be 0)
    tUInt64,       INTENT(IN)       :: Val  ! an unsigned integer (can be 0)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Idx, Max
    tUInt64     :: Num, Add

!** FLOW

    Num = Big%Digit(0)
    Add = Num + Val
    Big%Digit(0) = Add
    IF ((Add .UGE. Num).OR.(Add .UGE. Val)) RETURN
    ! add digit
    Max = Big%Length
    DO Idx = 1, Max-1
        IF (Big%Digit(Idx) /= MaxU64) THEN
            Big%Digit(Idx) = Big%Digit(Idx) + 1_kInt64
            RETURN
        END IF
        Big%Digit(Idx) = 0_kInt64
    END DO
    Big%Digit(Big%Length) = 1_kInt64
    Big%Length = Big%Length + 1

    RETURN

END SUBROUTINE BigInt_Add_U64

!******************************************************************************

SUBROUTINE BigInt_Mul_U64(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To evaluate 'Big = Big * Val'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), INTENT(INOUT)    :: Big  ! a big number (can be 0)
    tUInt64,       INTENT(IN)       :: Val  ! an unsigned integer (cannot be 0)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Idx, Max
    tUInt64     :: Hi, Lo, Carry

!** FLOW

    ! initialize
    Idx = 0
    Max = Big%Length
    Carry = 0_kInt64
    DO WHILE (Idx < Max)
        IF (Big%Digit(Idx) /= 0_kInt64) EXIT
        Idx = Idx + 1
    END DO
    DO WHILE (Idx < Max)
        CALL UMul128_N_Add(Big%Digit(Idx), Val, Carry, Hi, Lo)
        Big%Digit(Idx) = Lo
        Carry = Hi
        Idx = Idx + 1
    END DO
    IF (Carry /= 0_kInt64) THEN
        Big%Digit(Big%Length) = Carry
        Big%Length = Big%Length + 1
    END IF

    RETURN

END SUBROUTINE BigInt_Mul_U64

!******************************************************************************

SUBROUTINE BigInt_Mul_Pow2(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
    ! To evaluate 'Big = Big * (2**Exp)'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), INTENT(INOUT)    :: Big  ! a big number (can be 0)
    tSInt32,       INTENT(IN)       :: Exp  ! an exponent integer (can be 0)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Shift, Move, Idx
    tUInt64     :: Num

!** FLOW

    ! initialize
    Shift = IAND(Exp, 63)   ! small shift == MOD(Exp, 64)
    Move  = SHIFTR(Exp, 6)  ! large shift == Exp / 64
    Idx   = Big%Length
    IF (Shift == 0) THEN
        DO WHILE (Idx > 0)
            Big%Digit(Idx + Move - 1) = Big%Digit(Idx - 1)
            Idx = Idx - 1
        END DO
        Big%Length = Big%Length + Move
        DO WHILE (Move /= 0)
            Move = Move - 1
            Big%Digit(Move) = 0_kInt64
        END DO
    ELSE
        Big%Digit(Idx) = 0_kInt64
        DO WHILE (Idx > 0)
            Num = SHIFTL(Big%Digit(Idx), Shift)
            Num = IOR(Num, SHIFTR(Big%Digit(Idx - 1), (64 - Shift)))
            Big%Digit(Idx + Move) = Num
            Idx = Idx - 1
        END DO
        Big%Digit(Move) = SHIFTL(Big%Digit(0), Shift)
        IF (Big%Digit(Big%Length + Move) /= 0_kInt64) THEN
            Big%Length = Big%Length + (Move + 1)
        ELSE
            Big%Length = Big%Length + Move
        END IF
        DO WHILE (Move /= 0)
            Move = Move - 1
            Big%Digit(Move) = 0_kInt64
        END DO
    END IF

    RETURN

END SUBROUTINE BigInt_Mul_Pow2

!******************************************************************************

SUBROUTINE BigInt_Mul_Pow10(Big, Exp)

!** PURPOSE OF THIS SUBROUTINE:
    ! To evaluate 'Big = Big * (10**Exp)'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), INTENT(INOUT)    :: Big  ! a big number (can be 0)
    tSInt32,       INTENT(IN)       :: Exp  ! an exponent integer (cannot be 0)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Maximum exponent of exact pow10
    tSInt32, PARAMETER  :: U64_POW10_MAX_EXP = 19
    ! Table: [ 10^0, ..., 10^19 ]
    tUInt64, PARAMETER  :: U64_Pow10_Table(0:U64_POW10_MAX_EXP) = [ &
        ToInt64(Z'0000000000000001'), ToInt64(Z'000000000000000A'), &
        ToInt64(Z'0000000000000064'), ToInt64(Z'00000000000003E8'), &
        ToInt64(Z'0000000000002710'), ToInt64(Z'00000000000186A0'), &
        ToInt64(Z'00000000000F4240'), ToInt64(Z'0000000000989680'), &
        ToInt64(Z'0000000005F5E100'), ToInt64(Z'000000003B9ACA00'), &
        ToInt64(Z'00000002540BE400'), ToInt64(Z'000000174876E800'), &
        ToInt64(Z'000000E8D4A51000'), ToInt64(Z'000009184E72A000'), &
        ToInt64(Z'00005AF3107A4000'), ToInt64(Z'00038D7EA4C68000'), &
        ToInt64(Z'002386F26FC10000'), ToInt64(Z'016345785D8A0000'), &
        ToInt64(Z'0DE0B6B3A7640000'), ToInt64(Z'8AC7230489E80000')]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Xpn

!** FLOW

    ! initialize
    Xpn = Exp

    DO WHILE (Xpn >= U64_POW10_MAX_EXP)
        CALL BigInt_Mul_U64(Big, U64_Pow10_Table(U64_POW10_MAX_EXP))
        Xpn = Xpn - U64_POW10_MAX_EXP
    END DO

    IF (Xpn /= 0) CALL BigInt_Mul_U64(Big, U64_Pow10_Table(Xpn))

    RETURN

END SUBROUTINE BigInt_Mul_Pow10

!******************************************************************************

FUNCTION BigInt_Compare(A, B) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compare two BigUInt.
    ! return -1 if 'a < b', +1 if 'a > b', 0 if 'a == b'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), INTENT(IN)   :: A
    TYPE(BigUInt), INTENT(IN)   :: B
    tSInt32                     :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Idx

!** FLOW

    ! first check Length components
    IF (A%Length < B%Length) THEN
        Flag = -1
        RETURN
    END IF
    IF (A%Length > B%Length) THEN
        Flag = +1
        RETURN
    END IF

    ! next check Digit components
    Idx = A%Length
    DO WHILE (Idx > 0)
        Idx = Idx - 1
        ASSOCIATE (Av => A%Digit(Idx), Bv => B%Digit(Idx))
            IF (Av .ULT. Bv) THEN
                Flag = -1
                RETURN
            END IF
            IF (Av .UGT. Bv) THEN
                Flag = +1
                RETURN
            END IF
        END ASSOCIATE
    END DO
    Flag = 0

    RETURN

END FUNCTION BigInt_Compare

!******************************************************************************

SUBROUTINE BigInt_Set_UIntType(Big, Val)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set 'Big' with the specified unsigned integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt), INTENT(INOUT)    :: Big  ! a big number (can be 0)
    tUIntType,     INTENT(IN)       :: Val  ! an unsigned integer (can be 0)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

#ifdef  tFloat_is_tQuad
    Big%Length = 2
    Big%Digit(0) = Val%Low
    Big%Digit(1) = Val%High
#else
#ifdef  tFloat_is_tDouble
    Big%Length = 1
    Big%Digit(0) = Val
#else
    Big%Length = 1
    Big%Digit(0) = ToUnsignedLong(Val)
#endif
#endif

    RETURN

END SUBROUTINE BigInt_Set_UIntType

!******************************************************************************

SUBROUTINE BigInt_Set_String(Big, SigDec, ExpDec, cStr, Aux)

!** PURPOSE OF THIS SUBROUTINE:
    ! To set 'Big' with the specified floating point number string and its related information.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(BigUInt),     INTENT(INOUT)    :: Big      ! a big number (can be 0)
    tUIntType,         INTENT(IN)       :: SigDec   ! significand in base 10
    tSInt32,           INTENT(INOUT)    :: ExpDec   ! exponent in base 10
    tCharStar, TARGET, INTENT(IN)       :: cStr     ! floating-point number string
    TYPE(StringAux),   INTENT(IN)       :: Aux      ! auxiliary string information

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0       = IACHAR('0')
    tSInt32, PARAMETER  :: IBase    = 10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (.NOT.Aux%Truncated) THEN
        ! no digit cut, set significant part only
        CALL BigInt_Set_UIntType(Big, SigDec)
        RETURN
    END IF

    ! some digits were cut, read them from 'SigCut' to 'SigEnd'
    BLOCK
        ! +++ local variables +++
        tSInt32     :: Header, SigEnd, CurIdx, Length, DigitTotLen, DotPos
        tUInt64     :: CurVal
        tLogical    :: DigitCut, HasDot
        ! +++ execution +++
        ! initialize and process auxiliary string information needed
        Header = Aux%SigCut
        SigEnd = Aux%Indices(4)
        CurIdx = Header
        Length = 0
        CurVal = 0_kInt64
        DigitCut = FalseVal
        HasDot = TrueVal
        DotPos = Aux%Indices(3) - 1
        IF (SigEnd == 0) THEN
            SigEnd = Aux%Indices(2)
            HasDot = FalseVal
            DotPos = 0
        END IF
        DigitTotLen = UIntSafeDigits + (SigEnd - Header)
        IF (HasDot) DigitTotLen = DigitTotLen + 1

        IF (DigitTotLen > MaxDecDigits) THEN
            DigitCut = TrueVal
            SigEnd = SigEnd - (DigitTotLen - (MaxDecDigits + 1))
            IF ((Aux%Indices(3) == Aux%Indices(4))) SigEnd = SigEnd - 1
            DigitTotLen = (MaxDecDigits + 1)
        END IF
        ExpDec = ExpDec - (DigitTotLen - UIntSafeDigits)

        ! set the truncated significand
        CALL BigInt_Set_UIntType(Big, SigDec)
        IF (HasDot) THEN
            ! SigCut occurred before encountering the period so we must check
            ! whether the current position is at the period
            DO WHILE (CurIdx <= SigEnd)
                IF (CurIdx /= DotPos) THEN
                    CurVal = CurVal*IBase + (IACHAR(cStr(CurIdx:CurIdx))-A0)
                    CurIdx = CurIdx + 1
                    Length = Length + 1
                    IF ((CurIdx > SigEnd).AND.(DigitCut)) THEN
                        ! The last digit must be non-zero, set it to '1' for correct rounding.
                        CurVal = CurVal - UMOD(CurVal, 10_kInt64) + 1_kInt64
                    END IF
                    IF ((Length == UIntSafeDigits).OR.(CurIdx > SigEnd)) THEN
                        CALL Bigint_Mul_Pow10(Big, Length)
                        CALL Bigint_Add_U64(Big, CurVal)
                        CurVal = 0_kInt64
                        Length = 0
                    END IF
                ELSE
                    CurIdx = CurIdx + 1
                END IF
            END DO
        ELSE
            ! SigCut occurred after encountering the period so we do not need to check
            ! whether the current position is at the period
            DO WHILE (CurIdx <= SigEnd)
                CurVal = CurVal*IBase + (IACHAR(cStr(CurIdx:CurIdx))-A0)
                CurIdx = CurIdx + 1
                Length = Length + 1
                IF ((CurIdx > SigEnd).AND.(DigitCut)) THEN
                    ! The last digit must be non-zero, set it to '1' for correct rounding.
                    CurVal = CurVal - UMOD(CurVal, 10_kInt64) + 1_kInt64
                END IF
                IF ((Length == UIntSafeDigits).OR.(CurIdx > SigEnd)) THEN
                    CALL Bigint_Mul_Pow10(Big, Length)
                    CALL Bigint_Add_U64(Big, CurVal)
                    CurVal = 0_kInt64
                    Length = 0
                END IF
            END DO
        END IF
    END BLOCK

    RETURN

END SUBROUTINE BigInt_Set_String

!******************************************************************************
