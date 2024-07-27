
FUNCTION Parse_Eight_Digits_Unrolled(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To parse eight digits immediately.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: InVal
    tUInt64             :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: K1 = ToInt64(Z'0F0F0F0F0F0F0F0F')
    tUInt64, PARAMETER  :: K2 = ToInt64(Z'00FF00FF00FF00FF')
    tUInt64, PARAMETER  :: K3 = ToInt64(Z'0000FFFF0000FFFF')
    tUInt64, PARAMETER  :: M1 = 2561_kInt64
    tUInt64, PARAMETER  :: M2 = 6553601_kInt64
    tUInt64, PARAMETER  :: M3 = 42949672960001_kInt64
    ! parameters for alternative implementation
!    tUInt64, PARAMETER  :: Mask = ToInt64(Z'000000FF000000FF')
!    tUInt64, PARAMETER  :: Mul1 = ToInt64(Z'000F424000000064')   ! 100 + (1000000ULL << 32)
!    tUInt64, PARAMETER  :: Mul2 = ToInt64(Z'0000271000000001')   ! 1 + (10000ULL << 32)
!    tUInt64, PARAMETER  :: Sub  = ToInt64(Z'3030303030303030')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
        
    OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)
    ! alternative implementation
!    OutVal = InVal - Sub
!    OutVal = (OutVal*10) + SHIFTR(OutVal, 8)    ! OutVal = (OutVal * 2561) >> 8
!    OutVal = SHIFTR(((IAND(OutVal, Mask)*Mul1) + (IAND(SHIFTR(OutVal, 16), Mask)*Mul2)), 32)
    
    RETURN

END FUNCTION Parse_Eight_Digits_Unrolled

!******************************************************************************

FUNCTION Is_Made_Of_Eight_Digits(InVal) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether we can process eight digits immediately

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: InVal
    tLogical            :: Flag

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: C1 = ToInt64(Z'F0F0F0F0F0F0F0F0')
    tUInt64, PARAMETER  :: C2 = ToInt64(Z'3333333333333333')
    tUInt64, PARAMETER  :: C3 = ToInt64(Z'0606060606060606')
!    tUInt64, PARAMETER  :: K1 = ToInt64(Z'4646464646464646')
!    tUInt64, PARAMETER  :: K2 = ToInt64(Z'3030303030303030')
!    tUInt64, PARAMETER  :: K3 = ToInt64(Z'8080808080808080')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
        
    Flag = IOR(IAND(InVal, C1), SHIFTR(IAND((InVal + C3), C1), 4)) ==  C2
    ! alternative implementations
!    Flag = (IAND(InVal, C1) == K2).AND.(IAND(InVal + C3, C1) ==  K2)
!    Flag = (IAND(IOR((InVal + K1), (InVal - K2)), K3) == 0_kInt64)
!    Flag = IAND(IAND(InVal, InVal + C3), C1) == C2

    RETURN

END FUNCTION Is_Made_Of_Eight_Digits

!******************************************************************************

FUNCTION Floor_Log10_ThreeQuartersPow2(E) RESULT(K)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute K = FLOOR(LOG10((3/4)*(2**E))) where -2956395 <= E <= 2500325

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: E    ! base-2 exponent
    tSInt32             :: K    ! base-10 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! Multiplier = FLOOR(LOG10(2) * 2**Shift)
    ! Addend     = FLOOR(LOG10(3/4) * 2**Shift)
    tSInt32, PARAMETER  :: Shift      = 41
    tSInt64, PARAMETER  :: Multiplier = 661971961083_kInt64
    tSInt64, PARAMETER  :: Addend     = -274743187321_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    K = ToInt32(SHIFTA(ToInt64(E)*Multiplier + Addend, Shift))
    
    RETURN
    
END FUNCTION Floor_Log10_ThreeQuartersPow2

!******************************************************************************

FUNCTION Floor_Log10_Pow2(E) RESULT(K)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute K = FLOOR(LOG10(2**E)) where -5456721 <= E <= 5456721

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: E    ! base-2 exponent
    tSInt32             :: K    ! base-10 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! Multiplier = FLOOR(LOG10(2) * 2**Shift)
    tSInt32, PARAMETER  :: Shift      = 41
    tSInt64, PARAMETER  :: Multiplier = 661971961083_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    K = ToInt32(SHIFTA(ToInt64(E)*Multiplier, Shift))
    
    RETURN
    
END FUNCTION Floor_Log10_Pow2

!******************************************************************************

FUNCTION Floor_Log2_Pow10(K) RESULT(E)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute E = FLOOR(LOG2(10**K)) where -1838394 <= K <= 1838394

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: K    ! base-10 exponent
    tSInt32             :: E    ! base-2 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! Multiplier = FLOOR(LOG2(10) * 2**Shift)
    tSInt32, PARAMETER  :: Shift      = 38
    tSInt64, PARAMETER  :: Multiplier = 913124641741_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    E = ToInt32(SHIFTA(ToInt64(K)*Multiplier, Shift))
    
    RETURN
    
END FUNCTION Floor_Log2_Pow10

!******************************************************************************

FUNCTION Floor_Log2_Pow5(P) RESULT(E)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute E = FLOOR(LOG2(5**P)) where -32768 <= Exp <= 32768

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: P    ! base-5 exponent
    tSInt32             :: E    ! base-2 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! Multiplier = FLOOR(LOG2(5) * 2**Shift)
    tSInt32, PARAMETER  :: Shift      = 46
    tSInt64, PARAMETER  :: Multiplier = 163391164108059_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    E = ToInt32(SHIFTA(ToInt64(P)*Multiplier, Shift))
    
    RETURN
    
END FUNCTION Floor_Log2_Pow5

!******************************************************************************

FUNCTION Floor_Log10_Pow5(E) RESULT(K)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute K = Floor(Log10(5**E))

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN)    :: E    ! ! 0 <= Exp <= 2**15
    tSInt32                :: K

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32, PARAMETER  :: Shift      = 48
    tSInt64, PARAMETER  :: Multiplier = 196742565691928_kInt64
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! The first value this approximation fails for is 5^2621 which is just greater than 10^1832.
    K = ToInt32(SHIFTR(ToInt64(E)*Multiplier, Shift))
    
    RETURN
    
END FUNCTION Floor_Log10_Pow5

!******************************************************************************

FUNCTION Ceiling_Log2_Pow5(P) RESULT(E)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute E = CEILING(LOG2(5**P))

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: P    ! base-5 exponent
    tSInt32             :: E    ! base-2 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    E = Floor_Log2_Pow5(P) + 1
    
    RETURN
    
END FUNCTION Ceiling_Log2_Pow5

!******************************************************************************

FUNCTION Pow5Bits(Exp) RESULT(Pow5)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Pow5 = Ceiling(Log2(5**Exp)).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Exp       ! 0 <= Exp <= 32768
    tUInt32             :: Pow5

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! Multiplier = FLOOR(LOG2(5) * 2**Shift)
    tSInt32, PARAMETER  :: Shift      = 46
    tUInt64, PARAMETER  :: Multiplier = 163391164108059_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! note: this is similar to 'Ceiling_Log2_Pow5' but only for positive Exp
    Pow5 = ToInt32(SHIFTR(Exp*Multiplier, Shift) + 1_kInt64)
    
    RETURN

END FUNCTION Pow5Bits

!**************************************************************************

SUBROUTINE MultiplyBasic(X, XLen, Y, YLen, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To multiply two magnitude arrays and return the result using grade-school algorithm

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN)     :: XLen             ! The length of the first array
    tUInt64, INTENT(IN)     :: X(0:XLen-1)      ! The first magnitude array
    tSInt32, INTENT(IN)     :: YLen             ! The length of the second array
    tUInt64, INTENT(IN)     :: Y(0:YLen-1)      ! The second magnitude array
    tUInt64, INTENT(OUT)    :: Z(0:XLen+YLen-1) ! The result array

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  :: MinI64   = ToInt64(Z'8000000000000000')   ! min signed 64-bit
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry64, ProductHi, ProductLo, Sum
    tSInt32     :: I, J

!** FLOW

    Carry64 = 0_kInt64
    DO J = 0, YLen-1
        CALL UMul128(X(0), Y(J), ProductHi, ProductLo)
        Z(J) = ProductLo + Carry64
        IF (IEOR(Z(J), MinI64) < IEOR(ProductLo, MinI64)) THEN
            Carry64 = ProductHi + 1_kInt64
        ELSE
            Carry64 = ProductHi
        END IF
    END DO
    Z(YLen) = Carry64
    DO I = 1, XLen-1
        Carry64 = 0_kInt64
        DO J = 0, YLen-1
            CALL UMul128(X(I), Y(J), ProductHi, ProductLo)
            Sum = ProductLo + Z(I+J)
            IF (IEOR(Sum, MinI64) < IEOR(ProductLo, MinI64)) ProductHi = ProductHi + 1_kInt64
            Z(I+J) = Sum + Carry64
            IF (IEOR(Z(I+J), MinI64) < IEOR(Sum, MinI64)) THEN
                Carry64 = ProductHi + 1_kInt64
            ELSE
                Carry64 = ProductHi
            END IF
        END DO
        Z(I+YLen) = Carry64
    END DO

    RETURN

END SUBROUTINE MultiplyBasic

!******************************************************************************

SUBROUTINE ShiftRight(X, ShiftPos)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the input right by the specified amount

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(INOUT)  :: X(0:)
    tSInt32, INTENT(IN)     :: ShiftPos

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: LargeShift, SmallShift

!** FLOW

    IF (ShiftPos == 0) RETURN
    LargeShift = SHIFTR(ShiftPos, 6)
    IF (LargeShift >= SIZE(X)) THEN
        X = 0_kInt64
    ELSE
        SmallShift = IAND(ShiftPos, 63)
        IF (LargeShift > 0) CALL ShiftLarge(X, LargeShift)
        IF (SmallShift > 0) CALL ShiftSmall(X, SmallShift)
    END IF
        
    RETURN
    CONTAINS

    SUBROUTINE ShiftSmall(X, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift the input right by the given amount (less than 64).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(INOUT)  :: X(0:)
        tSInt32, INTENT(IN)     :: Shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: I, XLen
        tUInt64     :: Nxt

    !** FLOW
        
        XLen = SIZE(X)
        Nxt = X(0)
        DO I = 0, XLen-2
            X(I) = IOR(SHIFTR(Nxt, Shift), SHIFTL(X(I+1), 64-Shift))
            Nxt = X(I+1)
        END DO
        X(XLen-1) = SHIFTR(X(XLen-1), Shift)
    
        RETURN

    END SUBROUTINE ShiftSmall

    !******************************************************************************

    SUBROUTINE ShiftLarge(X, Shift)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shift the input right by 64*shift, i.e. moves each
        ! element of the array shift positions to the right.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(INOUT)  :: X(0:)
        tSInt32, INTENT(IN)     :: Shift

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Index, XLen

    !** FLOW
    
        XLen = SIZE(X)
        DO Index = 0, XLen-Shift-1
            X(Index) = X(Shift+Index)
        END DO
        X(XLen-Shift:) = 0_kInt64

        RETURN

    END SUBROUTINE ShiftLarge

    !******************************************************************************

END SUBROUTINE ShiftRight

!******************************************************************************

SUBROUTINE Multiply_N_ShiftRight(X, XLen, Y, YLen, Shift, Z)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform multiplication and then right shift

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN)     :: XLen             ! The length of the first array
    tUInt64, INTENT(IN)     :: X(0:XLen-1)      ! The first magnitude array
    tSInt32, INTENT(IN)     :: YLen             ! The length of the second array
    tUInt64, INTENT(IN)     :: Y(0:YLen-1)      ! The second magnitude array
    tSInt32, INTENT(IN)     :: Shift            ! shift position of bits in the result array
    tUInt64, INTENT(OUT)    :: Z(0:XLen+YLen-1) ! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL MultiplyBasic(X, XLen, Y, YLen, Z)
    CALL ShiftRight(Z, Shift)

    RETURN

END SUBROUTINE Multiply_N_ShiftRight

!******************************************************************************

SUBROUTINE Increment_Value(X)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increase value of the input by 1

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,  INTENT(INOUT) :: X(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: XLen, I
    tUInt64     :: Sum, Carry

!** FLOW

    XLen = SIZE(X)
    Sum = X(0) + 1_kInt64
    Carry = SHIFTR(IOR(IAND(X(0), 1_kInt64), IAND(IOR(X(0), 1_kInt64), NOT(Sum))), 63)
    X(0) = Sum
    IF (Carry /= 0_kInt64) THEN
        I = 1_kIndex
        DO
            X(I) = X(I) + 1_kInt64
            IF (.NOT.((I < XLen).AND.(X(I) == 0))) EXIT
            I = I + 1_kIndex
        END DO
    END IF

    RETURN

END SUBROUTINE Increment_Value

!******************************************************************************
#ifndef     tFloat_is_tQuad
#ifdef      tFloat_is_tSingle

FUNCTION Get_Pow10_64Bits(K) RESULT(Pow10)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the 64-bit approximation of power of ten
    ! -> Pow10 = 10**K

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: K        ! the power
    tUInt64             :: Pow10    ! the power of ten

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Pow10Cache(0:1)  ! the power of ten in little-endian order

!** FLOW

    IF ((K >= Pow10_256_Small_MinExp).AND.(K <= Pow10_256_Small_MaxExp)) THEN
        ! get only the upper 64 bits
        Pow10 = Pow10_256_Small_Table(3, K)
    ELSE
        CALL Compute_Pow10_128Bits(K, Pow10Cache)
        ! get only the upper 64 bits
        Pow10 = Pow10Cache(1)
    END IF

    RETURN

END FUNCTION Get_Pow10_64Bits

#endif
!******************************************************************************

FUNCTION Get_Pow10_128Bits(K) RESULT(Pow10)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the 128-bit approximation of power of ten
    ! -> Pow10 = 10**K

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: K        ! the power
    tUInt128            :: Pow10    ! the power of ten

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Pow10Cache(0:1)  ! the power of ten in little-endian order

!** FLOW
    
    IF ((K >= Pow10_256_Small_MinExp).AND.(K <= Pow10_256_Small_MaxExp)) THEN
        ! get only the upper 128 bits
        Pow10Cache(0:1) = Pow10_256_Small_Table(2:3, K)
    ELSE
        CALL Compute_Pow10_128Bits(K, Pow10Cache)
    END IF
    Pow10 = UInt128(Pow10Cache(1), Pow10Cache(0))

    RETURN

END FUNCTION Get_Pow10_128Bits

!******************************************************************************

SUBROUTINE Compute_Pow10_128Bits(K, Pow10)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the 128-bit approximation of power of ten
    ! -> Pow10 = 10**K

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN)     :: K            ! the power
    tUInt64, INTENT(OUT)    :: Pow10(0:1)   ! the power of ten in little-endian order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Pow10_Index, KBase, Offset, Alpha
    tUInt64     :: Pow10_Cache(0:1), Pow5(0:1)
    tUInt64     :: Pow10_256(0:3)

!** FLOW
    
    ! compute essential indices
    Pow10_Index = (K - Pow10_256_Compressed_MinExp) / Pow5_128_Size
    KBase = Pow10_Index * Pow5_128_Size + Pow10_256_Compressed_MinExp
    Offset = K - KBase
    
    ! get base cache (only upper 128 bits) where table data is stored in little-endian
    ! order (i.e. the least significant byte is 0 and the most significant byte is 3)
    Pow10_Cache(0:1) = Pow10_256_Compressed_Table(2:3, Pow10_Index)
    IF (Offset == 0) THEN
        Pow10 = Pow10_Cache
        RETURN
    END IF

    ! compute the required amount of bit-shift where Alpha should be in the range (0, 128)
    Alpha = Floor_Log2_Pow10(KBase + Offset) - Floor_Log2_Pow10(KBase) - Offset

    ! compute the approximation for the specified power K
    Pow5 = Pow5_128_Table(:, Offset)
    CALL Multiply_N_ShiftRight(Pow10_Cache, 2, Pow5, 2, Alpha, Pow10_256)
    Pow10(0:1) = Pow10_256(0:1)
    CALL Increment_Value(Pow10)
    
    RETURN
    
END SUBROUTINE Compute_Pow10_128Bits

#endif
!******************************************************************************
#ifdef      tFloat_is_tQuad

FUNCTION Get_Pow10_256Bits(K) RESULT(Pow10)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the 256-bit approximation of power of ten
    ! -> Pow10 = 10**K

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: K            ! the power
    tUInt64             :: Pow10(0:3)   ! the power of ten in little-endian order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
#ifdef     USE_FULL_TABLE_OF_POWERS_OF_TEN
    SELECT CASE (K)
    CASE (-5005:-4000)
        Pow10 = Pow10_256_Full_Table_1(:, K)
    CASE (-3999:-3000)
        Pow10 = Pow10_256_Full_Table_2(:, K)
    CASE (-2999:-2000)
        Pow10 = Pow10_256_Full_Table_3(:, K)
    CASE (-1999:-1000)
        Pow10 = Pow10_256_Full_Table_4(:, K)
    CASE (-999:0)
        Pow10 = Pow10_256_Full_Table_5(:, K)
    CASE (1:1000)
        Pow10 = Pow10_256_Full_Table_6(:, K)
    CASE (1001:2000)
        Pow10 = Pow10_256_Full_Table_7(:, K)
    CASE (2001:3000)
        Pow10 = Pow10_256_Full_Table_8(:, K)
    CASE (3001:4000)
        Pow10 = Pow10_256_Full_Table_9(:, K)
    CASE (4001:5019)
        Pow10 = Pow10_256_Full_Table_10(:, K)
    END SELECT
#else
    IF ((K >= Pow10_256_Small_MinExp).AND.(K <= Pow10_256_Small_MaxExp)) THEN
        Pow10 = Pow10_256_Small_Table(:, K)
    ELSE
        CALL Compute_Pow10_256Bits(K, Pow10)
    END IF
#endif

    RETURN

END FUNCTION Get_Pow10_256Bits

!******************************************************************************
#ifndef USE_FULL_TABLE_OF_POWERS_OF_TEN

SUBROUTINE Compute_Pow10_256Bits(K, Pow10)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the 256-bit approximation of power of ten
    ! -> Pow10 = 10**K

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN)     :: K            ! the power
    tUInt64, INTENT(OUT)    :: Pow10(0:3)   ! the power of ten in little-endian order

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Pow10_Index, KBase, Offset, Alpha
    tUInt64     :: Pow10_Cache(0:3), Pow5(0:1)
    tUInt64     :: Pow10_384(0:5)

!** FLOW
    
    ! compute essential indices
    Pow10_Index = (K - Pow10_256_Compressed_MinExp) / Pow5_128_Size
    KBase = Pow10_Index * Pow5_128_Size + Pow10_256_Compressed_MinExp
    Offset = K - KBase
    
    ! get base cache
    Pow10_Cache = Pow10_256_Compressed_Table(:, Pow10_Index)
    IF (Offset == 0) THEN
        Pow10 = Pow10_Cache
        RETURN
    END IF

    ! compute the required amount of bit-shift Alpha should be in the range (0, 256)
    Alpha = Floor_Log2_Pow10(KBase + Offset) - Floor_Log2_Pow10(KBase) - Offset

    ! compute the approximation for the specified power K
    Pow5 = Pow5_128_Table(:, Offset)
    CALL Multiply_N_ShiftRight(Pow10_Cache, 4, Pow5, 2, Alpha, Pow10_384)
    Pow10(0:3) = Pow10_384(0:3)
    CALL Increment_Value(Pow10)

    RETURN

END SUBROUTINE Compute_Pow10_256Bits

#endif
#endif
!******************************************************************************

FUNCTION Handle_Invalid_String(cStr, Start, Negative) RESULT(RealNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To handle invalid input string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr
    tSInt32,   INTENT(IN)   :: Start
    tLogical,  INTENT(IN)   :: Negative
    tFloat                  :: RealNum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32    :: Finish
    tSInt32    :: Ptr, Q

!** FLOW
    
    ! Could not parse a decimal floating-point number.  Start has been
    ! advanced over any leading spaces.
    Ptr = Start
    Finish = LEN_TRIM(cStr)
    IF (Start > Finish) THEN
        ! empty string
        RealNum = IEEE_VALUE(ZeroFloat, IEEE_QUIET_NAN)
    ELSEIF (Finish == Ptr + 2) THEN
        IF ((ToUpper(cStr(Ptr:Ptr)) == 'N').AND.(ToUpper(cStr(Ptr+1:Ptr+1)) == 'A').AND. &
            (ToUpper(cStr(Ptr+2:Ptr+2)) == 'N')) THEN
            ! Exact NAN
            RealNum = IEEE_VALUE(ZeroFloat, IEEE_QUIET_NAN)
        ELSE
            ! Invalid NAN
            RealNum = IEEE_VALUE(ZeroFloat, IEEE_SIGNALING_NAN)
        END IF
    ELSE
        ! Try to parse Inf, maybe with a sign
        Q = Ptr
        IF (Q <= Finish) THEN
            IF (Is_Character_Sign(cStr(Q:Q))) Q = Q + 1
        END IF
        IF (Finish == Q + 2) THEN
            IF ((ToUpper(cStr(Q:Q)) == 'I').AND.(ToUpper(cStr(Q+1:Q+1)) == 'N').AND. &
                (ToUpper(cStr(Q+2:Q+2)) == 'F')) THEN
                IF (Negative) THEN
                    RealNum = IEEE_VALUE(OneFloat, IEEE_NEGATIVE_INF)
                ELSE
                    RealNum = IEEE_VALUE(OneFloat, IEEE_POSITIVE_INF)
                END IF
            ELSE
                ! Invalid NAN
                RealNum = IEEE_VALUE(ZeroFloat, IEEE_SIGNALING_NAN)
            END IF
        ELSEIF (Finish == Q + 7) THEN
            IF ((ToUpper(cStr(Q:Q)) == 'I').AND.(ToUpper(cStr(Q+1:Q+1)) == 'N').AND. &
                (ToUpper(cStr(Q+2:Q+2)) == 'F').AND.(ToUpper(cStr(Q+3:Q+3)) == 'I').AND. &
                (ToUpper(cStr(Q+4:Q+4)) == 'N').AND.(ToUpper(cStr(Q+5:Q+5)) == 'I').AND. &
                (ToUpper(cStr(Q+6:Q+6)) == 'T').AND.(ToUpper(cStr(Q+7:Q+7)) == 'Y')) THEN
                IF (Negative) THEN
                    RealNum = IEEE_VALUE(OneFloat, IEEE_NEGATIVE_INF)
                ELSE
                    RealNum = IEEE_VALUE(OneFloat, IEEE_POSITIVE_INF)
                END IF
            ELSE
                ! Invalid NAN
                RealNum = IEEE_VALUE(ZeroFloat, IEEE_SIGNALING_NAN)
            END IF
        ELSE
            ! Invalid input
            RealNum = IEEE_VALUE(ZeroFloat, IEEE_SIGNALING_NAN)
        END IF
    END IF
    
    RETURN

CONTAINS

    FUNCTION Is_Character_Sign(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given character is a 'sign' character

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tChar,  INTENT(IN)  :: Chr          ! character
        tLogical            :: Flag         ! true if the character is valid

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tCharParam  :: SET_SIGNS = '+-'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW:
    
        Flag = (INDEX(SET_SIGNS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Sign

    !**************************************************************************

    FUNCTION ToUpper(ChrIn) RESULT(ChrOut)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To change case of the input character to upper case if applicable

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tChar,  INTENT(IN)  :: ChrIn
        tChar               :: ChrOut
        
    !** SUBROUTINE PARAMETER DECLARATIONS:
        tCharParam  :: SET_ALPHABETS_LOWER = 'abcdefghijklmnopqrstuvwxyz'
        tCharParam  :: SET_ALPHABETS_UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: ID

    !** FLOW:

        ID = INDEX(SET_ALPHABETS_LOWER, ChrIn)
        IF (ID > 0) THEN
            ChrOut = SET_ALPHABETS_UPPER(ID:ID)
        ELSE
            ChrOut = ChrIn
        END IF

        RETURN

    END FUNCTION ToUpper

    !**************************************************************************

END FUNCTION Handle_Invalid_String

!******************************************************************************
