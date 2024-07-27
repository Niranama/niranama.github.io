
FUNCTION HPDec_Should_Round_Up(HP, RoundToDigit) RESULT(Flag)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(IN)    :: HP
    tSInt32,          INTENT(IN)    :: RoundToDigit
    tLogical                        :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF ((RoundToDigit < 0).OR.(RoundToDigit >= HP%NumDigits)) THEN
        Flag = FalseVal
        RETURN
    END IF

    ! If we're right in the middle and there are no extra digits
    IF ((HP%Digits(RoundToDigit) == 5) .AND.(RoundToDigit + 1 == HP%NumDigits)) THEN

        ! Round up if we've truncated (since that means the result is slightly
        ! higher than what's represented.)
        IF (HP%Truncated) THEN
            Flag = TrueVal
            RETURN
        END IF

        ! If this exactly halfway, round to even.
        IF (RoundToDigit == 0) THEN
            ! When the input is ".5".
            Flag = FalseVal
            RETURN
        END IF
        Flag = MOD(HP%Digits(RoundToDigit - 1), 2_kByte) /= 0
        RETURN
    END IF
    ! If there are digits after roundToDigit, they must be non-zero since we
    ! trim trailing zeroes after all operations that change digits.
    Flag = HP%Digits(RoundToDigit) >= 5
    
    RETURN
    
END FUNCTION HPDec_Should_Round_Up

!******************************************************************************

FUNCTION HPDec_Get_Num_New_Digits(HP, LShiftAmount) RESULT(NewDigits)

    ! Takes an amount to left shift and returns the number of new digits needed
    ! to store the result based on LEFT_SHIFT_DIGIT_TABLE.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(IN)    :: HP
    tUInt32,          INTENT(IN)    :: LShiftAmount
    tUInt32                         :: NewDigits
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0 = IACHAR('0')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: PowerOfFive
    tUInt8      :: CurDigit, P5Digit
    tUInt32     :: Indx
    tUInt32     :: Length

!** FLOW
    
    Length      = LShift_Length(LShiftAmount)
    PowerOfFive = LShift_PowFive(LShiftAmount)(1:Length)
    NewDigits   = LShift_Digits(LShiftAmount)

    Indx = 1
    DO WHILE (Indx <= Length)
        IF (Indx > HP%NumDigits) THEN
            NewDigits = NewDigits - 1
            RETURN
        END IF
        P5Digit  = ToInt8(IACHAR(PowerOfFive(Indx:Indx)) - A0)
        CurDigit = HP%Digits(Indx-1)
        IF (CurDigit /= P5Digit) THEN
            IF (CurDigit < P5Digit) NewDigits = NewDigits - 1
            RETURN
        END IF
        Indx = Indx + 1
    END DO
    
    RETURN
    
END FUNCTION HPDec_Get_Num_New_Digits

!******************************************************************************

SUBROUTINE HPDec_Trim_Trailing_Zeroes(HP)

    ! Trim all trailing 0s

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(INOUT) :: HP

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    DO WHILE ((HP%NumDigits > 0).AND.(HP%Digits(HP%NumDigits - 1) == 0))
        HP%NumDigits = HP%NumDigits - 1
    END DO
    IF (HP%NumDigits == 0) THEN
        HP%DecimalPoint = 0
    END IF
    
    RETURN
    
END SUBROUTINE HPDec_Trim_Trailing_Zeroes

!******************************************************************************

SUBROUTINE HPDec_Right_Shift(HP, ShiftAmount)

    ! Perform a digitwise binary non-rounding right shift on this value by ShiftAmount.
    ! The ShiftAmount can't be more than MAX_SHIFT_AMOUNT to prevent overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(INOUT) :: HP
    tUInt32,          INTENT(IN)    :: ShiftAmount

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: ReadIndx
    tUInt32     :: WriteIndx
    tUInt64     :: Accumulator
    tUInt64     :: ShiftMask
    tUInt64     :: ReadDigit
    tUInt64     :: WriteDigit

!** FLOW
    
    ! initialize
    ReadIndx = 0
    WriteIndx = 0
    Accumulator = 0_kInt64
    ShiftMask = SHIFTL(1_kInt64, ShiftAmount) - 1_kInt64

    ! Warm Up phase: we don't have enough digits to start writing, so just
    ! read them into the Accumulator.
    DO WHILE (SHIFTR(Accumulator, ShiftAmount) == 0_kInt64)
        ReadDigit = 0_kInt64
        ! If there are still digits to read, read the next one, else the digit is
        ! assumed to be 0.
        IF (ReadIndx < HP%NumDigits) ReadDigit = HP%Digits(ReadIndx)
        Accumulator = Accumulator * 10_kInt64 + ReadDigit
        ReadIndx = ReadIndx + 1
    END DO

    ! Shift the decimal point by the number of digits it took to fill the
    ! Accumulator.
    HP%DecimalPoint = HP%DecimalPoint - (ReadIndx - 1)

    ! Middle phase: we have enough digits to write, as well as more digits to
    ! read. Keep reading until we run out of digits.
    DO WHILE (ReadIndx < HP%NumDigits)
        ReadDigit = HP%Digits(ReadIndx)
        WriteDigit = SHIFTR(Accumulator, ShiftAmount)
        Accumulator = IAND(Accumulator, ShiftMask)
        HP%Digits(WriteIndx) = ToInt8(WriteDigit)
        Accumulator = Accumulator * 10_kInt64 + ReadDigit
        ReadIndx = ReadIndx + 1
        WriteIndx = WriteIndx + 1
    END DO

    ! Cool Down phase: All of the readable digits have been read, so just write
    ! the remainder, DO WHILE treating any more digits as 0.
    ! DO WHILE (Accumulator > 0_kInt64)
    DO WHILE (Accumulator /= 0_kInt64)   ! +++ unsigned comparison +++
        WriteDigit = SHIFTR(Accumulator, ShiftAmount)
        Accumulator = IAND(Accumulator, ShiftMask)
        IF (WriteIndx < MAX_NUM_DIGITS) THEN
            HP%Digits(WriteIndx) = ToInt8(WriteDigit)
            WriteIndx = WriteIndx + 1
        ELSEIF (WriteDigit /= 0_kInt64) THEN     ! +++ unsigned comparison +++
            HP%Truncated = TrueVal
        END IF
        Accumulator = Accumulator * 10_kInt64
    END DO
    HP%NumDigits = WriteIndx
    CALL HP%TrimTrailingZeroes()

    RETURN
    
END SUBROUTINE HPDec_Right_Shift

!******************************************************************************

SUBROUTINE HPDec_Left_Shift(HP, ShiftAmount)

    ! Perform a digitwise binary non-rounding left shift on this value by ShiftAmount.
    ! The ShiftAmount can't be more than MAX_SHIFT_AMOUNT to prevent overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(INOUT) :: HP
    tUInt32,          INTENT(IN)    :: ShiftAmount

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: NewDigits
    tSInt32     :: ReadIndx
    tUInt32     :: WriteIndx
    tUInt64     :: Accumulator
    tUInt64     :: NextAccumulator
    tUInt64     :: WriteDigit

!** FLOW
    
    ! initialize
    NewDigits = HP%GetNumNewDigits(ShiftAmount)
    ReadIndx  = HP%NumDigits - 1
    WriteIndx = HP%NumDigits + NewDigits
    Accumulator = 0_kInt64

    ! No Warm Up phase. Since we're putting digits in at the top and taking
    ! digits from the bottom we don't have to wait for the Accumulator to fill.

    ! Middle phase: while we have more digits to read, keep reading as well as
    ! writing.
    DO WHILE (ReadIndx >= 0)
        Accumulator = Accumulator + SHIFTL(ToInt64(HP%Digits(ReadIndx)), ShiftAmount)
        ! +++ unsigned division and modulation +++
        ! NextAccumulator = Accumulator / 10_kInt64
        ! WriteDigit = Accumulator - (10_kInt64 * NextAccumulator)
        CALL UDivMod(Accumulator, 10_kInt64, NextAccumulator, WriteDigit)
        WriteIndx = WriteIndx - 1
        IF (WriteIndx < MAX_NUM_DIGITS) THEN
            HP%Digits(WriteIndx) = ToInt8(WriteDigit)
        ELSEIF (WriteDigit /= 0_kInt64) THEN
            HP%Truncated = TrueVal
        END IF
        Accumulator = NextAccumulator
        ReadIndx = ReadIndx - 1
    END DO

    ! Cool Down phase: there are no more digits to read, so just write the
    ! remaining digits in the Accumulator.
    ! DO WHILE (Accumulator > 0_kInt64)
    DO WHILE (Accumulator /= 0_kInt64)   ! +++ unsigned comparison +++
        ! +++ unsigned division and modulation +++
        ! NextAccumulator = Accumulator / 10_kInt64
        ! WriteDigit = Accumulator - (10_kInt64 * NextAccumulator)
        CALL UDivMod(Accumulator, 10_kInt64, NextAccumulator, WriteDigit)
        WriteIndx = WriteIndx - 1
        IF (WriteIndx < MAX_NUM_DIGITS) THEN
            HP%Digits(WriteIndx) = ToInt8(WriteDigit)
        ELSEIF (WriteDigit /= 0_kInt64) THEN
            HP%Truncated = TrueVal
        END IF
        Accumulator = NextAccumulator
    END DO

    HP%NumDigits = HP%NumDigits + NewDigits
    IF (HP%NumDigits > MAX_NUM_DIGITS) THEN
        HP%NumDigits = MAX_NUM_DIGITS
    END IF
    HP%DecimalPoint = HP%DecimalPoint + NewDigits
    CALL HP%TrimTrailingZeroes()

    RETURN
    
END SUBROUTINE HPDec_Left_Shift

!******************************************************************************

SUBROUTINE HPDec_Construct(HP, cStr, Start, Finish)

    ! To construct 'HPDecimal' based on input string (cStr) where
    !   - Start is the index of the first valid numeric character, and
    !   - Finish is the index of the last valid character (== length of the input
    !     string excluding trailing space(s))
    ! The routine assumes that cStr is a 'VALID' floating point string and
    ! Start is less than Finish.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(INOUT) :: HP
    tCharStar,        INTENT(IN)    :: cStr
    tSInt32,          INTENT(IN)    :: Start
    tSInt32,          INTENT(IN)    :: Finish

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0 = IACHAR('0')
    tCharParam          :: SET_DIGITS    = '0123456789'
    tCharParam          :: SET_EXPONENTS = 'EeDdQq'
    tCharParam          :: SET_SIGNS     = '+-'
    tCharParam          :: SET_INTEGERS  = SET_DIGITS // SET_SIGNS

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Ptr
    tSInt32     :: Add2Exp
    tLogical    :: SawDot
    tUInt32     :: TotalDigits  ! This counts the digits in the number, even if
                                ! there isn't space to store them all.

!** FLOW

    ! initialize
    Ptr = Start
    SawDot = FalseVal
    TotalDigits = 0

    DO WHILE ((Is_Character_Digit(cStr(Ptr:Ptr))).OR.(cStr(Ptr:Ptr) == '.'))
        IF (cStr(Ptr:Ptr) == '.') THEN
            IF (SawDot) EXIT
            HP%DecimalPoint = TotalDigits
            SawDot = TrueVal
        ELSE
            IF ((cStr(Ptr:Ptr) == '0').AND.(HP%NumDigits == 0)) THEN
                HP%DecimalPoint = HP%DecimalPoint - 1
                Ptr = Ptr + 1
                IF (Ptr <= Finish) THEN
                    CYCLE
                ELSE
                    EXIT
                END IF
            END IF
            TotalDigits = TotalDigits + 1
            IF (HP%NumDigits < MAX_NUM_DIGITS) THEN
                HP%Digits(HP%NumDigits) = ToInt8(IACHAR(cStr(Ptr:Ptr))-A0)
                HP%NumDigits = HP%NumDigits + 1
            ELSEIF (cStr(Ptr:Ptr) /= '0') THEN
                HP%Truncated = TrueVal
            END IF
        END IF
        Ptr = Ptr + 1
        IF (Ptr > Finish) EXIT
    END DO

    IF (.NOT.SawDot) HP%DecimalPoint = TotalDigits

    IF (Ptr <= Finish) THEN
        IF (Is_Character_Exponent(cStr(Ptr:Ptr))) THEN
            Ptr = Ptr + 1
            IF (Ptr <= Finish) THEN
                IF (Is_Character_Integer(cStr(Ptr:Ptr))) THEN
                    Add2Exp = I32_FromChar(cStr(Ptr:))
                    IF (Add2Exp > 100000) THEN
                        Add2Exp = 100000
                    ELSEIF (Add2Exp < -100000) THEN
                        Add2Exp = -100000
                    END IF
                    HP%DecimalPoint = HP%DecimalPoint + Add2Exp
                END IF
            END IF
        ELSEIF (Is_Character_Sign(cStr(Ptr:Ptr))) THEN
            Add2Exp = I32_FromChar(cStr(Ptr:))
            IF (Add2Exp > 100000) THEN
                Add2Exp = 100000
            ELSEIF (Add2Exp < -100000) THEN
                Add2Exp = -100000
            END IF
            HP%DecimalPoint = HP%DecimalPoint + Add2Exp
        END IF
    END IF

    CALL HP%TrimTrailingZeroes()

    RETURN
    CONTAINS

    FUNCTION Is_Character_Digit(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given character is in the 'DIGIT' set

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tChar,  INTENT(IN)  :: Chr          ! character
        tLogical            :: Flag         ! true if the character is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW:
    
        Flag = (INDEX(SET_DIGITS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Digit

    !**************************************************************************

    FUNCTION Is_Character_Exponent(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given character is an 'exponent' character

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tChar,  INTENT(IN)  :: Chr          ! character
        tLogical            :: Flag         ! true if the character is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW:
    
        Flag = (INDEX(SET_EXPONENTS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Exponent

    !**************************************************************************

    FUNCTION Is_Character_Integer(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given character is in the 'INTEGER' set

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tChar,  INTENT(IN)  :: Chr          ! character
        tLogical            :: Flag         ! true if the character is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW:
    
        Flag = (INDEX(SET_INTEGERS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Integer

    !**************************************************************************

    FUNCTION Is_Character_Sign(Chr) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the given character is in the 'SIGN' set

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tChar,  INTENT(IN)  :: Chr          ! character
        tLogical            :: Flag         ! true if the character is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

    !** FLOW:
    
        Flag = (INDEX(SET_SIGNS, Chr) /= 0)

        RETURN

    END FUNCTION Is_Character_Sign

!**************************************************************************

END SUBROUTINE HPDec_Construct

!******************************************************************************

SUBROUTINE HPDec_Shift(HP, Shift)

    ! Binary shift left (ShiftAmount > 0) or right (ShiftAmount < 0)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(INOUT) :: HP
    tSInt32,          INTENT(IN)    :: Shift

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ShiftAmount

!** FLOW
    
    ShiftAmount = Shift
    IF (ShiftAmount > 0) THEN
        ! Left shift
        DO WHILE (ShiftAmount > MAX_SHIFT_AMOUNT)
            CALL HP%LeftShift(MAX_SHIFT_AMOUNT)
            ShiftAmount = ShiftAmount - MAX_SHIFT_AMOUNT
        END DO
        CALL HP%LeftShift(ShiftAmount)
    ELSEIF (ShiftAmount < 0) THEN
        ! Right shift
        DO WHILE (ShiftAmount < -MAX_SHIFT_AMOUNT)
            CALL HP%RightShift(MAX_SHIFT_AMOUNT)
            ShiftAmount = ShiftAmount + MAX_SHIFT_AMOUNT
        END DO
        CALL HP%RightShift(-ShiftAmount)
    END IF

    RETURN
    
END SUBROUTINE HPDec_Shift

!******************************************************************************
#ifdef tFloat_is_tSingle

SUBROUTINE HPDec_Round_To_UInt(HP, ResVal)

    ! Round the number represented to the closest value of UInt32.
    ! This is done ignoring overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(IN)    :: HP
    tUInt32,          INTENT(OUT)   :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: CurDigit

!** FLOW
    
    ResVal = 0
    CurDigit = 0

    DO WHILE ((CurDigit < HP%DecimalPoint).AND.(CurDigit < HP%NumDigits))
        ResVal = ResVal * 10 + ToInt32(HP%Digits(CurDigit))
        CurDigit = CurDigit + 1
    END DO

    ! If there are implicit 0s at the end of the number, include those.
    DO WHILE (CurDigit < HP%DecimalPoint)
        ResVal = ResVal * 10
        CurDigit = CurDigit + 1
    END DO
    IF (HP%ShouldRoundUp(HP%DecimalPoint)) THEN
        ResVal = ResVal + 1
    END IF
    
    RETURN
    
    END SUBROUTINE HPDec_Round_To_UInt

!******************************************************************************
#else
#ifdef tFloat_is_tDouble

SUBROUTINE HPDec_Round_To_UInt(HP, ResVal)

    ! Round the number represented to the closest value of UIntType.
    ! This is done ignoring overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(IN)    :: HP
    tUInt64,          INTENT(OUT)   :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: CurDigit

!** FLOW
    
    ResVal = 0_kInt64
    CurDigit = 0

    DO WHILE ((CurDigit < HP%DecimalPoint).AND.(CurDigit < HP%NumDigits))
        ResVal = ResVal * 10_kInt64 + ToInt64(HP%Digits(CurDigit))
        CurDigit = CurDigit + 1
    END DO

    ! If there are implicit 0s at the end of the number, include those.
    DO WHILE (CurDigit < HP%DecimalPoint)
        ResVal = ResVal * 10_kInt64
        CurDigit = CurDigit + 1
    END DO
    IF (HP%ShouldRoundUp(HP%DecimalPoint)) THEN
        ResVal = ResVal + 1_kInt64
    END IF
    
    RETURN
    
END SUBROUTINE HPDec_Round_To_UInt

!******************************************************************************
#else

SUBROUTINE HPDec_Round_To_UInt(HP, ResVal)

    ! Round the number represented to the closest value of UIntType.
    ! This is done ignoring overflow.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HPDecimal), INTENT(IN)    :: HP
    tUInt128,         INTENT(OUT)   :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: CurDigit

!** FLOW
    
    ResVal = ZeroU128
    CurDigit = 0

    DO WHILE ((CurDigit < HP%DecimalPoint).AND.(CurDigit < HP%NumDigits))
        CALL Multiply(ResVal, 10)
        CALL Add(ResVal, ToInt32(HP%Digits(CurDigit)))
        CurDigit = CurDigit + 1
    END DO

    ! If there are implicit 0s at the end of the number, include those.
    DO WHILE (CurDigit < HP%DecimalPoint)
        CALL Multiply(ResVal, 10)
        CurDigit = CurDigit + 1
    END DO
    IF (HP%ShouldRoundUp(HP%DecimalPoint)) THEN
        CALL Increment(ResVal)
    END IF
    
    RETURN
    
END SUBROUTINE HPDec_Round_To_UInt

!******************************************************************************
#endif
#endif
