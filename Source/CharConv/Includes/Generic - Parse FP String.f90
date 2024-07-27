
FUNCTION Parse_Fortran_String(cStr, SigDec, ExpDec, NegSign, Aux, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    ! To parse a valid Fortran real (floating point) number that has one of the two following forms:
    ! 1. A number without exponent part -> [S]N[N...]
    ! 2. A number with exponent part    -> [S]N[N...]E[S]N[N...]
    !   Where
    !   [ ] indicates an optional field
    !   S is a sign indicator (required if negative '-', optional if positive '+').
    !   N is a decimal digit (0 through 9). A decimal point may appear anywhere
    !       after the sign (but before the exponent).
    !   E is an exponent indicator (either 'e' or 'E')
    ! The valid number is similar to "Real" Fortran constant (literal) with some small differences.
    ! 1. A whole number without a decimal point (i.e. "Integer" constant) is considered valid.
    ! 2. The optional kind parameter (_k) is not allowed here.
    !
    ! Note: Leading and/or trailing space(s) are allowed.  For example, "  1.23"
    !   and "1.23   " are considered valid.  However, no space is allowed inside
    !   the supposedly valid number.  For instance, "1 .2 3" is considered NOT valid.
    !   Therefore, this routine is not totally compatible with Fortran READ statement
    !   where spaces inside the valid number are allowed.
    !   However, this can easily be done by adding an optional 'Inside Space' flag that
    !   provide an interpretation of the spaces as 'zero' or 'ignored'.  Then, the input
    !   will be pre-processed according to the flag.  Nonetheless, this routine neglects
    !   this optional input because it will make the routine much less efficient due to
    !   the fact that we will need to scan the whole string twice and we will also need
    !   to copy the input string into a buffer string and working with the buffer instead
    !   of directly handling the input string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr
    tUIntType,            INTENT(OUT)   :: SigDec   ! significand in base 10
    tSInt32,              INTENT(OUT)   :: ExpDec   ! exponent in base 10
    tLogical,             INTENT(OUT)   :: NegSign
    TYPE(StringAux),      INTENT(OUT)   :: Aux
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tLogical                            :: Valid

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0       = IACHAR('0')
    tSInt32, PARAMETER  :: IBase    = 10
    tSInt32, PARAMETER  :: ExpLimit = ToInt32(Z'10000000')
#ifdef tFloat_is_tQuad
    tSInt32, PARAMETER  :: I64SafeDigits = 18
    tSInt64, PARAMETER  :: TenPow18 = 10_kInt64**I64SafeDigits
    tSInt32, PARAMETER  :: FP_Max_Digits = 39
#else
#ifdef tFloat_is_tDouble
    tSInt32, PARAMETER  :: FP_Max_Digits = 19
#else
    tSInt32, PARAMETER  :: FP_Max_Digits = 9
#endif
#endif

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType               :: SigLimit
#ifdef tFloat_is_tQuad
    tSInt64                 :: CurVal
    tSInt32                 :: CurLen, AddCount
#endif
    tSInt32                 :: Indx, StrLen
    tSInt32                 :: NFrac
    tSInt32                 :: ESign
    tSInt32                 :: SigCount
    tSInt32                 :: IntegralStart, IntegralEnd
    tSInt32                 :: FractionStart, FractionEnd
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: AtLeastOneDigit, Truncated

!** FLOW

    ! initialize
    SigDec = ZeroUInt
    ExpDec = 0
    Valid = FalseVal
    AtLeastOneDigit = FalseVal
    Truncated = FalseVal
    StrLen = LEN_TRIM(cStr)     ! get valid string length by removing the trailing space(s)
    Aux%Truncated = Truncated
    Aux%Indices   = 0
    Aux%Start     = 0
    Aux%SigCut    = 0
    IF (StrLen == 0) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
        RETURN
    END IF
    
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
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
            RETURN
        END IF
    END IF
    
    ! check for sign of the significand
    NegSign = FalseVal
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this string only contains a sign without a digit.'
            RETURN
        END IF
        ! check whether the following character is a digit or a dot
        CurChr => cStr(Indx:Indx)
        IF (((CurChr < '0').OR.(CurChr > '9')).AND.(CurChr /= '.')) THEN
            ! current character is neither a digit nor a dot
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a sign must be followed by a digit or the dot.'
            RETURN
        END IF
    END IF
    
    Aux%Start = Indx
    
    ! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        AtLeastOneDigit = TrueVal
        ! the current digit is zero so loop through the following
        ! characters until a non-zero character is found
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            ! only zero digits encountered
            Valid = TrueVal
            RETURN
        END IF
    END IF
    
    ! compute for the significand in the integral part
#ifdef tFloat_is_tQuad
    CurVal   = 0_kInt64
    CurLen   = 0
    AddCount = 0
#endif
    IntegralStart = 0
    IntegralEnd   = 0
    SigCount      = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        AtLeastOneDigit = TrueVal
        IntegralStart = Indx
        DO WHILE (Indx <= StrLen)
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
            CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
            CurLen = CurLen +  1
            IF (CurLen == I64SafeDigits) THEN
                IF (AddCount == 0) THEN
                    SigDec%Low = CurVal
                ELSE
                    SigDec = SigDec*TenPow18 + CurVal
                END IF
                CurVal = 0_kInt64
                CurLen = 0
                AddCount = AddCount + 1
            END IF
#else
            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
            Indx = Indx + 1
        END DO
        SigCount = Indx - IntegralStart
        IntegralEnd = Indx - 1
#ifdef tFloat_is_tQuad
        IF (CurLen /= 0) THEN
            IF (AddCount == 0) THEN
                SigDec%Low = CurVal
            ELSE
                SigDec = SigDec*(10_kInt64**CurLen) + CurVal
            END IF
            CurVal = 0_kInt64
            CurLen = 0
            AddCount = AddCount + 1
        END IF
#endif
    END IF

    ! check whether the current character is a dot
    FractionStart = 0
    FractionEnd   = 0
    NFrac         = 0
    IF (Indx <= StrLen) THEN
        IF (cStr(Indx:Indx) == '.') THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                    AtLeastOneDigit = TrueVal
                    NFrac = Indx
                    IF (SigCount > 0) THEN
                        FractionStart = Indx
                        ! continue computing for the significand
                        DO WHILE (Indx <= StrLen)
                            CurChr => cStr(Indx:Indx)
                            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
                            CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
                            CurLen = CurLen +  1
                            IF (CurLen == I64SafeDigits) THEN
                                IF (AddCount > 0) THEN
                                    SigDec = SigDec*TenPow18 + CurVal
                                ELSE
                                    SigDec%Low = CurVal
                                END IF
                                CurVal = 0_kInt64
                                CurLen = 0
                                AddCount = AddCount + 1
                            END IF
#else
                            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
                            Indx = Indx + 1
                        END DO
                        NFrac = Indx - NFrac
                        SigCount = SigCount + NFrac
                        FractionEnd = Indx - 1
                    ELSE
                        ! check for leading zero(s)
                        IF (cStr(Indx:Indx) == '0') THEN
                            DO WHILE (Indx <= StrLen)
                                IF (cStr(Indx:Indx) /= '0') EXIT
                                Indx = Indx + 1
                            END DO
                            IF (Indx > StrLen) THEN
                                ! only zero digits encountered
                                Valid = TrueVal
                                RETURN
                            END IF
                        END IF
                        CurChr => cStr(Indx:Indx)
                        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
                            FractionStart = Indx
                            ! start computing for the significand
                            DO WHILE (Indx <= StrLen)
                                CurChr => cStr(Indx:Indx)
                                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
                                CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
                                CurLen = CurLen +  1
                                IF (CurLen == I64SafeDigits) THEN
                                    IF (AddCount == 0) THEN
                                        SigDec%Low = CurVal
                                    ELSE
                                        SigDec = SigDec*TenPow18 + CurVal
                                    END IF
                                    CurVal = 0_kInt64
                                    CurLen = 0
                                    AddCount = AddCount + 1
                                END IF
#else
                                SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
                                Indx = Indx + 1
                                SigCount = SigCount + 1
                            END DO
                            NFrac = Indx - NFrac
                            FractionEnd = Indx - 1
                        END IF
                    END IF
#ifdef tFloat_is_tQuad
                    IF (CurLen /= 0) THEN
                        IF (AddCount > 0) THEN
                            SigDec = SigDec*(10_kInt64**CurLen) + CurVal
                        ELSE
                            SigDec%Low = CurVal
                        END IF
                        CurVal = 0_kInt64
                        CurLen = 0
                        AddCount = AddCount + 1
                    END IF
#endif
                END IF
            END IF
        END IF
    END IF

    ! done for the significand part so check the number of significant digits
    ! (there must be at least one significant digit)
    IF (SigCount == 0) THEN
        IF (AtLeastOneDigit) THEN
            Valid = TrueVal
        ELSE
            ! this happens when not a number is encountered (i.e. the first non-blank character
            ! is not a sign, a digit or a period)
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid input: the first non-blank character is not a sign, a digit or a period.'
        END IF
        RETURN
    END IF
    
    ESign = 1
    ! check whether the current character is an exponent indicator
    IF (Indx <= StrLen) THEN
        CurChr => cStr(Indx:Indx)
        IF (.NOT.((CurChr == 'e').OR.(CurChr == 'E'))) THEN
            ! the current character is NOT an exponent indicator
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: invalid character after a digit.'
            RETURN
        END IF
        Indx = Indx + 1
        IF (Indx <= StrLen) THEN
            CurChr => cStr(Indx:Indx)
            IF ((CurChr == '+').OR.(CurChr == '-')) THEN
                IF (CurChr == '-') ESign = -1
                Indx = Indx + 1
                IF (Indx > StrLen) THEN
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent+sign indicators.'
                    RETURN
                END IF
                ! check whether the following character is a digit
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    ! current character is not a digit
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent+sign indicators.'
                    RETURN
                END IF
            ELSE
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    ! current character is not a digit
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent indicator.'
                    RETURN
                END IF
            END IF
            ! here the current character is a digit so this is likely a valid number
            ExpDec = (IACHAR(CurChr)-A0)
            DO
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    ! there is a non-integer character after the exponent indicator
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: not a digit after the exponent(+sign) indicator(s).'
                    RETURN
                END IF
                ExpDec = ExpDec*IBase + (IACHAR(CurChr)-A0)
                IF (ExpDec > ExpLimit) EXIT
            END DO
        END IF
    END IF
    
    ! check number of significant digits
    IF (SigCount > FP_Max_Digits) THEN
        ! start again this time and avoid overflow
        SigDec = ZeroUInt
        SigLimit = MaxDivbyBase - 10
        IF (IntegralStart > 0) THEN
            Indx = IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN    
            ! We have a big integer (but we can handle it) so determine exponent
            ExpDec = IntegralEnd + 1 - Indx + ESign*ExpDec
        ELSE                            
            ! We may have a value with a fractional component.
            IF (FractionStart > 0) THEN
                Indx = FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
            ! determine exponent
            ExpDec = FractionStart - Indx + ESign*ExpDec
        END IF
        ! We have now corrected both exponent and significand, to a truncated value
        Truncated  = TrueVal
        Aux%SigCut = Indx
    ELSE
        ! determine exponent
        ExpDec = ESign*ExpDec - NFrac    
    END IF
    
    ! set output
    Valid = TrueVal
    Aux%Truncated  = Truncated
    Aux%Indices(1) = IntegralStart
    Aux%Indices(2) = IntegralEnd
    Aux%Indices(3) = FractionStart
    Aux%Indices(4) = FractionEnd
    IF (PRESENT(ErrMsg)) ErrMsg = 'Valid Input: no error encountered.'

    RETURN

END FUNCTION Parse_Fortran_String

!******************************************************************************

FUNCTION Parse_JSON_String(cStr, SigDec, ExpDec, NegSign, Aux, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    ! To parse a valid JSON real (floating point) number where its differences
    ! from Fortran number are as follows:
    !   1. leading and trailing spaces are not allowed.
    !   2. a plus sign as the first character is not allowed.
    !   3. leading zero(s) is not allowed (if 0 is the first character, the second one
    !      must either be a period or an exponent indicator.)
    !   4. a period must be followed by at least one digit.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr
    tUIntType,            INTENT(OUT)   :: SigDec   ! significand in base 10
    tSInt32,              INTENT(OUT)   :: ExpDec   ! exponent in base 10
    tLogical,             INTENT(OUT)   :: NegSign
    TYPE(StringAux),      INTENT(OUT)   :: Aux
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tLogical                            :: Valid

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0       = IACHAR('0')
    tSInt32, PARAMETER  :: IBase    = 10
    tSInt32, PARAMETER  :: ExpLimit = ToInt32(Z'10000000')
#ifdef tFloat_is_tQuad
    tSInt32, PARAMETER  :: I64SafeDigits = 18
    tSInt64, PARAMETER  :: TenPow18 = 10_kInt64**I64SafeDigits
    tSInt32, PARAMETER  :: FP_Max_Digits = 39
#else
#ifdef tFloat_is_tDouble
    tSInt32, PARAMETER  :: FP_Max_Digits = 19
#else
    tSInt32, PARAMETER  :: FP_Max_Digits = 9
#endif
#endif

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType               :: SigLimit
#ifdef tFloat_is_tQuad
    tSInt64                 :: CurVal
    tSInt32                 :: CurLen, AddCount
#endif
    tSInt32                 :: Indx, StrLen
    tSInt32                 :: NFrac
    tSInt32                 :: ESign
    tSInt32                 :: SigCount
    tSInt32                 :: IntegralStart, IntegralEnd
    tSInt32                 :: FractionStart, FractionEnd
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Truncated

!** FLOW

    ! initialize
    SigDec = ZeroUInt
    ExpDec = 0
    Valid = FalseVal
    Truncated = FalseVal
    StrLen = LEN(cStr)
    Aux%Truncated = Truncated
    Aux%Indices   = 0
    Aux%Start     = 0
    Aux%SigCut    = 0
    IF (StrLen == 0) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
        RETURN
    END IF
    
    ! check for sign of the significand
    NegSign = FalseVal
    Indx = 1
    IF (cStr(Indx:Indx) == '-') THEN
        NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this string only contains a sign without a digit.'
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a sign must be followed by a digit.'
            RETURN
        END IF
    END IF
    
    Aux%Start = Indx
#ifdef tFloat_is_tQuad
    CurVal    = 0_kInt64
    CurLen    = 0
    AddCount  = 0
#endif
    
    ! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        ! the current (leading) digit is zero
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            ! only a zero digit encountered
            Valid = TrueVal
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
             ! leading zero cannot be followed by an integer (i.e. no leading zeros)
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: leading zero(s) is/are not allowed.'
            RETURN
        END IF
        IntegralStart = 0
        IntegralEnd   = 0
        SigCount      = 0
    ELSE
        ! check whether the current character is a non-zero digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! must start with an integer
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a leading character is not a digit.'
            RETURN
        END IF
        ! compute for the significand in the integral part
        IntegralStart = Indx
#ifdef tFloat_is_tQuad
        CurVal = IACHAR(cStr(Indx:Indx)) - A0
        CurLen = 1
#else
        SigDec = IACHAR(cStr(Indx:Indx)) - A0
#endif
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
            CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
            CurLen = CurLen +  1
            IF (CurLen == I64SafeDigits) THEN
                IF (AddCount == 0) THEN
                    SigDec%Low = CurVal
                ELSE
                    SigDec = SigDec*TenPow18 + CurVal
                END IF
                CurVal = 0_kInt64
                CurLen = 0
                AddCount = AddCount + 1
            END IF
#else
            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
            Indx = Indx + 1
        END DO
        SigCount = Indx - IntegralStart
        IntegralEnd = Indx - 1
#ifdef tFloat_is_tQuad
        IF (CurLen /= 0) THEN
            IF (AddCount == 0) THEN
                SigDec%Low = CurVal
            ELSE
                SigDec = SigDec*(10_kInt64**CurLen) + CurVal
            END IF
            CurVal = 0_kInt64
            CurLen = 0
            AddCount = AddCount + 1
        END IF
#endif
    END IF
    
    ! check whether the current character is a dot
    FractionStart = 0
    FractionEnd   = 0
    NFrac         = 0
    IF (Indx <= StrLen) THEN
        IF (cStr(Indx:Indx) == '.') THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                    NFrac = Indx
                    IF (SigCount > 0) THEN
                        FractionStart = Indx
                        ! continue computing for the significand
                        DO WHILE (Indx <= StrLen)
                            CurChr => cStr(Indx:Indx)
                            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
                            CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
                            CurLen = CurLen +  1
                            IF (CurLen == I64SafeDigits) THEN
                                IF (AddCount > 0) THEN
                                    SigDec = SigDec*TenPow18 + CurVal
                                ELSE
                                    SigDec%Low = CurVal
                                END IF
                                CurVal = 0_kInt64
                                CurLen = 0
                                AddCount = AddCount + 1
                            END IF
#else
                            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
                            Indx = Indx + 1
                        END DO
                        NFrac = Indx - NFrac
                        SigCount = SigCount + NFrac
                        FractionEnd = Indx - 1
                    ELSE
                        ! check for leading zero(s)
                        IF (cStr(Indx:Indx) == '0') THEN
                            DO WHILE (Indx <= StrLen)
                                IF (cStr(Indx:Indx) /= '0') EXIT
                                Indx = Indx + 1
                            END DO
                            IF (Indx > StrLen) THEN
                                ! only zero digits encountered
                                Valid = TrueVal
                                RETURN
                            END IF
                        END IF
                        CurChr => cStr(Indx:Indx)
                        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
                            FractionStart = Indx
                            ! start computing for the significand
                            DO WHILE (Indx <= StrLen)
                                CurChr => cStr(Indx:Indx)
                                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
                                CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
                                CurLen = CurLen +  1
                                IF (CurLen == I64SafeDigits) THEN
                                    IF (AddCount == 0) THEN
                                        SigDec%Low = CurVal
                                    ELSE
                                        SigDec = SigDec*TenPow18 + CurVal
                                    END IF
                                    CurVal = 0_kInt64
                                    CurLen = 0
                                    AddCount = AddCount + 1
                                END IF
#else
                                SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
                                Indx = Indx + 1
                                SigCount = SigCount + 1
                            END DO
                            NFrac = Indx - NFrac
                            FractionEnd = Indx - 1
                        END IF
                    END IF
#ifdef tFloat_is_tQuad
                    IF (CurLen /= 0) THEN
                        IF (AddCount > 0) THEN
                            SigDec = SigDec*(10_kInt64**CurLen) + CurVal
                        ELSE
                            SigDec%Low = CurVal
                        END IF
                        CurVal = 0_kInt64
                        CurLen = 0
                        AddCount = AddCount + 1
                    END IF
#endif
                END IF
            END IF
        END IF
    END IF

    ! done for the significand part so check the number of significant digits
    ! (there must be at least one significant digit)
    IF (SigCount == 0) THEN
        ! this should not happen here since the algorithm implemented above should take care of this already?
        IF (PRESENT(ErrMsg)) ErrMsg = 'There must be something wrong with the implementation.'
        RETURN
    END IF
    
    ESign = 1
    ! check whether the current character is an exponent indicator
    IF (Indx <= StrLen) THEN
        CurChr => cStr(Indx:Indx)
        IF (.NOT.((CurChr == 'e').OR.(CurChr == 'E'))) THEN
            ! the current character is NOT an exponent indicator
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: invalid character after a digit.'
            RETURN
        END IF
        Indx = Indx + 1
        IF (Indx <= StrLen) THEN
            CurChr => cStr(Indx:Indx)
            IF ((CurChr == '+').OR.(CurChr == '-')) THEN
                IF (CurChr == '-') ESign = -1
                Indx = Indx + 1
                IF (Indx > StrLen) THEN
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent+sign indicators.'
                    RETURN
                END IF
                ! check whether the following character is a digit
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    ! current character is not a digit
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent+sign indicators.'
                    RETURN
                END IF
            ELSE
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    ! current character is not a digit
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: no digit after the exponent indicator.'
                    RETURN
                END IF
            END IF
            ! here the current character is a digit so this is likely a valid number
            ExpDec = (IACHAR(CurChr)-A0)
            DO
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                    ! there is a non-integer character after the exponent indicator
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: not a digit after the exponent(+sign) indicator(s).'
                    RETURN
                END IF
                ExpDec = ExpDec*IBase + (IACHAR(CurChr)-A0)
                IF (ExpDec > ExpLimit) EXIT
            END DO
        END IF
    END IF
    
    ! check number of significant digits
    IF (SigCount > FP_Max_Digits) THEN
        ! start again this time and avoid overflow
        SigDec = ZeroUInt
        SigLimit = MaxDivbyBase - 10
        IF (IntegralStart > 0) THEN
            Indx = IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN    
            ! We have a big integer (but we can handle it) so determine exponent
            ExpDec = IntegralEnd + 1 - Indx + ESign*ExpDec
        ELSE                            
            ! We may have a value with a fractional component.
            IF (FractionStart > 0) THEN
                Indx = FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
            ! determine exponent
            ExpDec = FractionStart - Indx + ESign*ExpDec
        END IF
        ! We have now corrected both exponent and significand, to a truncated value
        Truncated  = TrueVal
        Aux%SigCut = Indx
    ELSE
        ! determine exponent
        ExpDec = ESign*ExpDec - NFrac    
    END IF
    
    ! set output
    Valid = TrueVal
    Aux%Truncated  = Truncated
    Aux%Indices(1) = IntegralStart
    Aux%Indices(2) = IntegralEnd
    Aux%Indices(3) = FractionStart
    Aux%Indices(4) = FractionEnd
    IF (PRESENT(ErrMsg)) ErrMsg = 'Valid Input: no error encountered.'

    RETURN

END FUNCTION Parse_JSON_String

!******************************************************************************

FUNCTION Parse_FPlus_String(cStr, SigDec, ExpDec, NegSign, Aux, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    ! To parse a valid Fortran real (floating point) number with more relaxed rules than
    ! those used in "Parse_Fortran_Number" routine.
    ! The relaxed rules consider the following numbers as valid:
    !   1. a number expressed in the scientific format can use 'd', 'D', 'q' and 'Q'
    !      in place of 'e' or 'E'.
    !   2. a number with '+' or '-' after digits (e.g. 1.23-20 or 123+50) is considered to
    !      be expressed in a valid number expressed in the scientific format
    !   3. digits before any invalid character encountered are treated as a valid number
    !      and any characters after the first encounter (including the first invalid one)
    !      are neglected.  therefore, for example, a '12.56ax-300' is considered to be
    !      a valid number with value of 12.56.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr
    tUIntType,            INTENT(OUT)   :: SigDec   ! significand in base 10
    tSInt32,              INTENT(OUT)   :: ExpDec   ! exponent in base 10
    tLogical,             INTENT(OUT)   :: NegSign
    TYPE(StringAux),      INTENT(OUT)   :: Aux
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tLogical                            :: Valid

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0       = IACHAR('0')
    tSInt32, PARAMETER  :: IBase    = 10
    tSInt32, PARAMETER  :: ExpLimit = ToInt32(Z'10000000')
#ifdef tFloat_is_tQuad
    tSInt32, PARAMETER  :: I64SafeDigits = 18
    tSInt64, PARAMETER  :: TenPow18 = 10_kInt64**I64SafeDigits
    tSInt32, PARAMETER  :: FP_Max_Digits = 39
#else
#ifdef tFloat_is_tDouble
    tSInt32, PARAMETER  :: FP_Max_Digits = 19
#else
    tSInt32, PARAMETER  :: FP_Max_Digits = 9
#endif
#endif

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUIntType               :: SigLimit
#ifdef tFloat_is_tQuad
    tSInt64                 :: CurVal
    tSInt32                 :: CurLen, AddCount
#endif
    tSInt32                 :: Indx, StrLen
    tSInt32                 :: NFrac
    tSInt32                 :: ESign
    tSInt32                 :: SigCount
    tSInt32                 :: IntegralStart, IntegralEnd
    tSInt32                 :: FractionStart, FractionEnd
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: AtLeastOneDigit, Truncated

!** FLOW

    ! initialize
    SigDec = ZeroUInt
    ExpDec = 0
    Valid = FalseVal
    AtLeastOneDigit = FalseVal
    Truncated = FalseVal
    StrLen = LEN_TRIM(cStr)     ! get valid string length by removing the trailing space(s)
    Aux%Truncated = Truncated
    Aux%Indices   = 0
    Aux%Start     = 0
    Aux%SigCut    = 0
    IF (StrLen == 0) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
        RETURN
    END IF
    
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
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
            RETURN
        END IF
    END IF
    
    ! check for sign of the significand
    NegSign = FalseVal
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this string only contains a sign without a digit.'
            RETURN
        END IF
        ! check whether the following character is a digit or a dot
        CurChr => cStr(Indx:Indx)
        IF (((CurChr < '0').OR.(CurChr > '9')).AND.(CurChr /= '.')) THEN
            ! current character is neither a digit nor a dot
            Aux%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a sign must be followed by a digit or the dot.'
            RETURN
        END IF
    END IF
    
    Aux%Start = Indx
    
    ! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        AtLeastOneDigit = TrueVal
        ! the current digit is zero so loop through the following
        ! characters until a non-zero character is found
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            ! only zero digits encountered
            Valid = TrueVal
            RETURN
        END IF
    END IF
    
    ! compute for the significand in the integral part
#ifdef tFloat_is_tQuad
    CurVal = 0_kInt64
    CurLen = 0
    AddCount = 0
#endif
    IntegralStart = 0
    IntegralEnd   = 0
    SigCount      = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        AtLeastOneDigit = TrueVal
        IntegralStart = Indx
        DO WHILE (Indx <= StrLen)
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
            CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
            CurLen = CurLen +  1
            IF (CurLen == I64SafeDigits) THEN
                IF (AddCount == 0) THEN
                    SigDec%Low = CurVal
                ELSE
                    SigDec = SigDec*TenPow18 + CurVal
                END IF
                CurVal = 0_kInt64
                CurLen = 0
                AddCount = AddCount + 1
            END IF
#else
            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
            Indx = Indx + 1
        END DO
        SigCount = Indx - IntegralStart
        IntegralEnd = Indx - 1
#ifdef tFloat_is_tQuad
        IF (CurLen /= 0) THEN
            IF (AddCount == 0) THEN
                SigDec%Low = CurVal
            ELSE
                SigDec = SigDec*(10_kInt64**CurLen) + CurVal
            END IF
            CurVal = 0_kInt64
            CurLen = 0
            AddCount = AddCount + 1
        END IF
#endif
    END IF

    ! check whether the current character is a dot
    FractionStart = 0
    FractionEnd   = 0
    NFrac         = 0
    IF (Indx <= StrLen) THEN
        IF (cStr(Indx:Indx) == '.') THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                    AtLeastOneDigit = TrueVal
                    NFrac = Indx
                    IF (SigCount > 0) THEN
                        FractionStart = Indx
                        ! continue computing for the significand
                        DO WHILE (Indx <= StrLen)
                            CurChr => cStr(Indx:Indx)
                            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
                            CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
                            CurLen = CurLen +  1
                            IF (CurLen == I64SafeDigits) THEN
                                IF (AddCount > 0) THEN
                                    SigDec = SigDec*TenPow18 + CurVal
                                ELSE
                                    SigDec%Low = CurVal
                                END IF
                                CurVal = 0_kInt64
                                CurLen = 0
                                AddCount = AddCount + 1
                            END IF
#else
                            SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
                            Indx = Indx + 1
                        END DO
                        NFrac = Indx - NFrac
                        SigCount = SigCount + NFrac
                        FractionEnd = Indx - 1
                    ELSE
                        ! check for leading zero(s)
                        IF (cStr(Indx:Indx) == '0') THEN
                            DO WHILE (Indx <= StrLen)
                                IF (cStr(Indx:Indx) /= '0') EXIT
                                Indx = Indx + 1
                            END DO
                            IF (Indx > StrLen) THEN
                                ! only zero digits encountered
                                Valid = TrueVal
                                RETURN
                            END IF
                        END IF
                        CurChr => cStr(Indx:Indx)
                        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
                            FractionStart = Indx
                            ! start computing for the significand
                            DO WHILE (Indx <= StrLen)
                                CurChr => cStr(Indx:Indx)
                                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
#ifdef tFloat_is_tQuad
                                CurVal = CurVal*IBase + (IACHAR(CurChr)-A0)
                                CurLen = CurLen +  1
                                IF (CurLen == I64SafeDigits) THEN
                                    IF (AddCount == 0) THEN
                                        SigDec%Low = CurVal
                                    ELSE
                                        SigDec = SigDec*TenPow18 + CurVal
                                    END IF
                                    CurVal = 0_kInt64
                                    CurLen = 0
                                    AddCount = AddCount + 1
                                END IF
#else
                                SigDec = SigDec*IBase + (IACHAR(CurChr)-A0)
#endif
                                Indx = Indx + 1
                                SigCount = SigCount + 1
                            END DO
                            NFrac = Indx - NFrac
                            FractionEnd = Indx - 1
                        END IF
                    END IF
#ifdef tFloat_is_tQuad
                    IF (CurLen /= 0) THEN
                        IF (AddCount > 0) THEN
                            SigDec = SigDec*(10_kInt64**CurLen) + CurVal
                        ELSE
                            SigDec%Low = CurVal
                        END IF
                        CurVal = 0_kInt64
                        CurLen = 0
                        AddCount = AddCount + 1
                    END IF
#endif
                END IF
            END IF
        END IF
    END IF

    ! done for the significand part so check the number of significant digits
    ! (there must be at least one significant digit)
    IF (SigCount == 0) THEN
        IF (AtLeastOneDigit) THEN
            Valid = TrueVal
        ELSE
            ! this happens when not a number is encountered (i.e. the first non-blank character
            ! is not a sign, a digit or a period)
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid input: the first non-blank character is not a sign, a digit or a period.'
        END IF
        RETURN
    END IF
    
    ESign = 1
    ! check whether the current character is an exponent indicator
    IF (Indx <= StrLen) THEN
        DO
            SELECT CASE (cStr(Indx:Indx))
            CASE ('e', 'E', 'd', 'D', 'q', 'Q')
                Indx = Indx + 1
                ! check for a sign of the exponent
                IF (Indx <= StrLen) THEN
                    CurChr => cStr(Indx:Indx)
                    IF (CurChr == '-') THEN
                        ESign = -1
                        Indx = Indx + 1
                    ELSEIF (CurChr == '+') THEN
                        Indx = Indx + 1
                    END IF
                ELSE
                    EXIT
                END IF
            CASE ('-')
                ESign = -1
                Indx = Indx + 1
            CASE ('+')
                Indx = Indx + 1
            CASE DEFAULT
                EXIT
            END SELECT
            IF (Indx <= StrLen) THEN
                DO
                    CurChr => cStr(Indx:Indx)
                    IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                    ExpDec = ExpDec*IBase + (IACHAR(CurChr)-A0)
                    IF (ExpDec > ExpLimit) EXIT
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT
                END DO
            END IF
            EXIT
        END DO
    END IF

    ! check number of significant digits
    IF (SigCount > FP_Max_Digits) THEN
        ! start again this time and avoid overflow
        SigDec = ZeroUInt
        SigLimit = MaxDivbyBase - 10
        IF (IntegralStart > 0) THEN
            Indx = IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN    
            ! We have a big integer (but we can handle it) so determine exponent
            ExpDec = IntegralEnd + 1 - Indx + ESign*ExpDec
        ELSE                            
            ! We may have a value with a fractional component.
            IF (FractionStart > 0) THEN
                Indx = FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
            ! determine exponent
            ExpDec = FractionStart - Indx + ESign*ExpDec
        END IF
        ! We have now corrected both exponent and significand, to a truncated value
        Truncated  = TrueVal
        Aux%SigCut = Indx
    ELSE
        ! determine exponent
        ExpDec = ESign*ExpDec - NFrac    
    END IF
    
    ! set output
    Valid = TrueVal
    Aux%Truncated  = Truncated
    Aux%Indices(1) = IntegralStart
    Aux%Indices(2) = IntegralEnd
    Aux%Indices(3) = FractionStart
    Aux%Indices(4) = FractionEnd
    IF (PRESENT(ErrMsg)) ErrMsg = 'Valid Input: no error encountered.'

    RETURN

END FUNCTION Parse_FPlus_String

!******************************************************************************
