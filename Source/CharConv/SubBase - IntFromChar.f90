
SUBMODULE (ModBase_CharConv) SubBase_IntFromChar

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation for routines relating to a conversion
!   from a decimal string to an integer value. <br>
!   <br>
! **TECHNICAL NOTES**: <br>
!   1. A Fortran number (FortNum) has the form as: [S]N[N...] where <br>
!       - S is a sign indicator (required if negative '-', optional if positive '+'). <br>
!       - N is a decimal digit (0 through 9). Any leading zeros, leading and trailing
!           spaces are ignored. <br>
!    Unlike Fortran constants, the optional kind parameter (_k) is not allowed here. <br>
!   2. A FPlus number (FortPlus) has a slightly more relaxed rule than that of a Fortran
!      number such that any invalid characters after characters that are valid are ignored.
!      For example, -3567e23 is treated as a valid number with a value of -3567. <br>
!   3. A JSON number (JsonNum) has a slightly stricter rule than that of a Fortran number
!      such that a plus sign and leading zeroes are not allowed. <br>
!   <br>
!  **REFERENCES**: <br>
!   [1] <a href="https://github.com/ibireme/c_numconv_benchmark">Number Conversion
!       Benchmark in C.</a> <br>

!** USE STATEMENTS:
    ! na
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tSInt32, PARAMETER  :: MinI32      = ToInt32(Z'80000000')           ! -2,147,483,648
    tSInt32, PARAMETER  :: MaxI32      = ToInt32(Z'7FFFFFFF')           !  2,147,483,647
    tSInt64, PARAMETER  :: MinI64      = ToInt64(Z'8000000000000000')   ! -9,223,372,036,854,775,808
    tSInt64, PARAMETER  :: MaxI64      = ToInt64(Z'7FFFFFFFFFFFFFFF')   !  9,223,372,036,854,775,807
    tSInt32, PARAMETER  :: MaxI32Div10 = MaxI32/10                      ! = 214,748,364
    tSInt32, PARAMETER  :: MaxI32Mod10 = MOD(MaxI32, 10)                ! = 7
    tSInt64, PARAMETER  :: MaxI64Div10 = MaxI64/10_kInt64               ! = 922,337,203,685,477,580
    tSInt64, PARAMETER  :: MaxI64Mod10 = MOD(MaxI64, 10_kInt64)         ! = 7
    tSInt32, PARAMETER  :: MaxDigitI32 = 10
    tSInt32, PARAMETER  :: MaxDigitI64 = 19
    tSInt32, PARAMETER  :: A0          = IACHAR('0')
    tSInt32, PARAMETER  :: A4          = IACHAR('4')
    tSInt32, PARAMETER  :: A9          = IACHAR('9')
    tSInt32, PARAMETER  :: IBase       = 10
    tSInt64, PARAMETER  :: LBase       = 10_kInt64

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!
!                           32-BIT INTEGER ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION I32_FromChar_CC_FortNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt32                             :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen
    tSInt32                 :: Sign
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
    Number = 0
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
            Number = 0
            RETURN
        END IF
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
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI32
                RETURN
            END IF
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI32
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI32) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI32) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MinI32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar_CC_FortNum

!******************************************************************************

MODULE FUNCTION I32_FromChar_CC_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt32                             :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen
    tSInt32                 :: Sign
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
    Number = 0
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
            Number = 0
            RETURN
        END IF
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
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
        END DO
        NumDigit = Indx - IStart
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI32) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI32) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MinI32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar_CC_FortPlus

!******************************************************************************

MODULE FUNCTION I32_FromChar_CC_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt32                             :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen
    tSInt32                 :: Sign
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1
    IF (cStr(Indx:Indx) == '-') THEN
        Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    ELSEIF (cStr(Indx:Indx) == '+') THEN
        ! < for JSON number, a plus sign is not allowed. >
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a plus sign is not allowed.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI32
        RETURN
    END IF

    ! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        IF (Indx == StrLen) THEN
            Number = 0
        ELSE
            CurChr => cStr(Indx+1:Indx+1)
            IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: leading zero(s) is not allowed.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI32
            ELSE
                Number = 0
            END IF
        END IF
        RETURN
    END IF
    
    ! compute value of the input string
    Number   = 0
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: the first character is not a digit.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI32
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI32) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI32) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MinI32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar_CC_JsonNum

!******************************************************************************

MODULE FUNCTION I32_FromChar_Lemire_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt32                             :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, IndxP7
    tSInt32                 :: Sign, StrLen
    tSInt32                 :: IStart, NumDigit
    tCharLen(1), POINTER    :: CurChr
    tCharLen(8)             :: wStr
    tSInt64                 :: wVal
    EQUIVALENCE(wStr, wVal)
    tLogical                :: Overflow

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
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
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
            Number = 0
            RETURN
        END IF
    END IF
    
    ! initialize
    Number = 0
    IStart   = 0
    NumDigit = 0
    
    ! process 8 digits immediately if possible
    IndxP7 = Indx + 7
    IF (IndxP7 <= StrLen) THEN
        wStr = cStr(Indx:IndxP7)
        IF (Is_Made_Of_Eight_Digits(WVal)) THEN
            ! process 8 digits at once
            Number = ToInt32(Parse_Eight_Digits_Unrolled(wVal))
            IStart = Indx
            NumDigit = 8
            Indx = Indx + 8
        END IF
    END IF
    
    ! process the remaining digits
    IF (Indx <= StrLen) THEN
        CurChr => cStr(Indx:Indx)
        IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
            IF (IStart == 0) IStart = Indx
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                Number = Number*IBase + (IACHAR(CurChr)-A0)
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
            END DO
            NumDigit = Indx - IStart
        END IF
    END IF
    
    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI32) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI32) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MinI32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar_Lemire_FortPlus

!******************************************************************************

MODULE FUNCTION I32_FromChar_YY_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt32                             :: Number   ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Digit: '0'
    tSInt8, PARAMETER   :: DIGI_TYPE_ZERO    = SHIFTL(1, 0)     ! 1 = Z'01'
    ! Digit: [1-9]
    tSInt8, PARAMETER   :: DIGI_TYPE_NONZERO = SHIFTL(1, 1)     ! 2 = Z'02'
    ! Digit type table (generate with misc/make_tables.c)
    tSInt8, PARAMETER   :: DigitTable(0:127) = [                    &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'04'), &
        ToInt8(Z'00'), ToInt8(Z'08'), ToInt8(Z'10'), ToInt8(Z'00'), &
        ToInt8(Z'01'), ToInt8(Z'02'), ToInt8(Z'02'), ToInt8(Z'02'), &
        ToInt8(Z'02'), ToInt8(Z'02'), ToInt8(Z'02'), ToInt8(Z'02'), &
        ToInt8(Z'02'), ToInt8(Z'02'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00')]
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64                 :: IVal, Add
    tSInt32                 :: Indx, StrLen, I
    tSInt32                 :: Sign, SignBit
    tSInt32                 :: CurCode      ! ASCII code of current character
    tLogical                :: Digit_End, Overflow

!** MACRO DEFINITIONS:
!*============================================================================
!* Digit Character Matcher
!*============================================================================
! Match a character with specified type
#define Digit_Type_Is(D, Type)      (IAND(DigitTable(D), Type) /= 0)
! Match a none zero digit: [1-9]
#define Digit_Is_NonZero(D)         Digit_Type_Is(D, DIGI_TYPE_NONZERO)
! Match a digit: [0-9]
#define Digit_Is_Digit(D)           Digit_Type_Is(D, IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO))

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
            Number = MinI32
            RETURN
        END IF
    END IF

    ! check for sign
    Sign  = 1
    IF (cStr(Indx:Indx) == '-') THEN
        Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
            RETURN
        END IF
    ELSEIF (cStr(Indx:Indx) == '+') THEN
        ! < for JSON number, a plus sign is not allowed. >
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a plus sign is not allowed.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI32
        RETURN
    END IF
    
    ! check whether first digit is zero
    ! < for JSON number, the first digit being zero is not allowed. >
    CurCode = IACHAR(cStr(Indx:Indx))
    IF (.NOT.Digit_Is_NonZero(CurCode)) THEN
        IF (CurCode == A0) THEN
            IF (Indx+1 > StrLen) THEN
                Number = 0
            ELSE
                IF (.NOT.Digit_Is_Digit(IACHAR(cStr(Indx+1:Indx+1)))) THEN
                    Number = 0
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: leading zero(s) is not allowed.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI32
                END IF
            END IF
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: the first character is not a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI32
        END IF
        RETURN
    END IF
    
    ! compute IVal for the next 8 characters (digits)
    IVal = CurCode - A0
    I = 1
    DO
        IF (Indx+I <= StrLen) THEN
            CurCode = IACHAR(cStr(Indx+I:Indx+I))
            IF (Digit_Is_Digit(CurCode)) THEN
                IVal = IVal*LBase + (CurCode-A0)
            ELSE
                Digit_End = TrueVal
                EXIT
            END IF
        ELSE
            Digit_End = TrueVal
            EXIT
        END IF
        I = I + 1
        IF (I > 8) THEN
            Digit_End = FalseVal
            EXIT
        END IF
    END DO
    
    IF ((Digit_End).OR.(Indx + I > StrLen)) THEN
        Number = ToInt32(IVal*Sign)
        RETURN
    END IF

    ! deal with more digit(s)
    Indx = Indx + I
    CurCode = IACHAR(cStr(Indx:Indx))
    IF (Digit_Is_Digit(CurCode)) THEN
        ! must check overflow
        Add = CurCode - A0
        ! check overflow
        IF (IVal < MaxI32Div10) THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                IF (Digit_Is_Digit(IACHAR(cStr(Indx:Indx)))) THEN
                    Overflow = TrueVal
                ELSE
                    Overflow = FalseVal
                END IF
            ELSE
                Overflow = FalseVal
            END IF
        ELSEIF (IVal > MaxI32Div10) THEN
            Overflow = TrueVal
        ELSE
            ! IVal is equal to MaxI32Div10
            SignBit = 0
            IF (Sign == -1) SignBit = 1
            IF (Add > MaxI32Mod10 + SignBit) THEN
                Overflow = TrueVal
            ELSE
                Overflow = FalseVal
            END IF
        END IF
        IF (Overflow) THEN
            ! overflow
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (Sign == 1) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI32
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI32
            END IF
        ELSE
            ! not overflow
            IVal = IVal*LBase + Add
            Number = ToInt32(IVal*Sign)
        END IF
    ELSE
        ! no overflow
        Number = ToInt32(IVal*Sign)
    END IF
    
    RETURN

#undef Digit_Type_Is
#undef Digit_Is_NonZero
#undef Digit_Is_Digit

END FUNCTION I32_FromChar_YY_JsonNum

!------------------------------------------------------------------------------
!
!                           64-BIT INTEGER ROUTINES
!
!------------------------------------------------------------------------------

MODULE FUNCTION I64_FromChar_CC_FortNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt64                             :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen
    tSInt64                 :: Sign
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1_kInt64
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1_kInt64
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
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
    
    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*LBase + ToInt64(IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI64
                RETURN
            END IF
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI64
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI64) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI64) THEN
        ! value might be in the applicable range
        IF (Number < 0_kInt64) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_kInt64).AND.(Number == MinI64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_kInt64
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_kInt64) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar_CC_FortNum

!******************************************************************************

MODULE FUNCTION I64_FromChar_CC_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt64                             :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen
    tSInt64                 :: Sign
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1_kInt64
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1_kInt64
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
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
    
    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*LBase + ToInt64(IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
        END DO
        NumDigit = Indx - IStart
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI64) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI64) THEN
        ! value might be in the applicable range
        IF (Number < 0_kInt64) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_kInt64).AND.(Number == MinI64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_kInt64
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_kInt64) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar_CC_FortPlus

!******************************************************************************

MODULE FUNCTION I64_FromChar_CC_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt64                             :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen
    tSInt64                 :: Sign
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1_kInt64
    IF (cStr(Indx:Indx) == '-') THEN
        Sign = -1_kInt64
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
    ELSEIF (cStr(Indx:Indx) == '+') THEN
        ! < for JSON number, a plus sign is not allowed. >
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a plus sign is not allowed.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI64
        RETURN
    END IF

    ! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        IF (Indx == StrLen) THEN
            Number = 0_kInt64
        ELSE
            CurChr => cStr(Indx+1:Indx+1)
            IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: leading zero(s) is not allowed.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MinI64
            ELSE
                Number = 0_kInt64
            END IF
        END IF
        RETURN
    END IF
    
    ! compute value of the input string
    Number   = 0_kInt64
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: the first character is not a digit.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI64
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI64) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI64) THEN
        ! value might be in the applicable range
        IF (Number < 0_kInt64) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_kInt64).AND.(Number == MinI64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_kInt64
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_kInt64) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar_CC_JsonNum

!******************************************************************************

MODULE FUNCTION I64_FromChar_Lemire_FortPlus(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt64                             :: Number   ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, IndxP7, StrLen
    tSInt64                 :: Sign
    tSInt32                 :: IStart, NumDigit
    tCharLen(1), POINTER    :: CurChr
    tCharLen(8)             :: wStr
    tSInt64                 :: wVal
    EQUIVALENCE(wStr, wVal)
    tLogical                :: Overflow

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
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for sign
    Sign  = 1_kInt64
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1_kInt64
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
    END IF
    
    ! check for leading zero(s)
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
    
    ! initialize
    Number = 0_kInt64
    IStart   = 0
    NumDigit = 0
    
    ! process 8 digits immediately if possible
    IndxP7 = Indx + 7
    IF (IndxP7 <= StrLen) THEN
        wStr = cStr(Indx:IndxP7)
        IF (Is_Made_Of_Eight_Digits(WVal)) THEN
            ! process 8 digits at once
            Number = Parse_Eight_Digits_Unrolled(wVal)
            IStart = Indx
            NumDigit = 8
            Indx = Indx + 8
            ! process another 8 digits immediately if possible
            IndxP7 = Indx + 7
            IF (IndxP7 <= StrLen) THEN
                wStr = cStr(Indx:IndxP7)
                IF (Is_Made_Of_Eight_Digits(WVal)) THEN
                    ! process 8 digits at once
                    Number = Number*100000000_kInt64 + Parse_Eight_Digits_Unrolled(wVal)
                    NumDigit = 16
                    Indx = Indx + 8
                END IF
            END IF
        END IF
    END IF
    
    ! process the remaining digits
    IF (Indx <= StrLen) THEN
        CurChr => cStr(Indx:Indx)
        IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
            IF (IStart == 0) IStart = Indx
            DO
                ! compute the value without checking if it will overflow
                ! we will check it after we process all the characters if valid
                Number = Number*LBase + ToInt64(IACHAR(CurChr)-A0)
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => cStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
            END DO
            NumDigit = Indx - IStart
        END IF
    END IF
    
    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigitI64) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI64) THEN
        ! value might be in the applicable range
        IF (Number < 0_kInt64) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_kInt64).AND.(Number == MinI64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_kInt64
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_kInt64) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar_Lemire_FortPlus

!******************************************************************************

MODULE FUNCTION I64_FromChar_YY_JsonNum(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt64                             :: Number   ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Digit: '0'
    tSInt8, PARAMETER   :: DIGI_TYPE_ZERO    = SHIFTL(1, 0)     ! 1 = Z'01'
    ! Digit: [1-9]
    tSInt8, PARAMETER   :: DIGI_TYPE_NONZERO = SHIFTL(1, 1)     ! 2 = Z'02'
    ! Digit type table (generate with misc/make_tables.c)
    tSInt8, PARAMETER   :: DigitTable(0:127) = [                    &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'04'), &
        ToInt8(Z'00'), ToInt8(Z'08'), ToInt8(Z'10'), ToInt8(Z'00'), &
        ToInt8(Z'01'), ToInt8(Z'02'), ToInt8(Z'02'), ToInt8(Z'02'), &
        ToInt8(Z'02'), ToInt8(Z'02'), ToInt8(Z'02'), ToInt8(Z'02'), &
        ToInt8(Z'02'), ToInt8(Z'02'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), &
        ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00'), ToInt8(Z'00')]
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64                 :: IVal, Add
    tSInt32                 :: Indx, StrLen, I
    tSInt64                 :: Sign, SignBit
    tSInt32                 :: CurCode      ! ASCII code of current character
    tLogical                :: Digit_End, Overflow

!** MACRO DEFINITIONS:
!*============================================================================
!* Digit Character Matcher
!*============================================================================
! Match a character with specified type
#define Digit_Type_Is(D, Type)      (IAND(DigitTable(D), Type) /= 0)
! Match a none zero digit: [1-9]
#define Digit_Is_NonZero(D)         Digit_Type_Is(D, DIGI_TYPE_NONZERO)
! Match a digit: [0-9]
#define Digit_Is_Digit(D)           Digit_Type_Is(D, IOR(DIGI_TYPE_ZERO, DIGI_TYPE_NONZERO))

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
            Number = MinI64
            RETURN
        END IF
    END IF

    ! check for sign
    Sign  = 1_kInt64
    IF (cStr(Indx:Indx) == '-') THEN
        Sign = -1_kInt64
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
            RETURN
        END IF
    ELSEIF (cStr(Indx:Indx) == '+') THEN
        ! < for JSON number, a plus sign is not allowed. >
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a plus sign is not allowed.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MinI64
        RETURN
    END IF
    
    ! check whether first digit is zero
    ! < for JSON number, the first digit being zero is not allowed. >
    CurCode = IACHAR(cStr(Indx:Indx))
    IF (.NOT.Digit_Is_NonZero(CurCode)) THEN
        IF (CurCode == A0) THEN
            IF (Indx+1 > StrLen) THEN
                Number = 0_kInt64
            ELSE
                IF (.NOT.Digit_Is_Digit(IACHAR(cStr(Indx+1:Indx+1)))) THEN
                    Number = 0_kInt64
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: leading zero(s) is not allowed.'
                    IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                    Number = MinI64
                END IF
            END IF
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: the first character is not a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MinI64
        END IF
        RETURN
    END IF
    
    ! compute IVal for the next 17 characters (digits)
    IVal = ToInt64(CurCode - A0)
    I = 1
    DO
        IF (Indx+I <= StrLen) THEN
            CurCode = IACHAR(cStr(Indx+I:Indx+I))
            IF (Digit_Is_Digit(CurCode)) THEN
                IVal = IVal*LBase + ToInt64(CurCode-A0)
            ELSE
                Digit_End = TrueVal
                EXIT
            END IF
        ELSE
            Digit_End = TrueVal
            EXIT
        END IF
        I = I + 1
        IF (I > 17) THEN
            Digit_End = FalseVal
            EXIT
        END IF
    END DO
    
    IF ((Digit_End).OR.(Indx + I > StrLen)) THEN
        Number = IVal*Sign
        RETURN
    END IF

    ! deal with more digit(s)
    Indx = Indx + I
    CurCode = IACHAR(cStr(Indx:Indx))
    IF (Digit_Is_Digit(CurCode)) THEN
        ! must check overflow
        Add = ToInt64(CurCode - A0)
        ! check overflow
        IF (IVal < MaxI64Div10) THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                IF (Digit_Is_Digit(IACHAR(cStr(Indx:Indx)))) THEN
                    Overflow = TrueVal
                ELSE
                    Overflow = FalseVal
                END IF
            ELSE
                Overflow = FalseVal
            END IF
        ELSEIF (IVal > MaxI64Div10) THEN
            Overflow = TrueVal
        ELSE
            ! IVal is equal to MaxI64Div10
            SignBit = 0_kInt64
            IF (Sign == -1_kInt64) SignBit = 1_kInt64
            IF (Add > MaxI64Mod10 + SignBit) THEN
                Overflow = TrueVal
            ELSE
                Overflow = FalseVal
            END IF
        END IF
        IF (Overflow) THEN
            ! overflow
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            IF (Sign == 1_kInt64) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                Number = MaxI64
            ELSE
                IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                Number = MinI64
            END IF
        ELSE
            ! not overflow
            IVal = IVal*LBase + Add
            Number = IVal*Sign
        END IF
    ELSE
        ! no overflow
        Number = IVal*Sign
    END IF
    
    RETURN

#undef Digit_Type_Is
#undef Digit_Is_NonZero
#undef Digit_Is_Digit

END FUNCTION I64_FromChar_YY_JsonNum

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

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

END SUBMODULE SubBase_IntFromChar

!******************************************************************************
