
SUBMODULE (ModBase_ChrStr) SubBase_ChrStr_Inquiry

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *inquiry* procedures (i.e. routines
!   that inquire information relating to a character string).

!** USE STATEMENTS:
    USE ModBase_Error_Handlers
    USE ModBase_Memory_Handlers,    ONLY: MemAlloc
#ifdef Indx64Bits
    USE ModBase_DoublyLinkedLists,  ONLY: QueueIndex => ListInteger8B
#else
    USE ModBase_DoublyLinkedLists,  ONLY: QueueIndex => ListInteger4B
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** SUBMODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE SUBROUTINES OR FUNCTIONS:

MODULE FUNCTION IsStringNumber(cStr,Strict,NumVal) RESULT(NumFlag)

    ! PURPOSE OF THIS ROUTINE:
    ! To check whether a character string is a valid number and
    ! if so, what kind of number it is.

    ! TECHNICAL INFORMATION:
    ! An (strict) integer number is a whole number with no decimal point.
    ! It can have a leading sign and is interpreted as a decimal number.
    ! It takes a general form of:  [s]n[n...]
    ! where s is a sign; required if negative (-), optional if positive (+).
    !       n is a decimal digit (0 through 9).
    !
    ! A (strict) real number is a number with decimal point or an exponent part.
    ! The general form of a real number with no exponent part: [s]n[n...]
    ! A real number with an exponent part has a general forms: [s]n[n...]E[s]nn...
    ! where s is a sign; required if negative (-), optional if positive (+).
    !       n is a decimal digit (0 through 9). A decimal point must appear if
    !         the real number has no exponent part.
    !       E is an exponent indicator where it can be 'E', 'e', 'D', 'd'.
    !
    ! A complex number is a pair of real or integer numbers, separated by a comma,
    ! and enclosed in parentheses. The first number represents the real part and
    ! the second number represents the imaginary part.
    ! A complex number takes a general form of: [s](c,c)
    ! where s is a sign; required if negative (-), optional if positive (+).
    !       c is a real or an integer number.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,             INTENT(IN)   :: cStr     ! character string
    tLogical,              INTENT(IN)   :: Strict   ! true if requesting strict integer/real number
                                                    ! default is false
    CLASS(*), ALLOCATABLE, INTENT(OUT)  :: NumVal   ! value of number if it is valid
    OPTIONAL                            :: Strict, NumVal

    tInteger                            :: NumFlag  ! flag
    ! NumFlag = -1, the string is NOT a number
    !         =  0, the string is a valid integer or real number
    !         =  1, the string is strictly an integer number
    !         =  2, the string is strictly a real number
    !         =  3, the string is a valid complex number

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc      :: pStr     ! working character string
    tIndex          :: pLen     ! length of pStr
    tQuad           :: RealVal  ! real value
    tQuad           :: ImagVal  ! real value for imaginary part
    tLong           :: IntgVal  ! integer value
    tCmpxQuad       :: CmplVal  ! complex value
    tInteger        :: IoStatus

    ! FLOW:
    
    ! initialize output
    NumFlag = -1

    ! Check whether a character string is a valid real number.
!    IF (IsRealNumber(WStr,WLen)) THEN
    IF (Is_Fortran_Real(cStr)) THEN
        ! The string can be either an integer or a real number.
        NumFlag = 0
        IF (PRESENT(Strict)) THEN
            IF (Strict) THEN
                IF (Is_Fortran_Integer(cStr)) THEN
                    ! The string is an integer.
                    NumFlag = 1
                    IF (PRESENT(NumVal)) THEN
                        READ(cStr,*, IOSTAT=IoStatus) IntgVal
                        ALLOCATE(NumVal, SOURCE=IntgVal)
                    END IF
                ELSE
                    ! The string is a real number.
                    NumFlag = 2
                    IF (PRESENT(NumVal)) THEN
                        READ(cStr,*, IOSTAT=IoStatus) RealVal
                        ALLOCATE(NumVal, SOURCE=RealVal)
                    END IF
                END IF
            END IF
        ELSE
            IF (PRESENT(NumVal)) THEN
                READ(cStr,*, IOSTAT=IoStatus) RealVal
                ALLOCATE(NumVal, SOURCE=RealVal)
            END IF
        END IF
    ELSE
        ! remove leading and trailing blanks
        pStr = CropBlanks(cStr)
        pLen = LEN(pStr)
        IF (pLen == 0) RETURN
        ! Check if the string is possibly a complex number
        IF (Is_Fortran_Complex(pStr, pLen)) THEN
            ! The string is a complex number.
            NumFlag = 3
            IF (PRESENT(NumVal)) THEN
                IF (pStr(1:1) == '-') THEN
                    CmplVal = -CMPLX(RealVal, ImagVal, KIND=kQP)
                ELSE
                    CmplVal =  CMPLX(RealVal, ImagVal, KIND=kQP)
                END IF
                ALLOCATE(NumVal, SOURCE=CmplVal)
            END IF
        END IF
    END IF
    
    RETURN

CONTAINS

    FUNCTION Is_Fortran_Real(wStr) RESULT(Valid)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given character string is a valid Fortran real number
        ! that has one of the two following forms:
        ! 1. A number without exponent part -> [S]N[N...]
        ! 2. A number with exponent part    -> [S]N[N...]E[S]N[N...]
        !   Where
        !   S is a sign indicator (required if negative '-', optional if positive '+').
        !   N is a decimal digit (0 through 9). A decimal point may appear anywhere
        !       after the sign (but before the exponent).
        !   E is an exponent indicator (either 'e' or 'E')
        ! The valid number is similar to "Real" Fortran constant with some small differences.
        ! 1. A whole number without a decimal (i.e. "Integer" constant) is considered valid.
        ! 2. The optional kind parameter (_k) is not allowed here.
        !
        ! Notes:
        ! 1. Leading and/or trailing space(s) are allowed.  For example, "  1.23"
        !   and "1.23   " are considered valid.  However, no space is allowed inside
        !   the valid number.  For instance, "1 .2 3" is considered NOT valid.
        ! 2. This routine is the same (in concept) as 'Parse_Fortran_Number' routine
        !    in module 'ModBase_NumCharConv', but it only check if the string is a valid number.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, TARGET, INTENT(IN)   :: wStr     ! character string
        tLogical                        :: Valid    ! true if input number is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger                :: Indx, StrLen
        tInteger                :: SigCount
        tInteger                :: IntegralStart
        tInteger                :: FractionStart
        tCharLen(1), POINTER    :: CurChr
        tLogical                :: AtLeastOneDigit

    !** FLOW

        ! initialize
        Valid = FalseVal
        AtLeastOneDigit = FalseVal
        StrLen = LEN_TRIM(wStr) ! get valid string length by removing the trailing space(s)
    
        ! check whether there are spaces in front of the number
        ! (only allow space(s) in front of the number but no spaces inside it)
        Indx = 1
        IF (wStr(Indx:Indx) == ' ') THEN
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (wStr(Indx:Indx) /= ' ') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) RETURN
        END IF
    
        ! check for sign of the significand
        CurChr => wStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            Indx = Indx + 1
            IF (Indx > StrLen) RETURN
            ! check whether the following character is a digit or a dot
            CurChr => wStr(Indx:Indx)
            IF (((CurChr < '0').OR.(CurChr > '9')).AND.(CurChr /= '.')) RETURN
        END IF
    
        ! check for leading zero(s)
        IF (wStr(Indx:Indx) == '0') THEN
            AtLeastOneDigit = TrueVal
            ! the current digit is zero so loop through the following
            ! characters until a non-zero character is found
            DO WHILE (Indx <= StrLen)
                IF (wStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                ! only zero digits encountered
                Valid = TrueVal
                RETURN
            END IF
        END IF
    
        ! check for the significand in the integral part
        IntegralStart = 0
        SigCount      = 0
        CurChr => wStr(Indx:Indx)
        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
            AtLeastOneDigit = TrueVal
            IntegralStart = Indx
            DO WHILE (Indx <= StrLen)
                CurChr => wStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                Indx = Indx + 1
            END DO
            SigCount = Indx - IntegralStart
        END IF
    
        ! check whether the current character is a dot
        FractionStart = 0
        IF ((Indx <= StrLen).AND.(wStr(Indx:Indx) == '.')) THEN
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                CurChr => wStr(Indx:Indx)
                IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                    AtLeastOneDigit = TrueVal
                    FractionStart = Indx
                    IF (SigCount > 0) THEN
                        ! continue checking for the significand
                        DO WHILE (Indx <= StrLen)
                            CurChr => wStr(Indx:Indx)
                            IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                            Indx = Indx + 1
                        END DO
                        SigCount = SigCount + Indx - FractionStart
                    ELSE
                        ! check for leading zero(s)
                        IF (wStr(Indx:Indx) == '0') THEN
                            DO WHILE (Indx <= StrLen)
                                IF (wStr(Indx:Indx) /= '0') EXIT
                                Indx = Indx + 1
                            END DO
                            IF (Indx > StrLen) THEN
                                ! only zero digits encountered
                                Valid = TrueVal
                                RETURN
                            END IF
                        END IF
                        CurChr => wStr(Indx:Indx)
                        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
                            ! start checking for the significand
                            DO WHILE (Indx <= StrLen)
                                CurChr => wStr(Indx:Indx)
                                IF ((CurChr < '0').OR.(CurChr > '9')) EXIT
                                Indx = Indx + 1
                                SigCount = SigCount + 1
                            END DO
                        END IF
                    END IF
                END IF
            END IF
        END IF
    
        ! done for the significand part so check the number of significant digits
        ! (there must be at least one significant digit)
        IF (SigCount == 0) THEN
            IF (AtLeastOneDigit) Valid = TrueVal
            RETURN
        END IF
    
        ! check whether the current character is a exponent indicator
        IF (Indx <= StrLen) THEN
            CurChr => wStr(Indx:Indx)
            IF ((CurChr == 'e').OR.(CurChr == 'E')) THEN
                Indx = Indx + 1
                CurChr => wStr(Indx:Indx)
                IF ((Indx <= StrLen).AND.((CurChr == '-').OR.(CurChr == '+'))) THEN
                    Indx = Indx + 1
                END IF
                CurChr => wStr(Indx:Indx)
                IF ((Indx > StrLen).OR.((CurChr < '0').OR.(CurChr > '9'))) THEN
                    ! no digit or not a digit after the exponent
                    RETURN
                ELSE
                    ! this is likely a valid number
                    DO WHILE (Indx <= StrLen)
                        CurChr => wStr(Indx:Indx)
                        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                            ! there is a non-integer character after the exponent indicator
                            RETURN
                        END IF
                        Indx = Indx + 1
                    END DO
                END IF
            ELSE
                ! the current character is NOT a exponent indicator
                RETURN
            END IF
        END IF
    
        Valid = TrueVal

        RETURN

    END FUNCTION Is_Fortran_Real

    !******************************************************************************

    FUNCTION Is_Fortran_Integer(wStr) RESULT(Valid)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given character string is a valid Fortran integer number
        ! that has the has the form as: [S]N[N...] where
        !       S is a sign indicator (required if negative '-', optional if positive '+').
        !       N is a decimal digit (0 through 9). Any leading zeros, leading and trailing
        !           spaces are ignored.
        ! Unlike Fortran constants, the optional kind parameter (_k) is not allowed here.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, TARGET, INTENT(IN)   :: wStr     ! character string
        tLogical                        :: Valid    ! true if input number is valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger                :: Indx, StrLen
        tCharLen(1), POINTER    :: CurChr

    !** FLOW

        ! get valid string length by removing the trailing space(s)
        Valid = FalseVal
        StrLen = LEN_TRIM(wStr)
    
        ! check whether there are spaces in front of the number
        ! (only allow space(s) in front of the number but no spaces inside it)
        Indx = 1
        IF (wStr(Indx:Indx) == ' ') THEN
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (wStr(Indx:Indx) /= ' ') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) RETURN
        END IF
    
        ! check for sign
        CurChr => wStr(Indx:Indx)
        IF ((CurChr == '-').OR.(CurChr == '+')) THEN
            Indx = Indx + 1
            IF (Indx > StrLen) RETURN
            ! check whether the following character is a digit or not
            CurChr => wStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) RETURN
        END IF
    
        ! check for leading zero(s)
        IF (wStr(Indx:Indx) == '0') THEN
            ! the first digit is zero so loop through the following
            ! characters until a non-zero character is found
            Indx = Indx + 1
            DO WHILE (Indx <= StrLen)
                IF (wStr(Indx:Indx) /= '0') EXIT
                Indx = Indx + 1
            END DO
            IF (Indx > StrLen) THEN
                ! only zero digits encountered
                Valid = TrueVal
                RETURN
            END IF
        END IF
    
        ! check whether the rest of the input string are only digits
        CurChr => wStr(Indx:Indx)
        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
            DO
                Indx = Indx + 1
                IF (Indx > StrLen) EXIT
                CurChr => wStr(Indx:Indx)
                IF ((CurChr < '0').OR.(CurChr > '9')) RETURN
            END DO
        ELSE
            RETURN
        END IF

        Valid = TrueVal

        RETURN

    END FUNCTION Is_Fortran_Integer

    !******************************************************************************

    FUNCTION Is_Fortran_Complex(wStr,wLen) RESULT(NumFlag)

        ! PURPOSE OF THIS ROUTINE:
        ! To check whether a character string is a valid complex number.

        ! SUBROUTINE ARGUMENT DEFINITIONS:
        tCharStar, INTENT(IN)   :: wStr     ! character string with ADJUSTL
        tIndex,    INTENT(IN)   :: wLen     ! length of wStr being trimmed
        tLogical                :: NumFlag  ! true if character string is a real number

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        tIndex          :: ComPos   ! position of comma in wStr
        tIndex          :: StartPos ! starting position

        ! FLOW:
    
        ! initialize output
        NumFlag = FalseVal
        
        ! check whether the first character is a sign
        IF ((wStr(1:1) == '+').OR.(wStr(1:1) == '-')) THEN
            ! a sign exists
            StartPos = 2
        ELSE
            ! there is no sign
            StartPos = 1
        END IF

        ! check whether the starting and the last characters are valid parentheses
        IF (wStr(StartPos:StartPos) == '(') THEN
            IF (wStr(wLen:wLen) == ')') THEN
                ! check whether there is a comma
                ComPos = INDEX(wStr(1:wLen),',')
                IF ((ComPos > StartPos+1).AND.(ComPos < wLen-1)) THEN
                    ! check whether the first substring is a valid real number
                    IF (Is_Fortran_Real(wStr(StartPos+1:Compos-1))) THEN
                        IF (PRESENT(NumVal)) THEN
                            READ(wStr(StartPos+1:Compos-1),*, IOSTAT=IoStatus) RealVal
                        END IF
                        ! check whether the second substring is a valid real number
                        IF (Is_Fortran_Real(wStr(ComPos+1:wLen-1))) THEN
                            IF (PRESENT(NumVal)) THEN
                                READ(wStr(ComPos+1:wLen-1),*, IOSTAT=IoStatus) ImagVal
                            END IF
                            ! The string is a complex number.
                            NumFlag = TrueVal
                        END IF
                    END IF
                END IF
            END IF
        END IF
        
        RETURN

    END FUNCTION Is_Fortran_Complex

    !******************************************************************************

END FUNCTION IsStringNumber

!******************************************************************************

FUNCTION IsStringNumber_SlowVersion(cStr,Strict,NumVal) RESULT(NumFlag)

    ! PURPOSE OF THIS ROUTINE:
    ! To check whether a character string is a valid number and
    ! if so, what kind of number it is.

    ! TECHNICAL INFORMATION:
    ! An (strict) integer number is a whole number with no decimal point.
    ! It can have a leading sign and is interpreted as a decimal number.
    ! It takes a general form of:  [s]n[n...]
    ! where s is a sign; required if negative (-), optional if positive (+).
    !       n is a decimal digit (0 through 9).
    !
    ! A (strict) real number is a number with decimal point or an exponent part.
    ! The general form of a real number with no exponent part: [s]n[n...]
    ! A real number with an exponent part has a general forms: [s]n[n...]E[s]nn...
    ! where s is a sign; required if negative (-), optional if positive (+).
    !       n is a decimal digit (0 through 9). A decimal point must appear if
    !         the real number has no exponent part.
    !       E is an exponent indicator where it can be 'E', 'e', 'D', 'd'.
    !
    ! A complex number is a pair of real or integer numbers, separated by a comma,
    ! and enclosed in parentheses. The first number represents the real part and
    ! the second number represents the imaginary part.
    ! A complex number takes a general form of: [s](c,c)
    ! where s is a sign; required if negative (-), optional if positive (+).
    !       c is a real or an integer number.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,             INTENT(IN)   :: cStr     ! character string
    tLogical,              INTENT(IN)   :: Strict   ! true if requesting strict integer/real number
                                                    ! default is false
    CLASS(*), ALLOCATABLE, INTENT(OUT)  :: NumVal   ! value of number if it is valid
    OPTIONAL                            :: Strict, NumVal

    tInteger                            :: NumFlag  ! flag
    ! NumFlag = -1, the string is NOT a number
    !         =  0, the string is a valid integer or real number
    !         =  1, the string is strictly an integer number
    !         =  2, the string is strictly a real number
    !         =  3, the string is a valid complex number

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc      :: WStr     ! working character string
    tIndex          :: WLen     ! length of WStr
    tQuad           :: RealVal  ! real value
    tQuad           :: ImagVal  ! real value for imaginary part
    tLong           :: IntgVal  ! integer value
    tCmpxQuad       :: CmplVal  ! complex value

    ! FLOW:
    
    ! initialize output
    NumFlag = -1

    ! remove leading and trailing blanks
    WStr = CropBlanks(cStr)
    WLen = LEN(WStr)
    IF (WLen == 0) RETURN
    
    ! Check whether a character string is a valid real number.
    IF (IsRealNumber(WStr,WLen)) THEN
        ! The string can be either an integer or a real number.
        NumFlag = 0
        IF (PRESENT(Strict)) THEN
            IF (Strict) THEN
                IF (IsStrictlyIntegerNumber(WStr, WLen)) THEN
                    ! The string is an integer.
                    NumFlag = 1
                    IF (PRESENT(NumVal)) ALLOCATE(NumVal, SOURCE=IntgVal)
                ELSE
                    ! The string is a real number.
                    NumFlag = 2
                    IF (PRESENT(NumVal)) ALLOCATE(NumVal, SOURCE=RealVal)
                END IF
            END IF
        ELSE
            IF (PRESENT(NumVal)) ALLOCATE(NumVal, SOURCE=RealVal)
        END IF
    ELSE
        ! Check if the string is possibly a complex number
        IF (IsCmpxNumber(WStr, WLen)) THEN
            ! The string is a complex number.
            NumFlag = 3
            IF (PRESENT(NumVal)) THEN
                IF (WStr(1:1) == '-') THEN
                    CmplVal = -CMPLX(RealVal, ImagVal, KIND=kQP)
                ELSE
                    CmplVal =  CMPLX(RealVal, ImagVal, KIND=kQP)
                END IF
                ALLOCATE(NumVal, SOURCE=CmplVal)
            END IF
        END IF
    END IF
    
    RETURN

CONTAINS

    FUNCTION IsRealNumber(PStr,StrLen,Imaginary) RESULT(NumFlag)

        ! PURPOSE OF THIS ROUTINE:
        ! To check whether a character string is a valid real number.

        ! SUBROUTINE ARGUMENT DEFINITIONS:
        tCharStar,          INTENT(IN)  :: PStr         ! character string with ADJUSTL
        tIndex,             INTENT(IN)  :: StrLen       ! length of PStr being trimmed
        tLogical, OPTIONAL, INTENT(IN)  :: Imaginary    ! true if determine the imaginary part
        tLogical                        :: NumFlag      ! true if character string is a real number

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        tInteger        :: IoStatus
        tIndex          :: StartPos
        tIndex          :: SignPos, ExpPos
        tLogical        :: ImagPart

        ! FLOW:
    
        ! initialize output
        NumFlag  = FalseVal
        ImagPart = FalseVal
        IF (PRESENT(Imaginary)) THEN
            ImagPart = Imaginary
        END IF

        ! check length
        IF (StrLen == 0) RETURN
        
        ! check if there is any blank within the string
        IF (CountWords(PStr) /= 1) RETURN
    
        ! try reading the value
        IF (ImagPart) THEN
            READ(PStr,*,IOSTAT=IoStatus) ImagVal
        ELSE
            READ(PStr,*,IOSTAT=IoStatus) RealVal
        END IF
        IF (IoStatus == 0) THEN
            ! it is in a sense a real number but check some more
            ! for strange (confusing) numeric cases such as
            ! "12-2" converted to 1.2E-01
            ! "1.2e" converted to 1.2E-00
            ! check whether the first character is a sign
            IF ((PStr(1:1) == '+').OR.(PStr(1:1) == '-')) THEN
                ! a sign exists
                StartPos = 2
            ELSE
                ! there is no sign
                StartPos = 1
            END IF
            ! check sign position
            SignPos = SCAN(PStr(StartPos:StrLen), SET_SIGNS)
            IF (SignPos /= 0) THEN
                SignPos = SignPos + StartPos - 1
                SELECT CASE (PStr(SignPos-1:SignPos-1))
                CASE ('E','D','e','d')
                    ! do nothing
                CASE DEFAULT
                    ! this is an addition/subtraction of two numbers or
                    ! two consecutive operators (-+ or +-)
                    RETURN
                END SELECT
            END IF
            ! check exponent position
            ExpPos = SCAN(PStr(1:StrLen), SET_EXPONENTS)
            IF (ExpPos < StrLen) THEN
                ! it is a real or integer number without strange (confusing) cases
                NumFlag = TrueVal
            END IF
        ENDIF

        RETURN

    END FUNCTION IsRealNumber

    !******************************************************************************

    FUNCTION IsStrictlyIntegerNumber(PStr,StrLen) RESULT(NumFlag)

        ! PURPOSE OF THIS ROUTINE:
        ! To check whether a character string is strictly an integer number.

        ! SUBROUTINE ARGUMENT DEFINITIONS:
        tCharStar, INTENT(IN)   :: PStr     ! character string with ADJUSTL
        tIndex,    INTENT(IN)   :: StrLen   ! length of PStr being trimmed
        tLogical                :: NumFlag  ! true if character string is an integer number

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        tInteger    :: IoStatus
        tIndex      :: VerNumber
        tIndex      :: SignPos

        ! FLOW:
    
        ! initialize output
        NumFlag = FalseVal

        ! check length
        IF (StrLen == 0) RETURN
    
        ! verify if PStr contains all valid numeric symbols
        VerNumber = VERIFY(PStr(1:StrLen), SET_INTEGERS)
    
        ! check if it is really an integer number
        IF (VerNumber == 0) THEN
            
            ! check sign position
            SignPos = SCAN(PStr(1:StrLen), SET_SIGNS)
            IF (SignPos > 1) THEN
                ! this is an addition of two numbers
                RETURN
            ELSEIF (SignPos == 1) THEN
                ! check for another sign
                SignPos = SCAN(PStr(2:StrLen), SET_SIGNS)
                ! return if there is one
                IF (SignPos /= 0) RETURN
            END IF

            ! try reading the value
            READ(PStr,*,IOSTAT=IoStatus) IntgVal
        
            IF (IoStatus == 0) THEN
                ! it is an integer number
                NumFlag = TrueVal
            END IF

        ENDIF

        RETURN

    END FUNCTION IsStrictlyIntegerNumber

    !******************************************************************************

    FUNCTION IsCmpxNumber(PStr,StrLen) RESULT(NumFlag)

        ! PURPOSE OF THIS ROUTINE:
        ! To check whether a character string is a valid complex number.

        ! SUBROUTINE ARGUMENT DEFINITIONS:
        tCharStar, INTENT(IN)   :: PStr     ! character string with ADJUSTL
        tIndex,    INTENT(IN)   :: StrLen   ! length of PStr being trimmed
        tLogical                :: NumFlag  ! true if character string is a real number

        ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
        tCharAlloc      :: RStr     ! working character string
        tIndex          :: RLen     ! length of RStr
        tCharAlloc      :: IStr     ! working character string
        tIndex          :: ILen     ! length of IStr
        tIndex          :: ComPos   ! position of comma in PStr
        tIndex          :: StartPos ! starting position

        ! FLOW:
    
        ! initialize output
        NumFlag = FalseVal
        
        ! check whether the first character is a sign
        IF ((PStr(1:1) == '+').OR.(PStr(1:1) == '-')) THEN
            ! a sign exists
            StartPos = 2
        ELSE
            ! there is no sign
            StartPos = 1
        END IF

        ! check whether the starting and the last characters are valid parentheses
        IF (PStr(StartPos:StartPos) == '(') THEN
            IF (PStr(StrLen:StrLen) == ')') THEN
                ! check whether there is a comma
                ComPos = INDEX(PStr(1:StrLen),',')
                IF ((ComPos > StartPos+1).AND.(ComPos < StrLen-1)) THEN
                    ! check whether the first substring is a valid real number
                    RStr = ADJUSTL(PStr(StartPos+1:ComPos-1))
                    RLen = LEN_TRIM(RStr)
                    IF (IsRealNumber(RStr(1:RLen), RLen)) THEN
                        ! check whether the second substring is a valid real number
                        IStr = ADJUSTL(PStr(ComPos+1:StrLen-1))
                        ILen = LEN_TRIM(IStr)
                        IF (IsRealNumber(IStr(1:ILen), ILen, TrueVal)) THEN
                            ! The string is a complex number.
                            NumFlag = TrueVal
                        END IF
                    END IF
                END IF
            END IF
        END IF
        
        RETURN

    END FUNCTION IsCmpxNumber

    !******************************************************************************

END FUNCTION IsStringNumber_SlowVersion

!******************************************************************************

MODULE FUNCTION IsStringLogical(cStr,Boolean) RESULT(LogFlag)

    ! PURPOSE OF THIS ROUTINE:
    ! To check whether a character string is a logical value where valid one include
    ! 'T', 'F', 't', 'f', 'TRUE', 'FALSE', 'true', 'false'.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStr     ! character string
    tLogical, OPTIONAL, INTENT(OUT) :: Boolean  ! logical value if flag is true
                                                ! otherwise, set to FalseVal
    tLogical                        :: LogFlag  ! true if character string is a logical value

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex          :: StrLen
    tCharAlloc      :: PStr

    ! FLOW:
    
    ! set default flags
    LogFlag = FalseVal
    IF (PRESENT(Boolean)) Boolean = FalseVal
    
    !  Make sure the string has all what we think a logical value should have
    PStr = ADJUSTL(cStr)
    StrLen = LEN_TRIM(PStr)
    IF (StrLen == 0) RETURN
    
    SELECT CASE (PStr(1:StrLen))
    CASE ('T', 't', 'TRUE', 'true')
        LogFlag = TrueVal
        IF (PRESENT(Boolean)) Boolean = TrueVal
    CASE ('F', 'f', 'FALSE', 'false')
        LogFlag = TrueVal
        IF (PRESENT(Boolean)) Boolean = FalseVal
    CASE DEFAULT
        LogFlag = FalseVal
        IF (PRESENT(Boolean)) THEN
            Boolean = FalseVal
            CALL Handle_ErrLevel('IsStringLogical', ModName, ErrWarning, &
                              'Character string is Not a logical value so set Boolean to false.')
        END IF
    END SELECT

    RETURN

END FUNCTION IsStringLogical

!******************************************************************************

RECURSIVE MODULE FUNCTION IsStringInClass(cStr,ClassType,FailIndex) RESULT(ClassFlag)

    ! PURPOSE OF THIS ROUTINE:
    ! To check whether the given character string is in the specified class.
    ! To be in the specified class, all characters in the string must be
    ! valid characters of that class.
    !
    ! The recognized character string classes include those of character classes
    ! (described in detail in "IsCharacterInClass" routine) and the following 
    ! additional classes:
    !   COMPLEX
    !       The character string is a valid complex constant in Fortran, with optional sign and
    !       surrounding white spaces. 
    !   FNAME
    !       The character string is a valid FORTRAN name that can contain letters, digits, and
    !       underscores(_). The first character must be a letter.
    !   INTEGER
    !       The character string is a valid integer constant in Fortran, with optional 
    !       surrounding white spaces. 
    !   LOGICAL
    !       The character string is considered a valid logical value, that is 't', 'T', 'true', 
    !       'TRUE', 'f', 'F', 'false' and 'FALSE', with optional surrounding white spaces. 
    !   REAL
    !       The character string is a valid real constant in Fortran, with optional
    !       surrounding white spaces. 

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,        INTENT(IN)    :: cStr         ! character string
    tCharStar,        INTENT(IN)    :: ClassType    ! character string class
    tIndex, OPTIONAL, INTENT(OUT)   :: FailIndex    ! flag indicating position of the failed character
    tLogical                        :: ClassFlag    ! true if the string is in the specified class

    ! SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER     :: ToUpper = TrueVal
    
    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(ClassType))    :: TypeOfClass
    tIndex                      :: FailID
    tIndex                      :: ID
    tIndex                      :: Length

    ! FLOW:
    
    !check length of the character string
    Length = LEN(cStr)
    IF (Length == 0) THEN
        ClassFlag = FalseVal
        IF (PRESENT(FailIndex)) FailIndex = ID_ZERO_LENGTH
        CALL Handle_ErrLevel('IsStringInClass', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    END IF
    IF (.NOT.IsStringInCharacterSet(ClassType, SET_ALPHABETS, FailID)) THEN
        ClassFlag = FalseVal
        IF (PRESENT(FailIndex)) FailIndex = ID_UNKNOWN_INDEX
        CALL Handle_ErrLevel('IsStringInClass', ModName, ErrWarning, &
                          'ClassType must contain letters only.')
        RETURN
    END IF
        
    ! convert ClassType to uppercase
    TypeOfClass = ClassType
    CALL ChangeCase(TypeOfClass, ToUpper)
    
    SELECT CASE (TypeOfClass)
    CASE ('COMPLEX')
        ClassFlag = IsStringComplex(cStr, FailID)
    CASE ('FNAME')
        ClassFlag = IsStringFortranName(cStr, FailID)
    CASE ('INTEGER')
        ClassFlag = IsStringInteger(cStr, FailID)
    CASE ('LOGICAL')
        ClassFlag = IsStringLogical(cStr, FailID)
    CASE ('REAL')
        ClassFlag = IsStringReal(cStr, FailID)
    CASE DEFAULT
        DO ID = 1, Length
            ClassFlag = IsCharacterInClass(cStr(ID:ID), TypeOfClass, FailID)
            IF (.NOT.ClassFlag) THEN
                ! not in the class, check FailID
                IF (FailID == 1) THEN
                    ! recognized class
                    IF (PRESENT(FailIndex)) FailIndex = ID
                    EXIT
                ELSE
                    ! unrecognized class
                    IF (PRESENT(FailIndex)) FailIndex = ID_UNKNOWN_INDEX
                    CALL Handle_ErrLevel('IsStringInClass', ModName, ErrWarning, &
                                      'The specified '// ClassType // ' class is NOT recognized.')
                    RETURN
                END IF
            END IF
        END DO
    END SELECT

    RETURN

CONTAINS

    FUNCTION IsStringComplex(cStr,FailID) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the character string is (strictly) a valid complex number
        ! (FORTRAN complex constant), with optional surrounding white space and sign
        ! A valid complex number is a pair of real or integer numbers, separated by a comma,
        ! and enclosed in parentheses. The first number represents the real part and
        ! the second number represents the imaginary part.
        ! A complex number takes a general form of: [s](c,c)
        ! where s is a sign; required if negative (-), optional if positive (+).
        !       c is a valid real or a valid integer number.
        ! Note: If the sign exists, it must be followed immediately by the opening parenthesis
        !       (i.e. no space between the two is allowed).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)   :: cStr     ! character string
        tIndex,    INTENT(OUT)  :: FailID   ! index indicating position of invalid character
        tLogical                :: Flag     ! True if valid real number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tCharStar, PARAMETER    :: SET_REAL_INDICATORS = CHR_PERIOD//SET_EXPONENTS
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharAlloc  :: wStr     ! character string without blanks
        tIndex      :: wLen     ! length of wStr
        tCharAlloc  :: rStr     ! working variable for real part
        tCharAlloc  :: iStr     ! working variable for imaginary part
        tIndex      :: LenDiff  ! length difference
        tIndex      :: ComPos   ! position of comma in wStr
        tIndex      :: RealPos  ! position of period in wStr
        tIndex      :: StartPos ! starting position

        ! FLOW:
    
        ! remove leading and trailing blanks
        wStr = CropBlanks(cStr)
        wLen = LEN(wStr)
        LenDiff = LEN(RemoveCharacters(cStr, SET_BLANKS, Option=2)) - wLen
        
        ! check whether the first character is a sign
        IF ((wStr(1:1) == '+').OR.(wStr(1:1) == '-')) THEN
            ! a sign exists
            StartPos = 2
        ELSE
            ! there is no sign
            StartPos = 1
        END IF
        
        ! initialize flag
        Flag = FalseVal

        ! check whether the starting and the last characters are valid parentheses
        IF (wStr(StartPos:StartPos) == '(') THEN
            IF (wStr(wLen:wLen) == ')') THEN
                ! check whether there is a comma
                ComPos = INDEX(wStr(1:wLen), ',')
                IF (ComPos > StartPos) THEN
                    ! there is a comma
                    IF (ComPos == StartPos+1) THEN
                        ! the real part is blank so compute FailID
                        FailID = ComPos + LenDiff
                    ELSEIF (ComPos == wLen-1) THEN
                        ! the imaginary part is blank so compute FailID
                        FailID = ComPos + LenDiff
                    ELSE
                        ! check whether the real part is a valid number
                        rStr = wStr(StartPos+1:ComPos-1)
                        ! check whether there is any period or exponent symbol in the real part
                        RealPos = SCAN(rStr, SET_REAL_INDICATORS)
                        IF (RealPos == 0) THEN
                            ! the real part may be an integer number
                            Flag = IsStringInteger(rStr, FailID)
                        ELSE
                            ! the real part may be a real number
                            Flag = IsStringReal(rStr, FailID)
                        END IF
                        IF (Flag) THEN
                            ! check whether the imaginary part is a valid real number
                            iStr = wStr(ComPos+1:wLen-1)
                            ! check whether there is any period or exponent symbol in
                            ! the imaginary part
                            RealPos = SCAN(iStr, SET_REAL_INDICATORS)
                            IF (RealPos == 0) THEN
                                ! the imaginary part may be an integer number
                                Flag = IsStringInteger(iStr, FailID)
                            ELSE
                                ! the imaginary part may be a real number
                                Flag = IsStringReal(iStr, FailID)
                            END IF
                            IF (.NOT.Flag) THEN
                                ! the imaginary part is not valid number so compute FailID
                                FailID = FailID + ComPos + LenDiff
                            END IF
                        ELSE
                            ! the real part is not valid so compute FailID
                            FailID = FailID + StartPos + LenDiff
                        END IF
                    END IF
                ELSE
                    ! the comma is missing so let wLen be the index and compute FailID
                    FailID = wLen + LenDiff
                END IF
            ELSE
                ! the closing parenthesis is missing so compute FailID
                FailID = wLen + LenDiff
            END IF
        ELSE
            ! the opening parenthesis is missing so compute FailID
            FailID = StartPos + LenDiff
        END IF
        
        RETURN

    END FUNCTION IsStringComplex

    !**************************************************************************************

    FUNCTION IsStringFortranName(cStr,FailID) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the character string is a valid FORTRAN name

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)   :: cStr     ! character string
        tIndex,    INTENT(OUT)  :: FailID   ! index indicating position of invalid character
        tLogical                :: Flag     ! True if all characters are valid

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharAlloc  :: ChrSet

    !** FLOW
    
        ! set character set for valid FORTRAN name
        ChrSet = SET_ALPHANUM // '_'
    
        ! check if all characters are in the set
        Flag = IsStringInCharacterSet(cStr, ChrSet, FailID)
        IF (Flag) THEN
            ! check whether the first character is a letter or not
            IF (INDEX(SET_ALPHABETS, cStr(1:1)) == 0) THEN
                Flag = FalseVal
                FailID = 1
            END IF
        END IF
    
        RETURN

    END FUNCTION IsStringFortranName

    !**************************************************************************************

    FUNCTION IsStringInteger(cStr,FailID,NoSpace) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the character string is (strictly) a valid integer number
        ! (FORTRAN integer constant), with optional surrounding white space.
        ! A valid integer number is a whole number with no decimal point.
        ! It can have a leading sign and is interpreted as a decimal number.
        ! It takes a general form of:  [s]n[n...]
        ! where s is a sign; required if negative (-), optional if positive (+).
        !       n is a decimal digit (0 through 9).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar,          INTENT(IN)  :: cStr     ! character string
        tIndex,             INTENT(OUT) :: FailID   ! index indicating position of invalid character
        tLogical, OPTIONAL, INTENT(IN)  :: NoSpace  ! true if no white pace allowed
        tLogical                        :: Flag     ! True if valid integer

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharAlloc  :: wStr         ! character string without blanks
        tIndex      :: wLen         ! length of wStr
        tIndex      :: StartPos     ! starting position
        tLogical    :: SignExists   ! true if sign exists
        tLogical    :: SpaceAllowed ! true if white pace allowed

    !** FLOW
        
        ! check optional input
        SpaceAllowed = TrueVal
        IF (PRESENT(NoSpace)) SpaceAllowed = .NOT.NoSpace
    
        ! remove leading and trailing blanks
        IF (SpaceAllowed) THEN
            wStr = CropBlanks(cStr)
        ELSE
            wStr = cStr
        END IF
        wLen = LEN(wStr)
        IF (wLen == 0) THEN
            ! empty string case
            Flag = FalseVal
            FailID = ID_EMPTY_STRING
            RETURN
        END IF
        SignExists = FalseVal
        
        IF ((wStr(1:1) == '+').OR.(wStr(1:1) == '-')) THEN
            ! check wLen
            IF (wLen == 1) THEN
                ! string contains only sign
                Flag = FalseVal
                FailID = ID_MISSING_CHARACTER
                RETURN
            END IF
            ! remove sign
            StartPos = 2
            SignExists = TrueVal
        ELSE
            StartPos = 1
        END IF
        
        ! check if all the rest of characters are digits
        Flag = IsStringInClass(wStr(StartPos:wLen), 'DIGIT', FailID)
        IF (.NOT.Flag) THEN
            ! compute FailID
            IF (SpaceAllowed) THEN
                FailID = FailID + LEN(RemoveCharacters(cStr, SET_BLANKS, Option=2)) - wLen
            END IF
            IF (SignExists) FailID = FailID + 1
        END IF
    
        RETURN

    END FUNCTION IsStringInteger

    !**************************************************************************************

    FUNCTION IsStringLogical(cStr,FailID) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the character string is considered as a valid logical value.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)   :: cStr     ! character string
        tIndex,    INTENT(OUT)  :: FailID   ! index indicating position of invalid character
        tLogical                :: Flag     ! True if valid integer

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharAlloc  :: wStr     ! character string without blanks
        tIndex      :: wLen     ! length of wStr

    !** FLOW
        
        ! remove white space
        wStr = CropBlanks(cStr)
        wLen = LEN(wStr)
        IF (wLen==0) THEN
            ! empty string
            Flag = FalseVal
            FailID = ID_EMPTY_STRING
            RETURN
        END IF
    
        SELECT CASE (wStr(1:wLen))
        CASE ('T', 't', 'TRUE', 'true', 'F', 'f', 'FALSE', 'false')
            Flag = TrueVal
        CASE DEFAULT
            Flag = FalseVal
            ! determine FailD
            SELECT CASE (wStr(1:1))
            CASE ('T', 't', 'F', 'f')
                IF (wLen==2) THEN
                    ! the first character is valid so the last one is the fail one
                    FailID = 2
                ELSE
                    ! because the first character is valid so we cannot identify exactly
                    ! which character is the one that causes false since it depends on
                    ! which logical character we are comparing with
                    FailID = ID_UNKNOWN_INDEX
                END IF
            CASE DEFAULT
                ! the first character is not valid
                FailID = 1
            END SELECT
        END SELECT

        RETURN

    END FUNCTION IsStringLogical

    !**************************************************************************************

    FUNCTION IsStringReal(cStr,FailID) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the character string is (strictly) a valid real number
        ! (FORTRAN real constant), with optional surrounding white space
        ! A valid real number is a number with decimal point or an exponent part.
        ! The general form of a real number with no exponent part: [s]n[n...]
        ! A real number with an exponent part has a general forms: [s]n[n...]E[s]nn...
        ! where s is a sign; required if negative (-), optional if positive (+).
        !       n is a decimal digit (0 through 9). A decimal point must appear if
        !         the real number has no exponent part.
        !       E is an exponent indicator where it can be 'E', 'e', 'D', 'd'.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)   :: cStr     ! character string
        tIndex,    INTENT(OUT)  :: FailID   ! index indicating position of invalid character
        tLogical                :: Flag     ! True if valid real number

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tCharStar, PARAMETER    :: SET_REAL_NO_EXPONENT = SET_DIGITS//CHR_PERIOD
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharAlloc  :: wStr         ! character string without blanks
        tCharAlloc  :: rStr         ! wStr without sign
        tIndex      :: rLen         ! length of rStr
        tIndex      :: LenDiff      ! length difference
        tLogical    :: SignExists   ! true if sign exists
        tIndex      :: ePos         ! position of exponent
        tIndex      :: dPos         ! position of period (dot)
        tIndex      :: bPos         ! position of blank

    !** FLOW
    
        ! remove leading and trailing blanks
        wStr = CropBlanks(cStr)
        rLen = LEN(wStr)
        LenDiff = LEN(RemoveCharacters(cStr, SET_BLANKS, Option=2)) - rLen
        
        ! check rLen
        IF (rLen== 0) THEN
            ! empty string
            Flag = FalseVal
            FailID = ID_EMPTY_STRING
            RETURN
        END IF
        
        ! check if there is a blank in the string
        bPos = SCAN(wStr,SET_BLANKS)
        IF (bPos /= 0) THEN
            ! a blank in a number is not allowed; hence, this is not valid
            Flag = FalseVal
            FailID = bPos + LenDiff
            RETURN
        END IF
        
        ! check if there is a sign
        IF ((wStr(1:1) == '+').OR.(wStr(1:1) == '-')) THEN
            ! remove sign
            rStr = wStr(2:rLen)
            rLen = rLen - 1
            LenDiff = LenDiff - 1
            SignExists = TrueVal
        ELSE
            rStr = wStr
            SignExists = FalseVal
        END IF
        
        IF (rLen == 1) THEN
            ! real number must have at least rLen of 2 meaningful characters
            Flag = FalseVal
            FailID = ID_MISSING_CHARACTER
            RETURN
        END IF

        ! check if the exponent exists
        ePos = SCAN(rStr, SET_EXPONENTS)
        IF (ePos == 0) THEN
            ! there is no exponent so check if the period exists
            dPos = INDEX(rStr, CHR_PERIOD)
            IF (dPos == 0) THEN
                ! also, there is also no period; hence, this is not valid
                Flag = FalseVal
                FailID = ID_MISSING_CHARACTER
                RETURN
            END IF
            ! double check if there is more than one period
            IF (dPos < rLen) THEN
                dPos = INDEX(rStr(dPos+1:rLen), CHR_PERIOD)
                IF (dPos /= 0) THEN
                    ! this is also not valid
                    Flag = FalseVal
                    FailID = dPos + LenDiff
                    IF (SignExists) FailID = FailID + 1
                    RETURN
                END IF
            END IF
            ! check if all characters are digits and a period
            Flag = IsStringInCharacterSet(rStr, SET_REAL_NO_EXPONENT, FailID)
        ELSEIF ((ePos == 1).OR.(ePos == rLen)) THEN
            ! this is not valid too
            Flag = FalseVal
            FailID = ePos
        ELSE
            ! there is an exponent so split the character string into two parts
            ! and check the exponent part first
            Flag = IsStringInteger(rStr(ePos+1:rLen), FailID, NoSpace=TrueVal)
            IF (.NOT.Flag) THEN
                ! compute FailID
                FailID = FailID + LenDiff + ePos
                IF (SignExists) FailID = FailID + 1
                RETURN
            END IF
            ! then, check the significand part
            dPos = INDEX(rStr(1:ePos-1),CHR_PERIOD)
            IF (dPos == 0) THEN
                ! there is no period so check if all characters are digits
                Flag = IsStringInCharacterSet(rStr(1:ePos-1), SET_DIGITS, FailID)
            ELSE
                ! there is a period so check if this part is a real number (without exponent)
                ! by double checking if there is more than one period
                IF (dPos < ePos-1) THEN
                    dPos = INDEX(rStr(dPos+1:ePos-1), CHR_PERIOD)
                    IF (dPos /= 0) THEN
                        ! this is not valid
                        Flag = FalseVal
                        FailID = dPos + LenDiff
                        IF (SignExists) FailID = FailID + 1
                        RETURN
                    END IF
                END IF
                ! check if all characters are digits and a period
                Flag = IsStringInCharacterSet(rStr(1:ePos-1), SET_REAL_NO_EXPONENT, FailID)
            END IF
        END IF
        IF (.NOT.Flag) THEN
            ! compute FailID
            FailID = FailID + LenDiff
            IF (SignExists) FailID = FailID + 1
        END IF

        RETURN

    END FUNCTION IsStringReal

    !**************************************************************************************

END FUNCTION IsStringInClass

!******************************************************************************

MODULE FUNCTION IsStringInCharacterSet(cStr,ChrSet,FailID) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether all characters in the character string are
    ! in the given character set.
    ! This routine is an alternative to VERIFY intrinsic function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tCharStar, INTENT(IN)   :: ChrSet   ! character set
    tIndex,    INTENT(OUT)  :: FailID   ! index indicating position of invalid character
    tLogical                :: Flag     ! true if all characters are in the set

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: IChr         ! index
    tChar       :: CurrChr      ! current character
    tIndex      :: ChrIndex     ! character index

!** FLOW:
    
    ! initialize
    Flag = TrueVal
    FailID = 0
    
    ! find the cases when it is not in the given character set
    DO IChr = 1 , LEN(cStr)
        CurrChr = cStr(IChr:IChr)
        ChrIndex = INDEX(ChrSet, CurrChr)
        IF (ChrIndex == 0) THEN
            ! this character is not in the set
            Flag = FalseVal
            FailID = IChr
            EXIT
        END IF
    END DO

    RETURN

END FUNCTION IsStringInCharacterSet

!******************************************************************************

MODULE FUNCTION CountSubstring(cStr,sStr,Overlap) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of occurrences of substring in the given character string

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStr     ! character string
    tCharStar,          INTENT(IN)  :: sStr     ! substring
    tLogical, OPTIONAL, INTENT(IN)  :: Overlap  ! true for overlapping occurrences
                                                ! false for non-overlapping occurrences
                                                ! default = FalseVal
    tIndex                          :: nCount   ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen
    tIndex      :: iBegin, iNext
    tLogical    :: NonOverlap

    ! FLOW:
    
    ! initialize and check input
    nCount = 0
    cLen = LEN(cStr)
    sLen = LEN(sStr)
    NonOverlap = TrueVal
    IF (cLen == 0) THEN
        RETURN
    ELSEIF (sLen == 0) THEN
        RETURN
    ELSEIF (cLen < sLen) THEN
        RETURN
    END IF
    IF (PRESENT(Overlap)) NonOverlap = .NOT.Overlap
    
    ! start counting
    IF (NonOverlap) THEN
        iBegin = 1
        DO
            iNext = INDEX(cStr(iBegin:cLen), sStr) + iBegin - 1
            IF (iNext >= iBegin) THEN
                ! one more substring found
                nCount = nCount + 1
                iBegin = iNext + sLen
                ! exit if at the end of the string
                IF (iBegin > cLen) EXIT
            ELSE
                ! no more substring found
                EXIT
            END IF
        END DO
    ELSE
        DO iBegin = 1, cLen
            iNext = iBegin + sLen - 1
            IF (iNext > cLen) iNext = cLen
            IF (cStr(iBegin:iNext)==sStr) THEN
                ! a substring found
                nCount = nCount + 1
            END IF
        END DO
    END IF

    RETURN

END FUNCTION CountSubstring

!******************************************************************************

MODULE FUNCTION CountCharacters(cStr,ChrSet) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of occurrences of character(s) in the given character string
    ! for any character appearing in the given set

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tCharStar, INTENT(IN)   :: ChrSet   ! a set of characters
    tIndex                  :: nCount   ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex         :: cLen, I

    ! FLOW:
    
    ! initialize and check input
    nCount = 0
    cLen = LEN(cStr)
    IF (cLen == 0) THEN
        RETURN
    ELSEIF (LEN(ChrSet) == 0) THEN
        RETURN
    END IF
    
    ! start counting
    DO I = 1, cLen
        IF (INDEX(ChrSet, cStr(I:I))/=0) THEN
            ! a character found
            nCount = nCount + 1
        END IF
    END DO

    RETURN

END FUNCTION CountCharacters

!******************************************************************************

MODULE FUNCTION CountCharactersProtect(cStr,ChrSet,ExclMrk) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of occurrences of delimiter(s) in the unprotected
    ! region(s) of the given character string. A delimiter is any character
    ! appearing in the given character set.
    ! See explanations about the protected region(s) in routine "FindProtectedRegions".

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStr     ! character string
    tCharStar,          INTENT(IN)  :: ChrSet   ! a set of characters
    tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk  ! true if exclamation mark is also used to protect regions
                                                ! false if only quotes are used to protect regions
                                                ! default = TrueVal
    tIndex                          :: nCount   ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex                  :: cLen
    tIndex, ALLOCATABLE     :: lPos(:)
    tIndex, ALLOCATABLE     :: rPos(:)
    tIndex                  :: nRegion
    tCharAlloc              :: wStr

    ! FLOW:
    
    ! initialize and check input
    nCount = 0
    cLen = LEN(cStr)
    IF (cLen == 0) THEN
        RETURN
    ELSEIF (LEN(ChrSet) == 0) THEN
        RETURN
    END IF
    
    ! find protected region(s)
    nRegion = FindProtectedRegions(cStr, lPos, rPos, ExclMrk)
    
    ! replace protected regions with a valid character
    wStr = ReplaceProtectedRegionsWithValidCharacter(cStr, ChrSet, nRegion, lPos,rPos)
    
    ! find the substring in the working string
    nCount = CountCharacters(wStr,ChrSet)
    
    RETURN

END FUNCTION CountCharactersProtect

!******************************************************************************

MODULE FUNCTION CountWords(cStr) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of words (separated by blanks) in the given character string

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tIndex                  :: nCount   ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc  :: wStr     ! character string without blanks
    tIndex      :: wLen     ! length of wStr
    tIndex      :: I, IM1   ! indices

    ! FLOW:
    
    ! initialize
    nCount = 0
    
    ! remove leading and trailing blanks
    wStr = CropBlanks(cStr)
    wLen = LEN(wStr)
    IF (wLen == 0) RETURN

    nCount = 1
    IF (wLen >= 2) THEN
        DO I = 2, wLen
            IM1 = I - 1
            IF ((IsCharacterInClass(wStr(IM1:IM1), 'BLANK')).AND. &
                (.NOT.IsCharacterInClass(wStr(I:I), 'BLANK'))) THEN
                nCount = nCount + 1
            END IF
        END DO
    END IF

    RETURN

END FUNCTION CountWords

!******************************************************************************

MODULE FUNCTION FindProtectedRegions(cStr,lPos,rPos,ExclMrk) RESULT(nRegion)

    ! PURPOSE OF THIS ROUTINE:
    ! To look for quotes (and exclamation marks) to find regions
    ! that must be protected from character string editing

    ! TECHNICAL INFORMATION:
    ! Single quote, double quote and optionally exclamation mark are used as
    !   delimiters to find protected regions.
    ! Two single quotes or two double quotes are used to mark a protected region
    !   whereas an exclamation mark indicates that all characters following it
    !   are all protected.
    ! This routine is designed specifically for manipulating Fortran source code
    !   where an exclamation mark is used for a comment and two (single or double)
    !   quotes are used to specify a value to a character variable or constant
    !

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,           INTENT(IN)     :: cStr     ! character string
    tIndex, ALLOCATABLE, INTENT(OUT)    :: lPos(:)  ! positions of the first character of protected regions
    tIndex, ALLOCATABLE, INTENT(OUT)    :: rPos(:)  ! positions of the last character of protected regions
    tLogical, OPTIONAL,  INTENT(IN)     :: ExclMrk  ! true if exclamation mark is also used to protect regions
                                                    ! false if only quotes are used to protect regions
                                                    ! default = TrueVal
    tIndex                              :: nRegion  ! number of protected regions

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharStar, PARAMETER    :: DelimAll    = CHR_EXCLAMATION_MARK // CHR_APOSTROPHE // &
                                             CHR_DOUBLEQUOTE
    tCharStar, PARAMETER    :: DelimQuotes = CHR_APOSTROPHE // CHR_DOUBLEQUOTE
    
    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: cLen
    tLogical            :: NoExclMrk
    TYPE(QueueIndex)    :: lPosQueue, rPosQueue
    tIndex              :: I, iStart, cPos
    tIndex              :: Pos1_Mark, Pos2_Mark
    tChar,    SAVE      :: Quote = ' '
    tLogical, SAVE      :: Protect = FalseVal
    tCharAlloc          :: Delim
    tLogical            :: Success

    ! FLOW:
    
    ! initialize and check input
    nRegion = 0
    NoExclMrk = FalseVal
    cLen = LEN(cStr)
    IF (cLen == 0) THEN
        RETURN
    END IF
    IF (PRESENT(ExclMrk)) NoExclMrk = .NOT.ExclMrk
    
    ! set delimiters
    IF (NoExclMrk) THEN
        Delim = DelimQuotes
    ELSE
        Delim = DelimAll
    END IF

    ! check whether this is a continuation from the previous line
    IF (Protect) THEN
        ! check whether 'Quote' is a delimiter
        IF (VERIFY(Quote,Delim)==0) THEN
            ! a call with SAVE variables are valid
            nRegion = 1
            ! looking for matching quote from the previous line
            cPos = INDEX(cStr(1:cLen),Quote)
            IF ((cPos > 0).AND.(cPos < cLen)) THEN
                ! a matching quote found
                Pos1_Mark = 1
                Pos2_Mark = cPos
                ! add Pos1_Mark and Pos2_Mark to lPosQueue and rPosQueue, respectively
                CALL lPosQueue%EnQueue(Pos1_Mark)
                CALL rPosQueue%EnQueue(Pos2_Mark)
                iStart = Pos2_Mark + 1
                ! reset SAVE variables
                Quote = ' '
                Protect = FalseVal
            ELSE
                ! either a matching quote found with no more protected regions
                ! or no matching end quote found -> still continue to the next line?
                IF (cPos == cLen) THEN
                    ! a matching quote found so reset SAVE variables
                    Quote = ' '
                    Protect = FalseVal
                END IF
                ! return output data
                CALL MemAlloc(lPos,1_kIndex)
                CALL MemAlloc(rPos,1_kIndex)
                lPos(1) = 1
                rPos(1) = cLen
                RETURN
            END IF
        ELSE
            CALL Handle_ErrLevel('FindProtectedRegions', ModName, ErrWarning, &
                              '"Protect" is set to TrueVal but "Quote" is NOT a delimiter.')
            RETURN
        END IF
    ELSE
        iStart = 1
    END IF
    
    DO
        ! looking for a delimiter
        cPos = SCAN(cStr(iStart:cLen), Delim) + iStart - 1
        IF (cPos >= iStart) THEN
            nRegion = nRegion + 1
            ! one of the delimiters is found so check whether it is an exclamation mark or not
            IF ((.NOT.NoExclMrk).AND.(cStr(cPos:cPos) == CHR_EXCLAMATION_MARK)) THEN
                ! an exclamation mark found -> the rest is the comment
                Pos1_Mark = cPos
                Pos2_Mark = cLen
                ! add Pos1_Mark and Pos2_Mark to lPosQueue and rPosQueue, respectively
                CALL lPosQueue%EnQueue(Pos1_Mark)
                CALL rPosQueue%EnQueue(Pos2_Mark)
                ! no more protected regions
                EXIT
            ELSE
                ! a (single or double) quote found
                Quote = cStr(cPos:cPos)
                Pos1_Mark = cPos
                ! looking for matching quote
                iStart = Pos1_Mark + 1
                cPos = INDEX(cStr(iStart:cLen), Quote) + iStart - 1
                IF (cPos >= iStart) THEN
                    ! a matching quote found
                    Pos2_Mark = cPos
                ELSE
                    ! no matching end quote found -> a continuation to the next line
                    Pos2_Mark = cLen
                END IF
                ! add Pos1_Mark and Pos2_Mark to lPosQueue and rPosQueue, respectively
                CALL lPosQueue%EnQueue(Pos1_Mark)
                CALL rPosQueue%EnQueue(Pos2_Mark)
                IF (Pos2_Mark < cLen) THEN
                    ! set iStart for next scan
                    iStart = Pos2_Mark + 1
                ELSE
                    ! no more protected regions so set flag for next line and exit
                    IF (VERIFY(cStr(cLen:cLen), Delim) /= 0) THEN
                        Protect = TrueVal
                    END IF
                    EXIT
                END IF
            END IF
        ELSE
            ! no more protected regions
            EXIT
        END IF
    END DO

    IF (nRegion > 0 ) THEN
        ! found protected region(s) so allocate output variables
        CALL MemAlloc(lPos, nRegion)
        CALL MemAlloc(rPos, nRegion)
        ! transfer output data
        DO I = 1, nRegion
            Success = lPosQueue%DeQueue(lPos(I))
            Success = rPosQueue%DeQueue(rPos(I))
        END DO
    END IF
    
    RETURN

END FUNCTION FindProtectedRegions

!******************************************************************************

MODULE FUNCTION FindSubstring(cStr,sStr,sPos) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of non-overlapping occurrences of substring in
    ! the given character string and also return position(s) of
    ! the first character of substring found

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,           INTENT(IN)     :: cStr     ! character string
    tCharStar,           INTENT(IN)     :: sStr     ! substring
    tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)  ! position(s) of the first character of substring found
    tIndex                              :: nCount   ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: cLen, sLen
    tIndex              :: iBegin, iNext, I
    TYPE(QueueIndex)    :: sPosQueue
    tLogical            :: Success

    ! FLOW:
    
    ! initialize and check input
    nCount = 0
    cLen = LEN(cStr)
    sLen = LEN(sStr)
    IF (cLen == 0) THEN
        RETURN
    ELSEIF (sLen == 0) THEN
        RETURN
    ELSEIF (cLen < sLen) THEN
        RETURN
    END IF
    
    ! start counting
    iBegin = 1
    DO
        iNext = INDEX(cStr(iBegin:cLen), sStr) + iBegin - 1
        IF (iNext >= iBegin) THEN
            ! one more substring found
            nCount = nCount + 1
            ! add iNext to sPosQueue
            CALL sPosQueue%EnQueue(iNext)
            iBegin = iNext + sLen
            ! exit if at the end of the string
            IF (iBegin > cLen) EXIT
        ELSE
            ! no more substring found
            EXIT
        END IF
    END DO
    
    ! transfer output
    IF ((nCount /= 0).AND.(nCount == sPosQueue%GetSize())) THEN
        CALL MemAlloc(sPos, nCount)
        DO I = 1, nCount
            Success = sPosQueue%DeQueue(sPos(I))
        END DO
    END IF
    
    RETURN

END FUNCTION FindSubstring

!******************************************************************************

MODULE FUNCTION FindSubstringProtect(cStr,sStr,sPos,ExclMrk) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of non-overlapping occurrences of substring in unprotected
    !   region(s) of the given character string and also return position(s) of
    !   the first character of substring found.
    ! See explanations about the protected region(s) in routine "FindProtectedRegions".

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,           INTENT(IN)     :: cStr     ! character string
    tCharStar,           INTENT(IN)     :: sStr     ! substring
    tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)  ! position(s) of the first character of substring found
    tLogical, OPTIONAL,  INTENT(IN)     :: ExclMrk  ! true if exclamation mark is also used to protect regions
                                                    ! false if only quotes are used to protect regions
                                                    ! default = TrueVal
    tIndex                              :: nCount   ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex                  :: cLen, sLen
    tIndex, ALLOCATABLE     :: lPos(:)
    tIndex, ALLOCATABLE     :: rPos(:)
    tIndex                  :: nRegion
    tCharAlloc              :: wStr

    ! FLOW:
    
    ! initialize and check input
    nCount = 0
    cLen = LEN(cStr)
    sLen = LEN(sStr)
    IF (cLen == 0) THEN
        RETURN
    ELSEIF (sLen == 0) THEN
        RETURN
    ELSEIF (cLen < sLen) THEN
        RETURN
    END IF
    
    ! find protected region(s)
    nRegion = FindProtectedRegions(cStr, lPos, rPos, ExclMrk)
    
    ! replace protected regions with a valid character
    wStr = ReplaceProtectedRegionsWithValidCharacter(cStr, sStr, nRegion, lPos, rPos)
    
    ! find the substring in the working string
    nCount = FindSubstring(wStr, sStr, sPos)
    
    RETURN

END FUNCTION FindSubstringProtect

!******************************************************************************

MODULE FUNCTION FindDelimiters(cStr,ChrSet,dPos) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of occurrences of delimiter(s) in the given character string
    ! and also return position(s) of the delimiter(s) found.
    ! A delimiter is any character appearing in the given character set.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,           INTENT(IN)     :: cStr     ! character string
    tCharStar,           INTENT(IN)     :: ChrSet   ! a set of characters
    tIndex, ALLOCATABLE, INTENT(OUT)    :: dPos(:)  ! position(s) of the delimiter(s) found
    tIndex                              :: nCount   ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: cLen, I
    TYPE(QueueIndex)    :: dPosQueue
    tLogical            :: Success

    ! FLOW:
    
    ! initialize and check input
    nCount = 0
    cLen = LEN(cStr)
    IF (cLen == 0) THEN
        RETURN
    ELSEIF (LEN(ChrSet) == 0) THEN
        RETURN
    END IF
    
    ! start finding delimiter(s)
    DO I = 1, cLen
        IF (INDEX(ChrSet, cStr(I:I))/=0) THEN
            ! a delimiter found
            nCount = nCount + 1
            ! add I to dPosQueue
            CALL dPosQueue%EnQueue(I)
        END IF
    END DO

    ! transfer output
    IF ((nCount /= 0).AND.(nCount == dPosQueue%GetSize())) THEN
        CALL MemAlloc(dPos,nCount)
        DO I = 1, nCount
            Success = dPosQueue%DeQueue(dPos(I))
        END DO
    END IF
    
    RETURN

END FUNCTION FindDelimiters

!******************************************************************************

MODULE FUNCTION FindDelimitersProtect(cStr,ChrSet,dPos,ExclMrk) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of occurrences of delimiter(s) in the unprotected
    ! region(s) of the given character string and also return position(s)
    ! of the delimiter(s) found.
    ! A delimiter is any character appearing in the given character set.
    ! See explanations about the protected region(s) in routine "FindProtectedRegions".

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,           INTENT(IN)     :: cStr     ! character string
    tCharStar,           INTENT(IN)     :: ChrSet   ! a set of characters
    tIndex, ALLOCATABLE, INTENT(OUT)    :: dPos(:)  ! position(s) of the delimiter(s) found
    tLogical, OPTIONAL,  INTENT(IN)     :: ExclMrk  ! true if exclamation mark is also used to protect regions
                                                    ! false if only quotes are used to protect regions
                                                    ! default = TrueVal
    tIndex                              :: nCount   ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex                  :: cLen
    tIndex, ALLOCATABLE     :: lPos(:)
    tIndex, ALLOCATABLE     :: rPos(:)
    tIndex                  :: nRegion
    tCharAlloc              :: wStr

    ! FLOW:
    
    ! initialize and check input
    nCount = 0
    cLen = LEN(cStr)
    IF (cLen == 0) THEN
        RETURN
    ELSEIF (LEN(ChrSet) == 0) THEN
        RETURN
    END IF
    
    ! find protected region(s)
    nRegion = FindProtectedRegions(cStr, lPos, rPos, ExclMrk)
    
    ! replace protected regions with a valid character
    wStr = ReplaceProtectedRegionsWithValidCharacter(cStr, ChrSet, nRegion, lPos, rPos)
    
    ! find the substring in the working string
    nCount = FindDelimiters(wStr, ChrSet, dPos)
    
    RETURN

END FUNCTION FindDelimitersProtect

!******************************************************************************

MODULE FUNCTION FindSeparators(cStr,Separator,CharSet,sPos) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of occurrences of separator(s) in the given character string
    !   and also return (first) position(s) of the separator(s) found.
    ! A separator can be a (single) character or a character string (multiple characters).
    ! The argument "CharSet" is a flag used to specify whether the separator is a character
    !   or a character string. If it is true, the argument "Separator" contains a set of
    !   characters where a separator is any character in the set.  If it is false,
    !   the argument "Separator" specifies the character-string separator.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,           INTENT(IN)     :: cStr         ! character string
    tCharStar,           INTENT(IN)     :: Separator    ! separator
    tLogical,            INTENT(IN)     :: CharSet      ! a flag indicating type of the separator
    tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)      ! position(s) of the separator(s) found
    tIndex                              :: nCount       ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! FLOW:
    
    IF (CharSet) THEN
        ! separator is a character
        nCount = FindDelimiters(cStr, Separator, sPos)
    ELSE
        ! separator is a character sring
        nCount = FindSubstring(cStr, Separator, sPos)
    END IF
    
    RETURN

END FUNCTION FindSeparators

!******************************************************************************

MODULE FUNCTION FindSeparatorsProtect(cStr,Separator,CharSet,sPos,ExclMrk) RESULT(nCount)

    ! PURPOSE OF THIS ROUTINE:
    ! To count the number of occurrences of separator(s) in the unprotected region(s) of
    !   the given character string and also return position(s) of the separator(s) found.
    ! See explanations of separator and its types in routine "FindSeparators".
    ! See explanations about the protected region(s) in routine "FindProtectedRegions".

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,           INTENT(IN)     :: cStr         ! character string
    tCharStar,           INTENT(IN)     :: Separator    ! separator
    tLogical,            INTENT(IN)     :: CharSet      ! a flag indicating type of the separator
    tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)      ! position(s) of the separator(s) found
    tLogical, OPTIONAL,  INTENT(IN)     :: ExclMrk      ! true if exclamation mark is also used to protect regions
                                                        ! false if only quotes are used to protect regions
                                                        ! default = TrueVal
    tIndex                              :: nCount       ! number of occurrences

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! FLOW:
    
    IF (CharSet) THEN
        ! separator is a character
        nCount = FindDelimitersProtect(cStr, Separator, sPos, ExclMrk)
    ELSE
        ! separator is a character sring
        nCount = FindSubstringProtect(cStr, Separator, sPos, ExclMrk)
    END IF
    
    RETURN

END FUNCTION FindSeparatorsProtect

!******************************************************************************

MODULE FUNCTION StartWithSubstring(cStr,sStr) RESULT(Flag)

    ! PURPOSE OF THIS ROUTINE:
    ! To check whether the given string starts with the specified substring or not
    ! Note: both the string and the substring must not have a zero length.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tCharStar, INTENT(IN)   :: sStr     ! substring
    tLogical                :: Flag     ! true if the string starts with the substring

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen

    ! FLOW:
    
    cLen = LEN(cStr)
    sLen = LEN(sStr)
    IF ((cLen == 0).OR.(sLen == 0).OR.(cLen < sLen)) THEN
        Flag = FalseVal
    ELSE
        Flag = cStr(1:sLen) == sStr
    END IF
    
    RETURN

END FUNCTION StartWithSubstring

!******************************************************************************

MODULE FUNCTION EndWithSubstring(cStr,sStr) RESULT(Flag)

    ! PURPOSE OF THIS ROUTINE:
    ! To check whether the given string ends with the specified substring or not
    ! Note: both the string and the substring must not have a zero length.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tCharStar, INTENT(IN)   :: sStr     ! substring
    tLogical                :: Flag     ! true if the string ends with the substring

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen

    ! FLOW:
    
    cLen = LEN(cStr)
    sLen = LEN(sStr)
    IF ((cLen == 0).OR.(sLen == 0).OR.(cLen < sLen)) THEN
        Flag = FalseVal
    ELSE
        Flag = cStr(cLen-sLen+1:cLen) == sStr
    END IF
    
    RETURN

END FUNCTION EndWithSubstring

!******************************************************************************

MODULE FUNCTION GetSubstring(cStr,lPos,rPos) RESULT(cSub)

    ! PURPOSE OF THIS ROUTINE:
    ! To get the specified substring from the given string based on lPos and rPos.
    ! If lPos is less than 1, then 1 is used as a starting point of the substring.
    ! Similarly, if rPos is greater than the length of the string, then the length is
    ! used as an ending point.
    ! Also, If rPos is less than lPos, a zero-length string is returned.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tIndex,    INTENT(IN)   :: lPos     ! the leftmost index
    tIndex,    INTENT(IN)   :: rPos     ! the rightmost index
    tCharAlloc              :: cSub     ! character substring

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: sPos, ePos

    ! FLOW:
    
    ! set starting position
    sPos = MAX(1, lPos)
    ! set ending position
    ePos = MIN(LEN(cStr), rPos)
    
    ! set the output
    IF (sPos <= ePos) THEN
        cSub = cStr(sPos:ePos)
    ELSE
        cSub = ''
    END IF

    RETURN

END FUNCTION GetSubstring

!******************************************************************************

MODULE FUNCTION GetSlice(cStr,First,Last,Stride) RESULT(Slice)

    ! PURPOSE OF THIS ROUTINE:
    ! To extract the characters from the region between 'first' and 'last' indices
    ! (both inclusive) of the given string by taking strides of length 'stride'.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,        INTENT(IN)    :: cStr     ! character string
    tIndex, OPTIONAL, INTENT(IN)    :: First    ! the first index
    tIndex, OPTIONAL, INTENT(IN)    :: Last     ! the last index
    tIndex, OPTIONAL, INTENT(IN)    :: Stride   ! the stride
    tCharAlloc                      :: Slice    ! character slice

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: sPos, ePos, Step, NStep, cLen, I, J

    ! FLOW:
    
    ! get string length
    cLen = LEN(cStr)
    
    ! set default values
    sPos = 0
    ePos = cLen + 1
    Step = 1
    
    ! check optional input
    IF (PRESENT(Stride)) THEN
        IF (Stride /= 0) THEN
            IF (Stride < 0) THEN
                sPos = cLen + 1
                ePos = 0
            END IF
            Step = Stride
        END IF
    ELSE
        IF (PRESENT(First).AND.PRESENT(Last)) THEN
            IF (Last < First) THEN
                Step = -1
            END IF
        END IF
    END IF

    IF (PRESENT(First)) THEN
        sPos = First
    END IF
    IF (PRESENT(Last)) THEN
        ePos = Last
    END IF

    ! set starting and ending indices
    IF (Step > 0) THEN
        ! forward step
        sPos = MAX(sPos, 1)
        ePos = MIN(ePos, cLen)
    ELSE
        ! backward step
        sPos = MIN(sPos, cLen)
        ePos = MAX(ePos, 1)
    END IF

    ! allocate the slice
    NStep = FLOOR(REAL(ePos - sPos, KIND=kDouble)/REAL(Step, KIND=kDouble) )
    ALLOCATE(tCharLen(MAX(0, NStep + 1)) :: Slice)
        
    ! get characters from the string
    J = 1
    DO I = sPos, ePos, Step
        Slice(J:J) = cStr(I:I)
        J = J + 1
    END DO

    RETURN

END FUNCTION GetSlice

!******************************************************************************

END SUBMODULE SubBase_ChrStr_Inquiry

!******************************************************************************
