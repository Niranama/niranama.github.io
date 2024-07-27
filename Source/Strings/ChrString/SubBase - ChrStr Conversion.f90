
SUBMODULE (ModBase_ChrStr) SubBase_ChrStr_Conversion

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *conversion* procedures.  These
!   procedures are conversion routines (NOT intended for use in an assignment
!   expression) between a character string and an other (Fortran intrinsic) type.

!** USE STATEMENTS:
    USE ModBase_CharConv,   ONLY: Int32ToChar    => I32_ToChar_CC, &
                                  Int64ToChar    => I64_ToChar_CC, &
                                  Int32FromChar  => I32_FromChar_CC_FortNum, &
                                  Int64FromChar  => I64_FromChar_CC_FortNum, &
                                  SingleToChar   => RealSP_ToString_DragonBox, &
                                  SingleFromChar => RealSP_FromString_Lemire, &
                                  DoubleToChar   => RealDP_ToString_DragonBox, &
                                  DoubleFromChar => RealDP_FromString_Lemire, &
                                  QuadToChar     => RealQP_ToString_DragonBox, &
                                  QuadFromChar   => RealQP_FromString_YY
    USE, INTRINSIC  :: IEEE_ARITHMETIC
    USE, INTRINSIC  :: ISO_C_BINDING

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** SUBMODULE PARAMETERS:
    tInteger, PARAMETER :: AEB = IACHAR('E')
    tInteger, PARAMETER :: AEL = IACHAR('e')
    tInteger, PARAMETER :: PLUS = IACHAR('+')
    tInteger, PARAMETER :: MINUS = IACHAR('-')
    tInteger, PARAMETER :: PERIOD = IACHAR('.')
    tInteger, PARAMETER :: SPACE = IACHAR(' ')
    tInteger, PARAMETER :: A0 = IACHAR('0')
    tInteger, PARAMETER :: A9 = IACHAR('9')

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na

!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE FUNCTIONS OR FUNCTIONS:

MODULE FUNCTION CharacterArray_To_ChrStr(cArr,IsCString) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from a character array

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,              INTENT(IN)  :: cArr(:)      ! character array
    tLogical, OPTIONAL, INTENT(IN)  :: IsCString    ! is the array a 'C' string
                                                    ! if true, it must contain a null character
                                                    ! default is FALSE
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Is_C_String
    tIndex      :: Length, I

!** FLOW
    
    ! initialize
    SET_OPTION(Is_C_String, FalseVal, IsCString)
    Length = SIZE(cArr)
    
    ! if the input array is a 'C' string, find null character and set length
    IF (Is_C_String) THEN
        DO I = 1, Length
            IF (cArr(I) == C_NULL_CHAR) THEN
                Length = I - 1
                EXIT
            END IF
        END DO
    END IF
    
    ! allocate the output string and assign it from character array
    ALLOCATE(tCharLen(Length) :: cStr)
    FORALL (I=1:Length) cStr(I:I) = cArr(I)

    RETURN

END FUNCTION CharacterArray_To_ChrStr

!******************************************************************************

MODULE FUNCTION IByte_To_ChrStr(IntNum) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc          :: cStr     ! character string
    tByte, INTENT(IN)   :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = Int32ToChar(ToInteger(IntNum))

    RETURN

END FUNCTION IByte_To_ChrStr

!******************************************************************************

MODULE FUNCTION IShort_To_ChrStr(IntNum) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc          :: cStr     ! character string
    tShort, INTENT(IN)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = Int32ToChar(ToInteger(IntNum))

    RETURN

END FUNCTION IShort_To_ChrStr

!******************************************************************************

MODULE FUNCTION Integer_To_ChrStr(IntNum) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc              :: cStr     ! character string
    tInteger,   INTENT(IN)  :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = Int32ToChar(IntNum)

    RETURN

END FUNCTION Integer_To_ChrStr

!******************************************************************************

MODULE FUNCTION ILong_To_ChrStr(IntNum) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc          :: cStr     ! character string
    tLong, INTENT(IN)   :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = Int64ToChar(IntNum)

    RETURN

END FUNCTION ILong_To_ChrStr

!******************************************************************************

MODULE FUNCTION RSingle_To_ChrStr(RealNum, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle,            INTENT(IN)  :: RealNum      ! real number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = SingleToChar(RealNum, IsScientific)

    RETURN

END FUNCTION RSingle_To_ChrStr

!******************************************************************************

MODULE FUNCTION RDouble_To_ChrStr(RealNum, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,            INTENT(IN)  :: RealNum      ! real number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = DoubleToChar(RealNum, IsScientific)
    
    RETURN

END FUNCTION RDouble_To_ChrStr

!******************************************************************************

MODULE FUNCTION RQuad_To_ChrStr(RealNum, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tQuad,              INTENT(IN)  :: RealNum      ! real number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = QuadToChar(RealNum, IsScientific)

    RETURN

END FUNCTION RQuad_To_ChrStr

!******************************************************************************

MODULE FUNCTION CSingle_To_ChrStr(CmpxNum, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxSingle,        INTENT(IN)  :: CmpxNum      ! complex number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = '(' // SingleToChar(CmpxNum%RE, IsScientific) // ', ' &
               // SingleToChar(CmpxNum%IM, IsScientific) // ')'

    RETURN

END FUNCTION CSingle_To_ChrStr

!******************************************************************************

MODULE FUNCTION CDouble_To_ChrStr(CmpxNum, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxDouble,        INTENT(IN)  :: CmpxNum      ! complex number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = '(' // DoubleToChar(CmpxNum%RE, IsScientific) // ', ' &
               // DoubleToChar(CmpxNum%IM, IsScientific) // ')'

    RETURN

END FUNCTION CDouble_To_ChrStr

!******************************************************************************

MODULE FUNCTION CQuad_To_ChrStr(CmpxNum, IsScientific) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxQuad,          INTENT(IN)  :: CmpxNum      ! complex number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific ! format flag
                                                    ! true  if to write the given number in scientific format
                                                    ! false if to write the given number in general format
                                                    ! default is false
    tCharAlloc                      :: cStr         ! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    cStr = '(' // QuadToChar(CmpxNum%RE, IsScientific) // ', ' &
               // QuadToChar(CmpxNum%IM, IsScientific) // ')'

    RETURN

END FUNCTION CQuad_To_ChrStr

!******************************************************************************

MODULE FUNCTION Logical_To_ChrStr(Boolean) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string from a logical value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharAlloc              :: cStr     ! character string
    tLogical, INTENT(IN)    :: Boolean  ! logical value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! assign character string
    IF (Boolean) THEN
        cStr = 'TRUE'
    ELSE
        cStr = 'FALSE'
    END IF

    RETURN

END FUNCTION Logical_To_ChrStr

!******************************************************************************

MODULE FUNCTION IByte_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(IntNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tByte                               :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    IntNum = ToByte(Int32FromChar(cStr, ErrFlag, ErrMsg))

    RETURN

END FUNCTION IByte_From_ChrStr

!******************************************************************************

MODULE FUNCTION IShort_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(IntNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tShort                              :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    IntNum = ToShort(Int32FromChar(cStr, ErrFlag, ErrMsg))

    RETURN

END FUNCTION IShort_From_ChrStr

!******************************************************************************

MODULE FUNCTION Integer_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(IntNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tInteger                            :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    IntNum = Int32FromChar(cStr, ErrFlag, ErrMsg)

    RETURN

END FUNCTION Integer_From_ChrStr

!******************************************************************************

MODULE FUNCTION ILong_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(IntNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to an integer number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tLong                               :: IntNum   ! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    IntNum = Int64FromChar(cStr, ErrFlag, ErrMsg)

    RETURN

END FUNCTION ILong_From_ChrStr

!******************************************************************************

MODULE FUNCTION RSingle_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(RealNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSingle                             :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    RealNum = SingleFromChar(cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)

    RETURN

END FUNCTION RSingle_From_ChrStr

!******************************************************************************

MODULE FUNCTION RDouble_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(RealNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tDouble                             :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    RealNum = DoubleFromChar(cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)

    RETURN

END FUNCTION RDouble_From_ChrStr

!******************************************************************************

MODULE FUNCTION RQuad_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(RealNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a real number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tQuad                               :: RealNum  ! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! parse number
    RealNum = QuadFromChar(cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)
    
    RETURN

END FUNCTION RQuad_From_ChrStr

!******************************************************************************

MODULE FUNCTION CSingle_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tCmpxSingle                         :: CmpxNum  ! complex number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSingle, PARAMETER      :: MinSingle = -HUGE(1.0_kSP)
    tSingle, PARAMETER      :: MaxSingle =  HUGE(1.0_kSP)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: NumFlag  ! number flag
    tQuad                   :: RealVal  ! working variable
    tQuad                   :: ImagVal  ! working variable
    CLASS(*), ALLOCATABLE   :: NumVal   ! working variable

!** FLOW

    ! set default values for real and imaginary parts
    RealVal = IEEE_VALUE(Zero, IEEE_SIGNALING_NAN)
    ImagVal = RealVal
    
    ! check whether the string is a complex number
    NumFlag = IsStringNumber(cStr, NumVal=NumVal)
    IF (NumFlag.EQ.3) THEN
        SELECT TYPE (NumVal)
        TYPE IS (tCmpxQuad)
            ! get real part
            RealVal = REAL(NumVal, KIND=kQP)
            IF (RealVal > MaxSingle) THEN
                RealVal = IEEE_VALUE(One, IEEE_POSITIVE_INF)
            ELSEIF (RealVal < MinSingle) THEN
                RealVal = IEEE_VALUE(One, IEEE_NEGATIVE_INF)
            END IF
            ! get imaginary part
            ImagVal = AIMAG(NumVal)
            IF (ImagVal > MaxSingle) THEN
                ImagVal = IEEE_VALUE(One, IEEE_POSITIVE_INF)
            ELSEIF (ImagVal < MinSingle) THEN
                ImagVal = IEEE_VALUE(One, IEEE_NEGATIVE_INF)
            END IF
        END SELECT
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Input string is a valid complex number.'
    ELSE
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (PRESENT(ErrMsg)) THEN
            SELECT CASE (NumFlag)
            CASE (-1)
                ErrMsg  = 'Invalid Input: the string is NOT a number.'
            CASE (0)
                ErrMsg  = 'Invalid Input: the string is an integer or a real number.'
            CASE (1)
                ErrMsg  = 'Invalid Input: the string is strictly an integer number.'
            CASE (2)
                ErrMsg  = 'Invalid Input: the string is strictly a real number.'
            END SELECT
        END IF
    END IF

    ! set complex number
    CmpxNum = CMPLX(RealVal, ImagVal, KIND=kSP)

    RETURN

END FUNCTION CSingle_From_ChrStr

!******************************************************************************

MODULE FUNCTION CDouble_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tCmpxDouble                         :: CmpxNum  ! complex number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tDouble, PARAMETER      :: MinDouble = -HUGE(1.0_kDP)
    tDouble, PARAMETER      :: MaxDouble =  HUGE(1.0_kDP)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: NumFlag  ! number flag
    tQuad                   :: RealVal  ! working variable
    tQuad                   :: ImagVal  ! working variable
    CLASS(*), ALLOCATABLE   :: NumVal   ! working variable

!** FLOW

    ! set default values for real and imaginary parts
    RealVal = IEEE_VALUE(Zero, IEEE_SIGNALING_NAN)
    ImagVal = RealVal
    
    ! check whether the string is a complex number
    NumFlag = IsStringNumber(cStr, NumVal=NumVal)
    IF (NumFlag.EQ.3) THEN
        SELECT TYPE (NumVal)
        TYPE IS (tCmpxQuad)
            ! get real part
            RealVal = REAL(NumVal, KIND=kQP)
            IF (RealVal > MaxDouble) THEN
                RealVal = IEEE_VALUE(One, IEEE_POSITIVE_INF)
            ELSEIF (RealVal < MinDouble) THEN
                RealVal = IEEE_VALUE(One, IEEE_NEGATIVE_INF)
            END IF
            ! get imaginary part
            ImagVal = AIMAG(NumVal)
            IF (ImagVal > MaxDouble) THEN
                ImagVal = IEEE_VALUE(One, IEEE_POSITIVE_INF)
            ELSEIF (ImagVal < MinDouble) THEN
                ImagVal = IEEE_VALUE(One, IEEE_NEGATIVE_INF)
            END IF
        END SELECT
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Input string is a valid complex number.'
    ELSE
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (PRESENT(ErrMsg)) THEN
            SELECT CASE (NumFlag)
            CASE (-1)
                ErrMsg  = 'Invalid Input: the string is NOT a number.'
            CASE (0)
                ErrMsg  = 'Invalid Input: the string is an integer or a real number.'
            CASE (1)
                ErrMsg  = 'Invalid Input: the string is strictly an integer number.'
            CASE (2)
                ErrMsg  = 'Invalid Input: the string is strictly a real number.'
            END SELECT
        END IF
    END IF

    ! set complex number
    CmpxNum = CMPLX(RealVal, ImagVal, KIND=kDP)

    RETURN

END FUNCTION CDouble_From_ChrStr

!******************************************************************************

MODULE FUNCTION CQuad_From_ChrStr(cStr,ErrFlag,ErrMsg) RESULT(CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a complex number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tCmpxQuad                           :: CmpxNum  ! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger                :: NumFlag  ! number flag
    tQuad                   :: RealVal  ! working variable
    tQuad                   :: ImagVal  ! working variable
    CLASS(*), ALLOCATABLE   :: NumVal   ! working variable

!** FLOW

    ! set default values for real and imaginary parts
    RealVal = IEEE_VALUE(Zero, IEEE_SIGNALING_NAN)
    ImagVal = RealVal
    
    ! check whether the string is a complex number
    NumFlag = IsStringNumber(cStr, NumVal=NumVal)
    IF (NumFlag.EQ.3) THEN
        SELECT TYPE (NumVal)
        TYPE IS (tCmpxQuad)
            ! get real part
            RealVal = REAL(NumVal, KIND=kQP)
            ! get imaginary part
            ImagVal = AIMAG(NumVal)
        END SELECT
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Input string is a valid complex number.'
    ELSE
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (PRESENT(ErrMsg)) THEN
            SELECT CASE (NumFlag)
            CASE (-1)
                ErrMsg  = 'Invalid Input: the string is NOT a number.'
            CASE (0)
                ErrMsg  = 'Invalid Input: the string is an integer or a real number.'
            CASE (1)
                ErrMsg  = 'Invalid Input: the string is strictly an integer number.'
            CASE (2)
                ErrMsg  = 'Invalid Input: the string is strictly a real number.'
            END SELECT
        END IF
    END IF

    ! set complex number
    CmpxNum = CMPLX(RealVal, ImagVal, KIND=kQP)

    RETURN

END FUNCTION CQuad_From_ChrStr

!******************************************************************************

MODULE FUNCTION Logical_From_ChrStr(cStr) RESULT(Boolean)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a logical value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr     ! character string
    tLogical                :: Boolean  ! logical value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical  :: LogFlag

!** FLOW

    LogFlag = IsStringLogical(cStr, Boolean)

    RETURN

END FUNCTION Logical_From_ChrStr

!******************************************************************************

MODULE FUNCTION CharArray_From_ChrStr_I(cStr,IsCString) RESULT(cArr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a character array (allocatable)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,          INTENT(IN)  :: cStr         ! character string
    tLogical, OPTIONAL, INTENT(IN)  :: IsCString    ! is the output array to be a 'C' string
                                                    ! if true, it will contain a null character
                                                    ! default is FALSE
    tChar, ALLOCATABLE              :: cArr(:)      ! character array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Is_C_String
    tIndex      :: Length, I

!** FLOW
    
    ! initialize
    SET_OPTION(Is_C_String, FalseVal, IsCString)
    Length = LEN(cStr)
    IF (Is_C_String) Length = Length + 1

    ! allocate the output array and assign it from the character string
    ALLOCATE(cArr(Length))
    IF (LEN(cStr).GE.1) THEN
        FORALL (I=1:LEN(cStr)) cArr(I) = cStr(I:I)
    END IF
    IF (Is_C_String) cArr(Length) = C_NULL_CHAR

    RETURN

END FUNCTION CharArray_From_ChrStr_I

!******************************************************************************

MODULE FUNCTION CharArray_From_ChrStr_II(cStr) RESULT(cArr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a character array (explicit shape)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr             ! character string
    tChar                   :: cArr(LEN(cStr))  ! character array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Length

!** FLOW

    Length = LEN(cStr)
    IF (Length.GE.1) THEN
        FORALL (I=1:Length) cArr(I) = cStr(I:I)
    END IF

    RETURN

END FUNCTION CharArray_From_ChrStr_II

!******************************************************************************

MODULE FUNCTION CString_From_ChrStr(cStr) RESULT(cArr)

!** PURPOSE OF THIS ROUTINE:
    ! To convert a character string to a 'C' style string (explicit-shape character array)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr                 ! character string
    tChar                   :: cArr(LEN(cStr)+1)    ! character array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Length

!** FLOW

    Length = LEN(cStr)
    IF (Length.GE.1) THEN
        FORALL (I=1:Length) cArr(I) = cStr(I:I)
    END IF
    cArr(Length+1) = C_NULL_CHAR

    RETURN

END FUNCTION CString_From_ChrStr

!******************************************************************************

END SUBMODULE SubBase_ChrStr_Conversion

!******************************************************************************
