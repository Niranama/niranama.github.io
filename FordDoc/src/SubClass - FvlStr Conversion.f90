
SUBMODULE (Class_FvlStr) SubClass_FvlStr_Conversion

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *conversion* procedures.  These
!   procedures are conversion routines (NOT intended for use in an assignment
!   expression) between a *FvlStr* object and an other (Fortran intrinsic) type.

!** USE STATEMENTS:
    USE ModBase_ChrStr,     ONLY: ParseLogical, ParseCSingle, ParseCDouble, ParseCQuad
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
    ! na

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE DEFINITIONS:
    ! na
    
!** SUBMODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** SUBMODULE SUBROUTINES OR FUNCTIONS:

MODULE FUNCTION CharacterArray_To_FvlStr(cArr,IsCString) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert an array of characters to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar,              INTENT(IN)  :: cArr(:)  !! array of characters
    tLogical, OPTIONAL, INTENT(IN)  :: IsCString
    !^ flag indicating whether the array is a 'C' string or not. <br>
    !  If true, the array must contain a null character. <br>
    !  Default is FALSE.
    TYPE(FvlStr)                    :: vStr     !! FvlStr object

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
    ALLOCATE(tCharLen(Length) :: vStr%cStr)
    FORALL (I=1:Length) vStr%cStr(I:I) = cArr(I)

    RETURN

END FUNCTION CharacterArray_To_FvlStr

!******************************************************************************

MODULE FUNCTION IByte_To_FvlStr(IntNum) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert an 8-bit integer number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, INTENT(IN)   :: IntNum   !! integer number
    TYPE(FvlStr)        :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = Int32ToChar(ToInteger(IntNum))

    RETURN

END FUNCTION IByte_To_FvlStr

!******************************************************************************

MODULE FUNCTION IShort_To_FvlStr(IntNum) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a 16-bit integer number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tShort, INTENT(IN)  :: IntNum   !! integer number
    TYPE(FvlStr)        :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = Int32ToChar(ToInteger(IntNum))

    RETURN

END FUNCTION IShort_To_FvlStr

!******************************************************************************

MODULE FUNCTION Integer_To_FvlStr(IntNum) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a 32-bit integer number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: IntNum   !! integer number
    TYPE(FvlStr)            :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = Int32ToChar(IntNum)

    RETURN

END FUNCTION Integer_To_FvlStr

!******************************************************************************

MODULE FUNCTION ILong_To_FvlStr(IntNum) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a 64-bit integer number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(IN)   :: IntNum   !! integer number
    TYPE(FvlStr)        :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = Int64ToChar(IntNum)

    RETURN

END FUNCTION ILong_To_FvlStr

!******************************************************************************

MODULE FUNCTION RSingle_To_FvlStr(RealNum, IsScientific) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a single-precision real number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSingle,            INTENT(IN)  :: RealNum  !! real number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
    !^ flag indicating whether the string is expressed in the scientific format. <br>
    !  Default is false where the string is expressed in the general format.
    TYPE(FvlStr)                    :: vStr   !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = SingleToChar(RealNum, IsScientific)

    RETURN

END FUNCTION RSingle_To_FvlStr

!******************************************************************************

MODULE FUNCTION RDouble_To_FvlStr(RealNum, IsScientific) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a double-precision real number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tDouble,            INTENT(IN)  :: RealNum  !! real number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
    !^ flag indicating whether the string is expressed in the scientific format. <br>
    !  Default is false where the string is expressed in the general format.
    TYPE(FvlStr)                    :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = DoubleToChar(RealNum, IsScientific)
    
    RETURN

END FUNCTION RDouble_To_FvlStr

!******************************************************************************

MODULE FUNCTION RQuad_To_FvlStr(RealNum, IsScientific) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a quadruple-precision real number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tQuad,              INTENT(IN)  :: RealNum  !! real number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
    !^ flag indicating whether the string is expressed in the scientific format. <br>
    !  Default is false where the string is expressed in the general format.
    TYPE(FvlStr)                    :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = QuadToChar(RealNum, IsScientific)

    RETURN

END FUNCTION RQuad_To_FvlStr

!******************************************************************************

MODULE FUNCTION CSingle_To_FvlStr(CmpxNum, IsScientific) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a single-precision complex number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxSingle,        INTENT(IN)  :: CmpxNum  !! complex number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
    !^ flag indicating whether the string is expressed in the scientific format. <br>
    !  Default is false where the string is expressed in the general format.
    TYPE(FvlStr)                      :: vStr   !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = '(' // SingleToChar(CmpxNum%RE, IsScientific) // ', ' &
                    // SingleToChar(CmpxNum%IM, IsScientific) // ')'

    RETURN

END FUNCTION CSingle_To_FvlStr

!******************************************************************************

MODULE FUNCTION CDouble_To_FvlStr(CmpxNum, IsScientific) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a double-precision complex number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxDouble,        INTENT(IN)  :: CmpxNum  !! complex number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
    !^ flag indicating whether the string is expressed in the scientific format. <br>
    !  Default is false where the string is expressed in the general format.
    TYPE(FvlStr)                      :: vStr   !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = '(' // DoubleToChar(CmpxNum%RE, IsScientific) // ', ' &
                    // DoubleToChar(CmpxNum%IM, IsScientific) // ')'

    RETURN

END FUNCTION CDouble_To_FvlStr

!******************************************************************************

MODULE FUNCTION CQuad_To_FvlStr(CmpxNum, IsScientific) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a quadruple-precision complex number to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxQuad,          INTENT(IN)  :: CmpxNum  !! complex number
    tLogical, OPTIONAL, INTENT(IN)  :: IsScientific
    !^ flag indicating whether the string is expressed in the scientific format. <br>
    !  Default is false where the string is expressed in the general format.
    TYPE(FvlStr)                    :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vStr%cStr = '(' // QuadToChar(CmpxNum%RE, IsScientific) // ', ' &
                    // QuadToChar(CmpxNum%IM, IsScientific) // ')'

    RETURN

END FUNCTION CQuad_To_FvlStr

!******************************************************************************

MODULE FUNCTION Logical_To_FvlStr(Boolean) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a logical value to a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, INTENT(IN)    :: Boolean  !! logical value
    TYPE(FvlStr)            :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! assign FvlStr object
    IF (Boolean) THEN
        vStr%cStr = 'TRUE'
    ELSE
        vStr%cStr = 'FALSE'
    END IF

    RETURN

END FUNCTION Logical_To_FvlStr

!******************************************************************************

MODULE FUNCTION IByte_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(IntNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to an 8-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tByte                               :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        IntNum = ToByte(Int32FromChar(vStr%cStr, ErrFlag, ErrMsg))
    ELSE
        ! set output to mininum value
        IntNum = ToByte(Z'80')
    END IF

    RETURN

END FUNCTION IByte_From_FvlStr

!******************************************************************************

MODULE FUNCTION IShort_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(IntNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a 16-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tShort                              :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        IntNum = ToShort(Int32FromChar(vStr%cStr, ErrFlag, ErrMsg))
    ELSE
        ! set output to mininum value
        IntNum = ToShort(Z'8000')
    END IF

    RETURN

END FUNCTION IShort_From_FvlStr

!******************************************************************************

MODULE FUNCTION Integer_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(IntNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tInteger                            :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        IntNum = Int32FromChar(vStr%cStr, ErrFlag, ErrMsg)
    ELSE
        ! set output to mininum value
        IntNum = ToInteger(Z'80000000')
    END IF

    RETURN

END FUNCTION Integer_From_FvlStr

!******************************************************************************

MODULE FUNCTION ILong_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(IntNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLong                               :: IntNum   !! integer number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        IntNum = Int64FromChar(vStr%cStr, ErrFlag, ErrMsg)
    ELSE
        ! set output to mininum value
        IntNum = ToLong(Z'8000000000000000')
    END IF

    RETURN

END FUNCTION ILong_From_FvlStr

!******************************************************************************

MODULE FUNCTION RSingle_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(RealNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a single-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tSingle                             :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        RealNum = SingleFromChar(vStr%cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)
    ELSE
        ! set output to NAN
        RealNum = IEEE_VALUE(0.0E0_kSingle, IEEE_SIGNALING_NAN)
    END IF

    RETURN

END FUNCTION RSingle_From_FvlStr

!******************************************************************************

MODULE FUNCTION RDouble_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(RealNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a double-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tDouble                             :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        RealNum = DoubleFromChar(vStr%cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)
    ELSE
        ! set output to NAN
        RealNum = IEEE_VALUE(0.0E0_kDouble, IEEE_SIGNALING_NAN)
    END IF

    RETURN

END FUNCTION RDouble_From_FvlStr

!******************************************************************************

MODULE FUNCTION RQuad_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(RealNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a quadruple-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tQuad                               :: RealNum  !! real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        RealNum = QuadFromChar(vStr%cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)
    ELSE
        ! set output to NAN
        RealNum = IEEE_VALUE(0.0E0_kQuad, IEEE_SIGNALING_NAN)
    END IF

    RETURN

END FUNCTION RQuad_From_FvlStr

!******************************************************************************

MODULE FUNCTION CSingle_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a single-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tCmpxSingle                         :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSingle     :: RealVal  ! working variable

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        CmpxNum = ParseCSingle(vStr%cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)
    ELSE
        ! set output to (NAN,NAN)
        RealVal = IEEE_VALUE(0.0E0_kSingle, IEEE_SIGNALING_NAN)
        CmpxNum = CMPLX(RealVal, RealVal, KIND=kSingle)
    END IF

    RETURN

END FUNCTION CSingle_From_FvlStr

!******************************************************************************

MODULE FUNCTION CDouble_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a double-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tCmpxDouble                         :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tDouble     :: RealVal  ! working variable

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        CmpxNum = ParseCDouble(vStr%cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)
    ELSE
        ! set output to (NAN,NAN)
        RealVal = IEEE_VALUE(0.0E0_kDouble, IEEE_SIGNALING_NAN)
        CmpxNum = CMPLX(RealVal, RealVal, KIND=kDouble)
    END IF

    RETURN

END FUNCTION CDouble_From_FvlStr

!******************************************************************************

MODULE FUNCTION CQuad_From_FvlStr(vStr,ErrFlag,ErrMsg) RESULT(CmpxNum)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a quadruple-precision complex number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),        INTENT(IN)    :: vStr     !! FvlStr object
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tCmpxQuad                           :: CmpxNum  !! complex number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tQuad     :: RealVal  ! working variable

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        ! parse number
        CmpxNum = ParseCQuad(vStr%cStr, ErrFlag=ErrFlag, ErrMsg=ErrMsg)
    ELSE
        ! set output to (NAN,NAN)
        RealVal = IEEE_VALUE(0.0E0_kQuad, IEEE_SIGNALING_NAN)
        CmpxNum = CMPLX(RealVal, RealVal, KIND=kQuad)
    END IF

    RETURN

END FUNCTION CQuad_From_FvlStr

!******************************************************************************

MODULE FUNCTION Logical_From_FvlStr(vStr) RESULT(Boolean)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to a logical value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    tLogical                    :: Boolean  !! logical value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        Boolean = ParseLogical(vStr%cStr)
    ELSE
        ! set value to FalseVal and show warning message
        Boolean = FalseVal
    END IF

    RETURN

END FUNCTION Logical_From_FvlStr

!******************************************************************************

MODULE FUNCTION CharArray_From_FvlStr(vStr,IsCString) RESULT(cArr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to an allocatable array of characters.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
    tLogical, OPTIONAL, INTENT(IN)  :: IsCString
    !^ flag indicating whether the array is a 'C' string or not. <br>
    !  If true, the array will contain a null character. <br>
    !  Default is FALSE.
    tChar, ALLOCATABLE              :: cArr(:)  !! character array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Is_C_String
    tIndex      :: Length, I

!** FLOW
    
    ! initialize
    SET_OPTION(Is_C_String, FalseVal, IsCString)

    IF (ALLOCATED(vStr%cStr)) THEN
        Length = LEN(vStr%cStr)
        IF (Is_C_String) Length = Length + 1
        ! allocate the output array and assign it from the FvlStr object
        ALLOCATE(cArr(Length))
        FORALL (I=1:LEN(vStr%cStr)) cArr(I) = vStr%cStr(I:I)
        IF (Is_C_String) cArr(Length) = C_NULL_CHAR
    ELSE
        IF (Is_C_String) THEN
            ALLOCATE(cArr(1))
            cArr(1) = C_NULL_CHAR
        ELSE
            ALLOCATE(cArr(0))
        END IF
    END IF

    RETURN

END FUNCTION CharArray_From_FvlStr

!******************************************************************************

MODULE FUNCTION CharAlloc_From_FvlStr(vStr) RESULT(cStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert a FvlStr object to an allocatable character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr !! FvlStr object
    tCharAlloc                  :: cStr !! character string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        cStr = vStr%cStr
    ELSE
        ALLOCATE(tCharLen(0) :: cStr)
    END IF

    RETURN

END FUNCTION CharAlloc_From_FvlStr

!******************************************************************************

END SUBMODULE SubClass_FvlStr_Conversion

!******************************************************************************
