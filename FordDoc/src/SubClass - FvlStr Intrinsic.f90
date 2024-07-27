
SUBMODULE (Class_FvlStr) SubClass_FvlStr_Intrinsic

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *intrinsic-like* procedures
!   (i.e. routines that can be overloaded with Fortran intrinsic routines
!    for the *CHARACTER* type).

!** USE STATEMENTS:
    ! na

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

!** SUBMODULE SUBROUTINES OR MODULE FUNCTIONS:

!------------------------------------------------------------------------------
!                           INQUIRY PROCEDURES
!------------------------------------------------------------------------------

MODULE FUNCTION GetLength(vStr) RESULT(Length)

!** PURPOSE OF THIS ROUTINE:
    !^ To return length of the character string of a FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    tIndex                      :: Length   !! length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        Length = LEN(vStr%cStr)
    ELSE
        Length = 0
    END IF

    RETURN

END FUNCTION GetLength

!******************************************************************************

MODULE FUNCTION GetLengthTrim(vStr) RESULT(Length)

!** PURPOSE OF THIS ROUTINE:
    !^ To return length of the character string of a FvlStr object 
    !  without counting trailing blank characters.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr     !! FvlStr object
    tIndex                      :: Length   !! length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStr%cStr)) THEN
        Length = LEN_TRIM(vStr%cStr)
    ELSE
        Length = 0
    END IF

    RETURN

END FUNCTION GetLengthTrim

!******************************************************************************

MODULE FUNCTION FindIndex_CHS(vStr,sStr,Back) RESULT(Indx)

!** PURPOSE OF THIS ROUTINE:
    !^ To return the starting position of a substring within a string of FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)  :: sStr     !! substring
    tLogical, OPTIONAL, INTENT(IN)  :: Back
    !^ If present and true, searching from the back; otherwise, searching from the front.
    tIndex                          :: Indx     !! starting position of a substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Indx = 0
    IF (ALLOCATED(vStr%cStr).AND.(LEN(sStr) > 0)) THEN
        Indx = INDEX(vStr%cStr, sStr, Back)
    END IF

    RETURN

END FUNCTION FindIndex_CHS

!******************************************************************************

MODULE FUNCTION FindIndex_VLS(vStr,sStr,Back) RESULT(Indx)

!** PURPOSE OF THIS ROUTINE:
    !^ To return the starting position of a substring within a string of FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)  :: sStr     !! substring
    tLogical, OPTIONAL, INTENT(IN)  :: Back
    !^ If present and true, searching from the back; otherwise, searching from the front.
    tIndex                          :: Indx     !! starting position of a substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Indx = 0
    IF (ALLOCATED(vStr%cStr).AND.ALLOCATED(sStr%cStr)) THEN
        Indx = INDEX(vStr%cStr, sStr%cStr, Back)
    END IF

    RETURN

END FUNCTION FindIndex_VLS

!******************************************************************************

MODULE FUNCTION ScanCharacters_CHS(vStr,ChrSet,Back) RESULT(Pos)

!** PURPOSE OF THIS ROUTINE:
    !^ To scan the string of a FvlStr object for any character in a set of
    !  characters and return the position of the first character found in the
    !  string that is in the specified set depending on the scanning direction.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)  :: ChrSet   !! a set of characters
    tLogical, OPTIONAL, INTENT(IN)  :: Back
    !^ If present and true, scanning from the back; otherwise, scanning from the front.
    tIndex                          :: Pos      !! position of the first character found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Pos = 0
    IF (ALLOCATED(vStr%cStr).AND.(LEN(ChrSet) > 0)) THEN
        Pos = SCAN(vStr%cStr, ChrSet, Back)
    END IF

    RETURN

END FUNCTION ScanCharacters_CHS

!******************************************************************************

MODULE FUNCTION ScanCharacters_VLS(vStr,ChrSet,Back) RESULT(Pos)

!** PURPOSE OF THIS ROUTINE:
    !^ To scan the string of a FvlStr object for any character in a set of
    !  characters and return the position of the first character found in the
    !  string that is in the specified set depending on the scanning direction.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)  :: ChrSet   !! a set of characters
    tLogical, OPTIONAL, INTENT(IN)  :: Back
    !^ If present and true, scanning from the back; otherwise, scanning from the front.
    tIndex                          :: Pos      !! position of the first character found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Pos = 0
    IF (ALLOCATED(vStr%cStr).AND.ALLOCATED(ChrSet%cStr)) THEN
        Pos = SCAN(vStr%cStr, ChrSet%cStr, Back)
    END IF

    RETURN

END FUNCTION ScanCharacters_VLS

!******************************************************************************

MODULE FUNCTION VerifyCharacters_CHS(vStr,ChrSet,Back) RESULT(Pos)

!** PURPOSE OF THIS ROUTINE:
    !^ To verify that a set of characters contains all the characters in
    !  the string of a FvlStr object by identifying the first character
    !  in the string that is not in the set and to return the position
    !  of the first character found in the string that is NOT in the
    !  specified set depending on the scanning direction.  If all characters
    !  of string are in the specified set or the length of string is zero,
    !  the returned value is zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)  :: ChrSet   !! a set of characters
    tLogical, OPTIONAL, INTENT(IN)  :: Back
    !^ If present and true, scanning from the back; otherwise, scanning from the front.
    tIndex                          :: Pos      !! position of the first character found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Pos = 0
    IF (ALLOCATED(vStr%cStr).AND.(LEN(ChrSet) > 0)) THEN
        Pos = VERIFY(vStr%cStr, ChrSet, Back)
    END IF

    RETURN

END FUNCTION VerifyCharacters_CHS

!******************************************************************************

MODULE FUNCTION VerifyCharacters_VLS(vStr,ChrSet,Back) RESULT(Pos)

!** PURPOSE OF THIS ROUTINE:
    !^ To verify that a set of characters contains all the characters in
    !  the string of a FvlStr object by identifying the first character
    !  in the string that is not in the set and to return the position
    !  of the first character found in the string that is NOT in the
    !  specified set depending on the scanning direction.  If all characters
    !  of string are in the specified set or the length of string is zero,
    !  the returned value is zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr),       INTENT(IN)  :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)  :: ChrSet   !! a set of characters
    tLogical, OPTIONAL, INTENT(IN)  :: Back
    !^ If present and true, scanning from the back; otherwise, scanning from the front.
    tIndex                          :: Pos      !! position of the first character found

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Pos = 0
    IF (ALLOCATED(vStr%cStr).AND.ALLOCATED(ChrSet%cStr)) THEN
        Pos = VERIFY(vStr%cStr, ChrSet%cStr, Back)
    END IF

    RETURN

END FUNCTION VerifyCharacters_VLS

!------------------------------------------------------------------------------
!                           STRING HANDLING PROCEDURES
!------------------------------------------------------------------------------

MODULE FUNCTION AdjustToLeft(vStrIn) RESULT(vStrOut)

!** PURPOSE OF THIS ROUTINE:
    !^ To adjust the string of FvlStr object to the left, removing leading
    !  blanks and inserting trailing blanks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN) :: vStrIn       !! input FvlStr object
    TYPE(FvlStr)             :: vStrOut      !! output FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStrIn%cStr)) THEN
        vStrOut%cStr = ADJUSTL(vStrIn%cStr)
    ELSE
        vStrOut%cStr = ''
    END IF

    RETURN

END FUNCTION AdjustToLeft

!******************************************************************************

MODULE FUNCTION AdjustToRight(vStrIn) RESULT(vStrOut)

!** PURPOSE OF THIS ROUTINE:
    !^ To adjust the string of FvlStr object to the right, removing trailing
    !  blanks and inserting leading blanks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN) :: vStrIn       !! input FvlStr object
    TYPE(FvlStr)             :: vStrOut      !! output FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStrIn%cStr)) THEN
        vStrOut%cStr = ADJUSTR(vStrIn%cStr)
    ELSE
        vStrOut%cStr = ''
    END IF

    RETURN

END FUNCTION AdjustToRight

!******************************************************************************

MODULE FUNCTION TrimFvlStr(vStrIn) RESULT(vStrOut)

!** PURPOSE OF THIS ROUTINE:
    !^ To return the argument with trailing blanks removed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN) :: vStrIn       !! input FvlStr object
    TYPE(FvlStr)             :: vStrOut      !! output FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStrIn%cStr)) THEN
        vStrOut%cStr = TRIM(vStrIn%cStr)
    ELSE
        vStrOut%cStr = ''
    END IF

    RETURN

END FUNCTION TrimFvlStr

!******************************************************************************

MODULE FUNCTION RepeatString(vStrIn,nCopies) RESULT(vStrOut)

!** PURPOSE OF THIS ROUTINE:
    !^ To concatenate several copies of the specified string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStrIn   !! input FvlStr object to be repeated
    tIndex,       INTENT(IN)    :: nCopies  !! number of copies
    TYPE(FvlStr)                :: vStrOut  !! output FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(vStrIn%cStr)) THEN
        vStrOut%cStr = REPEAT(vStrIn%cStr, nCopies)
    ELSE
        vStrOut%cStr = ''
    END IF

    RETURN

END FUNCTION RepeatString

!******************************************************************************

MODULE FUNCTION Concatenate_VLS_CHS(Str1st,Str2nd) RESULT(vStrOut)

!** PURPOSE OF THIS ROUTINE:
    !^ To concatenate the first and second strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: Str1st   !! the first string
    tCharStar,    INTENT(IN)    :: Str2nd   !! the second string
    TYPE(FvlStr)                :: vStrOut  !! output FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Str1st%cStr)) THEN
        vStrOut%cStr = Str1st%cStr // Str2nd
    ELSE
        vStrOut%cStr = Str2nd
    END IF

    RETURN

END FUNCTION Concatenate_VLS_CHS

!******************************************************************************

MODULE FUNCTION Concatenate_CHS_VLS(Str1st,Str2nd) RESULT(vStrOut)

!** PURPOSE OF THIS ROUTINE:
    !^ To concatenate the first and second strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: Str1st   !! the first string
    TYPE(FvlStr), INTENT(IN)    :: Str2nd   !! the second string
    TYPE(FvlStr)                :: vStrOut  !! output FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Str2nd%cStr)) THEN
        vStrOut%cStr = Str1st // Str2nd%cStr
    ELSE
        vStrOut%cStr = Str1st
    END IF

    RETURN

END FUNCTION Concatenate_CHS_VLS

!******************************************************************************

MODULE FUNCTION Concatenate_VLS_VLS(Str1st,Str2nd) RESULT(vStrOut)

!** PURPOSE OF THIS ROUTINE:
    !^ To concatenate the first and second strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: Str1st   !! the first string
    TYPE(FvlStr), INTENT(IN)    :: Str2nd   !! the second string
    TYPE(FvlStr)                :: vStrOut  !! output FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Str1st%cStr).AND.ALLOCATED(Str2nd%cStr)) THEN
        vStrOut%cStr = Str1st%cStr // Str2nd%cStr
    ELSEIF (ALLOCATED(Str1st%cStr)) THEN
        vStrOut%cStr = Str1st%cStr
    ELSEIF (ALLOCATED(Str2nd%cStr)) THEN
        vStrOut%cStr = Str2nd%cStr
    ELSE
        vStrOut = ''
    END IF

    RETURN

END FUNCTION Concatenate_VLS_VLS

!------------------------------------------------------------------------------
!                           COMPARISON PROCEDURES
!------------------------------------------------------------------------------

MODULE FUNCTION VLS_EQ_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = lStr%cStr.EQ.rStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_EQ_CHS

!******************************************************************************

MODULE FUNCTION CHS_EQ_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = lStr.EQ.rStr%cStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_EQ_VLS

!******************************************************************************

MODULE FUNCTION VLS_NE_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform not-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = lStr%cStr.NE.rStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_NE_CHS

!******************************************************************************

MODULE FUNCTION CHS_NE_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform not-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = lStr.NE.rStr%cStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_NE_VLS

!******************************************************************************

MODULE FUNCTION VLS_GT_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = lStr%cStr.GT.rStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_GT_CHS

!******************************************************************************

MODULE FUNCTION CHS_GT_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = lStr.GT.rStr%cStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_GT_VLS

!******************************************************************************

MODULE FUNCTION VLS_GE_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = lStr%cStr.GE.rStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_GE_CHS

!******************************************************************************

MODULE FUNCTION CHS_GE_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = lStr.GE.rStr%cStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_GE_VLS

!******************************************************************************

MODULE FUNCTION VLS_LT_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = lStr%cStr.LT.rStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LT_CHS

!******************************************************************************

MODULE FUNCTION CHS_LT_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = lStr.LT.rStr%cStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_LT_VLS

!******************************************************************************

MODULE FUNCTION VLS_LE_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = lStr%cStr.LE.rStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LE_CHS

!******************************************************************************

MODULE FUNCTION CHS_LE_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = lStr.LE.rStr%cStr
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_LE_VLS

!------------------------------------------------------------------------------
!                       ASCII LEXIAL COMPARISON PROCEDURES
!------------------------------------------------------------------------------

MODULE FUNCTION VLS_LGT_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr).AND.ALLOCATED(rStr%cStr)) THEN
        Flag = LGT(lStr%cStr, rStr%cStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LGT_VLS

!******************************************************************************

MODULE FUNCTION VLS_LGT_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = LGT(lStr%cStr, rStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LGT_CHS

!******************************************************************************

MODULE FUNCTION CHS_LGT_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = LGT(lStr, rStr%cStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_LGT_VLS

!******************************************************************************

MODULE FUNCTION VLS_LGE_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr).AND.ALLOCATED(rStr%cStr)) THEN
        Flag = LGE(lStr%cStr, rStr%cStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LGE_VLS

!******************************************************************************

MODULE FUNCTION VLS_LGE_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = LGE(lStr%cStr, rStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LGE_CHS

!******************************************************************************

MODULE FUNCTION CHS_LGE_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform greater-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = LGE(lStr, rStr%cStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_LGE_VLS

!******************************************************************************

MODULE FUNCTION VLS_LLT_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr).AND.ALLOCATED(rStr%cStr)) THEN
        Flag = LLT(lStr%cStr, rStr%cStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LLT_VLS

!******************************************************************************

MODULE FUNCTION VLS_LLT_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = LLT(lStr%cStr, rStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LLT_CHS

!******************************************************************************

MODULE FUNCTION CHS_LLT_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = LLT(lStr, rStr%cStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_LLT_VLS

!******************************************************************************

MODULE FUNCTION VLS_LLE_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr).AND.ALLOCATED(rStr%cStr)) THEN
        Flag = LLE(lStr%cStr, rStr%cStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LLE_VLS

!******************************************************************************

MODULE FUNCTION VLS_LLE_CHS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: lStr     !! left hand side string
    tCharStar,    INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(lStr%cStr)) THEN
        Flag = LLE(lStr%cStr, rStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION VLS_LLE_CHS

!******************************************************************************

MODULE FUNCTION CHS_LLE_VLS(lStr,rStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To perform less-than-or-equal-to operation of two strings.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,    INTENT(IN)    :: lStr     !! left hand side string
    TYPE(FvlStr), INTENT(IN)    :: rStr     !! right hand side string
    tLogical                    :: Flag     !! result of the operation

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(rStr%cStr)) THEN
        Flag = LLE(lStr, rStr%cStr)
    ELSE
        Flag = FalseVal
    END IF

    RETURN

END FUNCTION CHS_LLE_VLS

!------------------------------------------------------------------------------
!                           CONVERSION PROCEDURES
!------------------------------------------------------------------------------

MODULE FUNCTION Get_ACHAR(I) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To get the character in the specified position of the ASCII character set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I    !! character code (position in the set)
    TYPE(FvlStr)            :: vStr !! FvlStr object with length of 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTRINSIC   :: ACHAR

!** FLOW

    vStr%cStr = ACHAR(I)

    RETURN

END FUNCTION Get_ACHAR

!******************************************************************************

MODULE FUNCTION Get_CHAR(I) RESULT(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To get the character in the specified position of the processor's character set.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(IN)    :: I    !! character code (position in the set)
    TYPE(FvlStr)            :: vStr !! FvlStr object with length of 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    INTRINSIC   :: CHAR

!** FLOW

    vStr%cStr = CHAR(I)

    RETURN

END FUNCTION Get_CHAR

!******************************************************************************

MODULE FUNCTION Get_IACHAR(vStr, Pos) RESULT(I)

!** PURPOSE OF THIS ROUTINE:
    !^ To get the character code based on the ASCII character set of the specified
    !  character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr !! FvlStr object
    tInteger,     INTENT(IN)    :: Pos  !! index specifying the FvlStr's character
    tInteger                    :: I
    !^ the requested character code (position in the character set);
    !  I = -1 if invalid set of input is given.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I = -1
    IF (ALLOCATED(vStr%cStr)) THEN
        IF (IN_RANGE(Pos, 1, LEN(vStr%cStr))) I = IACHAR(vStr%cStr(Pos:Pos))
    END IF

    RETURN

END FUNCTION Get_IACHAR

!******************************************************************************

MODULE FUNCTION Get_ICHAR(vStr, Pos) RESULT(I)

!** PURPOSE OF THIS ROUTINE:
    !^ To get the character code based on the processor's character set of the specified
    !  character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(FvlStr), INTENT(IN)    :: vStr !! FvlStr object
    tInteger,     INTENT(IN)    :: Pos  !! index specifying the FvlStr's character
    tInteger                    :: I
    !^ the requested character code (position in the character set);
    !  I = -1 if invalid set of input is given.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    I = -1
    IF (ALLOCATED(vStr%cStr)) THEN
        IF (IN_RANGE(Pos, 1, LEN(vStr%cStr))) I = ICHAR(vStr%cStr(Pos:Pos))
    END IF

    RETURN

END FUNCTION Get_ICHAR

!******************************************************************************

END SUBMODULE SubClass_FvlStr_Intrinsic

!******************************************************************************
