
SUBMODULE (Class_FvlStr) SubClass_FvlStr_Manipulation

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *manipulation* procedures (i.e. routines
!   that perform a manipulation on the character string of a FvlStr object).

!** USE STATEMENTS:
    USE ModBase_ChrStr

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

!--------------------------------------------------------------------------------------
!                               CASE PROCEDURES
!--------------------------------------------------------------------------------------

MODULE ELEMENTAL SUBROUTINE AlterCase(vStr,ToUpper)

!** PURPOSE OF THIS ROUTINE:
    !^ To change case of the character string of the FvlStr object according to flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
    tLogical,      INTENT(IN)       :: ToUpper
    !^ flag indicating whether to change the string's characters to
    !  upper-case characters or not. <br>
    !  - If true, the string contains upper-case characters on exit. <br>
    !  - If false, the string contains lower-case characters on exit. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        CALL ChangeCase(vStr%cStr, ToUpper)
    END IF
    
    RETURN

END SUBROUTINE AlterCase

!******************************************************************************

MODULE SUBROUTINE BlankCompressAlterCase(vStr,ToUpper)

!** PURPOSE OF THIS ROUTINE:
    !^ To first remove all blanks and then change case of the character string
    !  of the FvlStr object according to flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
    tLogical,      INTENT(IN)       :: ToUpper
    !^ flag indicating whether to change the string's characters to
    !  upper-case characters or not. <br>
    !  - If true, the string contains upper-case characters on exit. <br>
    !  - If false, the string contains lower-case characters on exit. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc  :: cStrOut  ! output character string

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        cStrOut = BlankCompressChangeCase(vStr%cStr, ToUpper)
        vStr = cStrOut
    END IF
    
    RETURN

END SUBROUTINE BlankCompressAlterCase

!******************************************************************************

MODULE SUBROUTINE AlterCaseProtect(vStr,nRegion,lPos,rPos,ToUpper)

!** PURPOSE OF THIS ROUTINE:
    !^ To change case of the character string of the FvlStr object according
    !  to flag where only unprotected region(s) are allowed to be changed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
    tIndex,        INTENT(IN)       :: nRegion  !! number of protected regions
    tIndex,        INTENT(IN)       :: lPos(:)  !! positions of the first character of protected regions
    tIndex,        INTENT(IN)       :: rPos(:)  !! positions of the last character of protected regions
    tLogical,      INTENT(IN)       :: ToUpper
    !^ flag indicating whether to change the string's characters to
    !  upper-case characters or not. <br>
    !  - If true, the string contains upper-case characters on exit. <br>
    !  - If false, the string contains lower-case characters on exit. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    
    IF (ALLOCATED(vStr%cStr)) THEN
        CALL ChangeCase(vStr%cStr, nRegion, lPos, rPos, ToUpper)
    END IF
    
    RETURN

END SUBROUTINE AlterCaseProtect

!--------------------------------------------------------------------------------------
!                           EDITING PROCEDURES
!--------------------------------------------------------------------------------------

MODULE SUBROUTINE InsertSubstring_CHS(vStr,Pos,sStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To insert substring into the FvlStr object at a specified position. <br>
    !  If Pos is less than 1, then 1 is used as an insertion point. <br>
    !  If Pos is greater than length of the character string, then
    !  the substring is inserted at the end of the character string. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
    tIndex,        INTENT(IN)       :: Pos      !! the insertion point
    tCharStar,     INTENT(IN)       :: sStr     !! substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc      :: cStrOut

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        cStrOut = InsertSubstring(vStr%cStr, Pos, sStr)
        Vstr = cStrOut
    END IF

    RETURN

END SUBROUTINE InsertSubstring_CHS

!******************************************************************************

MODULE SUBROUTINE InsertSubstring_VLS(vStr,Pos,sStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To insert substring into the FvlStr object at a specified position. <br>
    !  If Pos is less than 1, then 1 is used as an insertion point. <br>
    !  If Pos is greater than length of the character string, then
    !  the substring is inserted at the end of the character string. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
    tIndex,        INTENT(IN)       :: Pos      !! the insertion point
    TYPE(FvlStr),  INTENT(IN)       :: sStr     !! substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(sStr%cStr)) THEN
        CALL vStr%InsertSubstring(Pos, sStr%cStr)
    END IF

    RETURN

END SUBROUTINE InsertSubstring_VLS

!******************************************************************************

MODULE SUBROUTINE RemoveCharacters_CHS(vStr,ChrSet,Option,Protect,ExclMrk)

!** PURPOSE OF THIS ROUTINE:
    !^ To remove characters from the FvlStr object depending on the specified
    !  Option and ChrSet. <br>
    !  If Protect is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are removed. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions"
    !  procedure in the "SubClass_FvlStr_Inquiry" submodule. <br>
    !  See explanations of the *ChrSet* and *Option* arguments in the "RemoveCharacters"
    !  procedure in the "SubBase_ChrStr_Manipulation" submodule of the "ModBase_ChrStr"
    !  module. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)      :: ChrSet   !! set of characters to be removed
    tInteger, OPTIONAL, INTENT(IN)      :: Option   !! flag indicating how to remove characters
    tLogical, OPTIONAL, INTENT(IN)      :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc      :: cStrOut
    tLogical        :: Protection

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        Protection = FalseVal
        IF (PRESENT(Protect)) Protection = Protect
        IF (Protection) THEN
            cStrOut = RemoveCharactersProtect(vStr%cStr, ChrSet, Option, ExclMrk)
        ELSE
            cStrOut = RemoveCharacters(vStr%cStr, ChrSet, Option)
        END IF
        Vstr = cStrOut
    END IF
    
    RETURN

END SUBROUTINE RemoveCharacters_CHS

!******************************************************************************

MODULE SUBROUTINE RemoveCharacters_VLS(vStr,ChrSet,Option,Protect,ExclMrk)

!** PURPOSE OF THIS ROUTINE:
    !^ To remove characters from the FvlStr object depending on the specified
    !  Option and ChrSet. <br>
    !  If Protect is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are removed. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions"
    !  procedure in the "SubClass_FvlStr_Inquiry" submodule. <br>
    !  See explanations of the *ChrSet* and *Option* arguments in the "RemoveCharacters"
    !  procedure in the "SubBase_ChrStr_Manipulation" submodule of the "ModBase_ChrStr"
    !  module. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)      :: ChrSet   !! set of characters to be removed
    tInteger, OPTIONAL, INTENT(IN)      :: Option   !! flag indicating how to remove characters
    tLogical, OPTIONAL, INTENT(IN)      :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(ChrSet%cStr)) THEN
        CALL vStr%RemoveCharacters(ChrSet%cStr, Option, Protect, ExclMrk)
    END IF

    RETURN

END SUBROUTINE RemoveCharacters_VLS

!******************************************************************************

MODULE SUBROUTINE RemoveSubstring_CHS(vStr,sStr,Protect,ExclMrk,FirstOnly)

!** PURPOSE OF THIS ROUTINE:
    !^ To remove substring from the FvlStr object. <br>
    !  If Protect is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are removed. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions"
    !  procedure in the "SubClass_FvlStr_Inquiry" submodule. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)      :: sStr     !! substring to be removed
    tLogical, OPTIONAL, INTENT(IN)      :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tLogical, OPTIONAL, INTENT(IN)      :: FirstOnly
    !^ flag indicating whether only the first occurrence is removed. <br>
    !  - If true, only the first occurrence is removed. <br>
    !  - If false, all occurrences are removed. <br>
    !  Default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc      :: cStrOut
    tLogical        :: Protection

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        Protection = FalseVal
        IF (PRESENT(Protect)) Protection = Protect
        IF (Protection) THEN
            cStrOut = RemoveSubstringProtect(vStr%cStr, sStr, ExclMrk, FirstOnly)
        ELSE
            cStrOut = RemoveSubstring(vStr%cStr, sStr, FirstOnly)
        END IF
        Vstr = cStrOut
    END IF

    RETURN

END SUBROUTINE RemoveSubstring_CHS

!******************************************************************************

MODULE SUBROUTINE RemoveSubstring_VLS(vStr,sStr,Protect,ExclMrk,FirstOnly)

!** PURPOSE OF THIS ROUTINE:
    !^ To remove substring from the FvlStr object. <br>
    !  If Protect is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are removed. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions"
    !  procedure in the "SubClass_FvlStr_Inquiry" submodule. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)      :: sStr     !! substring to be removed
    tLogical, OPTIONAL, INTENT(IN)      :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tLogical, OPTIONAL, INTENT(IN)      :: FirstOnly
    !^ flag indicating whether only the first occurrence is removed. <br>
    !  - If true, only the first occurrence is removed. <br>
    !  - If false, all occurrences are removed. <br>
    !  Default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(sStr%cStr)) THEN
        CALL vStr%RemoveSubstring(sStr%cStr, Protect, ExclMrk, FirstOnly)
    END IF

    RETURN

END SUBROUTINE RemoveSubstring_VLS

!******************************************************************************

MODULE SUBROUTINE Delete_Substring(vStr,lPos,rPos)

!** PURPOSE OF THIS ROUTINE:
    !^ To remove substring from the FvlStr object at specified positions. <br>
    !  If lPos is less than 1, then 1 is used as a starting point of the substring. <br>
    !  Similarly, if rPos is greater than length of the FvlStr's string, then
    !  the length is used as a starting point. <br>
    !  If rPos is less than lPos, the same FvlStr object is returned. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object
    tIndex,        INTENT(IN)       :: lPos     !! the leftmost character position of the substring
    tIndex,        INTENT(IN)       :: rPos     !! the rightmost character position of the substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc   :: cStrOut

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        cStrOut = RemoveSubstring(vStr%cStr, lPos, rPos)
        Vstr = cStrOut
    END IF

    RETURN

END SUBROUTINE Delete_Substring

!******************************************************************************

MODULE SUBROUTINE CropBlanks_VLS(vStr,SpaceOnly)

!** PURPOSE OF THIS ROUTINE:
    !^ To remove leading and trailing blanks from the FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    tLogical, OPTIONAL, INTENT(IN)      :: SpaceOnly
    !^ flag indicating whether to only remove the space character or not. <br>
    ! - True if requesting to remove only the space character. <br>
    ! - False if requesting to remove both the tab and the space characters. <br>
    ! Default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        vStr%cStr = CropBlanks(vStr%cStr, SpaceOnly)
    END IF

    RETURN

END SUBROUTINE CropBlanks_VLS

!******************************************************************************

MODULE SUBROUTINE CompactString_VLS(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To convert multiple spaces and tabs into a single space, delete
    !  control characters and removes initial (leading and trailing) spaces.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        vStr%cStr = CompactString(vStr%cStr)
    END IF

    RETURN

END SUBROUTINE CompactString_VLS

!******************************************************************************

MODULE SUBROUTINE CompressString_VLS(vStr)

!** PURPOSE OF THIS ROUTINE:
    !^ To remove spaces, tabs and control characters from the string
    !  of the specified FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(INOUT)    :: vStr     !! FvlStr object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        vStr%cStr = CompressString(vStr%cStr)
    END IF

    RETURN

END SUBROUTINE CompressString_VLS

!******************************************************************************

MODULE SUBROUTINE ReplaceSubstring_CHS_CHS(vStr,oStr,nStr,Protect,Recur,ExclMrk)

!** PURPOSE OF THIS ROUTINE:
    !^ To replace (all) occurrences of the original substring found in the FvlStr
    !  object with the new substring. <br>
    !  Optional inputs (Protect, Recur and ExclMrk) can change how the substring
    !  are replaced. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)      :: oStr     !! original (old) substring
    tCharStar,          INTENT(IN)      :: nStr     !! new substring
    tLogical, OPTIONAL, INTENT(IN)      :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tLogical, OPTIONAL, INTENT(IN)      :: Recur
    !^ flag indicating how to replace substring(s). <br>
    !  - If true, to replace in a recursive way. <br>
    !  - If true, to replace in a non-recursive way. <br>
    !  Default is false. <br>
    !  For example: set vStr = 'abbbbb', oStr = 'ab', and nStr = 'a' <br>
    !  if Recur = TrueVal,  the returning vStr = 'ab' <br>
    !  if Recur = FalseVal, the returning vStr = 'abbbb' <br>
    !  *Important Note*: If *Protect* is present and set to true,
    !       the substring will be replaced in a non-recursive way
    !       regardless of the present and value of Recur. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharAlloc      :: cStrOut
    tLogical        :: Protection
    tLogical        :: Recursion

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        Protection = FalseVal
        Recursion  = FalseVal
        IF (PRESENT(Protect)) Protection = Protect
        IF (PRESENT(Recur))   Recursion  = Recur
        IF (Protection) THEN
            cStrOut = ReplaceSubstringProtect(vStr%cStr, oStr, nStr, ExclMrk)
        ELSE
            IF (Recursion) THEN
                cStrOut = ReplaceSubstringRecursive(vStr%cStr, oStr, nStr)
            ELSE
                cStrOut = ReplaceSubstring(vStr%cStr, oStr, nStr)
            END IF
        END IF
        Vstr = cStrOut
    END IF

    RETURN

END SUBROUTINE ReplaceSubstring_CHS_CHS

!******************************************************************************

MODULE SUBROUTINE ReplaceSubstring_VLS_CHS(vStr,oStr,nStr,Protect,Recur,ExclMrk)

!** PURPOSE OF THIS ROUTINE:
    !^ To replace (all) occurrences of the original substring found in the FvlStr
    !  object with the new substring. <br>
    !  Optional inputs (Protect, Recur and ExclMrk) can change how the substring
    !  are replaced. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)      :: oStr     !! original (old) substring
    tCharStar,          INTENT(IN)      :: nStr     !! new substring
    tLogical, OPTIONAL, INTENT(IN)      :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tLogical, OPTIONAL, INTENT(IN)      :: Recur
    !^ flag indicating how to replace substring(s). <br>
    !  - If true, to replace in a recursive way. <br>
    !  - If true, to replace in a non-recursive way. <br>
    !  Default is false. <br>
    !  For example: set vStr = 'abbbbb', oStr = 'ab', and nStr = 'a' <br>
    !  if Recur = TrueVal,  the returning vStr = 'ab' <br>
    !  if Recur = FalseVal, the returning vStr = 'abbbb' <br>
    !  *Important Note*: If *Protect* is present and set to true,
    !       the substring will be replaced in a non-recursive way
    !       regardless of the present and value of Recur. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    !  na

!** FLOW
    
    IF (ALLOCATED(oStr%cStr)) THEN
        CALL vStr%ReplaceSubstring(oStr%cStr, nStr, Protect, Recur, ExclMrk)
    END IF

    RETURN

END SUBROUTINE ReplaceSubstring_VLS_CHS

!******************************************************************************

MODULE SUBROUTINE ReplaceSubstring_CHS_VLS(vStr,oStr,nStr,Protect,Recur,ExclMrk)

!** PURPOSE OF THIS ROUTINE:
    !^ To replace (all) occurrences of the original substring found in the FvlStr
    !  object with the new substring. <br>
    !  Optional inputs (Protect, Recur and ExclMrk) can change how the substring
    !  are replaced. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)      :: oStr     !! original (old) substring
    TYPE(FvlStr),       INTENT(IN)      :: nStr     !! new substring
    tLogical, OPTIONAL, INTENT(IN)      :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tLogical, OPTIONAL, INTENT(IN)      :: Recur
    !^ flag indicating how to replace substring(s). <br>
    !  - If true, to replace in a recursive way. <br>
    !  - If true, to replace in a non-recursive way. <br>
    !  Default is false. <br>
    !  For example: set vStr = 'abbbbb', oStr = 'ab', and nStr = 'a' <br>
    !  if Recur = TrueVal,  the returning vStr = 'ab' <br>
    !  if Recur = FalseVal, the returning vStr = 'abbbb' <br>
    !  *Important Note*: If *Protect* is present and set to true,
    !       the substring will be replaced in a non-recursive way
    !       regardless of the present and value of Recur. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    !  na

!** FLOW
    
    IF (ALLOCATED(nStr%cStr)) THEN
        CALL vStr%ReplaceSubstring(oStr, nStr%cStr, Protect, Recur, ExclMrk)
    END IF

    RETURN

END SUBROUTINE ReplaceSubstring_CHS_VLS

!******************************************************************************

MODULE SUBROUTINE ReplaceSubstring_VLS_VLS(vStr,oStr,nStr,Protect,Recur,ExclMrk)

!** PURPOSE OF THIS ROUTINE:
    !^ To replace (all) occurrences of the original substring found in the FvlStr
    !  object with the new substring. <br>
    !  Optional inputs (Protect, Recur and ExclMrk) can change how the substring
    !  are replaced. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(INOUT)   :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)      :: oStr     !! original (old) substring
    TYPE(FvlStr),       INTENT(IN)      :: nStr     !! new substring
    tLogical, OPTIONAL, INTENT(IN)      :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tLogical, OPTIONAL, INTENT(IN)      :: Recur
    !^ flag indicating how to replace substring(s). <br>
    !  - If true, to replace in a recursive way. <br>
    !  - If true, to replace in a non-recursive way. <br>
    !  Default is false. <br>
    !  For example: set vStr = 'abbbbb', oStr = 'ab', and nStr = 'a' <br>
    !  if Recur = TrueVal,  the returning vStr = 'ab' <br>
    !  if Recur = FalseVal, the returning vStr = 'abbbb' <br>
    !  *Important Note*: If *Protect* is present and set to true,
    !       the substring will be replaced in a non-recursive way
    !       regardless of the present and value of Recur. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    !  na

!** FLOW
    
    IF (ALLOCATED(nStr%cStr)) THEN
        CALL vStr%ReplaceSubstring(oStr, nStr%cStr, Protect, Recur, ExclMrk)
    END IF

    RETURN

END SUBROUTINE ReplaceSubstring_VLS_VLS

!******************************************************************************

MODULE SUBROUTINE PartitionSepSub_CHS(vStr,SepSub,sStr,Back)

!** PURPOSE OF THIS ROUTINE:
    !^ To partition a string into two substrings where the specified separator
    !  is a multiple-character string.  The partition occurs at the first
    !  occurrence of the separator found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)      :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)      :: SepSub   !! multiple-character separator
    TYPE(FvlStr),       INTENT(OUT)     :: sStr(2)
    !^ substrings where sStr(1) is a substring before
    !  the separator found and sStr(2) is the one after.
    tLogical, OPTIONAL, INTENT(IN)      :: Back
    !^ If present and true, searching from the back;
    ! otherwise, searching from the front.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        CALL Partition(vStr%cStr, SepSub, sStr(1)%cStr, sStr(2)%cStr, Back)
    ELSE
        sStr(1) = ''
        sStr(2) = ''
    END IF
        
    RETURN

END SUBROUTINE PartitionSepSub_CHS

!******************************************************************************

MODULE SUBROUTINE PartitionSepSub_VLS(vStr,SepSub,sStr,Back)

!** PURPOSE OF THIS ROUTINE:
    !^ To partition a string into two substrings where the specified separator
    !  is a multiple-character string.  The partition occurs at the first
    !  occurrence of the separator found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)      :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)      :: SepSub   !! multiple-character separator
    TYPE(FvlStr),       INTENT(OUT)     :: sStr(2)
    !^ substrings where sStr(1) is a substring before
    !  the separator found and sStr(2) is the one after.
    tLogical, OPTIONAL, INTENT(IN)      :: Back
    !^ If present and true, searching from the back;
    ! otherwise, searching from the front.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(0)     :: Dummy

!** FLOW
    
    IF (ALLOCATED(SepSub%cStr)) THEN
        CALL vStr%Partition(SepSub%cStr, sStr, Back)
    ELSE
        CALL vStr%Partition(Dummy, sStr, Back)
    END IF
        
    RETURN

END SUBROUTINE PartitionSepSub_VLS

!******************************************************************************

MODULE SUBROUTINE PartitionSepChr_CHS(vStr,SepSet,sStr,SepChr,Back)

!** PURPOSE OF THIS ROUTINE:
    !^ To partition a string into two substrings where the separator is a single
    !  character (any character in the specified set of characters).  The partition
    !  occurs at the first occurrence of the separator found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)      :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)      :: SepSet
    !^ set of characters representing valid separators
    TYPE(FvlStr),       INTENT(OUT)     :: sStr(2)
    !^ substrings where sStr(1) is a substring before
    ! the separator found and sStr(2) is the one after.
    tChar,              INTENT(OUT)     :: SepChr   !! the separator found
    tLogical, OPTIONAL, INTENT(IN)      :: Back
    !^ If present and true, searching from the back;
    ! otherwise, searching from the front.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        CALL Partition(vStr%cStr, SepSet, sStr(1)%cStr, sStr(2)%cStr, SepChr,Back)
    ELSE
        sStr(1) = ''
        sStr(2) = ''
        SepChr = ''
    END IF
        
    RETURN

END SUBROUTINE PartitionSepChr_CHS

!******************************************************************************

MODULE SUBROUTINE PartitionSepChr_VLS(vStr,SepSet,sStr,SepChr,Back)

!** PURPOSE OF THIS ROUTINE:
    !^ To partition a string into two substrings where the separator is a single
    !  character (any character in the specified set of characters).  The partition
    !  occurs at the first occurrence of the separator found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)      :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)      :: SepSet
    !^ set of characters representing valid separators
    TYPE(FvlStr),       INTENT(OUT)     :: sStr(2)
    !^ substrings where sStr(1) is a substring before
    ! the separator found and sStr(2) is the one after.
    tChar,              INTENT(OUT)     :: SepChr   !! the separator found
    tLogical, OPTIONAL, INTENT(IN)      :: Back
    !^ If present and true, searching from the back;
    ! otherwise, searching from the front.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(0)     :: Dummy

!** FLOW
    
    IF (ALLOCATED(SepSet%cStr)) THEN
        CALL vStr%Partition(SepSet%cStr, sStr, SepChr, Back)
    ELSE
        CALL vStr%Partition(Dummy, sStr, SepChr, Back)
    END IF
        
    RETURN

END SUBROUTINE PartitionSepChr_VLS

!******************************************************************************

MODULE FUNCTION SplitSepSub_CHS(vStr,SepSub,sStr,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To split a string of the FvlStr object into multiple substrings where the
    !  specified separator is a multiple-character string.  The number of substrings
    !  is equal to the number of occurrences of the separator found plus one.  The
    !  substrings may be a zero-length string if the separator is found at the
    !  beginning or at the end of the given string. <br>
    !  If Protect is present and its value is true, only those occurrences found in
    !  the unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions"
    !  procedure in the "SubClass_FvlStr_Inquiry" submodule. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),             INTENT(IN)   :: vStr     !! FvlStr object
    tCharStar,                 INTENT(IN)   :: SepSub   !! multiple-character separator
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)  :: sStr(:)  !! output substrings
    tLogical,        OPTIONAL, INTENT(IN)   :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,        OPTIONAL, INTENT(IN)   :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                                  :: nCount   !! number of occurrences of separator found

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: CharSet = FalseVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex, ALLOCATABLE :: sPos(:)      ! position(s) of the delimiter(s) found
    tIndex              :: I            ! index
    tIndex              :: cLen         ! length of the input string
    tIndex              :: sLen         ! length of separator

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        ! first, find number of occurrences of the separator found
        nCount = vStr%FindSeparators(SepSub, CharSet, sPos, Protect, ExclMrk)
        ! allocate the substrings and set output values
        ALLOCATE(sStr(nCount+1))
        IF (nCount > 0) THEN
            cLen = vStr%Length()
            sLen = LEN(SepSub)
            DO I = 1, nCount + 1
                IF (I == 1) THEN
                    sStr(I) = vStr%vSubStr(1,sPos(I)-1)
                ELSEIF (I == nCount + 1) THEN
                    sStr(I) = vStr%vSubStr(sPos(I-1)+sLen,cLen)
                ELSE
                    sStr(I) = vStr%vSubStr(sPos(I-1)+sLen,sPos(I)-1)
                END IF
            END DO
        ELSE
            sStr(1) = ''
        END IF
    ELSE
        ALLOCATE(sStr(1))
        sStr(1) = ''
    END IF
        
    RETURN

END FUNCTION SplitSepSub_CHS

!******************************************************************************

MODULE FUNCTION SplitSepSub_VLS(vStr,SepSub,sStr,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To split a string of the FvlStr object into multiple substrings where the
    !  specified separator is a multiple-character string.  The number of substrings
    !  is equal to the number of occurrences of the separator found plus one.  The
    !  substrings may be a zero-length string if the separator is found at the
    !  beginning or at the end of the given string. <br>
    !  If Protect is present and its value is true, only those occurrences found in
    !  the unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions"
    !  procedure in the "SubClass_FvlStr_Inquiry" submodule. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),             INTENT(IN)   :: vStr     !! FvlStr object
    TYPE(FvlStr),              INTENT(IN)   :: SepSub   !! multiple-character separator
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)  :: sStr(:)  !! output substrings
    tLogical,        OPTIONAL, INTENT(IN)   :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,        OPTIONAL, INTENT(IN)   :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                                  :: nCount   !! number of occurrences of separator found

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: CharSet = FalseVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(0)     :: Dummy

!** FLOW
    
    IF (ALLOCATED(SepSub%cStr)) THEN
        nCount = vStr%Split(SepSub%cStr, sStr, Protect, ExclMrk)
    ELSE
        nCount = vStr%Split(Dummy, sStr, Protect, ExclMrk)
    END IF
        
    RETURN

END FUNCTION SplitSepSub_VLS

!******************************************************************************

MODULE FUNCTION SplitSepChr_CHS(vStr,SepSet,sStr,SepChr,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To split a string of the FvlStr object into multiple substrings where a
    !  separator is a single character (any character in the specified set of
    !  characters).  The number of substrings is equal to the number of occurrences
    !  of the separator found plus one.  The substrings may be a zero-length string
    !  if the separator is found at the beginning or at the end of the given string. <br>
    !  If Protect is present and its value is true, only those occurrences found in
    !  the unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions"
    !  procedure in the "SubClass_FvlStr_Inquiry" submodule. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),             INTENT(IN)   :: vStr     !! FvlStr object
    tCharStar,                 INTENT(IN)   :: SepSet   !! set of characters representing valid separators
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)  :: sStr(:)  !! output substrings
    tChar,        ALLOCATABLE, INTENT(OUT)  :: SepChr(:)!! the separators found
    tLogical,        OPTIONAL, INTENT(IN)   :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,        OPTIONAL, INTENT(IN)   :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                                  :: nCount   !! number of occurrences of separators found

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: CharSet = TrueVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex, ALLOCATABLE :: sPos(:)      ! position(s) of the delimiter(s) found
    tIndex              :: I            ! index
    tIndex              :: cLen         ! length of the input string

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        ! first, find number of occurrences of the separator found
        nCount = vStr%FindSeparators(SepSet, CharSet, sPos, Protect, ExclMrk)
        ! allocate the substrings and set output values
        ALLOCATE(sStr(nCount+1))
        ALLOCATE(SepChr(nCount))
        IF (nCount > 0) THEN
            cLen = vStr%Length()
            DO I = 1, nCount + 1
                IF (I == 1) THEN
                    sStr(I) = vStr%vSubStr(1,sPos(I)-1)
                    SepChr(I) = vStr%Char(sPos(I))
                ELSEIF (I == nCount + 1) THEN
                    sStr(I) = vStr%vSubStr(sPos(I-1)+1,cLen)
                ELSE
                    sStr(I) = vStr%vSubStr(sPos(I-1)+1,sPos(I)-1)
                    SepChr(I) = vStr%Char(sPos(I))
                END IF
            END DO
        ELSE
            sStr(1) = ''
        END IF
    ELSE
        ALLOCATE(sStr(1))
        ALLOCATE(SepChr(0))
        sStr(1) = ''
    END IF
        
    RETURN

END FUNCTION SplitSepChr_CHS

!******************************************************************************

MODULE FUNCTION SplitSepChr_VLS(vStr,SepSet,sStr,SepChr,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To split a string of the FvlStr object into multiple substrings where a
    !  separator is a single character (any character in the specified set of
    !  characters).  The number of substrings is equal to the number of occurrences
    !  of the separator found plus one.  The substrings may be a zero-length string
    !  if the separator is found at the beginning or at the end of the given string. <br>
    !  If Protect is present and its value is true, only those occurrences found in
    !  the unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions"
    !  procedure in the "SubClass_FvlStr_Inquiry" submodule. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),             INTENT(IN)   :: vStr     !! FvlStr object
    TYPE(FvlStr),              INTENT(IN)   :: SepSet   !! set of characters representing valid separators
    TYPE(FvlStr), ALLOCATABLE, INTENT(OUT)  :: sStr(:)  !! output substrings
    tChar,        ALLOCATABLE, INTENT(OUT)  :: SepChr(:)!! the separators found
    tLogical,        OPTIONAL, INTENT(IN)   :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,        OPTIONAL, INTENT(IN)   :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                                  :: nCount   !! number of occurrences of separators found

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: CharSet = TrueVal
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(0)     :: Dummy

!** FLOW
    
    IF (ALLOCATED(SepSet%cStr)) THEN
        nCount = vStr%Split(SepSet%cStr, sStr, SepChr, Protect, ExclMrk)
    ELSE
        nCount = vStr%Split(Dummy, sStr, SepChr, Protect, ExclMrk)
    END IF
        
    RETURN

END FUNCTION SplitSepChr_VLS

!******************************************************************************

END SUBMODULE SubClass_FvlStr_Manipulation

!******************************************************************************
