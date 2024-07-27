
SUBMODULE (Class_FvlStr) SubClass_FvlStr_Inquiry

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *inquiry* procedures (i.e. routines
!   that inquire information relating to the character string of a FvlStr object).

!** USE STATEMENTS:
    USE ModBase_ChrStr

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

MODULE FUNCTION Is_FvlStr_Number(vStr,Strict,NumVal) RESULT(NumFlag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether a FvlStr object represents a valid number and
    !  if so, what kind of number it is.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),         INTENT(IN)   :: vStr     !! FvlStr object
    tLogical,              INTENT(IN)   :: Strict
    !^ Flag requesting for strict integer/real number. <br>
    ! If true, NumFlag is 1 or 2 if the FvlStr object is a valid integer or real number. <br>
    ! Otherwise, NumFlag is 0 if the FvlStr object is a valid integer or real number. <br>
    ! Default is false. <br>
    CLASS(*), ALLOCATABLE, INTENT(OUT)  :: NumVal   !! Value of number if it is valid.
    OPTIONAL                            :: Strict, NumVal
    tInteger                            :: NumFlag
    !^ Flag indicating what kind of number the FvlStr object represents. <br>
    ! NumFlag = -1, the string is NOT a number. <br>
    ! NumFlag =  0, the string is a valid integer or real number. <br>
    ! NumFlag =  1, the string is strictly an integer number. <br>
    ! NumFlag =  2, the string is strictly a real number. <br>
    ! NumFlag =  3, the string is a valid complex number. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    NumFlag = -1
    IF (ALLOCATED(vStr%cStr)) THEN
        NumFlag = IsStringNumber(vStr%cStr, Strict, NumVal)
    END IF

    RETURN

END FUNCTION Is_FvlStr_Number

!******************************************************************************

MODULE FUNCTION Is_FvlStr_Logical(vStr,Boolean) RESULT(LogFlag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether a FvlStr object represents a logical value where valid
    !  string include 'T', 'F', 't', 'f', 'TRUE', 'FALSE', 'true', and 'false'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
    tLogical, OPTIONAL, INTENT(OUT) :: Boolean  !! logical value if flag is true; otherwise, set to FalseVal
    tLogical                        :: LogFlag  !! true if FvlStr object is a logical value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    LogFlag = FalseVal
    IF (PRESENT(Boolean)) Boolean = FalseVal
    IF (ALLOCATED(vStr%cStr)) LogFlag = IsStringLogical(vStr%cStr, Boolean)

    RETURN

END FUNCTION Is_FvlStr_Logical

!******************************************************************************

MODULE FUNCTION Is_FvlStr_InClass(vStr,ClassType,FailIndex) RESULT(ClassFlag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether a FvlStr object is in the specified class. <br>
    ! The following FvlStr classes are recognized: <br>
    !   ALPHABET, ALPHANUM, ASCII, BLANK, COMPLEX, CONTROL, DIGIT, FNAME,
    !   GRAPHICAL, INTEGER, LOGICAL, LOWERCASE, PUNCTUATION, PRINTABLE,
    !   REAL, UPPERCASE, WHITESPACE, HEXDIGIT, OCTDIGIT. <br>
    ! See explanations of classes in the "IsStringInClass" and
    !   "IsCharacterInClass" routines in the "ModBase_ChrStr" module. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),    INTENT(IN)    :: vStr         !! FvlStr object
    tCharStar,        INTENT(IN)    :: ClassType    !! FvlStr class
    tIndex, OPTIONAL, INTENT(OUT)   :: FailIndex    !! flag indicating position of the failed character
    tLogical                        :: ClassFlag    !! true if FvlStr object is in the specified class

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        ClassFlag = IsStringInClass(vStr%cStr, ClassType, FailIndex)
    ELSE
        ! set flag and fail index
        ClassFlag = FalseVal
        IF (PRESENT(FailIndex)) FailIndex = ID_NOT_ALLOCATED
        CALL Handle_ErrLevel('Is_FvlStr_InClass', ModName, ErrWarning, &
                   'The string has not yet been allocated.')
        
    END IF
    
    RETURN

END FUNCTION Is_FvlStr_InClass

!******************************************************************************

MODULE FUNCTION CountSubstring_CHS(vStr,sStr,Overlap) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of occurrences of the specified substring in
    !  the given FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)  :: sStr     !! substring
    tLogical, OPTIONAL, INTENT(IN)  :: Overlap
    !^ flag indicating whether overlapping occurrences of the substring
    !  are allowed or not. <br>
    !  - If true, count the overlapping occurrences. <br>
    !  - If false, count the non-overlapping occurrences . <br>
    !  Default is false.
    tIndex                          :: nCount   !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        nCount = CountSubstring(vStr%cStr, sStr, Overlap)
    ELSE
        ! set output
        nCount = 0
    END IF
    
    RETURN

END FUNCTION CountSubstring_CHS

!******************************************************************************

MODULE FUNCTION CountSubstring_VLS(vStr,sStr,Overlap) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of occurrences of the specified substring in
    !  the given FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)  :: sStr     !! substring
    tLogical, OPTIONAL, INTENT(IN)  :: Overlap
    !^ flag indicating whether overlapping occurrences of the substring
    !  are allowed or not. <br>
    !  - If true, count the overlapping occurrences. <br>
    !  - If false, count the non-overlapping occurrences . <br>
    !  Default is false.
    tIndex                          :: nCount   !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr).AND.ALLOCATED(sStr%cStr)) THEN
        nCount = CountSubstring(vStr%cStr, sStr%cStr, Overlap)
    ELSE
        ! set output
        nCount = 0
    END IF
    
    RETURN

END FUNCTION CountSubstring_VLS

!******************************************************************************

MODULE FUNCTION CountCharacters_CHS(vStr,ChrSet,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of occurrences of character(s) in the FvlStr object
    !  for any character appearing in the given character set.  Optionally, a
    !  user can specify flags indicating whether protected regions exist or not
    !  and whether the exclamation mark is used to protect regions.  If protected
    !  regions exist, only characters in unprotected regions are counted. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
    tCharStar,          INTENT(IN)  :: ChrSet   !! Set of characters
    tLogical, OPTIONAL, INTENT(IN)  :: Protect
    !^ Protect region flag. <br>
    ! - True if protected regions exists. <br>
    ! - False if protected regions do not exist. <br>
    ! Default is false.
    tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
    !^ Exclamation mark flag. <br>
    ! - True if exclamation mark is also used to protect regions. <br>
    ! - False if only (single and/or double) quotes are used to protect regions. <br>
    ! Default is true.
    tIndex                          :: nCount   !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical  :: Protection

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        Protection = FalseVal
        IF (PRESENT(Protect)) Protection = Protect
        IF (Protection) THEN
            nCount = CountCharactersProtect(vStr%cStr, ChrSet, ExclMrk)
        ELSE
            nCount = CountCharacters(vStr%cStr, ChrSet)
        END IF
    ELSE
        ! set output
        nCount = 0
    END IF

    RETURN

END FUNCTION CountCharacters_CHS

!******************************************************************************

MODULE FUNCTION CountCharacters_VLS(vStr,ChrSet,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of occurrences of character(s) in the FvlStr object
    !  for any character appearing in the given character set.  Optionally, a
    !  user can specify flags indicating whether protected regions exist or not
    !  and whether the exclamation mark is used to protect regions.  If protected
    !  regions exist, only characters in unprotected regions are counted. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),      INTENT(IN)  :: vStr     !! FvlStr object
    TYPE(FvlStr),       INTENT(IN)  :: ChrSet   !! Set of characters
    tLogical, OPTIONAL, INTENT(IN)  :: Protect
    !^ Protect region flag. <br>
    ! - True if protected regions exists. <br>
    ! - False if protected regions do not exist. <br>
    ! Default is false.
    tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
    !^ Exclamation mark flag. <br>
    ! - True if exclamation mark is also used to protect regions. <br>
    ! - False if only (single and/or double) quotes are used to protect regions. <br>
    ! Default is true.
    tIndex                          :: nCount   !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(ChrSet%cStr)) THEN
        nCount = vStr%CountCharacters(ChrSet%cStr, Protect, ExclMrk)
    ELSE
        ! set output
        nCount = 0
    END IF

    RETURN

END FUNCTION CountCharacters_VLS

!******************************************************************************

MODULE FUNCTION CountWords_VLS(vStr) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of words (separated by blanks) in the FvlStr object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    tIndex                      :: nCount   !! number of words

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        nCount = CountWords(vStr%cStr)
    ELSE
        ! set output
        nCount = 0
    END IF

    RETURN

END FUNCTION CountWords_VLS

!******************************************************************************

MODULE FUNCTION FindProtectedRegions_VLS(vStr,lPos,rPos,ExclMrk) RESULT(nRegion)

!** PURPOSE OF THIS ROUTINE:
    !^ To look for quotes (and/or an exclamation mark) to find regions
    !  that must be protected from string editing.  Return the number
    !  of protected regions as well as positions of the first and last
    !  characters of each region. <br>
    !  **Technical Notes**: <br>
    !  - Single quote, double quote and optionally exclamation mark are used as
    !    delimiters to find protected regions. <br>
    !  - Two single quotes or two double quotes are used to define a protected
    !    region whereas an exclamation mark indicates that all characters
    !    following it are all protected. <br>
    !  - This routine is designed specifically for manipulating Fortran source code
    !    where an exclamation mark is used for a comment and two (single or double)
    !    quotes are used to specify a value to a character variable or literal.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
    tIndex, ALLOCATABLE, INTENT(OUT)    :: lPos(:)
    !^ positions of the first character of protected regions
    tIndex, ALLOCATABLE, INTENT(OUT)    :: rPos(:)
    !^ positions of the last character of protected regions
    tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                              :: nRegion  !! number of protected regions

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        nRegion = FindProtectedRegions(vStr%cStr, lPos, rPos, ExclMrk)
    ELSE
        ! set output
        nRegion = 0
    END IF
    
    RETURN

END FUNCTION FindProtectedRegions_VLS

!******************************************************************************

MODULE FUNCTION FindSubstring_CHS(vStr,sStr,sPos,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of non-overlapping occurrences of substring in the FvlStr object
    !  and also return position(s) of the first character of substring found. <br>
    !  If *Protect* is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
    tCharStar,           INTENT(IN)     :: sStr     !! substring
    tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)
    !^ position(s) of the first character of substring found
    tLogical,  OPTIONAL, INTENT(IN)     :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                              :: nCount   !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Protection

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        Protection = FalseVal
        IF (PRESENT(Protect)) Protection = Protect
        IF (Protection) THEN
            nCount = FindSubstringProtect(vStr%cStr, sStr, sPos, ExclMrk)
        ELSE
            nCount = FindSubstring(vStr%cStr, sStr, sPos)
        END IF
    ELSE
        ! set output
        nCount = 0
    END IF

    RETURN

END FUNCTION FindSubstring_CHS

!******************************************************************************

MODULE FUNCTION FindSubstring_VLS(vStr,sStr,sPos,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of non-overlapping occurrences of substring in the FvlStr object
    !  and also return position(s) of the first character of substring found. <br>
    !  If *Protect* is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
    TYPE(FvlStr),        INTENT(IN)     :: sStr     !! substring
    tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)
    !^ position(s) of the first character of substring found
    tLogical,  OPTIONAL, INTENT(IN)     :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                              :: nCount   !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(sStr%cStr)) THEN
        nCount = vStr%FindSubstring(sStr%cStr, sPos, Protect, ExclMrk)
    ELSE
        ! set output
        nCount = 0
    END IF

    RETURN

END FUNCTION FindSubstring_VLS

!******************************************************************************

MODULE FUNCTION FindDelimiters_CHS(vStr,ChrSet,dPos,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of occurrences of delimiter(s) in the FvlStr object
    !  and also return position(s) of the delimiter(s) found. <br>
    !  A delimiter is any character appearing in the given character set. <br>
    !  If Protect is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
    tCharStar,           INTENT(IN)     :: ChrSet   !! a set of characters
    tIndex, ALLOCATABLE, INTENT(OUT)    :: dPos(:)  !! position(s) of the delimiter(s) found
    tLogical,  OPTIONAL, INTENT(IN)     :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                              :: nCount   !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Protection

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        Protection = FalseVal
        IF (PRESENT(Protect)) Protection = Protect
        IF (Protection) THEN
            nCount = FindDelimitersProtect(vStr%cStr, ChrSet, dPos, ExclMrk)
        ELSE
            nCount = FindDelimiters(vStr%cStr, ChrSet, dPos)
        END IF
    ELSE
        ! set output
        nCount = 0
    END IF
    
    RETURN

END FUNCTION FindDelimiters_CHS

!******************************************************************************

MODULE FUNCTION FindDelimiters_VLS(vStr,ChrSet,dPos,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of occurrences of delimiter(s) in the FvlStr object
    !  and also return position(s) of the delimiter(s) found. <br>
    !  A delimiter is any character appearing in the given character set. <br>
    !  If Protect is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),       INTENT(IN)     :: vStr     !! FvlStr object
    TYPE(FvlStr),        INTENT(IN)     :: ChrSet   !! a set of characters
    tIndex, ALLOCATABLE, INTENT(OUT)    :: dPos(:)  !! position(s) of the delimiter(s) found
    tLogical,  OPTIONAL, INTENT(IN)     :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                              :: nCount   !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (ALLOCATED(ChrSet%cStr)) THEN
        nCount = vStr%FindDelimiters(ChrSet%cStr, dPos, Protect, ExclMrk)
    ELSE
        ! set output
        nCount = 0
    END IF
    
    RETURN

END FUNCTION FindDelimiters_VLS

!******************************************************************************

MODULE FUNCTION FindSeparators_CHS(vStr,Separator,CharSet,sPos,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of occurrences of separator(s) in the FvlStr object
    !  and also return position(s) of the separator(s) found. <br>
    !  A separator can be a (single) character or a character string (multiple characters). <br>
    !  The argument "CharSet" is a flag used to specify whether the separator is a character
    !  or a character string. If it is true, the argument "Separator" contains a set of
    !  characters where a separator is any character in the set.  If it is false,
    !  the argument "Separator" specifies the character-string separator. <br>
    !  If Protect is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),       INTENT(IN)     :: vStr         !! FvlStr object
    tCharStar,           INTENT(IN)     :: Separator    !! separator
    tLogical,            INTENT(IN)     :: CharSet      !! a flag indicating type of the separator
    tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)      !! position(s) of the delimiter(s) found
    tLogical,  OPTIONAL, INTENT(IN)     :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                              :: nCount       !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (CharSet) THEN
        ! separator is a character
        nCount = vStr%FindDelimiters(Separator, sPos, Protect, ExclMrk)
    ELSE
        ! separator is a character sring
        nCount = vStr%FindSubstring(Separator, sPos, Protect, ExclMrk)
    END IF
    
    RETURN

END FUNCTION FindSeparators_CHS

!******************************************************************************

MODULE FUNCTION FindSeparators_VLS(vStr,Separator,CharSet,sPos,Protect,ExclMrk) RESULT(nCount)

!** PURPOSE OF THIS ROUTINE:
    !^ To count the number of occurrences of separator(s) in the FvlStr object
    !  and also return position(s) of the separator(s) found. <br>
    !  A separator can be a (single) character or a character string (multiple characters). <br>
    !  The argument "CharSet" is a flag used to specify whether the separator is a character
    !  or a character string. If it is true, the argument "Separator" contains a set of
    !  characters where a separator is any character in the set.  If it is false,
    !  the argument "Separator" specifies the character-string separator. <br>
    !  If Protect is present and its value is true, only those occurrences found in the
    !  unprotected region(s) are counted. <br>
    !  See explanations about the protected region(s) in the "FindProtectedRegions" procedure. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),       INTENT(IN)     :: vStr         !! FvlStr object
    TYPE(FvlStr),        INTENT(IN)     :: Separator    !! separator
    tLogical,            INTENT(IN)     :: CharSet      !! a flag indicating type of the separator
    tIndex, ALLOCATABLE, INTENT(OUT)    :: sPos(:)      !! position(s) of the delimiter(s) found
    tLogical,  OPTIONAL, INTENT(IN)     :: Protect
    !^ flag indicating whether protected regions exist or not. <br>
    ! -> If true, protected regions exist. <br>
    ! -> If false, protected regions do not exist. <br>
    !  Default is false.
    tLogical,  OPTIONAL, INTENT(IN)     :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.
    tIndex                              :: nCount       !! number of occurrences

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    IF (CharSet) THEN
        ! separator is a character
        nCount = vStr%FindDelimiters(Separator, sPos, Protect, ExclMrk)
    ELSE
        ! separator is a character sring
        nCount = vStr%FindSubstring(Separator, sPos, Protect, ExclMrk)
    END IF
    
    RETURN

END FUNCTION FindSeparators_VLS

!******************************************************************************

MODULE FUNCTION GetCharacter(vStr,Pos) RESULT(Chr)

!** PURPOSE OF THIS ROUTINE:
    !^ To get a character from FvlStr object based on the specified position. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    tIndex,        INTENT(IN)   :: Pos      !! the position of the desired character
    tChar                       :: Chr      !! the character desired

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! set default
    Chr = ''
    
    ! get the substring
    IF (ALLOCATED(vStr%cStr)) THEN
        IF (IN_RANGE(Pos, 1, vStr%Length())) Chr = vStr%cStr(Pos:Pos)
    END IF

    RETURN

END FUNCTION GetCharacter

!******************************************************************************

MODULE FUNCTION GetSubstring_CHS(vStr,lPos,rPos) RESULT(cSub)

!** PURPOSE OF THIS ROUTINE:
    !^ To get a specified substring from FvlStr object based on lPos and rPos. <br>
    !  If lPos is less than 1, then 1 is used as a starting point of the substring. <br>
    !  Similarly, if rPos is greater than the length of the FvlStr's string, then
    !  the length is used as an ending point. <br>
    !  If rPos is less than lPos, a zero-length string is returned. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    tIndex,        INTENT(IN)   :: lPos     !! the leftmost character position of the substring
    tIndex,        INTENT(IN)   :: rPos     !! the rightmost character position of the substring
    tCharAlloc                  :: cSub     !! substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: sPos, ePos

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        ! set starting position
        sPos = MAX(1, lPos)
        ! set ending position
        ePos = MIN(LEN(vStr%cStr), rPos)
    
        ! set the output
        IF (sPos <= ePos) THEN
            cSub = vStr%cStr(sPos:ePos)
        ELSE
            cSub = ''
        END IF
    ELSE
        cSub = ''
    END IF

    RETURN

END FUNCTION GetSubstring_CHS

!******************************************************************************

MODULE FUNCTION GetSubstring_VLS(vStr,lPos,rPos) RESULT(vSub)

!** PURPOSE OF THIS ROUTINE:
    !^ To get a specified substring from FvlStr object based on lPos and rPos. <br>
    !  If lPos is less than 1, then 1 is used as a starting point of the substring. <br>
    !  Similarly, if rPos is greater than the length of the FvlStr's string, then
    !  the length is used as an ending point. <br>
    !  If rPos is less than lPos, a zero-length string is returned. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    tIndex,        INTENT(IN)   :: lPos     !! the leftmost character position of the substring
    tIndex,        INTENT(IN)   :: rPos     !! the rightmost character position of the substring
    TYPE(FvlStr)                :: vSub     !! substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    vSub%cStr = vStr%cSubStr(lPos,rPos)

    RETURN

END FUNCTION GetSubstring_VLS

!******************************************************************************

MODULE FUNCTION GetSlice_CHS(vStr,First,Last,Stride) RESULT(Slice)

!** PURPOSE OF THIS ROUTINE:
    !^ To extract the characters from the region between *First* and *Last* indices
    ! (both inclusive) of the given string by taking strides of length *Stride*. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),    INTENT(IN)    :: vStr     !! FvlStr object
    tIndex, OPTIONAL, INTENT(IN)    :: First    !! the first index
    tIndex, OPTIONAL, INTENT(IN)    :: Last     !! the last index
    tIndex, OPTIONAL, INTENT(IN)    :: Stride   !! the stride
    tCharAlloc                      :: Slice    !! character slice

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: sPos, ePos, Step, NStep, cLen, I, J

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        ! get string length
        cLen = LEN(vStr%cStr)
    
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
            IF (PRESENT(FIrst).AND.PRESENT(Last)) THEN
                IF (Last < FIrst) THEN
                    Step = -1
                END IF
            END IF
        END IF

        IF (PRESENT(FIrst)) THEN
            sPos = FIrst
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
            Slice(J:J) = vStr%cStr(I:I)
            J = J + 1
        END DO
    ELSE
        Slice = ''
    END IF

    RETURN

END FUNCTION GetSlice_CHS

!******************************************************************************

MODULE FUNCTION GetSlice_VLS(vStr,First,Last,Stride) RESULT(Slice)

!** PURPOSE OF THIS ROUTINE:
    !^ To extract the characters from the region between *First* and *Last* indices
    ! (both inclusive) of the given string by taking strides of length *Stride*. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr),    INTENT(IN)    :: vStr     !! FvlStr object
    tIndex, OPTIONAL, INTENT(IN)    :: First    !! the first index
    tIndex, OPTIONAL, INTENT(IN)    :: Last     !! the last index
    tIndex, OPTIONAL, INTENT(IN)    :: Stride   !! the stride
    TYPE(FvlStr)                    :: Slice    !! character slice

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Slice%cStr = vStr%cSlice(First,Last,Stride)

    RETURN

END FUNCTION GetSlice_VLS

!******************************************************************************

MODULE FUNCTION StartWith_CHS(vStr,sStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether the string of the given FvlStr object starts with
    !  the specified substring or not. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    tCharStar,     INTENT(IN)   :: sStr     !! substring
    tLogical                    :: Flag     !! true if the string starts with the substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        cLen = LEN(vStr%cStr)
        sLen = LEN(sStr)
        IF ((cLen == 0).OR.(sLen == 0).OR.(cLen < sLen)) THEN
            Flag = FalseVal
        ELSE
            Flag = vStr%cStr(1:sLen) == sStr
        END IF
    ELSE
        Flag = FalseVal
    END IF
    
    RETURN

END FUNCTION StartWith_CHS

!******************************************************************************

MODULE FUNCTION StartWith_VLS(vStr,sStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether the string of the given FvlStr object starts with
    !  the specified substring or not. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    TYPE(FvlStr),  INTENT(IN)   :: sStr     !! substring
    tLogical                    :: Flag     !! true if the string starts with the substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen

!** FLOW
    
    IF (ALLOCATED(vStr%cStr).AND.ALLOCATED(sStr%cStr)) THEN
        cLen = LEN(vStr%cStr)
        sLen = LEN(sStr%cStr)
        IF ((cLen == 0).OR.(sLen == 0).OR.(cLen < sLen)) THEN
            Flag = FalseVal
        ELSE
            Flag = vStr%cStr(1:sLen) == sStr%cStr
        END IF
    ELSE
        Flag = FalseVal
    END IF
    
    RETURN

END FUNCTION StartWith_VLS

!******************************************************************************

MODULE FUNCTION EndWith_CHS(vStr,sStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether the string of the given FvlStr object ends with
    !  the specified substring or not. <br>
    !  Note: both the string and the substring must not have a zero length. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    tCharStar,     INTENT(IN)   :: sStr     !! substring
    tLogical                    :: Flag     !! true if the string ends with the substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen

!** FLOW
    
    IF (ALLOCATED(vStr%cStr)) THEN
        cLen = LEN(vStr%cStr)
        sLen = LEN(sStr)
        IF ((cLen == 0).OR.(sLen == 0).OR.(cLen < sLen)) THEN
            Flag = FalseVal
        ELSE
            Flag = vStr%cStr(cLen-sLen+1:cLen) == sStr
        END IF
    ELSE
        Flag = FalseVal
    END IF
    
    RETURN

END FUNCTION EndWith_CHS

!******************************************************************************

MODULE FUNCTION EndWith_VLS(vStr,sStr) RESULT(Flag)

!** PURPOSE OF THIS ROUTINE:
    !^ To check whether the string of the given FvlStr object ends with
    !  the specified substring or not. <br>
    !  Note: both the string and the substring must not have a zero length. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(FvlStr), INTENT(IN)   :: vStr     !! FvlStr object
    TYPE(FvlStr),  INTENT(IN)   :: sStr     !! substring
    tLogical                    :: Flag     !! true if the string ends with the substring

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen

!** FLOW
    
    IF (ALLOCATED(vStr%cStr).AND.ALLOCATED(sStr%cStr)) THEN
        cLen = LEN(vStr%cStr)
        sLen = LEN(sStr%cStr)
        IF ((cLen == 0).OR.(sLen == 0).OR.(cLen < sLen)) THEN
            Flag = FalseVal
        ELSE
            Flag = vStr%cStr(cLen-sLen+1:cLen) == sStr%cStr
        END IF
    ELSE
        Flag = FalseVal
    END IF
    
    RETURN

END FUNCTION EndWith_VLS

!******************************************************************************

END SUBMODULE SubClass_FvlStr_Inquiry

!******************************************************************************
