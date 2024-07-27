
SUBMODULE (ModBase_ChrStr) SubBase_ChrStr_Manipulation

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *manipulation* procedures (i.e. routines
!   that perform a manipulation on a character string).

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

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

!--------------------------------------------------------------------------------------
!                               CASE PROCEDURES
!--------------------------------------------------------------------------------------

MODULE ELEMENTAL SUBROUTINE ChangeCaseString(cStr,ToUpper)

    ! PURPOSE OF THIS ROUTINE:
    ! To change case of the given character string according to flag.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(INOUT)    :: cStr     ! character string
    tLogical,  INTENT(IN)       :: ToUpper  ! true if requesting an uppercase character
                                            ! false if requesting a lowercase character

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: I, Length

    ! FLOW:
    
    Length = LEN(cStr)
    IF (Length > 0) THEN
        DO I = 1, Length
            CALL ChangeCaseCharacter(cStr(I:I), ToUpper)
        END DO
    END IF
    
    RETURN

END SUBROUTINE ChangeCaseString

!******************************************************************************

MODULE FUNCTION BlankCompressChangeCase(cStrIn,ToUpper) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To first remove all blanks and then change case of the
    ! given character string according to flag.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn   ! input character string
    tLogical,  INTENT(IN)   :: ToUpper  ! true if requesting an uppercase character
                                        ! false if requesting a lowercase character
    tCharAlloc              :: cStrOut  ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! FLOW:
    
    ! first, remove all blanks
    cStrOut = RemoveCharacters(cStrIn, SET_BLANKS, Option=3)
    
    ! then, change case
    CALL ChangeCase(cStrOut, ToUpper)
    
    RETURN

END FUNCTION BlankCompressChangeCase

!******************************************************************************

MODULE SUBROUTINE ChangeCaseProtect(cStr,nRegion,lPos,rPos,ToUpper)

    ! PURPOSE OF THIS ROUTINE:
    ! To change case of the given character string according to flag
    ! where only unprotected region(s) are allowed to be changed.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(INOUT)    :: cStr     ! character string
    tIndex,    INTENT(IN)       :: nRegion  ! number of protected regions
    tIndex,    INTENT(IN)       :: lPos(:)  ! positions of the first character of protected regions
    tIndex,    INTENT(IN)       :: rPos(:)  ! positions of the last character of protected regions
    tLogical,  INTENT(IN)       :: ToUpper  ! true if requesting an uppercase character
                                            ! false if requesting a lowercase character

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Pos, Mark

    ! FLOW:
    
    Length = LEN(cStr)
    
    IF (Length == 0) RETURN     ! nothing to change

    IF (lPos(1) == 1 .AND. rPos(1) == Length) RETURN      ! Entire line protected

    ! initialize
    Pos = 1
    Mark = 1
    
    ! loop over the characters of the given string
    DO
        ! check whether the current character is in the protected region
        IF (nRegion >= Mark .AND. Pos == lPos(Mark)) THEN
            ! in the protected region so set position outside the region
            Pos = rPos(Mark) + 1
            ! set Mark for next protected region
            Mark = Mark + 1
            IF (Pos >= Length) EXIT
        END IF
        ! change case
        CALL ChangeCaseCharacter(cStr(Pos:Pos), ToUpper)
        ! move to next character
        Pos = Pos + 1
        IF (Pos > Length) EXIT
    END DO

    RETURN

END SUBROUTINE ChangeCaseProtect

!--------------------------------------------------------------------------------------
!                                   EDITING PROCEDURES
!--------------------------------------------------------------------------------------

MODULE FUNCTION InsertSubstring(cStrIn,Pos,sStr) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To insert substring into the character string at a specified position.
    ! If Pos is less than 1, then 1 is used as an insertion point.
    ! if Pos is greater than length of the character string, then
    !   the substring is inserted at the end of the character string.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn   ! input character string
    tIndex,    INTENT(IN)   :: Pos      ! the insertion point
    tCharStar, INTENT(IN)   :: sStr     ! substring
    tCharAlloc              :: cStrOut  ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    sLen = LEN(sStr)
    IF (cLen == 0) THEN
        cStrOut = sStr
        CALL Handle_ErrLevel('InsertSubstring', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (sLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('InsertSubstring', ModName, ErrWarning, &
                          'Length of the substring is zero.')
        RETURN
    END IF
    
    ! insert the substring
    IF (Pos <= 1) THEN
        cStrOut = sStr // cStrIn
    ELSEIF (Pos >= cLen) THEN
        cStrOut = cStrIn // sStr
    ELSE
        cStrOut = cStrIn(1:Pos-1) // sStr // cStrIn(Pos:cLen)
    END IF

    RETURN

END FUNCTION InsertSubstring

!******************************************************************************

MODULE FUNCTION RemoveCharacters(cStrIn,ChrSet,Option) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove characters from the character string depending on
    ! the specified Option and ChrSet.

    ! TECHNICAL INFORMATION:
    ! 
    ! The ChrSet is a required input argument that contains a set of characters to be
    !   removed.
    ! 
    ! The Option is an optional input argument that indicates where characters in the
    !   "ChrSet" set of characters to be removed are.  Available options include:
    !   - Option = -1 --> nothing to be removed
    !   - Option =  0 --> leading (prefix) and trailing (suffix) character(s) to be removed
    !   - Option =  1 --> leading (prefix) character(s) to be removed
    !   - Option =  2 --> trailing (suffix) character(s) to be removed
    !   - Option =  3 --> all character(s) [that are in the "ChrSet" set] to be removed
    ! If not present, the default option is 0.
    !
    ! Examples:   cStrIn = '   abc123 ijk456 xyz789 1+2-3*4/5^6.7,8;9   '
    ! If ChrSet = SET_BLANKS and Option is not present, 
    !       cStrOut = 'abc123 ijk456 xyz789 1+2-3*4/5^6.7,8;9'
    ! If ChrSet = SET_BLANKS and Option = 1,
    !       cStrOut = 'abc123 ijk456 xyz789 1+2-3*4/5^6.7,8;9   '
    ! If ChrSet = SET_BLANKS and Option = 3,
    !       cStrOut = 'abc123ijk456xyz7891+2-3*4/5^6.7,8;9'
    ! If ChrSet = '13579' and Option = 3,
    !       cStrOut = '   abc2 ijk46 xyz8 +2-*4/^6.,8;   '
    ! If ChrSet = '39' // SET_BLANKS and Option = 3,
    !       cStrOut = 'abc12ijk456xyz781+2-*4/5^6.7,8;'

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStrIn   ! input character string
    tCharStar,          INTENT(IN)  :: ChrSet   ! set of characters to be removed
    tInteger, OPTIONAL, INTENT(IN)  :: Option   ! flag indicating how to remove characters
    tCharAlloc                      :: cStrOut  ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tInteger    :: RemoveOpt    ! remove flag
    tIndex      :: StartPos     ! starting position
    tIndex      :: EndPos       ! ending postion

    ! FLOW:
    
    ! initialize and check input
    IF (LEN(cStrIn)==0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('RemoveCharacters', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    END IF
    IF (LEN(ChrSet)==0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('RemoveCharacters', ModName, ErrWarning, &
                          'Length of the character set is zero.')
        RETURN
    END IF
    SET_OPTION_WITH_LIMITS(RemoveOpt, 0, Option, -1, 3)

    ! remove characters
    SELECT CASE (RemoveOpt)
    CASE (0)
        ! remove both leading and trailing characters
        StartPos = VERIFY(cStrIn, ChrSet)
        IF (StartPos == 0) THEN
            ! all characters are removed
            cStrOut = ''
        ELSE
            EndPos  = VERIFY(cStrIn, ChrSet, BACK=TrueVal)
            cStrOut = cStrIn(StartPos:EndPos)
        END IF
    CASE (1)
        ! remove only leading characters
        StartPos = VERIFY(cStrIn, ChrSet)
        IF (StartPos == 0) THEN
            ! all characters are removed
            cStrOut = ''
        ELSE
            EndPos  = LEN(cStrIn)
            cStrOut = cStrIn(StartPos:EndPos)
        END IF
    CASE (2)
        ! remove only trailing characters
        EndPos = VERIFY(cStrIn, ChrSet, BACK=TrueVal)
        IF (EndPos == 0) THEN
            ! all characters are removed
            cStrOut = ''
        ELSE
            StartPos = 1
            cStrOut  = cStrIn(StartPos:EndPos)
        END IF
    CASE (3)
        ! remove leading, trailing, and within characters
        cStrOut = RemoveCharactersInSet(cStrIn, ChrSet)
    CASE DEFAULT
        ! do nothing
    END SELECT        

    RETURN

CONTAINS

    FUNCTION RemoveCharactersInSet(cStrIn,RemoveSet) RESULT(cStrOut)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To remove all characters in cStrIn that are also
        ! in the given character set.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)   :: cStrIn       ! input character string
        tCharStar, INTENT(IN)   :: RemoveSet    ! set of characters to be removed
        tCharAlloc              :: cStrOut      ! output character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tCharLen(LEN(cStrIn))   :: wStr     ! working string
        tIndex                  :: wLen     ! length of working string
        tIndex                  :: IChr     ! index
        tChar                   :: CurrChr  ! current character
        tIndex                  :: ChrIndex ! character index

    !** FLOW:
    
        ! initialize
        wLen = 0
    
        ! loop over all characters in cStrIn
        DO IChr = 1 , LEN(cStrIn)
            ! get current character
            CurrChr = cStrIn(IChr:IChr)
            ! check whether the current character is in the specified set
            ChrIndex = INDEX(RemoveSet, CurrChr)
            IF (ChrIndex == 0) THEN
                ! this character is not in the set so assign it to the working string
                wLen = wLen + 1
                wStr(wLen:wLen) = CurrChr
            END IF
        END DO
        
        ! transfer output
        IF (wLen == 0) THEN
            cStrOut = ''
        ELSE
            cStrOut = wStr(1:wLen)
        END IF

        RETURN

    END FUNCTION RemoveCharactersInSet

    !**************************************************************************************

END FUNCTION RemoveCharacters

!******************************************************************************

MODULE FUNCTION RemoveSubstring(cStrIn,sStr,FirstOnly) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove substring from the character string.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStrIn       ! input character string
    tCharStar,          INTENT(IN)  :: sStr         ! substring to be removed
    tLogical, OPTIONAL, INTENT(IN)  :: FirstOnly    ! true if only the first substring found
                                                    ! is to be removed
                                                    ! false if all occurrences found is to be removed
                                                    ! default = FalseVal
    tCharAlloc                      :: cStrOut      ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc      :: wStr
    tLogical        :: RemoveAll
    tIndex          :: cLen, sLen, wLen
    tIndex          :: iBegin, iNext

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    sLen = LEN(sStr)
    RemoveAll = TrueVal
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('RemoveSubstring', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (sLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('RemoveSubstring', ModName, ErrWarning, &
                          'Length of the substring is zero.')
        RETURN
    ELSEIF (cLen < sLen) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('RemoveSubstring', ModName, ErrWarning, &
                          'Length of the character string is less than that of the substring.')
        RETURN
    END IF
    IF (PRESENT(FirstOnly)) RemoveAll = .NOT.FirstOnly
    
    IF (RemoveAll) THEN
        ! initialize working variables
        wStr = cStrIn
        wLen = cLen
        iBegin = 1
        ! loop over the string
        DO
            ! find the substring
            iNext = INDEX(wStr(iBegin:wLen), sStr) + iBegin - 1
            IF (iNext >= iBegin) THEN
                ! found one more substring so remove it
                wStr(iNext:wLen-sLen) = wStr(iNext+sLen:wLen)
                ! note: the following line is not really necessary but just in case
                wStr(wLen-sLen+1:wLen) = REPEAT(CHR_SPACE, sLen)
                wLen = wLen - sLen
                iBegin = iNext
                ! exit if at the end of the string
                IF (iBegin + sLen > wLen) EXIT
            ELSE
                ! found no more substring
                EXIT
            END IF
        END DO
        ! get the output
        cStrOut = wStr(1:wLen)
    ELSE
        ! remove only the first substring found
        iNext = INDEX(cStrIn(1:cLen), sStr)
        IF (iNext /= 0) THEN
            ! the first substring found so get the output
            cStrOut = cStrIn(1:iNext-1) // cStrIn(iNext+sLen:cLen)
        END IF
    END IF

    RETURN

END FUNCTION RemoveSubstring

!******************************************************************************

RECURSIVE MODULE FUNCTION RemoveSubstringKnownPos(cStrIn,sLen,sCount,sPos) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove substring from the character string by providing length, number of
    ! occurrences and position(s) of first character of the substring

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn       ! input character string
    tIndex,    INTENT(IN)   :: sLen         ! length of original substring
    tIndex,    INTENT(IN)   :: sCount       ! number of occurrences of original substring
    tIndex,    INTENT(IN)   :: sPos(sCount) ! position(s) of first character of original substring
    tCharAlloc              :: cStrOut      ! output character string

    ! SUBROUTINE PARAMTER DECLARATIONS:
    tCharStar, PARAMETER    :: xStr = '!!!'
    
    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc      :: wStr
    tIndex          :: cLen

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('RemoveSubstringKnownPos', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (sLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('RemoveSubstringKnownPos', ModName, ErrWarning, &
                          'Length of the substring is zero.')
        RETURN
    ELSEIF (cLen < sLen) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('RemoveSubstringKnownPos', ModName, ErrWarning, &
                          'Length of the character string is less than that of the substring.')
        RETURN
    END IF
    
    ! replace the substring with xStr
    wStr = ReplaceSubstring(cStrIn, xStr, sLen, sCount, sPos)
    
    ! remove xStr
    cStrOut = RemoveSubstring(wStr, xStr)

    RETURN

END FUNCTION RemoveSubstringKnownPos

!******************************************************************************

MODULE FUNCTION DeleteSubstring(cStrIn,lPos,rPos) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove substring from the character string at specified positions.
    ! If lPos is less than 1, then 1 is used as a starting point of the substring.
    ! Similarly, if rPos is greater than length of the character string, then
    !   the length is used as a ending point.
    ! If rPos is less than lPos, an original character string is returned.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn   ! input character string
    tIndex,    INTENT(IN)   :: lPos     ! the leftmost character position of the substring
    tIndex,    INTENT(IN)   :: rPos     ! the rightmost character position of the substring
    tCharAlloc              :: cStrOut  ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sPos, ePos

    ! FLOW:
    
    ! initialize and check length
    cLen = LEN(cStrIn)
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('DeleteSubstring', ModName, ErrWarning, &
                          'Length of the character string is zero.')
    END IF
    
    ! get staring and ending positions
    sPos = MAX(1, lPos)
    ePos = MIN(cLen, rPos)
    
    ! delete substring
    IF (sPos <= ePos) THEN
        cStrOut = cStrIn(:sPos-1) // cStrIn(ePos+1:)
    ELSE
        cStrOut = cStrIn
    END IF

    RETURN

END FUNCTION DeleteSubstring

!******************************************************************************

MODULE FUNCTION RemoveCharactersProtect(cStrIn,ChrSet,Option,ExclMrk) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove characters in unprotected region(s) from the character string
    !   depending on the specified Option and ChrSet.
    ! See explanations about the specified Option and ChrSet in routine
    !   "RemoveCharacters".
    ! See explanations about the protected region(s) in routine
    !   "FindProtectedRegions".

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStrIn   ! input character string
    tCharStar,          INTENT(IN)  :: ChrSet   ! set of characters to be removed
    tInteger, OPTIONAL, INTENT(IN)  :: Option   ! flag indicating how to remove characters
    tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk  ! true if "!" is also used to protect regions
                                                ! false if only quotes are used to protect regions
                                                ! default = TrueVal
    tCharAlloc                      :: cStrOut  ! output character string

    ! SUBROUTINE PARAMTER DECLARATIONS:
    tCharStar, PARAMETER     :: xStr = '!!!'
    
    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex, ALLOCATABLE     :: lPos(:)
    tIndex, ALLOCATABLE     :: rPos(:)
    tIndex                  :: nRegion
    tCharAlloc              :: wStr

    ! FLOW:
    
    ! initialize and check input
    IF (LEN(cStrIn)==0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('RemoveCharactersProtect', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    END IF
    IF (LEN(ChrSet)==0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('RemoveCharactersProtect', ModName, ErrWarning, &
                          'Length of the character set is zero.')
        RETURN
    END IF
    
    ! find protected region(s)
    nRegion = FindProtectedRegions(cStrIn, lPos, rPos, ExclMrk)
    
    ! replace protected regions with a valid character
    wStr = ReplaceProtectedRegionsWithValidCharacter(cStrIn, ChrSet, nRegion, lPos, rPos)
    
    ! remove characters in the unprotected region(s)
    cStrOut = RemoveCharacters(wStr, ChrSet, Option)

    RETURN

END FUNCTION RemoveCharactersProtect

!******************************************************************************

RECURSIVE MODULE FUNCTION RemoveSubstringProtect(cStrIn,sStr,ExclMrk,FirstOnly) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove substring in the unprotected region(s) from the character string.
    ! See explanations about the protected region(s) in routine
    ! "FindProtectedRegions".

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStrIn       ! input character string
    tCharStar,          INTENT(IN)  :: sStr         ! substring to be removed
    tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk      ! true if "!" is also used to protect regions
                                                    ! false if only quotes are used to protect regions
                                                    ! default = TrueVal
    tLogical, OPTIONAL, INTENT(IN)  :: FirstOnly    ! true if only the first substring found
                                                    ! is to be removed
                                                    ! false if all occurrences found is to be removed
                                                    ! default = FalseVal
    tCharAlloc                      :: cStrOut      ! output character string

    ! SUBROUTINE PARAMTER DECLARATIONS:
    tCharStar, PARAMETER    :: xStr = '!!!'
    
    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc      :: wStr
    tIndex          :: cLen, sLen

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    sLen = LEN(sStr)
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('RemoveSubstringProtect', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (sLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('RemoveSubstringProtect', ModName, ErrWarning, &
                          'Length of the substring is zero.')
        RETURN
    ELSEIF (cLen < sLen) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('RemoveSubstringProtect', ModName, ErrWarning, &
                          'Length of the character string is less than that of the substring.')
        RETURN
    END IF
    
    ! replace the substring with xStr
    wStr = ReplaceSubstringProtect(cStrIn, sStr, xStr, ExclMrk)
    
    ! remove xStr
    cStrOut = RemoveSubstring(wStr, xStr, FirstOnly)

    RETURN

END FUNCTION RemoveSubstringProtect

!******************************************************************************

MODULE FUNCTION CropBlanks(cStrIn,SpaceOnly) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove leading and trailing blanks from the character string

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStrIn       ! input character string
    tLogical, OPTIONAL, INTENT(IN)  :: SpaceOnly    ! true if requesting to remove only blank space 
                                                    ! false if requesting to remove both tab and space 
                                                    ! default = FalseVal
    tCharAlloc                      :: cStrOut      ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tLogical    :: TabAndSpace

    ! FLOW:
    
    ! initialize and check input
    TabAndSpace = TrueVal
    IF (LEN(cStrIn)==0) THEN
        cStrOut = ''
        RETURN
    END IF
    IF (PRESENT(SpaceOnly)) TabAndSpace = .NOT.SpaceOnly
    
    ! perform cropping
    IF (TabAndSpace) THEN
        cStrOut = RemoveCharacters(cStrIn, SET_BLANKS)  
    ELSE
        cStrOut = TRIM(ADJUSTL(cStrIn))  
    END IF

    RETURN

END FUNCTION CropBlanks

!******************************************************************************

MODULE FUNCTION CompactString(cStrIn) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To convert multiple spaces and tabs into a single space, delete control
    ! characters and removes initial spaces

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn       ! input character string
    tCharAlloc              :: cStrOut      ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharAlloc      :: iStr     ! string with initial spaces removed
    tIndex          :: iLen     ! length of iStr
    tCharAlloc      :: wStr     ! working string
    tChar           :: Chr      ! current character
    tIndex          :: iChr     ! character ASCII index
    tIndex          :: iSpace   ! space index
    tIndex          :: cPos     ! current character position
    tIndex          :: I        ! index

    ! FLOW:
    
    ! remove initial spaces
    iStr = CropBlanks(cStrIn)
    iLen = LEN(iStr)
    IF (iLen == 0) THEN
        cStrOut = ''
        RETURN
    END IF
    
    ! allocate working string
    ALLOCATE(tCharLen(iLen) :: wStr)
    
    ! initialize
    iSpace = 0
    cPos = 0
    
    ! iterate over each character
    DO I = 1, iLen
        Chr = iStr(I:I)
        iChr = IACHAR(Chr)
        SELECT CASE (iChr)
        CASE (9, 32)
            ! current character is a space or a tab
            IF (iSpace == 0) THEN
                cPos = cPos + 1
                wStr(cPos:cPos) = CHR_SPACE
            END IF
            iSpace = 1
        CASE (33:126, 128:255)
            ! current character is not a space, tab, or control character
            cPos = cPos + 1
            wStr(cPos:cPos) = Chr
            iSpace = 0
        END SELECT
    END DO
    
    ! transfer output
    cStrOut = wStr(1:cPos)

    RETURN

END FUNCTION CompactString

!******************************************************************************

MODULE FUNCTION CompressString(cStrIn) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To remove spaces, tabs and control characters from the character string

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn       ! input character string
    tCharAlloc              :: cStrOut      ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex          :: cLen     ! length of cStrIn
    tCharAlloc      :: wStr     ! working string
    tChar           :: Chr      ! current character
    tIndex          :: iChr     ! character ASCII index
    tIndex          :: cPos     ! current character position
    tIndex          :: I        ! index

    ! FLOW:
    
    ! check length of the input character string
    cLen = LEN(cStrIn)
    IF (cLen == 0) THEN
        cStrOut = ''
        RETURN
    END IF
    
    ! allocate working string
    ALLOCATE(tCharLen(cLen) :: wStr)
    
    ! initialize
    cPos = 0
    
    ! iterate over each character
    DO I = 1, cLen
        Chr = cStrIn(I:I)
        iChr = IACHAR(Chr)
        SELECT CASE (iChr)
        CASE (33:126, 128:255)
            ! current character is not a space, tab, or control character
            cPos = cPos + 1
            wStr(cPos:cPos) = Chr
        END SELECT
    END DO
    
    ! transfer output
    cStrOut = wStr(1:cPos)

    RETURN

END FUNCTION CompressString

!******************************************************************************

RECURSIVE MODULE FUNCTION ReplaceSubstringKnownPos(cStrIn,nStr,oLen,oCount,oPos) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To replace all occurrences of the original substring found in the given
    ! character string with the new substring by providing length, number of
    ! occurrences and position(s) of first character of original substring

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn       ! input character string
    tCharStar, INTENT(IN)   :: nStr         ! new substring
    tIndex,    INTENT(IN)   :: oLen         ! length of original substring
    tIndex,    INTENT(IN)   :: oCount       ! number of occurrences of original substring
    tIndex,    INTENT(IN)   :: oPos(oCount) ! position(s) of first character of original substring
    tCharAlloc              :: cStrOut      ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, nLen, coutLen
    tIndex      :: I, iStart, iEnd, oStart, oEnd

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    nLen = LEN(nStr)
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('ReplaceSubstringKnownPos', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (oLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstringKnownPos', ModName, ErrWarning, &
                          'Length of the original substring is zero.')
        RETURN
    ELSEIF (cLen < oLen) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstringKnownPos', ModName, ErrWarning, &
                          'Length of the character string is less than that of the original substring.')
        RETURN
    ELSEIF (nLen == 0) THEN
        cStrOut = RemoveSubstring(cStrIn, oLen, oCount, oPos)
        CALL Handle_ErrLevel('ReplaceSubstringKnownPos', ModName, ErrWarning, &
                          'Length of the new substring is zero so the original substrings are only removed.')
        RETURN
    ELSEIF (oCount == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstringKnownPos', ModName, ErrWarning, &
                          'Number of occurrence(s) of the old substring is zero.')
        RETURN
    END IF
    
    ! allocate the output character string with correct length
    coutLen = cLen + oCount*(nLen-oLen)
    ALLOCATE(tCharLen(coutLen) :: cStrOut)

    ! replace the new substring
    DO I = 1, oCount
        IF (oPos(I) == 1) THEN
            ! the original substring found at the start of the input character string
            oStart = 1
            oEnd   = nLen
            cStrOut(oStart:oEnd) = nStr
        ELSEIF ((oPos(I)+oLen-1) == cLen) THEN
            ! the original substring found at the end of the input character string
            IF (I == 1) THEN
                ! for this case, there is only one occurrence
                cStrOut = cStrIn(1:oPos(I)-1) // nStr
            ELSE
                iStart = oPos(I-1) + oLen
                iEnd = oPos(I) - 1
                oStart = oEnd + 1
                oEnd = oStart + (iEnd - iStart) + nLen
                cStrOut(oStart:oEnd) = cStrIn(iStart:iEnd) // nStr
            END IF
        ELSE
            ! the original substring found in the middle of the input character string
            IF (I == 1) THEN
                iStart = 1
                iEnd = oPos(I) - 1
                oStart = 1
                oEnd = oStart + (iEnd - iStart) + nLen
            ELSE
                iStart = oPos(I-1) + oLen
                iEnd = oPos(I) - 1
                oStart = oEnd + 1
                oEnd = oStart + (iEnd - iStart) + nLen
            END IF
            cStrOut(oStart:oEnd) = cStrIn(iStart:iEnd) // nStr
        END IF
    END DO
    IF (oEnd < coutLen) THEN
        iStart = oPos(oCount) + oLen
        iEnd = cLen
        oStart = oEnd + 1
        oEnd = oStart + (iEnd - iStart)
        IF (oEnd == coutLen) THEN
            cStrOut(oStart:oEnd) = cStrIn(iStart:iEnd)
        ELSE
            CALL Handle_ErrLevel('ReplaceSubstringKnownPos', ModName, ErrSevere, &
                              'This must be a bug since the computed lengths are not matched.')
            RETURN
        END IF
    END IF
    
    RETURN

END FUNCTION ReplaceSubstringKnownPos

!******************************************************************************

MODULE FUNCTION ReplaceProtectedRegionsWithValidCharacter(cStrIn,ChrSet,nRegion,lPos,rPos) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To replace the protected region(s) of the given character string with a valid
    ! character where any character not in the ChrSet argument is considered valid.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn       ! input character string
    tCharStar, INTENT(IN)   :: ChrSet       ! set of characters
    tIndex,    INTENT(IN)   :: nRegion      ! number of protected regions
    tIndex,    INTENT(IN)   :: lPos(nRegion)! positions of the first character of protected regions
    tIndex,    INTENT(IN)   :: rPos(nRegion)! positions of the last character of protected regions
    tCharAlloc              :: cStrOut      ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex          :: cLen
    tIndex          :: nCopies, I
    tChar           :: Dummy

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    IF (cLen == 0) THEN
        CALL Handle_ErrLevel('ReplaceProtectedRegionsWithValidCharacter', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (LEN(ChrSet) == 0) THEN
        CALL Handle_ErrLevel('ReplaceProtectedRegionsWithValidCharacter', ModName, ErrWarning, &
                          'Length of the set of characters is zero.')
        RETURN
    END IF
    
    ! replace protected regions with an exclamation mark or another symbol that
    ! is not in the character set
    IF (NRegion > 0) THEN
        Dummy = CHR_EXCLAMATION_MARK
        IF (VERIFY(Dummy, ChrSet)==0) THEN
            ! the dummy character might not be a valid replacement
            ! so find another character
            I = 34
            DO
                Dummy = CHAR(I)
                IF (VERIFY(Dummy, ChrSet)/=0) EXIT
                I = I + 1
                IF (I > 126) THEN
                    CALL Handle_ErrLevel('ReplaceProtectedRegionsWithValidCharacter', ModName, ErrWarning, &
                                        'Cannot find a valid character to replace those in ' // &
                                        'the protected region(s).')
                    RETURN
                END IF
            END DO
        END IF
        IF ((lPos(1) == 1).AND.(rPos(1) == cLen)) THEN
            ! the character string is entirely protected
            cStrOut = REPEAT(Dummy, cLen)
            RETURN
        ELSE
            ! get working character string and dummy character
            cStrOut = cStrIn
            DO I = 1, nRegion
                ! compute number of copies of the dummy character
                nCopies = rPos(I) - lPos(I) + 1
                IF (lPos(I) == 1) THEN
                    ! a protected region is at the start
                    cStrOut = REPEAT(Dummy, nCopies) // cStrOut(rPos(I)+1:cLen)
                ELSEIF (rPos(I) == cLen) THEN
                    ! a protected region is at the end
                    cStrOut = cStrOut(1:lPos(I)-1) // REPEAT(Dummy, nCopies)
                ELSE
                    ! a protected region is in the middle
                    cStrOut = cStrOut(1:lPos(I)-1) // REPEAT(Dummy, nCopies) // cStrOut(rPos(I)+1:cLen)
                END IF
            END DO
        END IF
    ELSE
        ! no protected region so get working character string
        cStrOut = cStrIn
    END IF
    
    RETURN

END FUNCTION ReplaceProtectedRegionsWithValidCharacter

!******************************************************************************

MODULE FUNCTION ReplaceSubstring(cStrIn,oStr,nStr) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To replace all occurrences of the original substring found in the given
    ! character string with the new substring

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn   ! input character string
    tCharStar, INTENT(IN)   :: oStr     ! original (old) substring
    tCharStar, INTENT(IN)   :: nStr     ! new substring
    tCharAlloc              :: cStrOut  ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: cLen, oLen, nLen
    tIndex              :: oCount
    tIndex, ALLOCATABLE :: oPos(:)

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    oLen = LEN(oStr)
    nLen = LEN(nStr)
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('ReplaceSubstring', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (oLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstring', ModName, ErrWarning, &
                          'Length of the original substring is zero.')
        RETURN
    ELSEIF (cLen < oLen) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstring', ModName, ErrWarning, &
                          'Length of the character string is less than that of the original substring.')
        RETURN
    ELSEIF (nLen == 0) THEN
        cStrOut = RemoveSubstring(cStrIn, oStr)
        CALL Handle_ErrLevel('ReplaceSubstring', ModName, ErrWarning, &
                          'Length of the new substring is zero so the original substrings are only removed.')
        RETURN
    END IF
    
    ! find number of occurrences of the original substring and its positions
    oCount = FindSubstring(cStrIn, oStr, oPos)
    
    ! replace the new substring
    cStrOut = ReplaceSubstringKnownPos(cStrIn, nStr, oLen, oCount, oPos)
    
    RETURN

END FUNCTION ReplaceSubstring

!******************************************************************************

RECURSIVE MODULE FUNCTION ReplaceSubstringProtect(cStrIn,oStr,nStr,ExclMrk) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To replace occurrences of the original substring found in unprotected
    ! region(s) of the given character string with the new substring.
    ! See explanations about the protected region(s) in routine
    ! "FindProtectedRegions".

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStrIn   ! input character string
    tCharStar,          INTENT(IN)  :: oStr     ! original (old) substring
    tCharStar,          INTENT(IN)  :: nStr     ! new substring
    tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk  ! true if exclamation mark is also used to protect regions
                                                ! false if only quotes are used to protect regions
                                                ! default = TrueVal
    tCharAlloc                      :: cStrOut  ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: cLen, oLen, nLen
    tIndex              :: oCount
    tIndex, ALLOCATABLE :: oPos(:)

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    oLen = LEN(oStr)
    nLen = LEN(nStr)
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('ReplaceSubstringProtect', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (oLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstringProtect', ModName, ErrWarning, &
                          'Length of the original substring is zero.')
        RETURN
    ELSEIF (cLen < oLen) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstringProtect', ModName, ErrWarning, &
                          'Length of the character string is less than that of the original substring.')
        RETURN
    ELSEIF (nLen == 0) THEN
        cStrOut = RemoveSubstringProtect(cStrIn, oStr, ExclMrk)
        CALL Handle_ErrLevel('ReplaceSubstringProtect', ModName, ErrWarning, &
                          'Length of the new substring is zero so the original substrings are only removed.')
        RETURN
    END IF
    
    ! find number of occurrences in unprotected region(s) of the original substring and its positions
    oCount = FindSubstringProtect(cStrIn, oStr, oPos, ExclMrk)
    
    ! replace the new substring
    cStrOut = ReplaceSubstring(cStrIn, nStr, oLen, oCount, oPos)
    
    RETURN

END FUNCTION ReplaceSubstringProtect

!******************************************************************************

MODULE FUNCTION ReplaceSubstringRecursive(cStrIn,oStr,nStr) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To replace all occurrences of the original substring found in the given
    ! character string with the new substring in a recursive way
    ! Example: cStrIn = 'abbbbb', oStr = 'ab', nStr = 'a', cStrOut = 'ab'
    ! whereas in a non-recursive way, cStrOut = 'abbbb'

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn   ! input character string
    tCharStar, INTENT(IN)   :: oStr     ! original (old) substring
    tCharStar, INTENT(IN)   :: nStr     ! new substring
    tCharAlloc              :: cStrOut  ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, oLen, nLen

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    oLen = LEN(oStr)
    nLen = LEN(nStr)
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('ReplaceSubstring', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (oLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstring', ModName, ErrWarning, &
                          'Length of the original substring is zero.')
        RETURN
    ELSEIF (cLen < oLen) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstring', ModName, ErrWarning, &
                          'Length of the character string is less than that of the original substring.')
        RETURN
    ELSEIF (nLen == 0) THEN
        cStrOut = RemoveSubstring(cStrIn, oStr)
        CALL Handle_ErrLevel('ReplaceSubstring', ModName, ErrWarning, &
                          'Length of the new substring is zero so the original substrings are only removed.')
        RETURN
    END IF
    
    ! find number of occurrences of the original substring and its positions
    cStrOut = cStrIn
    DO
        IF (INDEX(CstrOut, oStr)/=0) THEN
            cStrOut = ReplaceSubstringOnce(cStrOut, oStr, nStr)
        ELSE
            EXIT
        END IF
    END DO
    
    RETURN
    
CONTAINS

FUNCTION ReplaceSubstringOnce(cStrIn,oStr,nStr) RESULT(cStrOut)

    ! PURPOSE OF THIS ROUTINE:
    ! To replace the first occurrence of the original substring found in the given
    ! character string with the new substring

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar, INTENT(IN)   :: cStrIn   ! input character string
    tCharStar, INTENT(IN)   :: oStr     ! original (old) substring
    tCharStar, INTENT(IN)   :: nStr     ! new substring
    tCharAlloc              :: cStrOut  ! output character string

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, oLen, nLen
    tIndex      :: oCount, I, iStart, iEnd, oStart, oEnd
    tIndex      :: oPos

    ! FLOW:
    
    ! initialize and check input
    cLen = LEN(cStrIn)
    oLen = LEN(oStr)
    nLen = LEN(nStr)
    IF (cLen == 0) THEN
        cStrOut = ''
        CALL Handle_ErrLevel('ReplaceSubstringOnce', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (oLen == 0) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstringOnce', ModName, ErrWarning, &
                          'Length of the original substring is zero.')
        RETURN
    ELSEIF (cLen < oLen) THEN
        cStrOut = cStrIn
        CALL Handle_ErrLevel('ReplaceSubstringOnce', ModName, ErrWarning, &
                          'Length of the character string is less than that of the original substring.')
        RETURN
    ELSEIF (nLen == 0) THEN
        cStrOut = RemoveSubstring(cStrIn, oStr)
        CALL Handle_ErrLevel('ReplaceSubstringOnce', ModName, ErrWarning, &
                          'Length of the new substring is zero so the original substrings are only removed.')
        RETURN
    END IF
    
    oPos = INDEX(cStrIn, oStr)
    IF (oPos == 0) THEN
        ! the old substring is not found
        cStrOut = cStrIn
    ELSEIF (oPos == 1) THEN
        ! the old substring is at the start
        cStrOut = nStr // cStrIn(oPos+oLen:cLen)
    ELSEIF ((oPos+oLen-1) == cLen) THEN
        ! the old substring is at the end
        cStrOut = cStrIn(1:oPos-1) // nStr
    ELSE
        ! the old substring is in the middle
        cStrOut = cStrIn(1:oPos-1) // nStr // cStrIn(oPos+oLen:cLen)
    END IF
    
    RETURN

END FUNCTION ReplaceSubstringOnce

!******************************************************************************

END FUNCTION ReplaceSubstringRecursive

!******************************************************************************

MODULE SUBROUTINE PartitionSepSub(cStr,SepSub,bStr,aStr,Back)

    ! PURPOSE OF THIS ROUTINE:
    ! To partition a character string into two substrings where the specified
    ! separator is a multiple-character string.  The partition occurs at the 
    ! first occurrence of the separator found.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStr     ! character string
    tCharStar,          INTENT(IN)  :: SepSub   ! multiple-character separator
    tCharAlloc,         INTENT(OUT) :: bStr     ! substring before the separator found
    tCharAlloc,         INTENT(OUT) :: aStr     ! substring after the separator found
    tLogical, OPTIONAL, INTENT(IN)  :: Back     ! If present and true, searching from the back
                                                ! otherwise, searching from the front

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen, sPos
    tLogical    :: Front

    ! FLOW:
    
    ! check input
    cLen = LEN(cStr)
    sLen = LEN(SepSub)
    Front = TrueVal
    IF (PRESENT(Back)) Front = .NOT.Back
    IF (cLen == 0) THEN
        bStr = ''
        aStr = ''
        CALL Handle_ErrLevel('PartitionSepSub', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (sLen == 0) THEN
        IF (Front) THEN
            bStr = cStr
            aStr = ''
        ELSE
            bStr = ''
            aStr = cStr
        END IF
        CALL Handle_ErrLevel('PartitionSepSub', ModName, ErrWarning, &
                          'Length of the separator is zero.')
        RETURN
    END IF
    
    sPos = INDEX(cStr, SepSub, Back=.NOT.Front)
    IF (Front) THEN
        IF (sPos == 0) THEN
            ! no separator found
            bStr = cStr
            aStr = ''
        ELSEIF (sPos == 1) THEN
            ! separator found at the beginning
            bStr = ''
            aStr = cStr(sPos+sLen:cLen)
        ELSEIF ((sPos+sLen-1) == cLen) THEN
            ! separator found at the end
            bStr = cStr(1:sPos-1)
            aStr = ''
        ELSE
            ! separator found in the middle
            bStr = cStr(1:sPos-1)
            aStr = cStr(sPos+sLen:cLen)
        END IF
    ELSE
        IF (sPos == 0) THEN
            ! no separator found
            bStr = ''
            aStr = cStr
        ELSEIF (sPos == 1) THEN
            ! separator found at the beginning
            bStr = cStr(sPos+sLen:cLen)
            aStr = ''
        ELSEIF ((sPos+sLen-1) == cLen) THEN
            ! separator found at the end
            bStr = ''
            aStr = cStr(1:sPos-1)
        ELSE
            ! separator found in the middle
            bStr = cStr(sPos+sLen:cLen)
            aStr = cStr(1:sPos-1)
        END IF
    END IF
        
    RETURN

END SUBROUTINE PartitionSepSub

!******************************************************************************

MODULE SUBROUTINE PartitionSepChr(cStr,SepSet,bStr,aStr,SepChr,Back)

    ! PURPOSE OF THIS ROUTINE:
    ! To partition a character string into two substrings where the separator
    ! is a single character (any character in the specified set of characters).
    ! The partition occurs at the first occurrence of the separator found.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStr     ! character string
    tCharStar,          INTENT(IN)  :: SepSet   ! set of characters representing valid separators
    tCharAlloc,         INTENT(OUT) :: bStr     ! substring before the separator found
    tCharAlloc,         INTENT(OUT) :: aStr     ! substring after the separator found
    tChar,              INTENT(OUT) :: SepChr   ! the separator found
    tLogical, OPTIONAL, INTENT(IN)  :: Back     ! If present and true, searching from the back
                                                ! otherwise, searching from the front

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: cLen, sLen, sPos
    tLogical    :: Front

    ! FLOW:
    
    ! check input
    cLen = LEN(cStr)
    sLen = LEN(SepSet)
    Front = TrueVal
    IF (PRESENT(Back)) Front = .NOT.Back
    IF (cLen == 0) THEN
        ! set output
        bStr = ''
        aStr = ''
        SepChr = ''
        ! report error
        CALL Handle_ErrLevel('PartitionSepChr', ModName, ErrWarning, &
                          'Length of the character string is zero.')
        RETURN
    ELSEIF (sLen == 0) THEN
        ! set output
        IF (Front) THEN
            bStr = cStr
            aStr = ''
        ELSE
            bStr = ''
            aStr = cStr
        END IF
        SepChr = ''
        ! report error
        CALL Handle_ErrLevel('PartitionSepChr', ModName, ErrWarning, &
                          'Length of the character set is zero.')
        RETURN
    END IF
    
    sPos = SCAN(cStr, SepSet, Back=.NOT.Front)
    IF (Front) THEN
        IF (sPos == 0) THEN
            ! no separator found
            bStr = cStr
            aStr = ''
            SepChr = ''
        ELSE
            IF (sPos == 1) THEN
                ! separator found at the beginning
                bStr = ''
                aStr = cStr(sPos+1:cLen)
            ELSEIF ((sPos+sLen-1) == cLen) THEN
                ! separator found at the end
                bStr = cStr(1:sPos-1)
                aStr = ''
            ELSE
                ! separator found in the middle
                bStr = cStr(1:sPos-1)
                aStr = cStr(sPos+1:cLen)
            END IF
            SepChr = cStr(sPos:sPos)
        END IF
    ELSE
        IF (sPos == 0) THEN
            ! no separator found
            bStr = ''
            aStr = cStr
            SepChr = ''
        ELSE
            IF (sPos == 1) THEN
                ! separator found at the beginning
                bStr = cStr(sPos+1:cLen)
                aStr = ''
            ELSEIF ((sPos+sLen-1) == cLen) THEN
                ! separator found at the end
                bStr = ''
                aStr = cStr(1:sPos-1)
            ELSE
                ! separator found in the middle
                bStr = cStr(sPos+1:cLen)
                aStr = cStr(1:sPos-1)
            END IF
            SepChr = cStr(sPos:sPos)
        END IF
    END IF
        
    RETURN

END SUBROUTINE PartitionSepChr

!******************************************************************************

MODULE SUBROUTINE SplitSepSub(cStr,SepSub,qStr)

    ! PURPOSE OF THIS ROUTINE:
    ! To split a character string into multiple substrings where the specified
    ! separator is a multiple-character string.  Return a queue of substrings
    ! where a substring may have zero length (i.e. empty string).
    ! If the separator is not found, return an empty queue.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,       INTENT(IN)     :: cStr     !! character string
    tCharStar,       INTENT(IN)     :: SepSub   !! multiple-character separator
    TYPE(QueueChar), INTENT(OUT)    :: qStr     !! queue of substrings

    ! SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: CharSet = FalseVal

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: nCount       ! number of occurrences of separator found
    tIndex, ALLOCATABLE :: sPos(:)      ! positions of the separator found
    tIndex              :: I            ! index
    tIndex              :: sLen         ! length of separator
    tIndex              :: first, Last  ! indices of current substring

    ! FLOW:

    ! find number of occurrences of the separator found
    nCount = FindSeparators(cStr, SepSub, CharSet, sPos)
    
    ! get a queue of substrings if nCount > 0
    IF (nCount > 0) THEN
        sLen = LEN(SepSub)
        DO I = 1, nCount + 1
            ! get indices of current substring
            IF (I == 1) THEN
                ! the first substring
                First = 1
                Last  = sPos(I) - 1
            ELSEIF (I == nCount + 1) THEN
                ! the last substring
                First = sPos(I-1) + sLen
                Last  = LEN(cStr)
            ELSE
                ! the substring in the middle
                First = sPos(I-1) + sLen
                Last  = sPos(I) - 1
            END IF
            ! add current substring to the queue
            IF (First < Last) THEN
                ! empty substring
                CALL qStr%EnQueue('')
            ELSE
                ! non-empty substring
                CALL qStr%EnQueue(cStr(First:Last))
            END IF
        END DO
    END IF

    RETURN

END SUBROUTINE SplitSepSub

!******************************************************************************

MODULE SUBROUTINE SplitSepChr(cStr,SepSet,qStr,SepChr)

    ! PURPOSE OF THIS ROUTINE:
    !  To split a character string into multiple substrings where the separator
    !  is a single character (any character in the specified set of characters).
    !  Return a queue of substrings where a substring may have zero length (i.e.
    !  empty string).  Also, return an array of separators.
    ! If the separator is not found, return an empty queue and a zero-length array.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStr         !! character string
    tCharStar,          INTENT(IN)  :: SepSet       !! set of characters representing valid separators
    TYPE(QueueChar),    INTENT(OUT) :: qStr         !! queue of substrings
    tChar, ALLOCATABLE, INTENT(OUT) :: SepChr(:)    !! separators found

    ! SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: CharSet = TrueVal

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: nCount       ! number of occurrences of separator found
    tIndex, ALLOCATABLE :: sPos(:)      ! positions of the separator found
    tIndex              :: I            ! index
    tIndex              :: first, Last  ! indices of current substring

    ! FLOW:

    ! find number of occurrences of the separator found
    nCount = FindSeparators(cStr, SepSet, CharSet, sPos)
    
    ! get a queue of substrings if nCount > 0
    IF (nCount > 0) THEN
        ! allocate memory for separators
        ALLOCATE(SepChr(nCount))
        DO I = 1, nCount + 1
            ! get current separator and indices of current substring
            IF (I == 1) THEN
                SepChr(I) = cStr(sPos(I):sPos(I))
                ! the first substring
                First = 1
                Last  = sPos(I) - 1
            ELSEIF (I == nCount + 1) THEN
                ! the last substring
                First = sPos(I-1) + 1
                Last  = LEN(cStr)
            ELSE
                SepChr(I) = cStr(sPos(I):sPos(I))
                ! the substring in the middle
                First = sPos(I-1) + 1
                Last  = sPos(I) - 1
            END IF
            ! add current substring to the queue
            IF (First < Last) THEN
                ! empty substring
                CALL qStr%EnQueue('')
            ELSE
                ! non-empty substring
                CALL qStr%EnQueue(cStr(First:Last))
            END IF
        END DO
    ELSE
        ALLOCATE(SepChr(0))
    END IF

    RETURN

END SUBROUTINE SplitSepChr

!******************************************************************************

MODULE SUBROUTINE SplitSepSubProtect(cStr,SepSub,qStr,ExclMrk)

    ! PURPOSE OF THIS ROUTINE:
    ! To split a character string into multiple substrings where the specified
    ! separator is a multiple-character string found in unprotected regions. 
    ! Return a queue of substrings where a substring may have zero length (i.e.
    ! empty string).  If the separator is not found, return an empty queue.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStr     !! character string
    tCharStar,          INTENT(IN)  :: SepSub   !! multiple-character separator
    TYPE(QueueChar),    INTENT(OUT) :: qStr     !! queue of substrings
    tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.

    ! SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: CharSet = FalseVal

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: nCount       ! number of occurrences of separator found
    tIndex, ALLOCATABLE :: sPos(:)      ! positions of the separator found
    tIndex              :: I            ! index
    tIndex              :: sLen         ! length of separator
    tIndex              :: first, Last  ! indices of current substring

    ! FLOW:

    ! find number of occurrences of the separator found
    nCount = FindSeparatorsProtect(cStr, SepSub, CharSet, sPos, ExclMrk)
    
    ! get a queue of substrings if nCount > 0
    IF (nCount > 0) THEN
        sLen = LEN(SepSub)
        DO I = 1, nCount + 1
            ! get indices of current substring
            IF (I == 1) THEN
                ! the first substring
                First = 1
                Last  = sPos(I) - 1
            ELSEIF (I == nCount + 1) THEN
                ! the last substring
                First = sPos(I-1) + sLen
                Last  = LEN(cStr)
            ELSE
                ! the substring in the middle
                First = sPos(I-1) + sLen
                Last  = sPos(I) - 1
            END IF
            ! add current substring to the queue
            IF (First < Last) THEN
                ! empty substring
                CALL qStr%EnQueue('')
            ELSE
                ! non-empty substring
                CALL qStr%EnQueue(cStr(First:Last))
            END IF
        END DO
    END IF

    RETURN

END SUBROUTINE SplitSepSubProtect

!******************************************************************************

MODULE SUBROUTINE SplitSepChrProtect(cStr,SepSet,qStr,SepChr,ExclMrk)

    ! PURPOSE OF THIS ROUTINE:
    !  To split a character string into multiple substrings where the separator
    !  is a single character (any character in the specified set of characters)
    !  found in unprotected regions.
    !  Return a queue of substrings where a substring may have zero length (i.e.
    !  empty string).  Also, return an array of separators.
    !  If the separator is not found, return an empty queue and a zero-length array.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tCharStar,          INTENT(IN)  :: cStr         !! character string
    tCharStar,          INTENT(IN)  :: SepSet       !! set of characters representing valid separators
    TYPE(QueueChar),    INTENT(OUT) :: qStr         !! queue of substrings
    tChar, ALLOCATABLE, INTENT(OUT) :: SepChr(:)    !! separators found
    tLogical, OPTIONAL, INTENT(IN)  :: ExclMrk
    !^ flag indicating whether the exclamation is used to define a protected
    !  region or not. <br>
    !  - If true, both the exclamation mark and quotes are used. <br>
    !  - If false, only quotes are used. <br>
    !  Default is true.

    ! SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: CharSet = TrueVal

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex              :: nCount       ! number of occurrences of separator found
    tIndex, ALLOCATABLE :: sPos(:)      ! positions of the separator found
    tIndex              :: I            ! index
    tIndex              :: first, Last  ! indices of current substring

    ! FLOW:

    ! find number of occurrences of the separator found
    nCount = FindSeparatorsProtect(cStr, SepSet, CharSet, sPos, ExclMrk)
    
    ! get a queue of substrings if nCount > 0
    IF (nCount > 0) THEN
        ! allocate memory for separators
        ALLOCATE(SepChr(nCount))
        DO I = 1, nCount + 1
            ! get current separator and indices of current substring
            IF (I == 1) THEN
                SepChr(I) = cStr(sPos(I):sPos(I))
                ! the first substring
                First = 1
                Last  = sPos(I) - 1
            ELSEIF (I == nCount + 1) THEN
                ! the last substring
                First = sPos(I-1) + 1
                Last  = LEN(cStr)
            ELSE
                SepChr(I) = cStr(sPos(I):sPos(I))
                ! the substring in the middle
                First = sPos(I-1) + 1
                Last  = sPos(I) - 1
            END IF
            ! add current substring to the queue
            IF (First < Last) THEN
                ! empty substring
                CALL qStr%EnQueue('')
            ELSE
                ! non-empty substring
                CALL qStr%EnQueue(cStr(First:Last))
            END IF
        END DO
    ELSE
        ALLOCATE(SepChr(0))
    END IF

    RETURN

END SUBROUTINE SplitSepChrProtect

!******************************************************************************

END SUBMODULE SubBase_ChrStr_Manipulation

!******************************************************************************
