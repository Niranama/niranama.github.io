
SUBMODULE (ModBase_ChrStr) SubBase_Character

!^ **PURPOSE OF THIS SUBMODULE**: <br>
!   This submodule contains an implementation of *character* procedures (i.e. routines
!   for a single character).

!** USE STATEMENTS:
    USE ModBase_Error_Handlers

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
!                              CHARACTER PROCEDURES
!--------------------------------------------------------------------------------------

MODULE FUNCTION IsCharacterInClass(Chr,ClassType,FailIndex) RESULT(ClassFlag)

    ! PURPOSE OF THIS FUNCTION:
    ! To check whether a given character is in the specified class.
    !
    ! The following character classes are recognized:
    !   ALPHABET
    !       The given character is a valid letter [a-zA-Z].
    !   ALPHANUM
    !       The given character is a valid letter or digit [a-zA-Z0-9].
    !   ASCII
    !       The given character is a valid ASCII character.
    !   BLANK
    !       The given character is a valid blank character, that is blank space or tab.
    !   CONTROL
    !       The given character is a valid control character where control
    !       characters are in the ranges 00..1F and 7F..9F, that is from ASCII #0 to #31 and
    !       from #127 to #159.
    !   DIGIT
    !       The given character is a valid digit [0-9].
    !   GRAPHICAL
    !       The given character is a valid graphical character not including space  
    !       that is from ASCII #33 to #126.
    !   LOGICAL
    !       The given character is a valid logical value, that is 't', 'T', 'f', and 'F'.
    !   LOWERCASE
    !       The given character is a valid lower-case letter, that is [a-z].
    !   PUNCTUATION
    !       The given character is a valid punctuation character, that is _,;:.?![](){}@"'
    !   PRINTABLE
    !       The given character is a valid printable character including space  
    !       that is from ASCII #32 to #126.
    !   UPPERCASE
    !       The given character is a valid upper-case letter, that is [A-Z].
    !   WHITESPACE
    !       The given character is a valid white space, that is space, tab, vertical tab, 
    !       form-feed, newline or carriage return.
    !   HEXDIGIT
    !       The given character is a valid hexadecimal digit characters
    !       that is [0-9A-Fa-f].
    !   OCTDIGIT
    !       The given character is a valid octal digit characters
    !       that is [0-9A-Fa-f].

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tChar,            INTENT(IN)    :: Chr          ! character
    tCharStar,        INTENT(IN)    :: ClassType    ! character class
    tIndex, OPTIONAL, INTENT(OUT)   :: FailIndex    ! use for unrecognized class
    tLogical                        :: ClassFlag    ! true if the character is in the specified class

    ! SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: ToUpper = TrueVal
    
    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tCharLen(LEN(ClassType))    :: TypeOfClass
    tIndex                      :: FailID

    ! FLOW:
    
    ! check input
    IF (.NOT.IsStringInCharacterSet(ClassType, SET_ALPHABETS, FailID)) THEN
        ClassFlag = FalseVal
        IF (PRESENT(FailIndex)) FailIndex = 0
        CALL Handle_ErrLevel('IsCharacterInClass', ModName, ErrWarning, &
                          'ClassType must contain letters only.')
        RETURN
    END IF
    
    IF (PRESENT(FailIndex)) FailIndex = 1
    
    ! convert ClassType to uppercase
    TypeOfClass = ClassType
    CALL ChangeCase(TypeOfClass, ToUpper)
    
    SELECT CASE (TypeOfClass)
    CASE ('ALPHABET')
        ClassFlag = (INDEX(SET_ALPHABETS, Chr) /= 0)
    CASE ('ALPHANUM')
        ClassFlag = (INDEX(SET_ALPHANUM, Chr) /= 0)
    CASE ('ASCII')
        ClassFlag = IsCharacterInASCHIIRange(Chr, 0, 127)
    CASE ('BLANK')
        ClassFlag = (INDEX(SET_BLANKS, Chr) /= 0)
    CASE ('CONTROL')
        ClassFlag = (IsCharacterInASCHIIRange(Chr, 0, 31).OR. &
                     IsCharacterInASCHIIRange(Chr, 127, 159))
    CASE ('DIGIT')
        ClassFlag = (INDEX(SET_DIGITS, Chr) /= 0)
    CASE ('GRAPHICAL')
        ClassFlag = IsCharacterInASCHIIRange(Chr, 33, 126)
    CASE ('LOGICAL')
        SELECT CASE (Chr)
        CASE ('T', 't', 'F', 'f')
            ClassFlag = TrueVal
        CASE DEFAULT
            ClassFlag = FalseVal
        END SELECT
    CASE ('LOWERCASE')
        ClassFlag = (INDEX(SET_ALPHABETS_LOWER, Chr) /= 0)
    CASE ('PRINTABLE')
        ClassFlag = IsCharacterInASCHIIRange(Chr, 32, 126)
    CASE ('PUNCTUATION')
        ClassFlag = (INDEX(SET_PUNCTUATIONS, Chr) /= 0)
    CASE ('UPPERCASE')
        ClassFlag = (INDEX(SET_ALPHABETS_UPPER, Chr) /= 0)
    CASE ('WHITESPACE')
        ClassFlag = (INDEX(SET_WHITESPACES, Chr) /= 0)
    CASE ('HEXDIGIT')
        ClassFlag = (INDEX(SET_HEXDIGITS, Chr) /= 0)
    CASE ('OCTDIGIT')
        ClassFlag = (INDEX(SET_OCTDIGITS, Chr) /= 0)
    CASE DEFAULT
        ClassFlag = FalseVal
        IF (PRESENT(FailIndex)) FailIndex = 0
        CALL Handle_ErrLevel('IsCharacterInClass', ModName, ErrWarning, &
                          'The specified ' // ClassType // ' class is NOT recognized.')
    END SELECT

    RETURN

CONTAINS

    FUNCTION IsCharacterInASCHIIRange(Chr, ASCII_Min, ASCII_Max) RESULT(Flag)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To check whether the given character is in the given ASCII range
        ! from ASCII_Min to ASCII_Max

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tChar,  INTENT(IN)  :: Chr          ! character
        tIndex, INTENT(IN)  :: ASCII_Min    ! minimum ASHCII index
        tIndex, INTENT(IN)  :: ASCII_Max    ! maximum ASHCII index
        tLogical            :: Flag         ! True if the character is in the range

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: ChrIndex     ! character index

    !** FLOW:
    
        ChrIndex = IACHAR(Chr)
        Flag = ((ChrIndex >= ASCII_Min).AND.(ChrIndex <= ASCII_Max))

        RETURN

    END FUNCTION IsCharacterInASCHIIRange

    !**************************************************************************************

END FUNCTION IsCharacterInClass

!**************************************************************************************

MODULE ELEMENTAL SUBROUTINE ChangeCaseCharacter(Chr,ToUpper)

    ! PURPOSE OF THIS FUNCTION:
    ! To change case of the given character according to flag.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tChar,    INTENT(INOUT) :: Chr      ! character
    tLogical, INTENT(IN)    :: ToUpper  ! true if requesting an uppercase character
                                        ! false if requesting a lowercase character

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    tIndex      :: ID

    ! FLOW:
    
    IF (ToUpper) THEN
        ! change to uppercase
        ID = INDEX(SET_ALPHABETS_LOWER, Chr)
        IF (ID > 0) Chr = SET_ALPHABETS_UPPER(ID:ID)
    ELSE
        ! change to lowercase
        ID = INDEX(SET_ALPHABETS_UPPER, Chr)
        IF (ID > 0) Chr = SET_ALPHABETS_LOWER(ID:ID)
    END IF
    
    RETURN

END SUBROUTINE ChangeCaseCharacter

!******************************************************************************

MODULE FUNCTION CharacterDescription(Chr) RESULT(Description)

    ! PURPOSE OF THIS FUNCTION:
    ! To provide a description of the given character.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tChar, INTENT(IN)   :: Chr          ! character
    tCharAlloc          :: Description  ! description of the character

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! FLOW:
    
    SELECT CASE (IACHAR(Chr))
    CASE(0)
        Description = '(NUL) Null Character'
    CASE(1)
        Description = '(SOH) Start of Heading'
    CASE(2)
        Description = '(STX) Start of Text'
    CASE(3)
        Description = '(ETX) End of Text'
    CASE(4)
        Description = '(EOT) End of Transmission'
    CASE(5)
        Description = '(ENQ) Inquiry'
    CASE(6)
        Description = '(ACK) Acknowledge'
    CASE(7)
        Description = '(BEL) Bell'
    CASE(8)
        Description = '(BS) Backspace'
    CASE(9)
        Description = '(HT) Horizontal Tab'
    CASE(10)
        Description = '(LF) Line Feed'
    CASE(11)
        Description = '(VT) Vertical Tab'
    CASE(12)
        Description = '(FF) Form Feed'
    CASE(13)
        Description = '(CR) Carriage Return'
    CASE(14)
        Description = '(SO) Shift Out'
    CASE(15)
        Description = '(SI) Shift In'
    CASE(16)
        Description = '(DLE) Data Link Escape'
    CASE(17)
        Description = '(DC1) Device Control 1'
    CASE(18)
        Description = '(DC2) Device Control 2'
    CASE(19)
        Description = '(DC3) Device Control 3'
    CASE(20)
        Description = '(DC4) Device Control 4'
    CASE(21)
        Description = '(NAK) Negative Acknowledge'
    CASE(22)
        Description = '(SYN) Synchronous Idle'
    CASE(23)
        Description = '(ETB) End of Transmission Block'
    CASE(24)
        Description = '(CAN) Cancel'
    CASE(25)
        Description = '(EM) End of Medium'
    CASE(26)
        Description = '(SUB) Substitute'
    CASE(27)
        Description = '(ESC) Escape'
    CASE(28)
        Description = '(FS) File Separator'
    CASE(29)
        Description = '(GS) Group Separator'
    CASE(30)
        Description = '(RS) Record Separator'
    CASE(31)
        Description = '(US) Unit Separator'
    CASE(32)
        Description = 'Space'
    CASE(33)
        Description = '! Exclamation Mark'
    CASE(34)
        Description = '" (Double) Quotation Marks'
    CASE(35)
        Description = '# Number Sign (or Hashtag)'
    CASE(36)
        Description = '$ Currency Symbol'
    CASE(37)
        Description = '% Percent'
    CASE(38)
        Description = '& Ampersand'
    CASE(39)
        Description = "' Apostrophe (or Single Quotation Mark)"
    CASE(40)
        Description = '( Left (or Open) Parenthesis'
    CASE(41)
        Description = ') Right (or Close) Parenthesis'
    CASE(42)
        Description = '* Asterisk'
    CASE(43)
        Description = '+ Plus'
    CASE(44)
        Description = ', Comma'
    CASE(45)
        Description = '- Hyphen (or Minus Sign)'
    CASE(46)
        Description = '. Period (Dot or Full Stop)'
    CASE(47)
        Description = '/ Slash'
    CASE(48)
        Description = '0 Zero'
    CASE(49)
        Description = '1 One'
    CASE(50)
        Description = '2 Two'
    CASE(51)
        Description = '3 Three'
    CASE(52)
        Description = '4 Four'
    CASE(53)
        Description = '5 Five'
    CASE(54)
        Description = '6 Six'
    CASE(55)
        Description = '7 Seven'
    CASE(56)
        Description = '8 Eight'
    CASE(57)
        Description = '9 Nine'
    CASE(58)
        Description = ': Colon'
    CASE(59)
        Description = '; Semicolon'
    CASE(60)
        Description = '< Less Than (or Open Angled Bracket)'
    CASE(61)
        Description = '= Equals'
    CASE(62)
        Description = '> Greater Than (or Close Angled Bracket)'
    CASE(63)
        Description = '? Question Mark'
    CASE(64)
        Description = '@ At Sign'
    CASE(65)
        Description = 'A Uppercase A'
    CASE(66)
        Description = 'B Uppercase B'
    CASE(67)
        Description = 'C Uppercase C'
    CASE(68)
        Description = 'D Uppercase D'
    CASE(69)
        Description = 'E Uppercase E'
    CASE(70)
        Description = 'F Uppercase F'
    CASE(71)
        Description = 'G Uppercase G'
    CASE(72)
        Description = 'H Uppercase H'
    CASE(73)
        Description = 'I Uppercase I'
    CASE(74)
        Description = 'J Uppercase J'
    CASE(75)
        Description = 'K Uppercase K'
    CASE(76)
        Description = 'L Uppercase L'
    CASE(77)
        Description = 'M Uppercase M'
    CASE(78)
        Description = 'N Uppercase N'
    CASE(79)
        Description = 'O Uppercase O'
    CASE(80)
        Description = 'P Uppercase P'
    CASE(81)
        Description = 'Q Uppercase Q'
    CASE(82)
        Description = 'R Uppercase R'
    CASE(83)
        Description = 'S Uppercase S'
    CASE(84)
        Description = 'T Uppercase T'
    CASE(85)
        Description = 'U Uppercase U'
    CASE(86)
        Description = 'V Uppercase V'
    CASE(87)
        Description = 'W Uppercase W'
    CASE(88)
        Description = 'X Uppercase X'
    CASE(89)
        Description = 'Y Uppercase Y'
    CASE(90)
        Description = 'Z Uppercase Z'
    CASE(91)
        Description = '[ Left (or Open Square) Bracket'
    CASE(92)
        Description = '\ Backslash'
    CASE(93)
        Description = '] Right (or Close Square) Bracket'
    CASE(94)
        Description = '^ Caret (or Circumflex or Hat Operator)'
    CASE(95)
        Description = '_ Underscore'
    CASE(96)
        Description = '` Grave Accent'
    CASE(97)
        Description = 'a Lowercase a'
    CASE(98)
        Description = 'b Lowercase b'
    CASE(99)
        Description = 'c Lowercase c'
    CASE(100)
        Description = 'd Lowercase d'
    CASE(101)
        Description = 'e Lowercase e'
    CASE(102)
        Description = 'f Lowercase f'
    CASE(103)
        Description = 'g Lowercase g'
    CASE(104)
        Description = 'h Lowercase h'
    CASE(105)
        Description = 'i Lowercase i'
    CASE(106)
        Description = 'j Lowercase j'
    CASE(107)
        Description = 'k Lowercase k'
    CASE(108)
        Description = 'l Lowercase l'
    CASE(109)
        Description = 'm Lowercase m'
    CASE(110)
        Description = 'n Lowercase n'
    CASE(111)
        Description = 'o Lowercase o'
    CASE(112)
        Description = 'p Lowercase p'
    CASE(113)
        Description = 'q Lowercase q'
    CASE(114)
        Description = 'r Lowercase r'
    CASE(115)
        Description = 's Lowercase s'
    CASE(116)
        Description = 't Lowercase t'
    CASE(117)
        Description = 'u Lowercase u'
    CASE(118)
        Description = 'v Lowercase v'
    CASE(119)
        Description = 'w Lowercase w'
    CASE(120)
        Description = 'x Lowercase x'
    CASE(121)
        Description = 'y Lowercase y'
    CASE(122)
        Description = 'z Lowercase z'
    CASE(123)
        Description = '{ Left Brace (or Open Curly Bracket)'
    CASE(124)
        Description = '| Vertical Bar'
    CASE(125)
        Description = '} Right Brace (or Close Curly Bracket)'
    CASE(126)
        Description = '~ Tilde (or Equivalency Sign)'
    CASE(127)
        Description = '(DEL) Delete'
    CASE DEFAULT
        Description = 'Unknown character: "' // Chr // '"'
    END SELECT

    RETURN

END FUNCTION CharacterDescription

!**************************************************************************************

END SUBMODULE SubBase_Character

!******************************************************************************
