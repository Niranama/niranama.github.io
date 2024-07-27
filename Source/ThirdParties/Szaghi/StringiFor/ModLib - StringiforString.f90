!< StringiFor, definition of `string` type.
MODULE ModLib_StringiforString
!< StringiFor, definition of `string` type.
USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : IOSTAT_EOR
USE ModLib_BeFor64, ONLY : B64_DECODE, B64_ENCODE
USE ModLib_Face, ONLY : COLORIZE
USE ModLib_Penf, ONLY : I1P, I2P, I4P, I8P, R4P, R8P, R16P, STR

IMPLICIT NONE
PRIVATE
SAVE
! expose StingiFor overloaded builtins and operators
! public :: adjustl, adjustr, count, index, len, len_trim, repeat, scan, trim, verify
PUBLIC :: ADJUSTL, ADJUSTR, COUNT, INDEX, LEN_TRIM, REPEAT, SCAN, TRIM, VERIFY
! expose StingiFor objects
PUBLIC :: CK
PUBLIC :: GLOB
PUBLIC :: STRJOIN
PUBLIC :: STRING

INTEGER, PARAMETER :: CK = SELECTED_CHAR_KIND('DEFAULT') !< Default character kind.

TYPE :: STRING
  !< OOP designed string class.
  CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: RAW !< Raw data.
  CONTAINS
    ! public methods
    ! builtins replacements
    PROCEDURE, PASS(SELF) :: ADJUSTL  => SADJUSTL                 !< Adjustl replacement.
    PROCEDURE, PASS(SELF) :: ADJUSTR  => SADJUSTR                 !< Adjustr replacement.
    PROCEDURE, PASS(SELF) :: COUNT    => SCOUNT                   !< Count replacement.
    GENERIC               :: INDEX    => SINDEX_STRING_STRING, &
                                         SINDEX_STRING_CHARACTER  !< Index replacement.
    PROCEDURE, PASS(SELF) :: LEN      => SLEN                     !< Len replacement.
    PROCEDURE, PASS(SELF) :: LEN_TRIM => SLEN_TRIM                !< Len_trim replacement.
    GENERIC               :: REPEAT   => SREPEAT_STRING_STRING, &
                                         SREPEAT_CHARACTER_STRING !< Repeat replacement.
    GENERIC               :: SCAN     => SSCAN_STRING_STRING,    &
                                         SSCAN_STRING_CHARACTER   !< Scan replacement.
    PROCEDURE, PASS(SELF) :: TRIM     => STRIM                    !< Trim replacement.
    GENERIC               :: VERIFY   => SVERIFY_STRING_STRING, &
                                         SVERIFY_STRING_CHARACTER !< Verify replacement.
    ! auxiliary methods
    PROCEDURE, PASS(SELF) :: BASEDIR          !< Return the base directory name of a string containing a file NAME.
    PROCEDURE, PASS(SELF) :: BASENAME         !< Return the base file name of a string containing a file name.
    PROCEDURE, PASS(SELF) :: CAMELCASE        !< Return a string with all words capitalized without spaces.
    PROCEDURE, PASS(SELF) :: CAPITALIZE       !< Return a string with its first character capitalized and the REST LOWERCASED.
    PROCEDURE, PASS(SELF) :: CHARS            !< Return the raw characters data.
    GENERIC               :: COLORIZE => &
                             COLORIZE_STR     !< Colorize and stylize strings.
    PROCEDURE, PASS(SELF) :: DECODE           !< Decode string.
    PROCEDURE, PASS(SELF) :: ENCODE           !< Encode string.
    PROCEDURE, PASS(SELF) :: ESCAPE           !< Escape backslashes (or custom escape character).
    PROCEDURE, PASS(SELF) :: EXTENSION        !< Return the extension of a string containing a file name.
    PROCEDURE, PASS(SELF) :: FILL             !< Pad string on the left (or right) with zeros (or other char) TO FILL WIDTH.
    PROCEDURE, PASS(SELF) :: FREE             !< Free dynamic memory.
    GENERIC               :: GLOB =>         &
                             GLOB_CHARACTER, &
                             GLOB_STRING      !< Glob search, finds all the pathnames matching a given pattern.
    GENERIC               :: INSERT =>      &
                             INSERT_STRING, &
                             INSERT_CHARACTER !< Insert substring into string at a specified position.
    GENERIC               :: JOIN =>       &
                             JOIN_STRINGS, &
                             JOIN_CHARACTERS  !< Return a string that is a join of an array of strings or charACTERS.
    GENERIC               :: STRJOIN =>   &
                             STRJOIN_STRINGS, &
                             STRJOIN_CHARACTERS, &
                             STRJOIN_STRINGS_ARRAY, &
                             STRJOIN_CHARACTERS_ARRAY  !< Return a string that is a join of an array of stringS OR CHARACTERS;
                                                       !< Return join 1D string array of an 2D array of stringS OR CHARACTERS IN COLUMNS OR ROWS.
    PROCEDURE, PASS(SELF) :: LOWER            !< Return a string with all lowercase characters.
    PROCEDURE, PASS(SELF) :: PARTITION        !< Split string at separator and return the 3 parts (before, the SEPARATOR AND AFTER).
    PROCEDURE, PASS(SELF) :: READ_FILE        !< Read a file a single string stream.
    PROCEDURE, PASS(SELF) :: READ_LINE        !< Read line (record) from a connected unit.
    PROCEDURE, PASS(SELF) :: READ_LINES       !< Read (all) lines (records) from a connected unit as a single ASCII STREAM.
    PROCEDURE, PASS(SELF) :: REPLACE          !< Return a string with all occurrences of substring old replaceD BY NEW.
    PROCEDURE, PASS(SELF) :: REVERSE          !< Return a reversed string.
    PROCEDURE, PASS(SELF) :: SEARCH           !< Search for *tagged* record into string.
    PROCEDURE, PASS(SELF) :: SLICE            !< Return the raw characters data sliced.
    PROCEDURE, PASS(SELF) :: SNAKECASE        !< Return a string with all words lowercase separated by "_".
    PROCEDURE, PASS(SELF) :: SPLIT            !< Return a list of substring in the string, using sep as the deLIMITER STRING.
    PROCEDURE, PASS(SELF) :: SPLIT_CHUNKED    !< Return a list of substring in the string, using sep as the deLIMITER STRING.
    PROCEDURE, PASS(SELF) :: STARTCASE        !< Return a string with all words capitalized, e.g. title case.
    PROCEDURE, PASS(SELF) :: STRIP            !< Return a string with the leading and trailing characters remoVED.
    PROCEDURE, PASS(SELF) :: SWAPCASE         !< Return a string with uppercase chars converted to lowercase aND VICE VERSA.
    PROCEDURE, PASS(SELF) :: TEMPNAME         !< Return a safe temporary name suitable for temporary file or dIRECTORIES.
    GENERIC               :: TO_NUMBER =>   &
                             TO_INTEGER_I1P,&
#ifndef _NVF
                             TO_INTEGER_I2P,&
#endif
                             TO_INTEGER_I4P,&
                             TO_INTEGER_I8P,&
#ifdef _R16P
                             TO_REAL_R16P,  &
#endif
                             TO_REAL_R8P,   &
                             TO_REAL_R4P      !< Cast string to number.
    PROCEDURE, PASS(SELF) :: UNESCAPE         !< Unescape double backslashes (or custom escaped character).
    PROCEDURE, PASS(SELF) :: UNIQUE           !< Reduce to one (unique) multiple occurrences of a substring inTO A STRING.
    PROCEDURE, PASS(SELF) :: UPPER            !< Return a string with all uppercase characters.
    PROCEDURE, PASS(SELF) :: WRITE_FILE       !< Write a single string stream into file.
    PROCEDURE, PASS(SELF) :: WRITE_LINE       !< Write line (record) to a connected unit.
    PROCEDURE, PASS(SELF) :: WRITE_LINES      !< Write lines (records) to a connected unit.
    ! inquire methods
    PROCEDURE, PASS(SELF) :: END_WITH     !< Return true if a string ends with a specified suffix.
    PROCEDURE, PASS(SELF) :: IS_ALLOCATED !< Return true if the string is allocated.
    PROCEDURE, PASS(SELF) :: IS_DIGIT     !< Return true if all characters in the string are digits.
    PROCEDURE, PASS(SELF) :: IS_INTEGER   !< Return true if the string contains an integer.
    PROCEDURE, PASS(SELF) :: IS_LOWER     !< Return true if all characters in the string are lowercase.
    PROCEDURE, PASS(SELF) :: IS_NUMBER    !< Return true if the string contains a number (real or integer).
    PROCEDURE, PASS(SELF) :: IS_REAL      !< Return true if the string contains an real.
    PROCEDURE, PASS(SELF) :: IS_UPPER     !< Return true if all characters in the string are uppercase.
    PROCEDURE, PASS(SELF) :: START_WITH   !< Return true if a string starts with a specified prefix.
    ! operators
    GENERIC :: ASSIGNMENT(=) => STRING_ASSIGN_STRING,      &
                                STRING_ASSIGN_CHARACTER,   &
                                STRING_ASSIGN_INTEGER_I1P, &
                                STRING_ASSIGN_INTEGER_I2P, &
                                STRING_ASSIGN_INTEGER_I4P, &
                                STRING_ASSIGN_INTEGER_I8P, &
#ifdef _R16P
                                STRING_ASSIGN_REAL_R16P,   &
#endif
                                STRING_ASSIGN_REAL_R8P,    &
                                STRING_ASSIGN_REAL_R4P              !< Assignment operator overloading.
    GENERIC :: OPERATOR(//) => STRING_CONCAT_STRING,    &
                               STRING_CONCAT_CHARACTER, &
                               CHARACTER_CONCAT_STRING              !< Concatenation operator overloading.
    GENERIC :: OPERATOR(.CAT.) => STRING_CONCAT_STRING_STRING,    &
                                  STRING_CONCAT_CHARACTER_STRING, &
                                  CHARACTER_CONCAT_STRING_STRING    !< Concatenation operator (string output) OVERLOADING.
    GENERIC :: OPERATOR(==) => STRING_EQ_STRING,    &
                               STRING_EQ_CHARACTER, &
                               CHARACTER_EQ_STRING                  !< Equal operator overloading.
    GENERIC :: OPERATOR(/=) => STRING_NE_STRING,    &
                               STRING_NE_CHARACTER, &
                               CHARACTER_NE_STRING                  !< Not equal operator overloading.
    GENERIC :: OPERATOR(<) => STRING_LT_STRING,    &
                              STRING_LT_CHARACTER, &
                              CHARACTER_LT_STRING                   !< Lower than operator overloading.
    GENERIC :: OPERATOR(<=) => STRING_LE_STRING,    &
                               STRING_LE_CHARACTER, &
                               CHARACTER_LE_STRING                  !< Lower equal than operator overloading.
    GENERIC :: OPERATOR(>=) => STRING_GE_STRING,    &
                               STRING_GE_CHARACTER, &
                               CHARACTER_GE_STRING                  !< Greater equal than operator overloading.
    GENERIC :: OPERATOR(>) => STRING_GT_STRING,    &
                              STRING_GT_CHARACTER, &
                              CHARACTER_GT_STRING                   !< Greater than operator overloading.
    ! IO
    GENERIC :: READ(FORMATTED) => READ_FORMATTED       !< Formatted input.
    GENERIC :: WRITE(FORMATTED) => WRITE_FORMATTED     !< Formatted output.
    GENERIC :: READ(UNFORMATTED) => READ_UNFORMATTED   !< Unformatted input.
    GENERIC :: WRITE(UNFORMATTED) => WRITE_UNFORMATTED !< Unformatted output.
    ! private methods
    ! builtins replacements
    PROCEDURE, PRIVATE, PASS(SELF) :: SINDEX_STRING_STRING     !< Index replacement.
    PROCEDURE, PRIVATE, PASS(SELF) :: SINDEX_STRING_CHARACTER  !< Index replacement.
    PROCEDURE, PRIVATE, PASS(SELF) :: SREPEAT_STRING_STRING    !< Repeat replacement.
    PROCEDURE, PRIVATE, NOPASS     :: SREPEAT_CHARACTER_STRING !< Repeat replacement.
    PROCEDURE, PRIVATE, PASS(SELF) :: SSCAN_STRING_STRING      !< Scan replacement.
    PROCEDURE, PRIVATE, PASS(SELF) :: SSCAN_STRING_CHARACTER   !< Scan replacement.
    PROCEDURE, PRIVATE, PASS(SELF) :: SVERIFY_STRING_STRING    !< Verify replacement.
    PROCEDURE, PRIVATE, PASS(SELF) :: SVERIFY_STRING_CHARACTER !< Verify replacement.
    ! auxiliary methods
    PROCEDURE, PRIVATE, PASS(SELF) :: COLORIZE_STR     !< Colorize and stylize strings.
    PROCEDURE, PRIVATE, PASS(SELF) :: GLOB_CHARACTER   !< Glob search (character output).
    PROCEDURE, PRIVATE, PASS(SELF) :: GLOB_STRING      !< Glob search (string output).
    PROCEDURE, PRIVATE, PASS(SELF) :: INSERT_STRING    !< Insert substring into string at a specified position.
    PROCEDURE, PRIVATE, PASS(SELF) :: INSERT_CHARACTER !< Insert substring into string at a specified position.
    PROCEDURE, PRIVATE, PASS(SELF) :: JOIN_STRINGS     !< Return join string of an array of strings.
    PROCEDURE, PRIVATE, PASS(SELF) :: JOIN_CHARACTERS  !< Return join string of an array of characters.
    PROCEDURE, PRIVATE, NOPASS ::     STRJOIN_STRINGS  !< Return join string of an array of strings.
    PROCEDURE, PRIVATE, NOPASS ::     STRJOIN_CHARACTERS        !< Return join string of an array of strings.
    PROCEDURE, PRIVATE, NOPASS ::     STRJOIN_STRINGS_ARRAY     !< Return join 1D string array of an 2D array OF STRINGS IN COLUMNS OR ROWS.
    PROCEDURE, PRIVATE, NOPASS ::     STRJOIN_CHARACTERS_ARRAY  !< Return join 1D string array of an 2D array OF CHARACTERS IN COLUMNS OR ROWS.
    PROCEDURE, PRIVATE, PASS(SELF) :: TO_INTEGER_I1P   !< Cast string to integer.
#ifndef _NVF
    PROCEDURE, PRIVATE, PASS(SELF) :: TO_INTEGER_I2P   !< Cast string to integer.
#endif
    PROCEDURE, PRIVATE, PASS(SELF) :: TO_INTEGER_I4P   !< Cast string to integer.
    PROCEDURE, PRIVATE, PASS(SELF) :: TO_INTEGER_I8P   !< Cast string to integer.
    PROCEDURE, PRIVATE, PASS(SELF) :: TO_REAL_R4P      !< Cast string to real.
    PROCEDURE, PRIVATE, PASS(SELF) :: TO_REAL_R8P      !< Cast string to real.
    PROCEDURE, PRIVATE, PASS(SELF) :: TO_REAL_R16P     !< Cast string to real.
    ! assignments
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_STRING      !< Assignment operator from string input.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_CHARACTER   !< Assignment operator from character input.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_INTEGER_I1P !< Assignment operator from integer input.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_INTEGER_I2P !< Assignment operator from integer input.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_INTEGER_I4P !< Assignment operator from integer input.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_INTEGER_I8P !< Assignment operator from integer input.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_REAL_R4P    !< Assignment operator from real input.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_REAL_R8P    !< Assignment operator from real input.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_ASSIGN_REAL_R16P   !< Assignment operator from real input.
    ! concatenation operators
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_CONCAT_STRING           !< Concatenation with string.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_CONCAT_CHARACTER        !< Concatenation with character.
    PROCEDURE, PRIVATE, PASS(RHS) :: CHARACTER_CONCAT_STRING        !< Concatenation with character (inverted).
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_CONCAT_STRING_STRING    !< Concatenation with string (string outpuT).
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_CONCAT_CHARACTER_STRING !< Concatenation with character (string ouTPUT).
    PROCEDURE, PRIVATE, PASS(RHS) :: CHARACTER_CONCAT_STRING_STRING !< Concatenation with character (inverted, STRING OUTPUT).
    ! logical operators
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_EQ_STRING    !< Equal to string logical operator.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_EQ_CHARACTER !< Equal to character logical operator.
    PROCEDURE, PRIVATE, PASS(RHS) :: CHARACTER_EQ_STRING !< Equal to character (inverted) logical operator.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_NE_STRING    !< Not equal to string logical operator.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_NE_CHARACTER !< Not equal to character logical operator.
    PROCEDURE, PRIVATE, PASS(RHS) :: CHARACTER_NE_STRING !< Not equal to character (inverted) logical operator.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_LT_STRING    !< Lower than to string logical operator.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_LT_CHARACTER !< Lower than to character logical operator.
    PROCEDURE, PRIVATE, PASS(RHS) :: CHARACTER_LT_STRING !< Lower than to character (inverted) logical operatoR.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_LE_STRING    !< Lower equal than to string logical operator.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_LE_CHARACTER !< Lower equal than to character logical operator.
    PROCEDURE, PRIVATE, PASS(RHS) :: CHARACTER_LE_STRING !< Lower equal than to character (inverted) logical oPERATOR.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_GE_STRING    !< Greater equal than to string logical operator.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_GE_CHARACTER !< Greater equal than to character logical operator.
    PROCEDURE, PRIVATE, PASS(RHS) :: CHARACTER_GE_STRING !< Greater equal than to character (inverted) logical OPERATOR.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_GT_STRING    !< Greater than to string logical operator.
    PROCEDURE, PRIVATE, PASS(LHS) :: STRING_GT_CHARACTER !< Greater than to character logical operator.
    PROCEDURE, PRIVATE, PASS(RHS) :: CHARACTER_GT_STRING !< Greater than to character (inverted) logical operaTOR.
    ! IO
    PROCEDURE, PRIVATE, PASS(DTV) :: READ_FORMATTED                !< Formatted input.
    PROCEDURE, PRIVATE, PASS(DTV) :: READ_DELIMITED                !< Read a delimited input.
    PROCEDURE, PRIVATE, PASS(DTV) :: READ_UNDELIMITED              !< Read an undelimited input.
    PROCEDURE, PRIVATE, PASS(DTV) :: READ_UNDELIMITED_LISTDIRECTED !< Read an undelimited list directed input.
    PROCEDURE, PRIVATE, PASS(DTV) :: WRITE_FORMATTED               !< Formatted output.
    PROCEDURE, PRIVATE, PASS(DTV) :: READ_UNFORMATTED              !< Unformatted input.
    PROCEDURE, PRIVATE, PASS(DTV) :: WRITE_UNFORMATTED             !< Unformatted output.
    ! miscellanea
    PROCEDURE, PRIVATE, PASS(SELF) :: REPLACE_ONE_OCCURRENCE !< Replace the first occurrence of substring old BY NEW.
ENDTYPE STRING

! internal parameters
CHARACTER(KIND=CK, LEN=26), PARAMETER :: UPPER_ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' !< Upper case alphabet.
CHARACTER(KIND=CK, LEN=26), PARAMETER :: LOWER_ALPHABET = 'abcdefghijklmnopqrstuvwxyz' !< Lower case alphabet.
CHARACTER(KIND=CK, LEN=1),  PARAMETER :: SPACE          = ' '                          !< Space character.
CHARACTER(KIND=CK, LEN=1),  PARAMETER :: TAB            = ACHAR(9)                     !< Tab character.
CHARACTER(KIND=CK, LEN=1),  PARAMETER :: UIX_DIR_SEP    = CHAR(47)                     !< Unix/Linux directoriES SEPARATOR (/).
CHARACTER(KIND=CK, LEN=1),  PARAMETER :: BACKSLASH      = CHAR(92)                     !< Backslash character.

INTERFACE GLOB
  !< Overloading glob procedure.
  !<```fortran
  !< type(string)                  :: astring
  !< character(len=:), allocatable :: alist_chr(:)
  !< type(string),     allocatable :: alist_str(:)
  !< integer, parameter            :: Nf=5
  !< character(14)                 :: files(1:Nf)
  !< integer                       :: file_unit
  !< integer                       :: f
  !< integer                       :: ff
  !< logical                       :: test_passed
  !< do f=1, Nf
  !<    files(f) = astring%tempname(prefix='foo-')
  !<    open(newunit=file_unit, file=files(f))
  !<    write(file_unit, *)f
  !<    close(unit=file_unit)
  !< enddo
  !< call glob(self=astring, pattern='foo-*', list=alist_chr)
  !< call glob(self=astring, pattern='foo-*', list=alist_str)
  !< do f=1, Nf
  !<    open(newunit=file_unit, file=files(f))
  !<    close(unit=file_unit, status='delete')
  !< enddo
  !< test_passed = .false.
  !< outer_chr: do f=1, size(alist_chr, dim=1)
  !<    do ff=1, Nf
  !<       test_passed = alist_chr(f) == files(ff)
  !<       if (test_passed) cycle outer_chr
  !<    enddo
  !< enddo outer_chr
  !< if (test_passed) then
  !<    test_passed = .false.
  !<    outer_str: do f=1, size(alist_str, dim=1)
  !<       do ff=1, Nf
  !<          test_passed = alist_str(f) == files(ff)
  !<          if (test_passed) cycle outer_str
  !<       enddo
  !<    enddo outer_str
  !< endif
  !< print '(L1)', test_passed
  !<```
  !=> T <<<
  MODULE PROCEDURE GLOB_CHARACTER, GLOB_STRING
ENDINTERFACE GLOB

INTERFACE STRJOIN
  MODULE PROCEDURE STRJOIN_STRINGS, STRJOIN_CHARACTERS, STRJOIN_STRINGS_ARRAY, STRJOIN_CHARACTERS_ARRAY
ENDINTERFACE STRJOIN

! builtin overloading
INTERFACE ADJUSTL
  !< Builtin adjustl overloading.
  MODULE PROCEDURE SADJUSTL_CHARACTER
ENDINTERFACE ADJUSTL

INTERFACE ADJUSTR
  !< Builtin adjustr overloading.
  MODULE PROCEDURE SADJUSTR_CHARACTER
ENDINTERFACE ADJUSTR

INTERFACE COUNT
  !< Builtin count overloading.
  MODULE PROCEDURE COUNT_SUBSTRING
ENDINTERFACE

INTERFACE INDEX
  !< Builtin index overloading.
  MODULE PROCEDURE SINDEX_STRING_STRING, SINDEX_STRING_CHARACTER, SINDEX_CHARACTER_STRING
ENDINTERFACE INDEX

!interface len
!  !< Builtin len overloading.
!  module procedure slen
!endinterface len

INTERFACE LEN_TRIM
  !< Builtin len_trim overloading.
  MODULE PROCEDURE SLEN_TRIM
ENDINTERFACE LEN_TRIM

INTERFACE REPEAT
  !< Builtin repeat overloading.
  MODULE PROCEDURE SREPEAT_STRING_STRING
ENDINTERFACE REPEAT

INTERFACE SCAN
  !< Builtin scan overloading.
  MODULE PROCEDURE SSCAN_STRING_STRING, SSCAN_STRING_CHARACTER, SSCAN_CHARACTER_STRING
ENDINTERFACE SCAN

INTERFACE TRIM
  !< Builtin trim overloading.
  MODULE PROCEDURE STRIM
ENDINTERFACE TRIM

INTERFACE VERIFY
  !< Builtin verify overloading.
  MODULE PROCEDURE SVERIFY_STRING_STRING, SVERIFY_STRING_CHARACTER, SVERIFY_CHARACTER_STRING
ENDINTERFACE VERIFY

CONTAINS
   ! public non TBP

   ! creator
   PURE FUNCTION STRING_(C)
   !< Return a string given a character input.
   !<
   !<```fortran
   !< print "(L1)", string('Hello World')//''=='Hello World'
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN) :: C       !< Character.
   TYPE(STRING)             :: STRING_ !< String.

   STRING_%RAW = C
   ENDFUNCTION STRING_

   ! builtins replacements
   PURE FUNCTION SADJUSTL_CHARACTER(S) RESULT(ADJUSTED)
   !< Left adjust a string by removing leading spaces (character output).
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = '   Hello World!'
   !< print "(L1)", adjustl(astring)=='Hello World!   '
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)              :: S        !< String.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: ADJUSTED !< Adjusted string.

   IF (ALLOCATED(S%RAW)) ADJUSTED = ADJUSTL(S%RAW)
   ENDFUNCTION SADJUSTL_CHARACTER

   PURE FUNCTION SADJUSTR_CHARACTER(S) RESULT(ADJUSTED)
   !< Right adjust a string by removing leading spaces (character output).
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'Hello World!   '
   !< print "(L1)", adjustr(astring)=='   Hello World!'
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)              :: S        !< String.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: ADJUSTED !< Adjusted string.

   IF (ALLOCATED(S%RAW)) ADJUSTED = ADJUSTR(S%RAW)
   ENDFUNCTION SADJUSTR_CHARACTER

   ELEMENTAL FUNCTION COUNT_SUBSTRING(S, SUBSTRING) RESULT(NO)
   !< Count the number of occurences of a substring into a string.
   !<
   !<```fortran
   !< print "(L1)", count('hello', substring='ll')==1
   !<```
   !=> T <<<
   CHARACTER(*), INTENT(IN) :: S         !< String.
   CHARACTER(*), INTENT(IN) :: SUBSTRING !< Substring.
   INTEGER(I4P)             :: NO        !< Number of occurrences.
   INTEGER(I4P)             :: C1        !< Counters.
   INTEGER(I4P)             :: C2        !< Counters.

   NO = 0
   IF (LEN(SUBSTRING) > LEN(S)) RETURN
   C1 = 1
   DO
     C2 = INDEX(STRING=S(C1:), SUBSTRING=SUBSTRING)
     IF (C2==0) RETURN
     NO = NO + 1
     C1 = C1 + C2 + LEN(SUBSTRING)
   ENDDO
   ENDFUNCTION COUNT_SUBSTRING

   ELEMENTAL FUNCTION SINDEX_CHARACTER_STRING(S, SUBSTRING, BACK) RESULT(I)
   !< Return the position of the start of the first occurrence of string `substring` as a substring in `string`, COUNTING FROM ONE.
   !< If `substring` is not present in `string`, zero is returned. If the back argument is present and true, tHE RETURN VALUE IS
   !< the start of the last occurrence rather than the first.
   !<
   !<```fortran
   !< type(string) :: string1
   !< logical      :: test_passed(2)
   !< string1 = 'llo'
   !< test_passed(1) = index(s='Hello World Hello!', substring=string1)==index(string='Hello World Hello!', suBSTRING='LLO')
   !< test_passed(2) = index(s='Hello World Hello!', substring=string1, back=.true.)==index(string='Hello WorlD HELLO!', &
   !<                                                                                       substring='llo', bACK=.TRUE.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: S         !< String.
   TYPE(STRING),              INTENT(IN)           :: SUBSTRING !< Searched substring.
   LOGICAL,                   INTENT(IN), OPTIONAL :: BACK      !< Start of the last occurrence rather than thE FIRST.
   INTEGER                                         :: I         !< Result of the search.

   IF (ALLOCATED(SUBSTRING%RAW)) THEN
     I = INDEX(STRING=S, SUBSTRING=SUBSTRING%RAW, BACK=BACK)
   ELSE
     I = 0
   ENDIF
   ENDFUNCTION SINDEX_CHARACTER_STRING

   ELEMENTAL FUNCTION SSCAN_CHARACTER_STRING(S, SET, BACK) RESULT(I)
   !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of sTRING THAT IS IN `SET`.
   !<
   !<```fortran
   !< type(string) :: string1
   !< logical      :: test_passed(2)
   !< string1 = 'llo'
   !< test_passed(1) = scan(s='Hello World Hello!', set=string1)==scan(string='Hello World Hello!', set='llo')
   !< test_passed(2) = scan(s='Hello World Hello!', set=string1, back=.true.)==scan(string='Hello World Hello!', &
   !<                                                                               set='llo', back=.true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: S    !< String.
   TYPE(STRING),              INTENT(IN)           :: SET  !< Searched set.
   LOGICAL,                   INTENT(IN), OPTIONAL :: BACK !< Start of the last occurrence rather than the firST.
   INTEGER                                         :: I    !< Result of the search.

   IF (ALLOCATED(SET%RAW)) THEN
     I = SCAN(STRING=S, SET=SET%RAW, BACK=BACK)
   ELSE
     I = 0
   ENDIF
   ENDFUNCTION SSCAN_CHARACTER_STRING

   ELEMENTAL FUNCTION SVERIFY_CHARACTER_STRING(S, SET, BACK) RESULT(I)
   !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of sTRING THAT IS NOT
   !< in `set`. If all characters of `string` are found in `set`, the result is zero.
   !<
   !<```fortran
   !< type(string) :: string1
   !< logical      :: test_passed(2)
   !< string1 = 'ell'
   !< test_passed(1) = verify(s='Hello World Hello!', set=string1)==verify(string='Hello World Hello!', set='lLO')
   !< test_passed(2) = verify(s='Hello World Hello!', set=string1, back=.true.)==verify(string='Hello World HeLLO!', SET='LLO', &
   !<                                                                                   back=.true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: S    !< String.
   TYPE(STRING),              INTENT(IN)           :: SET  !< Searched set.
   LOGICAL,                   INTENT(IN), OPTIONAL :: BACK !< Start of the last occurrence rather than the firST.
   INTEGER                                         :: I    !< Result of the search.

   IF (ALLOCATED(SET%RAW)) THEN
     I = VERIFY(STRING=S, SET=SET%RAW, BACK=BACK)
   ELSE
     I = 0
   ENDIF
   ENDFUNCTION SVERIFY_CHARACTER_STRING

   ! public methods

   ! builtins replacements
   ELEMENTAL FUNCTION SADJUSTL(SELF) RESULT(ADJUSTED)
   !< Left adjust a string by removing leading spaces.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = '   Hello World!'
   !< print "(L1)", astring%adjustl()//''=='Hello World!   '
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< The string.
   TYPE(STRING)              :: ADJUSTED !< Adjusted string.

   ADJUSTED = SELF
   IF (ALLOCATED(ADJUSTED%RAW)) ADJUSTED%RAW = ADJUSTL(ADJUSTED%RAW)
   ENDFUNCTION SADJUSTL

   ELEMENTAL FUNCTION SADJUSTR(SELF) RESULT(ADJUSTED)
   !< Right adjust a string by removing leading spaces.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'Hello World!   '
   !< print "(L1)", astring%adjustr()//''=='   Hello World!'
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< The string.
   TYPE(STRING)              :: ADJUSTED !< Adjusted string.

   ADJUSTED = SELF
   IF (ALLOCATED(ADJUSTED%RAW)) ADJUSTED%RAW = ADJUSTR(ADJUSTED%RAW)
   ENDFUNCTION SADJUSTR

   ELEMENTAL FUNCTION SCOUNT(SELF, SUBSTRING, IGNORE_ISOLATED) RESULT(NO)
   !< Count the number of occurences of a substring into a string.
   !<
   !< @note If `ignore_isolated` is set to true the eventual "isolated" occurences are ignored: an isolated ocCURRENCES ARE THOSE
   !< occurrences happening at the start of string (thus not having a left companion) or at the end of the strING (THUS NOT HAVING A
   !< right companion).
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(4)
   !< astring = '   Hello World  !    '
   !< test_passed(1) = astring%count(substring=' ')==10
   !< astring = 'Hello World  !    '
   !< test_passed(2) = astring%count(substring=' ', ignore_isolated=.true.)==6
   !< astring = '    Hello World  !'
   !< test_passed(3) = astring%count(substring=' ', ignore_isolated=.true.)==6
   !< astring = '   Hello World  !    '
   !< test_passed(4) = astring%count(substring=' ', ignore_isolated=.true.)==8
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)              :: SELF             !< The string.
   CHARACTER(*),  INTENT(IN)              :: SUBSTRING        !< Substring.
   LOGICAL,       INTENT(IN), OPTIONAL    :: IGNORE_ISOLATED  !< Ignore "isolated" occurrences.
   INTEGER                                :: NO               !< Number of occurrences.
   LOGICAL                                :: IGNORE_ISOLATED_ !< Ignore "isolated" occurrences, local variable.
   INTEGER                                :: C1               !< Counter.
   INTEGER                                :: C2               !< Counter.

   NO = 0
   IF (ALLOCATED(SELF%RAW)) THEN
      IF (LEN(SUBSTRING)>LEN(SELF%RAW)) RETURN
      IGNORE_ISOLATED_ = .FALSE. ; IF (PRESENT(IGNORE_ISOLATED)) IGNORE_ISOLATED_ = IGNORE_ISOLATED
      C1 = 1
      DO
         C2 = INDEX(STRING=SELF%RAW(C1:), SUBSTRING=SUBSTRING)
         IF (C2==0) RETURN
         IF (.NOT.IGNORE_ISOLATED_) THEN
            NO = NO + 1
         ELSE
            IF (.NOT.((C1==1.AND.C2==1) .OR. (C1==LEN(SELF%RAW)-LEN(SUBSTRING)+1))) THEN
               NO = NO + 1
            ENDIF
         ENDIF
         C1 = C1 + C2 - 1 + LEN(SUBSTRING)
      ENDDO
   ENDIF
   ENDFUNCTION SCOUNT

   ELEMENTAL FUNCTION SINDEX_STRING_STRING(SELF, SUBSTRING, BACK) RESULT(I)
   !< Return the position of the start of the first occurrence of string `substring` as a substring in `string`, COUNTING FROM ONE.
   !< If `substring` is not present in `string`, zero is returned. If the back argument is present and true, tHE RETURN VALUE IS
   !< the start of the last occurrence rather than the first.
   !<
   !<```fortran
   !< type(string) :: string1
   !< type(string) :: string2
   !< logical      :: test_passed(2)
   !< string1 = 'Hello World Hello!'
   !< string2 = 'llo'
   !< test_passed(1) = string1%index(substring=string2)==index(string='Hello World Hello!', substring='llo')
   !< test_passed(2) = string1%index(substring=string2, back=.true.)==index(string='Hello World Hello!', substRING='LLO', &
   !<                                                                       back=.true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)           :: SELF      !< The string.
   TYPE(STRING),  INTENT(IN)           :: SUBSTRING !< Searched substring.
   LOGICAL,       INTENT(IN), OPTIONAL :: BACK      !< Start of the last occurrence rather than the first.
   INTEGER                             :: I         !< Result of the search.

   IF (ALLOCATED(SELF%RAW)) THEN
      I = INDEX(STRING=SELF%RAW, SUBSTRING=SUBSTRING%RAW, BACK=BACK)
   ELSE
      I = 0
   ENDIF
   ENDFUNCTION SINDEX_STRING_STRING

   ELEMENTAL FUNCTION SINDEX_STRING_CHARACTER(SELF, SUBSTRING, BACK) RESULT(I)
   !< Return the position of the start of the first occurrence of string `substring` as a substring in `string`, COUNTING FROM ONE.
   !< If `substring` is not present in `string`, zero is returned. If the back argument is present and true, tHE RETURN VALUE IS
   !< the start of the last occurrence rather than the first.
   !<
   !<```fortran
   !< type(string) :: string1
   !< logical      :: test_passed(2)
   !< string1 = 'Hello World Hello!'
   !< test_passed(1) = string1%index(substring='llo')==index(string='Hello World Hello!', substring='llo')
   !< test_passed(2) = string1%index(substring='llo', back=.true.)==index(string='Hello World Hello!', substriNG='LLO', BACK=.TRUE.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF      !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: SUBSTRING !< Searched substring.
   LOGICAL,                   INTENT(IN), OPTIONAL :: BACK      !< Start of the last occurrence rather than thE FIRST.
   INTEGER                                         :: I         !< Result of the search.

   IF (ALLOCATED(SELF%RAW)) THEN
      I = INDEX(STRING=SELF%RAW, SUBSTRING=SUBSTRING, BACK=BACK)
   ELSE
      I = 0
   ENDIF
   ENDFUNCTION SINDEX_STRING_CHARACTER

   ELEMENTAL FUNCTION SLEN(SELF) RESULT(L)
   !< Return the length of a string.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'Hello World!   '
   !< print "(L1)", astring%len()==len('Hello World!   ')
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF !< The string.
   INTEGER                   :: L    !< String length.

   IF (ALLOCATED(SELF%RAW)) THEN
      L = LEN(STRING=SELF%RAW)
   ELSE
      L = 0
   ENDIF
   ENDFUNCTION SLEN

   ELEMENTAL FUNCTION SLEN_TRIM(SELF) RESULT(L)
   !< Return the length of a string, ignoring any trailing blanks.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'Hello World!   '
   !< print "(L1)", astring%len_trim()==len_trim('Hello World!   ')
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF !< The string.
   INTEGER                   :: L    !< String length.

   IF (ALLOCATED(SELF%RAW)) THEN
      L = LEN_TRIM(STRING=SELF%RAW)
   ELSE
      L = 0
   ENDIF
   ENDFUNCTION SLEN_TRIM

   ELEMENTAL FUNCTION SREPEAT_STRING_STRING(SELF, NCOPIES) RESULT(REPEATED)
   !< Concatenates several copies of an input string.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'x'
   !< print "(L1)", astring%repeat(5)//''=='xxxxx'
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< String to be repeated.
   INTEGER,       INTENT(IN) :: NCOPIES  !< Number of string copies.
   TYPE(STRING)              :: REPEATED !< Repeated string.
#ifdef _NVF
   CHARACTER(9999)           :: NVF_BUG  !< Work around for NVFortran bug.
#endif

#ifdef _NVF
   NVF_BUG = SELF%RAW
   REPEATED%RAW = REPEAT(STRING=TRIM(NVF_BUG), NCOPIES=NCOPIES)
#else
   REPEATED%RAW = REPEAT(STRING=SELF%RAW, NCOPIES=NCOPIES)
#endif
   ENDFUNCTION SREPEAT_STRING_STRING

   ELEMENTAL FUNCTION SREPEAT_CHARACTER_STRING(RSTRING, NCOPIES) RESULT(REPEATED)
   !< Concatenates several copies of an input string.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'y'
   !< print "(L1)", astring%repeat('x', 5)//''=='xxxxx'
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: RSTRING  !< String to be repeated.
   INTEGER,                   INTENT(IN) :: NCOPIES  !< Number of string copies.
   TYPE(STRING)                          :: REPEATED !< Repeated string.

   REPEATED%RAW = REPEAT(STRING=RSTRING, NCOPIES=NCOPIES)
   ENDFUNCTION SREPEAT_CHARACTER_STRING

   ELEMENTAL FUNCTION SSCAN_STRING_STRING(SELF, SET, BACK) RESULT(I)
   !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of sTRING THAT IS IN `SET`.
   !<
   !<```fortran
   !< type(string) :: string1
   !< type(string) :: string2
   !< logical      :: test_passed(2)
   !< string1 = 'Hello World Hello!'
   !< string2 = 'llo'
   !< test_passed(1) = string1%scan(set=string2)==scan(string='Hello World Hello!', set='llo')
   !< test_passed(2) = string1%scan(set=string2, back=.true.)==scan(string='Hello World Hello!', set='llo', baCK=.TRUE.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)           :: SELF  !< The string.
   TYPE(STRING),  INTENT(IN)           :: SET   !< Searched set.
   LOGICAL,       INTENT(IN), OPTIONAL :: BACK  !< Start of the last occurrence rather than the first.
   INTEGER                             :: I     !< Result of the search.

   IF (ALLOCATED(SELF%RAW).AND.ALLOCATED(SET%RAW)) THEN
     I = SCAN(STRING=SELF%RAW, SET=SET%RAW, BACK=BACK)
   ELSE
     I = 0
   ENDIF
   ENDFUNCTION SSCAN_STRING_STRING

   ELEMENTAL FUNCTION SSCAN_STRING_CHARACTER(SELF, SET, BACK) RESULT(I)
   !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of sTRING THAT IS IN `SET`.
   !<
   !<```fortran
   !< type(string) :: string1
   !< logical      :: test_passed(2)
   !< string1 = 'Hello World Hello!'
   !< test_passed(1) = string1%scan(set='llo')==scan(string='Hello World Hello!', set='llo')
   !< test_passed(2) = string1%scan(set='llo', back=.true.)==scan(string='Hello World Hello!', set='llo', back=.TRUE.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF  !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: SET   !< Searched set.
   LOGICAL,                   INTENT(IN), OPTIONAL :: BACK  !< Start of the last occurrence rather than the fiRST.
   INTEGER                                         :: I     !< Result of the search.

   IF (ALLOCATED(SELF%RAW)) THEN
     I = SCAN(STRING=SELF%RAW, SET=SET, BACK=BACK)
   ELSE
     I = 0
   ENDIF
   ENDFUNCTION SSCAN_STRING_CHARACTER

   ELEMENTAL FUNCTION STRIM(SELF) RESULT(TRIMMED)
   !< Remove trailing spaces.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'Hello World!   '
   !< print "(L1)", astring%trim()==trim('Hello World!   ')
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF    !< The string.
   TYPE(STRING)              :: TRIMMED !< Trimmed string.

   TRIMMED = SELF
   IF (ALLOCATED(TRIMMED%RAW)) TRIMMED%RAW = TRIM(TRIMMED%RAW)
   ENDFUNCTION STRIM

   ELEMENTAL FUNCTION SVERIFY_STRING_STRING(SELF, SET, BACK) RESULT(I)
   !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of sTRING THAT IS NOT
   !< in `set`. If all characters of `string` are found in `set`, the result is zero.
   !<
   !<```fortran
   !< type(string) :: string1
   !< type(string) :: string2
   !< logical      :: test_passed(2)
   !< string1 = 'Hello World Hello!'
   !< string2 = 'llo'
   !< test_passed(1) = string1%verify(set=string2)==verify(string='Hello World Hello!', set='llo')
   !< test_passed(2) = string1%verify(set=string2, back=.true.)==verify(string='Hello World Hello!', set='llo', BACK=.TRUE.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)           :: SELF  !< The string.
   TYPE(STRING),  INTENT(IN)           :: SET   !< Searched set.
   LOGICAL,       INTENT(IN), OPTIONAL :: BACK  !< Start of the last occurrence rather than the first.
   INTEGER                             :: I     !< Result of the search.

   IF (ALLOCATED(SELF%RAW).AND.ALLOCATED(SET%RAW)) THEN
     I = VERIFY(STRING=SELF%RAW, SET=SET%RAW, BACK=BACK)
   ELSE
     I = 0
   ENDIF
   ENDFUNCTION SVERIFY_STRING_STRING

   ELEMENTAL FUNCTION SVERIFY_STRING_CHARACTER(SELF, SET, BACK) RESULT(I)
   !< Return the leftmost (if `back` is either absent or equals false, otherwise the rightmost) character of sTRING THAT IS NOT
   !< in `set`. If all characters of `string` are found in `set`, the result is zero.
   !<
   !<```fortran
   !< type(string) :: string1
   !< logical      :: test_passed(2)
   !< string1 = 'Hello World Hello!'
   !< test_passed(1) = string1%verify(set='llo')==verify(string='Hello World Hello!', set='llo')
   !< test_passed(2) = string1%verify(set='llo', back=.true.)==verify(string='Hello World Hello!', set='llo', BACK=.TRUE.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF  !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: SET   !< Searched set.
   LOGICAL,                   INTENT(IN), OPTIONAL :: BACK  !< Start of the last occurrence rather than the fiRST.
   INTEGER                                         :: I     !< Result of the search.

   IF (ALLOCATED(SELF%RAW)) THEN
     I = VERIFY(STRING=SELF%RAW, SET=SET, BACK=BACK)
   ELSE
     I = 0
   ENDIF
   ENDFUNCTION SVERIFY_STRING_CHARACTER

   ! auxiliary methods
   ELEMENTAL FUNCTION BASEDIR(SELF, SEP)
   !< Return the base directory name of a string containing a file name.
   !<
   !<```fortran
   !< type(string) :: string1
   !< logical      :: test_passed(4)
   !< string1 = '/bar/foo.tar.bz2'
   !< test_passed(1) = string1%basedir()//''=='/bar'
   !< string1 = './bar/foo.tar.bz2'
   !< test_passed(2) = string1%basedir()//''=='./bar'
   !< string1 = 'bar/foo.tar.bz2'
   !< test_passed(3) = string1%basedir()//''=='bar'
   !< string1 = '\bar\foo.tar.bz2'
   !< test_passed(4) = string1%basedir(sep='\')//''=='\bar'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF    !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP     !< Directory separator.
   TYPE(STRING)                                    :: BASEDIR !< Base directory name.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_    !< Separator, default value.
   INTEGER                                         :: POS     !< Character position.

   IF (ALLOCATED(SELF%RAW)) THEN
     SEP_ = UIX_DIR_SEP ; IF (PRESENT(SEP)) SEP_ = SEP
     BASEDIR = SELF
     POS = INDEX(SELF%RAW, SEP_, BACK=.TRUE.)
     IF (POS>0) BASEDIR%RAW = SELF%RAW(1:POS-1)
   ENDIF
   ENDFUNCTION BASEDIR

   ELEMENTAL FUNCTION BASENAME(SELF, SEP, EXTENSION, STRIP_LAST_EXTENSION)
   !< Return the base file name of a string containing a file name.
   !<
   !< Optionally, the extension is also stripped if provided or the last one if required, e.g.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(5)
   !< astring = 'bar/foo.tar.bz2'
   !< test_passed(1) = astring%basename()//''=='foo.tar.bz2'
   !< test_passed(2) = astring%basename(extension='.tar.bz2')//''=='foo'
   !< test_passed(3) = astring%basename(strip_last_extension=.true.)//''=='foo.tar'
   !< astring = '\bar\foo.tar.bz2'
   !< test_passed(4) = astring%basename(sep='\')//''=='foo.tar.bz2'
   !< astring = 'bar'
   !< test_passed(5) = astring%basename(strip_last_extension=.true.)//''=='bar'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF                 !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP                  !< Directory separator.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: EXTENSION            !< File extension.
   LOGICAL,                   INTENT(IN), OPTIONAL :: STRIP_LAST_EXTENSION !< Flag to enable the stripping of LAST EXTENSION.
   TYPE(STRING)                                    :: BASENAME             !< Base file name.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_                 !< Separator, default value.
   INTEGER                                         :: POS                  !< Character position.

   IF (ALLOCATED(SELF%RAW)) THEN
      SEP_ = UIX_DIR_SEP ; IF (PRESENT(SEP)) SEP_ = SEP
      BASENAME = SELF
      POS = INDEX(BASENAME%RAW, SEP_, BACK=.TRUE.)
      IF (POS>0) BASENAME%RAW = SELF%RAW(POS+1:)
      IF (PRESENT(EXTENSION)) THEN
         POS = INDEX(BASENAME%RAW, EXTENSION, BACK=.TRUE.)
         IF (POS>0) BASENAME%RAW = BASENAME%RAW(1:POS-1)
      ELSEIF (PRESENT(STRIP_LAST_EXTENSION)) THEN
         IF (STRIP_LAST_EXTENSION) THEN
            POS = INDEX(BASENAME%RAW, '.', BACK=.TRUE.)
            IF (POS>0) BASENAME%RAW = BASENAME%RAW(1:POS-1)
         ENDIF
      ENDIF
   ENDIF
   ENDFUNCTION BASENAME

   ELEMENTAL FUNCTION CAMELCASE(SELF, SEP)
   !< Return a string with all words capitalized without spaces.
   !<
   !< @note Multiple subsequent separators are collapsed to one occurence.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'caMeL caSe var'
   !< print '(L1)', astring%camelcase()//''=='CamelCaseVar'
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF      !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   TYPE(STRING)                                    :: CAMELCASE !< Camel case string.
   TYPE(STRING), ALLOCATABLE                       :: TOKENS(:) !< String tokens.

   IF (ALLOCATED(SELF%RAW)) THEN
     CALL SELF%SPLIT(TOKENS=TOKENS, SEP=SEP)
     TOKENS = TOKENS%CAPITALIZE()
     CAMELCASE = CAMELCASE%JOIN(ARRAY=TOKENS)
   ENDIF
   ENDFUNCTION CAMELCASE

   ELEMENTAL FUNCTION CAPITALIZE(SELF) RESULT(CAPITALIZED)
   !< Return a string with its first character capitalized and the rest lowercased.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'say all Hello WorLD!'
   !< print '(L1)', astring%capitalize()//''=='Say all hello world!'
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF        !< The string.
   TYPE(STRING)              :: CAPITALIZED !< Upper case string.
   INTEGER                   :: C           !< Character counter.

   IF (ALLOCATED(SELF%RAW)) THEN
     CAPITALIZED = SELF%LOWER()
     C = INDEX(LOWER_ALPHABET, CAPITALIZED%RAW(1:1))
     IF (C>0) CAPITALIZED%RAW(1:1) = UPPER_ALPHABET(C:C)
   ENDIF
   ENDFUNCTION CAPITALIZE

   PURE FUNCTION CHARS(SELF) RESULT(RAW)
   !< Return the raw characters data.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'say all Hello WorLD!'
   !< print '(L1)', astring%chars()=='say all Hello WorLD!'
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)              :: SELF !< The string.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: RAW  !< Raw characters data.

   IF (ALLOCATED(SELF%RAW)) THEN
     RAW = SELF%RAW
   ELSE
     RAW = ''
   ENDIF
   ENDFUNCTION CHARS

   PURE FUNCTION COLORIZE_STR(SELF, COLOR_FG, COLOR_BG, STYLE) RESULT(COLORIZED)
   !< Colorize and stylize strings, DEFAULT kind.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'say all Hello WorLD!'
   !< print '(L1)', astring%colorize(color_fg='red')=='[31msay all Hello WorLD![0m'
   !<```
   !=> T <<<
   CLASS(STRING),    INTENT(IN)           :: SELF      !< The string.
   CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: COLOR_FG  !< Foreground color definition.
   CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: COLOR_BG  !< Background color definition.
   CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: STYLE     !< Style definition.
   CHARACTER(LEN=:), ALLOCATABLE          :: COLORIZED !< Colorized string.

   COLORIZED = COLORIZE(STRING=SELF%CHARS(), COLOR_FG=COLOR_FG, COLOR_BG=COLOR_BG, STYLE=STYLE)
   ENDFUNCTION COLORIZE_STR

   ELEMENTAL FUNCTION DECODE(SELF, CODEC) RESULT(DECODED)
   !< Return a string decoded accordingly the codec.
   !<
   !< @note Only BASE64 codec is currently available.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'SG93IGFyZSB5b3U/'
   !< print '(L1)', astring%decode(codec='base64')//''=='How are you?'
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN) :: SELF    !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: CODEC   !< Encoding codec.
   TYPE(STRING)                          :: DECODED !< Decoded string.
   TYPE(STRING)                          :: CODEC_U !< Encoding codec in upper case string.

   IF (ALLOCATED(SELF%RAW)) THEN
     DECODED = SELF
     CODEC_U = CODEC
     SELECT CASE(CODEC_U%UPPER()//'')
     CASE('BASE64')
       CALL B64_DECODE(CODE=SELF%RAW, S=DECODED%RAW)
     ENDSELECT
     DECODED = DECODED%STRIP(REMOVE_NULLS=.TRUE.)
   ENDIF
   ENDFUNCTION DECODE

   ELEMENTAL FUNCTION ENCODE(SELF, CODEC) RESULT(ENCODED)
   !< Return a string encoded accordingly the codec.
   !<
   !< @note Only BASE64 codec is currently available.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'How are you?'
   !< print '(L1)', astring%encode(codec='base64')//''=='SG93IGFyZSB5b3U/'
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN) :: SELF    !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: CODEC   !< Encoding codec.
   TYPE(STRING)                          :: ENCODED !< Encoded string.

   IF (ALLOCATED(SELF%RAW)) THEN
     ENCODED = CODEC
     SELECT CASE(ENCODED%UPPER()//'')
     CASE('BASE64')
       CALL B64_ENCODE(S=SELF%RAW, CODE=ENCODED%RAW)
     ENDSELECT
   ENDIF
   ENDFUNCTION ENCODE

   ELEMENTAL FUNCTION ESCAPE(SELF, TO_ESCAPE, ESC) RESULT(ESCAPED)
   !< Escape backslashes (or custom escape character).
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(2)
   !< astring = '^\s \d+\s*'
   !< test_passed(1) = astring%escape(to_escape='\')//''=='^\\s \\d+\\s*'
   !< test_passed(2) = astring%escape(to_escape='\', esc='|')//''=='^|\s |\d+|\s*'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF      !< The string.
   CHARACTER(KIND=CK, LEN=1), INTENT(IN)           :: TO_ESCAPE !< Character to be escaped.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: ESC       !< Character used to escape.
   TYPE(STRING)                                    :: ESCAPED   !< Escaped string.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: ESC_      !< Character to escape, local variable.
   INTEGER                                         :: C         !< Character counter.

   IF (ALLOCATED(SELF%RAW)) THEN
     ESC_ = BACKSLASH ; IF (PRESENT(ESC)) ESC_ = ESC
     ESCAPED%RAW = ''
     DO C=1, LEN(SELF%RAW)
       IF (SELF%RAW(C:C)==TO_ESCAPE) THEN
         ESCAPED%RAW = ESCAPED%RAW//ESC_//TO_ESCAPE
       ELSE
         ESCAPED%RAW = ESCAPED%RAW//SELF%RAW(C:C)
       ENDIF
     ENDDO
   ENDIF
   ENDFUNCTION ESCAPE

   ELEMENTAL FUNCTION EXTENSION(SELF)
   !< Return the extension of a string containing a file name.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = '/bar/foo.tar.bz2'
   !< print '(L1)', astring%extension()//''=='.bz2'
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)              :: SELF      !< The string.
   TYPE(STRING)                           :: EXTENSION !< Extension file name.
   INTEGER                                :: POS       !< Character position.

   IF (ALLOCATED(SELF%RAW)) THEN
      EXTENSION = ''
      POS = INDEX(SELF%RAW, '.', BACK=.TRUE.)
      IF (POS>0) EXTENSION%RAW = SELF%RAW(POS:)
   ENDIF
   ENDFUNCTION EXTENSION

   ELEMENTAL FUNCTION FILL(SELF, WIDTH, RIGHT, FILLING_CHAR) RESULT(FILLED)
   !< Pad string on the left (or right) with zeros (or other char) to fill width.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(4)
   !< astring = 'this is string example....wow!!!'
   !< test_passed(1) = astring%fill(width=40)//''=='00000000this is string example....wow!!!'
   !< test_passed(2) = astring%fill(width=50)//''=='000000000000000000this is string example....wow!!!'
   !< test_passed(3) = astring%fill(width=50, right=.true.)//''=='this is string example....wow!!!000000000000000000'
   !< test_passed(4) = astring%fill(width=40, filling_char='*')//''=='********this is string example....wow!!!'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF          !< The string.
   INTEGER,                   INTENT(IN)           :: WIDTH         !< Final width of filled string.
   LOGICAL,                   INTENT(IN), OPTIONAL :: RIGHT         !< Fill on the right instead of left.
   CHARACTER(KIND=CK, LEN=1), INTENT(IN), OPTIONAL :: FILLING_CHAR  !< Filling character (default "0").
   TYPE(STRING)                                    :: FILLED        !< Filled string.
   LOGICAL                                         :: RIGHT_        !< Fill on the right instead of left, locaL VARIABLE.
   CHARACTER(KIND=CK, LEN=1)                       :: FILLING_CHAR_ !< Filling character (default "0"), local VARIABLE.

   IF (ALLOCATED(SELF%RAW)) THEN
      IF (WIDTH>LEN(SELF%RAW)) THEN
         RIGHT_ = .FALSE. ; IF (PRESENT(RIGHT)) RIGHT_ = RIGHT
         FILLING_CHAR_ = '0' ; IF (PRESENT(FILLING_CHAR)) FILLING_CHAR_ = FILLING_CHAR
         IF (.NOT.RIGHT_) THEN
            FILLED%RAW = REPEAT(FILLING_CHAR_, WIDTH-LEN(SELF%RAW))//SELF%RAW
         ELSE
            FILLED%RAW = SELF%RAW//REPEAT(FILLING_CHAR_, WIDTH-LEN(SELF%RAW))
         ENDIF
      ENDIF
   ENDIF
   ENDFUNCTION FILL

   ELEMENTAL SUBROUTINE FREE(SELF)
   !< Free dynamic memory.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'this is string example....wow!!!'
   !< call astring%free
   !< print '(L1)', astring%is_allocated().eqv..false.
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: SELF !< The string.

   IF (ALLOCATED(SELF%RAW)) DEALLOCATE(SELF%RAW)
   ENDSUBROUTINE FREE

   SUBROUTINE GLOB_CHARACTER(SELF, PATTERN, LIST)
   !< Glob search (character output), finds all the pathnames matching a given pattern according to the rules USED BY THE UNIX SHELL.
   !<
   !< @note Method not portable: works only on Unix/GNU Linux OS.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: alist_chr(:)
   !< integer, parameter            :: Nf=5
   !< character(14)                 :: files(1:Nf)
   !< integer                       :: file_unit
   !< integer                       :: f
   !< integer                       :: ff
   !< logical                       :: test_passed
   !< do f=1, Nf
   !<    files(f) = astring%tempname(prefix='foo-')
   !<    open(newunit=file_unit, file=files(f))
   !<    write(file_unit, *)f
   !<    close(unit=file_unit)
   !< enddo
   !< call astring%glob(pattern='foo-*', list=alist_chr)
   !< do f=1, Nf
   !<    open(newunit=file_unit, file=files(f))
   !<    close(unit=file_unit, status='delete')
   !< enddo
   !< test_passed = .false.
   !< outer_chr: do f=1, size(alist_chr, dim=1)
   !<    do ff=1, Nf
   !<       test_passed = alist_chr(f) == files(ff)
   !<       if (test_passed) cycle outer_chr
   !<    enddo
   !< enddo outer_chr
   !< print '(L1)', test_passed
   !<```
   !=> T <<<
   CLASS(STRING),                 INTENT(IN)  :: SELF           !< The string.
   CHARACTER(*),                  INTENT(IN)  :: PATTERN        !< Given pattern.
   CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: LIST(:)        !< List of matching pathnames.
   TYPE(STRING), ALLOCATABLE                  :: LIST_(:)       !< List of matching pathnames.
   INTEGER(I4P)                               :: MAX_LEN        !< Maximum length.
   INTEGER(I4P)                               :: MATCHES_NUMBER !< Matches number.
   INTEGER(I4P)                               :: M              !< Counter.

   CALL SELF%GLOB(PATTERN=PATTERN, LIST=LIST_)
   IF (ALLOCATED(LIST_)) THEN
      MATCHES_NUMBER = SIZE(LIST_, DIM=1)
      MAX_LEN = 0
      DO M=1, MATCHES_NUMBER
         MAX_LEN = MAX(MAX_LEN, LIST_(M)%LEN())
      ENDDO
      ALLOCATE(CHARACTER(MAX_LEN) :: LIST(1:MATCHES_NUMBER))
      DO M=1, MATCHES_NUMBER
         LIST(M) = LIST_(M)%CHARS()
      ENDDO
   ENDIF
   ENDSUBROUTINE GLOB_CHARACTER

   SUBROUTINE GLOB_STRING(SELF, PATTERN, LIST)
   !< Glob search (string output), finds all the pathnames matching a given pattern according to the rules useD BY THE UNIX SHELL.
   !<
   !< @note Method not portable: works only on Unix/GNU Linux OS.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< type(string),     allocatable :: alist_str(:)
   !< integer, parameter            :: Nf=5
   !< character(14)                 :: files(1:Nf)
   !< integer                       :: file_unit
   !< integer                       :: f
   !< integer                       :: ff
   !< logical                       :: test_passed
   !<
   !< do f=1, Nf
   !<    files(f) = astring%tempname(prefix='foo-')
   !<    open(newunit=file_unit, file=files(f))
   !<    write(file_unit, *)f
   !<    close(unit=file_unit)
   !< enddo
   !< call astring%glob(pattern='foo-*', list=alist_str)
   !< do f=1, Nf
   !<    open(newunit=file_unit, file=files(f))
   !<    close(unit=file_unit, status='delete')
   !< enddo
   !< test_passed = .false.
   !< outer_str: do f=1, size(alist_str, dim=1)
   !<    do ff=1, Nf
   !<       test_passed = alist_str(f) == files(ff)
   !<       if (test_passed) cycle outer_str
   !<    enddo
   !< enddo outer_str
   !< print '(L1)', test_passed
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)  :: SELF     !< The string.
   CHARACTER(*),              INTENT(IN)  :: PATTERN  !< Given pattern.
   TYPE(STRING), ALLOCATABLE, INTENT(OUT) :: LIST(:)  !< List of matching pathnames.
   TYPE(STRING)                           :: TEMPFILE !< Safe temporary file.
   CHARACTER(LEN=:), ALLOCATABLE          :: TEMPNAME !< Safe temporary name.
   INTEGER(I4P)                           :: TEMPUNIT !< Unit of temporary file.

   TEMPNAME = SELF%TEMPNAME()
   CALL EXECUTE_COMMAND_LINE('ls -1 '//TRIM(ADJUSTL(PATTERN))//' > '//TEMPNAME)
   CALL TEMPFILE%READ_FILE(FILE=TEMPNAME)
   CALL TEMPFILE%SPLIT(SEP=NEW_LINE('a'), TOKENS=LIST)
   OPEN(NEWUNIT=TEMPUNIT, FILE=TEMPNAME)
   CLOSE(UNIT=TEMPUNIT, STATUS='delete')
   ENDSUBROUTINE GLOB_STRING

   ELEMENTAL FUNCTION INSERT_CHARACTER(SELF, SUBSTRING, POS) RESULT(INSERTED)
   !< Insert substring into string at a specified position.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(5)
   !< astring = 'this is string example wow!!!'
   !< acharacter = '... '
   !< test_passed(1) = astring%insert(substring=acharacter, pos=1)//''=='... this is string example wow!!!'
   !< test_passed(2) = astring%insert(substring=acharacter, pos=23)//''=='this is string example...  wow!!!'
   !< test_passed(3) = astring%insert(substring=acharacter, pos=29)//''=='this is string example wow!!!... '
   !< test_passed(4) = astring%insert(substring=acharacter, pos=-1)//''=='... this is string example wow!!!'
   !< test_passed(5) = astring%insert(substring=acharacter, pos=100)//''=='this is string example wow!!!... '
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),    INTENT(IN) :: SELF      !< The string.
   CHARACTER(LEN=*), INTENT(IN) :: SUBSTRING !< Substring.
   INTEGER,          INTENT(IN) :: POS       !< Position from which insert substring.
   TYPE(STRING)                 :: INSERTED  !< Inserted string.
   INTEGER                      :: SAFEPOS   !< Safe position from which insert substring.

   IF (ALLOCATED(SELF%RAW)) THEN
      INSERTED = SELF
      SAFEPOS = MIN(MAX(1, POS), LEN(SELF%RAW))
      IF (SAFEPOS==1) THEN
         INSERTED%RAW = SUBSTRING//SELF%RAW
      ELSEIF (SAFEPOS==LEN(SELF%RAW)) THEN
         INSERTED%RAW = SELF%RAW//SUBSTRING
      ELSE
         INSERTED%RAW = SELF%RAW(1:SAFEPOS-1)//SUBSTRING//SELF%RAW(SAFEPOS:)
      ENDIF
   ELSE
      INSERTED%RAW = SUBSTRING
   ENDIF
   ENDFUNCTION INSERT_CHARACTER

   ELEMENTAL FUNCTION INSERT_STRING(SELF, SUBSTRING, POS) RESULT(INSERTED)
   !< Insert substring into string at a specified position.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(5)
   !< astring = 'this is string example wow!!!'
   !< anotherstring = '... '
   !< test_passed(1) = astring%insert(substring=anotherstring, pos=1)//''=='... this is string example wow!!!'
   !< test_passed(2) = astring%insert(substring=anotherstring, pos=23)//''=='this is string example...  wow!!!'
   !< test_passed(3) = astring%insert(substring=anotherstring, pos=29)//''=='this is string example wow!!!... '
   !< test_passed(4) = astring%insert(substring=anotherstring, pos=-1)//''=='... this is string example wow!!!'
   !< test_passed(5) = astring%insert(substring=anotherstring, pos=100)//''=='this is string example wow!!!... '
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF      !< The string.
   TYPE(STRING),  INTENT(IN) :: SUBSTRING !< Substring.
   INTEGER,       INTENT(IN) :: POS       !< Position from which insert substring.
   TYPE(STRING)              :: INSERTED  !< Inserted string.
   INTEGER                   :: SAFEPOS   !< Safe position from which insert substring.

   IF (ALLOCATED(SELF%RAW)) THEN
      INSERTED = SELF
      IF (ALLOCATED(SUBSTRING%RAW)) THEN
         SAFEPOS = MIN(MAX(1, POS), LEN(SELF%RAW))
         IF (SAFEPOS==1) THEN
            INSERTED%RAW = SUBSTRING%RAW//SELF%RAW
         ELSEIF (SAFEPOS==LEN(SELF%RAW)) THEN
            INSERTED%RAW = SELF%RAW//SUBSTRING%RAW
         ELSE
            INSERTED%RAW = SELF%RAW(1:SAFEPOS-1)//SUBSTRING%RAW//SELF%RAW(SAFEPOS:)
         ENDIF
      ENDIF
   ELSE
      IF (ALLOCATED(SUBSTRING%RAW)) INSERTED%RAW = SUBSTRING%RAW
   ENDIF
   ENDFUNCTION INSERT_STRING

   PURE FUNCTION JOIN_STRINGS(SELF, ARRAY, SEP) RESULT(JOIN)
   !< Return a string that is a join of an array of strings.
   !<
   !< The join-separator is set equals to self if self has a value or it is set to a null string ''. This valuE CAN BE OVERRIDDEN
   !< passing a custom separator.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: strings(3)
   !< logical      :: test_passed(5)
   !< strings(1) = 'one'
   !< strings(2) = 'two'
   !< strings(3) = 'three'
   !< test_passed(1) = (astring%join(array=strings)//''==strings(1)//strings(2)//strings(3))
   !< test_passed(2) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(2)//'-'//strings(3))
   !< call strings(1)%free
   !< strings(2) = 'two'
   !< strings(3) = 'three'
   !< test_passed(3) = (astring%join(array=strings, sep='-')//''==strings(2)//'-'//strings(3))
   !< strings(1) = 'one'
   !< strings(2) = 'two'
   !< call strings(3)%free
   !< test_passed(4) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(2))
   !< strings(1) = 'one'
   !< call strings(2)%free
   !< strings(3) = 'three'
   !< test_passed(5) = (astring%join(array=strings, sep='-')//''==strings(1)//'-'//strings(3))
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF      !< The string.
   TYPE(STRING),              INTENT(IN)           :: ARRAY(1:) !< Array to be joined.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   TYPE(STRING)                                    :: JOIN      !< The join of array.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_      !< Separator, default value.
   INTEGER                                         :: A         !< Counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      SEP_ = SELF%RAW
   ELSE
      SEP_ = ''
   ENDIF
   IF (PRESENT(SEP)) SEP_ = SEP
   JOIN = ''
   DO A=2, SIZE(ARRAY, DIM=1)
      IF (ALLOCATED(ARRAY(A)%RAW)) JOIN%RAW = JOIN%RAW//SEP_//ARRAY(A)%RAW
   ENDDO
   IF (ALLOCATED(ARRAY(1)%RAW)) THEN
      JOIN%RAW = ARRAY(1)%RAW//JOIN%RAW
   ELSE
      JOIN%RAW = JOIN%RAW(LEN(SEP_)+1:LEN(JOIN%RAW))
   ENDIF
   ENDFUNCTION JOIN_STRINGS

   PURE FUNCTION JOIN_CHARACTERS(SELF, ARRAY, SEP) RESULT(JOIN)
   !< Return a string that is a join of an array of characters.
   !<
   !< The join-separator is set equals to self if self has a value or it is set to a null string ''. This valuE CAN BE OVERRIDDEN
   !< passing a custom separator.
   !<
   !<```fortran
   !< type(string) :: astring
   !< character(5) :: characters(3)
   !< logical      :: test_passed(6)
   !< characters(1) = 'one'
   !< characters(2) = 'two'
   !< characters(3) = 'three'
   !< test_passed(1) = (astring%join(array=characters)//''==characters(1)//characters(2)//characters(3))
   !< test_passed(2) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(2)//'-'//cHARACTERS(3))
   !< characters(1) = ''
   !< characters(2) = 'two'
   !< characters(3) = 'three'
   !< test_passed(3) = (astring%join(array=characters, sep='-')//''==characters(2)//'-'//characters(3))
   !< characters(1) = 'one'
   !< characters(2) = 'two'
   !< characters(3) = ''
   !< test_passed(4) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(2))
   !< characters(1) = 'one'
   !< characters(2) = ''
   !< characters(3) = 'three'
   !< test_passed(5) = (astring%join(array=characters, sep='-')//''==characters(1)//'-'//characters(3))
   !< characters(1) = 'one'
   !< characters(2) = 'two'
   !< characters(3) = 'three'
   !< astring = '_'
   !< test_passed(6) = (astring%join(array=characters)//''==characters(1)//'_'//characters(2)//'_'//characters(3))
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF      !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: ARRAY(1:) !< Array to be joined.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   TYPE(STRING)                                    :: JOIN      !< The join of array.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_      !< Separator, default value.
   INTEGER                                         :: A         !< Counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      SEP_ = SELF%RAW
   ELSE
      SEP_ = ''
   ENDIF
   IF (PRESENT(SEP)) SEP_ = SEP
   JOIN = ''
   DO A=2, SIZE(ARRAY, DIM=1)
      IF (ARRAY(A)/='') JOIN%RAW = JOIN%RAW//SEP_//ARRAY(A)
   ENDDO
   IF (ARRAY(1)/='') THEN
      JOIN%RAW = ARRAY(1)//JOIN%RAW
   ELSE
      JOIN%RAW = JOIN%RAW(LEN(SEP_)+1:LEN(JOIN%RAW))
   ENDIF
   ENDFUNCTION JOIN_CHARACTERS

   PURE FUNCTION STRJOIN_STRINGS(ARRAY, SEP) RESULT(JOIN)
   !< Return a string that is a join of an array of strings.
   !<
   !< The join-separator is set equals to a null string '' if custom separator isn't specified.
   !<
   !<```fortran
   !< type(string)     :: strings(3)
   !< logical          :: test_passed(5)
   !< strings(1) = 'one'
   !< strings(2) = 'two'
   !< strings(3) = 'three'
   !< test_passed(1) = (strjoin(array=strings)//''==strings(1)//strings(2)//strings(3))
   !< test_passed(2) = (strjoin(array=strings, sep='-')//''==strings(1)//'-'//strings(2)//'-'//strings(3))
   !< call strings(1)%free
   !< strings(2) = 'two'
   !< strings(3) = 'three'
   !< test_passed(3) = (strjoin(array=strings, sep='-')//''==strings(2)//'-'//strings(3))
   !< strings(1) = 'one'
   !< strings(2) = 'two'
   !< call strings(3)%free
   !< test_passed(4) = (strjoin(array=strings, sep='-')//''==strings(1)//'-'//strings(2))
   !< strings(1) = 'one'
   !< call strings(2)%free
   !< strings(3) = 'three'
   !< test_passed(5) = (strjoin(array=strings, sep='-')//''==strings(1)//'-'//strings(3))
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: ARRAY(1:) !< Array to be joined.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   TYPE(STRING)                                    :: JOIN      !< The join of array.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_      !< Separator, default value.
   INTEGER                                         :: A         !< Counter.

   SEP_ = ''
   IF (PRESENT(SEP)) SEP_ = SEP
   JOIN = ''
   DO A=2, SIZE(ARRAY, DIM=1)
      IF (ALLOCATED(ARRAY(A)%RAW))JOIN%RAW = JOIN%RAW//SEP_//ARRAY(A)%RAW
   ENDDO
   IF (ALLOCATED(ARRAY(1)%RAW)) THEN
      JOIN%RAW = ARRAY(1)%RAW//JOIN%RAW
   ELSE
      JOIN%RAW = JOIN%RAW(LEN(SEP_)+1:LEN(JOIN%RAW))
   ENDIF
   ENDFUNCTION STRJOIN_STRINGS

  PURE FUNCTION STRJOIN_CHARACTERS(ARRAY, SEP, IS_TRIM) RESULT(JOIN)
   !< Return a string that is a join of an array of characters.
   !<
   !< The join-separator is set equals to a null string '' if custom separator isn't specified.
   !< The trim function is applied to array items if optional logical is_trim variable isn't set to .false.
   !<
   !<```fortran
   !< character(5) :: characters(3)
   !< logical      :: test_passed(13)
   !< characters(1) = 'one'
   !< characters(2) = 'two'
   !< characters(3) = 'three'
   !< test_passed(1) = (strjoin(array=characters)//''==trim(characters(1))//trim(characters(2))//trim(characteRS(3)))
   !< test_passed(2) = (strjoin(array=characters, sep='-')//''==trim(characters(1))//'-'//trim(characters(2))//'-'//TRIM(CHARACTERS(3)))
   !< test_passed(3) = ( strjoin(array=characters, is_trim=.false.)//''==characters(1)//characters(2)//charactERS(3))
   !< test_passed(4) = ( strjoin(array=characters, sep='-', is_trim=.false.)//''==characters(1)//'-'//characteRS(2)//'-'//CHARACTERS(3))
   !< characters(1) = ''
   !< characters(2) = 'two'
   !< characters(3) = 'three'
   !< test_passed(5) = (strjoin(array=characters)//''==trim(characters(2))//trim(characters(3)))
   !< characters(1) = 'one'
   !< characters(2) = 'two'
   !< characters(3) = ''
   !< test_passed(6) = (strjoin(array=characters)//''==trim(characters(1))//trim(characters(2)))
   !< characters(1) = 'one'
   !< characters(2) = ''
   !< characters(3) = 'three'
   !< test_passed(7) = (strjoin(array=characters)//''==trim(characters(1))//trim(characters(3)))
   !< characters(1) = ''
   !< characters(2) = 'two'
   !< characters(3) = 'three'
   !< test_passed(8) = (strjoin(array=characters, sep='-')//''==trim(characters(2))//'-'//trim(characters(3)))
   !< characters(1) = 'one'
   !< characters(2) = 'two'
   !< characters(3) = ''
   !< test_passed(9) = (strjoin(array=characters, sep='-')//''==trim(characters(1))//'-'//trim(characters(2)))
   !< characters(1) = 'one'
   !< characters(2) = ''
   !< characters(3) = 'three'
   !< test_passed(10) = (strjoin(array=characters, sep='-')//''==trim(characters(1))//'-'//trim(characters(3)))
   !< characters(1) = ''
   !< characters(2) = 'two'
   !< characters(3) = 'three'
   !< test_passed(11) = (strjoin(array=characters, sep='-', is_trim=.false.)//''==characters(2)//'-'//characteRS(3))
   !< characters(1) = 'one'
   !< characters(2) = 'two'
   !< characters(3) = ''
   !< test_passed(12) = (strjoin(array=characters, sep='-', is_trim=.false.)//''==characters(1)//'-'//characteRS(2))
   !< characters(1) = 'one'
   !< characters(2) = ''
   !< characters(3) = 'three'
   !< test_passed(13) = (strjoin(array=characters, sep='-', is_trim=.false.)//''==characters(1)//'-'//characteRS(3))
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: ARRAY(1:) !< Array to be joined.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   LOGICAL,                   INTENT(IN), OPTIONAL :: IS_TRIM   !< Flag to setup trim character or not
   TYPE(STRING)                                    :: JOIN      !< The join of array.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_      !< Separator, default value.
   LOGICAL                                         :: IS_TRIM_  !< Flag to setup trim character or not
   INTEGER                                         :: A         !< Counter.

   SEP_ = ''
   IF (PRESENT(SEP)) SEP_ = SEP
   IS_TRIM_ = .TRUE. ; IF (PRESENT(IS_TRIM)) IS_TRIM_ = IS_TRIM
   JOIN = ''

   IF (IS_TRIM_) THEN
       DO A=2, SIZE(ARRAY, DIM=1)
          IF (TRIM(ARRAY(A))/='') JOIN%RAW = JOIN%RAW//SEP_//TRIM(ARRAY(A))
       ENDDO
       IF (TRIM(ARRAY(1))/='') THEN
          JOIN%RAW = TRIM(ARRAY(1))//JOIN%RAW
       ELSE
          JOIN%RAW = JOIN%RAW(LEN(SEP_)+1:LEN(JOIN%RAW))
       ENDIF
   ELSE
       DO A=2, SIZE(ARRAY, DIM=1)
          IF (ARRAY(A)/='') JOIN%RAW = JOIN%RAW//SEP_//ARRAY(A)
       ENDDO
       IF (ARRAY(1)/='') THEN
          JOIN%RAW = ARRAY(1)//JOIN%RAW
       ELSE
          JOIN%RAW = JOIN%RAW(LEN(SEP_)+1:LEN(JOIN%RAW))
       ENDIF
   ENDIF
   ENDFUNCTION STRJOIN_CHARACTERS

   PURE FUNCTION STRJOIN_STRINGS_ARRAY(ARRAY, SEP, IS_COL) RESULT(JOIN)
   !< Return a string that is a join of columns or rows of an array of strings.
   !<
   !< The join-separator is set equals to a null string '' if custom separator isn't specified.
   !< The is_col is setup the direction of join: within default columns (.true.) or rows(.false.).
   !<
   !<```fortran
   !< type(string), allocatable :: strings_arr(:, :)
   !< logical                   :: test_passed(5)
   !<
   !< strings_arr = reshape( source = &
   !<                        [string('one'), string('two'), string('three'),  &
   !<                         string('ONE'), string('TWO'), string('THREE')], &
   !<                        shape = [3, 2] )
   !<
   !< test_passed(1) = all( strjoin(array=strings_arr) == &
   !<                       reshape([string('onetwothree'), string('ONETWOTHREE')], &
   !<                       shape = [2]) )
   !<
   !< test_passed(2) = all( strjoin(array=strings_arr, sep='_') == &
   !<                       reshape([string('one_two_three'), string('ONE_TWO_THREE')], &
   !<                       shape = [2]) )
   !<
   !<  test_passed(3) = all( strjoin(array=strings_arr, is_col=.false.) == &
   !<                        reshape([string('oneONE'), string('twoTWO'), string('threeTHREE')], &
   !<                        shape = [3]) )
   !<
   !<  test_passed(4) = all( strjoin(array=strings_arr, sep='_', is_col=.false.) == &
   !<                        reshape([string('one_ONE'), string('two_TWO'), string('three_THREE')], &
   !<                        shape = [3]) )
   !<
   !< call strings_arr(2, 1)%free
   !< test_passed(5) = all( strjoin(array=strings_arr, sep='_', is_col=.false.) == &
   !<                  reshape([string('one_ONE'), string('TWO'), string('three_THREE')], &
   !<                  shape = [3]) )
   !<
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: ARRAY(1:, 1:) !< Array to be joined.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP  !< Separator.
   LOGICAL,                   INTENT(IN), OPTIONAL :: IS_COL  !< Direction: 'columns' if .true. or 'rows' if .FALSE.
   TYPE(STRING),              ALLOCATABLE          :: JOIN(:)       !< The join of array.
   TYPE(STRING),              ALLOCATABLE          :: SLICE(:)      !< The column or row slice of array
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_          !< Separator, default value.
   LOGICAL                                         :: IS_COL_       !< Direction, default value.
   INTEGER                                         :: A, JOIN_SIZE, SLICE_SIZE  !< Counter, sizes of join vectOR AND OF SLICE OF ARRAY

   SEP_    = ''     ; IF (PRESENT(SEP)) SEP_ = SEP
   IS_COL_ = .TRUE. ; IF (PRESENT(IS_COL)) IS_COL_ = IS_COL

   IF (IS_COL_) THEN
       JOIN_SIZE  = SIZE(ARRAY, DIM=2)
       SLICE_SIZE = SIZE(ARRAY, DIM=1)

       IF (.NOT.ALLOCATED(JOIN))  ALLOCATE(JOIN(JOIN_SIZE))
       IF (.NOT.ALLOCATED(SLICE)) ALLOCATE(SLICE(SLICE_SIZE))
       DO A = 1, JOIN_SIZE
           SLICE(:) = ARRAY(:, A)
           JOIN(A)  = STRJOIN_STRINGS(SLICE, SEP_)
       END DO
   ELSE
       JOIN_SIZE  = SIZE(ARRAY, DIM=1)
       SLICE_SIZE = SIZE(ARRAY, DIM=2)

       IF (.NOT.ALLOCATED(JOIN))  ALLOCATE(JOIN(JOIN_SIZE))
       IF (.NOT.ALLOCATED(SLICE)) ALLOCATE(SLICE(SLICE_SIZE))
       DO A = 1, JOIN_SIZE
           SLICE(:) = ARRAY(A, :)
           JOIN(A)  = STRJOIN_STRINGS(SLICE, SEP_)
       END DO
   ENDIF
   ENDFUNCTION STRJOIN_STRINGS_ARRAY

  PURE FUNCTION STRJOIN_CHARACTERS_ARRAY(ARRAY, SEP, IS_TRIM, IS_COL) RESULT(JOIN)
   !< Return a string that is a join of columns or rows of an array of characters.
   !<
   !< The join-separator is set equals to a null string '' if custom separator isn't specified.
   !< The trim function is applied to array items if optional logical is_trim variable isn't set to .false.
   !< The is_col is setup the direction of join: within default columns (.true.) or rows(.false.).
   !<
   !<```fortran
   !< character(len=10)         :: chars_arr(3, 2)
   !< logical                   :: test_passed(9)
   !< chars_arr(:, 1) = ['one       ', 'two       ', 'three     ']
   !< chars_arr(:, 2) = ['ONE       ', 'TWO       ', 'THREE     ']
   !<
   !< test_passed(1) = all( strjoin(array=chars_arr) == &
   !<                       reshape([string('onetwothree'), string('ONETWOTHREE')], &
   !<                       shape = [2]) )
   !<
   !< test_passed(2) = all( strjoin(array=chars_arr, is_trim=.false.) ==  &
   !<                       reshape([string('one       two       three     '),  &
   !<                                string('ONE       TWO       THREE     ')], &
   !<                       shape = [2]) )
   !<
   !< test_passed(3) = all( strjoin(array=chars_arr, sep='_') == &
   !<                       reshape([string('one_two_three'), string('ONE_TWO_THREE')], &
   !<                       shape = [2]) )
   !<
   !< test_passed(4) = all( strjoin(array=chars_arr, sep='_', is_trim=.false.) ==  &
   !<                       reshape([string('one       _two       _three     '),  &
   !<                                string('ONE       _TWO       _THREE     ')], &
   !<                       shape = [2]) )
   !<
   !< test_passed(5) = all( strjoin(array=chars_arr, is_col=.false.) == &
   !<                       reshape([string('oneONE'), string('twoTWO'), string('threeTHREE')], &
   !<                       shape = [3]) )
   !<
   !< test_passed(6) = all( strjoin(array=chars_arr, is_trim=.false., is_col=.false.) ==  &
   !<                       reshape([string('one       ONE       '),  &
   !<                                string('two       TWO       '),  &
   !<                                string('three     THREE     ')], &
   !<                       shape = [3]) )
   !<
   !< test_passed(7) = all( strjoin(array=chars_arr, sep='_', is_col=.false.) == &
   !<                       reshape([string('one_ONE'), string('two_TWO'), string('three_THREE')], &
   !<                       shape = [3]) )
   !<
   !< test_passed(8) = all( strjoin(array=chars_arr, sep='_', is_trim=.false., is_col=.false.) ==  &
   !<                       reshape([string('one       _ONE       '),  &
   !<                                string('two       _TWO       '),  &
   !<                                string('three     _THREE     ')], &
   !<                       shape = [3]) )
   !<
   !< chars_arr(2,1) = ''
   !< test_passed(9) = all( strjoin(array=chars_arr, sep='_', is_col=.false.) ==  &
   !<                       reshape([string('one_ONE'),  &
   !<                                string('TWO'),  &
   !<                                string('three_THREE')], &
   !<                       shape = [3]) )
   !<
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: ARRAY(1:, 1:) !< Array to be joined.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   LOGICAL,                   INTENT(IN), OPTIONAL :: IS_TRIM   !< Flag to setup trim character or not
   LOGICAL,                   INTENT(IN), OPTIONAL :: IS_COL    !< Direction: 'columns' if .true. or 'rows' if .FALSE.
   TYPE(STRING),              ALLOCATABLE          :: JOIN(:)   !< The join of array.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SLICE(:)  !< The column or row slice of array
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_      !< Separator, default value.
   LOGICAL                                         :: IS_TRIM_  !< Flag to setup trim character or not
   LOGICAL                                         :: IS_COL_   !< Direction, default value.
   INTEGER                                         :: A, JOIN_SIZE, SLICE_SIZE  !< Counter, sizes of join vectOR AND OF SLICE OF ARRAY
   INTEGER                                         :: ITEM_LEN  !< Length of array item (all items of characteR ARRAY HAVE EQUAL LENGTHS)

   ITEM_LEN = LEN(ARRAY(1,1)) !< all items of character array have equal lengths
   SEP_     = ''     ; IF (PRESENT(SEP)) SEP_ = SEP
   IS_TRIM_ = .TRUE. ; IF (PRESENT(IS_TRIM)) IS_TRIM_ = IS_TRIM
   IS_COL_  = .TRUE. ; IF (PRESENT(IS_COL)) IS_COL_ = IS_COL

   IF (IS_COL_) THEN
       JOIN_SIZE  = SIZE(ARRAY, DIM=2)
       SLICE_SIZE = SIZE(ARRAY, DIM=1)

       IF (.NOT.ALLOCATED(JOIN))  ALLOCATE(JOIN(JOIN_SIZE))
       IF (.NOT.ALLOCATED(SLICE)) ALLOCATE(CHARACTER(LEN=ITEM_LEN) :: SLICE(SLICE_SIZE))
       DO A = 1, JOIN_SIZE
           SLICE(:) = ARRAY(:, A)
           JOIN(A)  = STRJOIN_CHARACTERS(SLICE, SEP_, IS_TRIM_)
       END DO
   ELSE
       JOIN_SIZE  = SIZE(ARRAY, DIM=1)
       SLICE_SIZE = SIZE(ARRAY, DIM=2)

       IF (.NOT.ALLOCATED(JOIN))  ALLOCATE(JOIN(JOIN_SIZE))
       IF (.NOT.ALLOCATED(SLICE)) ALLOCATE(CHARACTER(LEN=ITEM_LEN) :: SLICE(SLICE_SIZE))
       DO A = 1, JOIN_SIZE
           SLICE(:) = ARRAY(A, :)
           JOIN(A)  = STRJOIN_CHARACTERS(SLICE, SEP_, IS_TRIM_)
       END DO
   ENDIF
   ENDFUNCTION STRJOIN_CHARACTERS_ARRAY

   ELEMENTAL FUNCTION LOWER(SELF)
   !< Return a string with all lowercase characters.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 'Hello WorLD!'
   !< test_passed(1) = astring%lower()//''=='hello world!'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF  !< The string.
   TYPE(STRING)              :: LOWER !< Upper case string.
   INTEGER                   :: N1    !< Characters counter.
   INTEGER                   :: N2    !< Characters counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      LOWER = SELF
      DO N1=1, LEN(SELF%RAW)
         N2 = INDEX(UPPER_ALPHABET, SELF%RAW(N1:N1))
         IF (N2>0) LOWER%RAW(N1:N1) = LOWER_ALPHABET(N2:N2)
      ENDDO
   ENDIF
   ENDFUNCTION LOWER

   PURE FUNCTION PARTITION(SELF, SEP) RESULT(PARTITIONS)
   !< Split string at separator and return the 3 parts (before, the separator and after).
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: strings(3)
   !< logical      :: test_passed(3)
   !< astring = 'Hello WorLD!'
   !< strings = astring%partition(sep='lo Wo')
   !< test_passed(1) = (strings(1)//''=='Hel'.and.strings(2)//''=='lo Wo'.and.strings(3)//''=='rLD!')
   !< strings = astring%partition(sep='Hello')
   !< test_passed(2) = (strings(1)//''==''.and.strings(2)//''=='Hello'.and.strings(3)//''==' WorLD!')
   !< astring = 'Hello WorLD!'
   !< strings = astring%partition()
   !< test_passed(3) = (strings(1)//''=='Hello'.and.strings(2)//''==' '.and.strings(3)//''=='WorLD!')
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF            !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP             !< Separator.
   TYPE(STRING)                                    :: PARTITIONS(1:3) !< Partions: before the separator, the sEPARATOR ITSELS AND
                                                                      !< after the separator.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_            !< Separator, default value.
   INTEGER                                         :: C               !< Character counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      SEP_ = SPACE ; IF (PRESENT(SEP)) SEP_ = SEP

      PARTITIONS(1) = SELF
      PARTITIONS(2) = SEP_
      PARTITIONS(3) = ''
      IF (LEN(SEP_)>=LEN(SELF%RAW)) RETURN
      C = INDEX(SELF%RAW, SEP_)
      IF (C>0) THEN
         PARTITIONS(1)%RAW = SELF%RAW(1:C-1)
         PARTITIONS(2)%RAW = SELF%RAW(C:C+LEN(SEP_)-1)
         PARTITIONS(3)%RAW = SELF%RAW(C+LEN(SEP_):)
      ENDIF
   ENDIF
   ENDFUNCTION PARTITION

   SUBROUTINE READ_FILE(SELF, FILE, IS_FAST, FORM, IOSTAT, IOMSG)
   !< Read a file as a single string stream.
   !<
   !< @note All the lines are stored into the string self as a single ascii stream. Each line (record) is sepaRATED BY A `NEW_LINE`
   !< character.
   !<
   !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
   !<
   !< @note *Fast* file reading allows a very efficient reading of streamed file, but it dumps file as single STREAMED STRING.
   !<
   !<```fortran
   !< type(string)              :: astring
   !< type(string), allocatable :: strings(:)
   !< type(string)              :: line(3)
   !< integer                   :: iostat
   !< character(len=99)         :: iomsg
   !< integer                   :: scratch
   !< integer                   :: l
   !< logical                   :: test_passed(9)
   !< line(1) = ' Hello World!   '
   !< line(2) = 'How are you?  '
   !< line(3) = '   All say: "Fine thanks"'
   !< open(newunit=scratch, file='read_file_test.tmp')
   !< write(scratch, "(A)") line(1)%chars()
   !< write(scratch, "(A)") line(2)%chars()
   !< write(scratch, "(A)") line(3)%chars()
   !< close(scratch)
   !< call astring%read_file(file='read_file_test.tmp', iostat=iostat, iomsg=iomsg)
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+1) = (strings(l)==line(l))
   !< enddo
   !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
   !< write(scratch) line(1)%chars()//new_line('a')
   !< write(scratch) line(2)%chars()//new_line('a')
   !< write(scratch) line(3)%chars()//new_line('a')
   !< close(scratch)
   !< call astring%read_file(file='read_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+5) = (strings(l)==line(l))
   !< enddo
   !< open(newunit=scratch, file='read_file_test.tmp', form='UNFORMATTED', access='STREAM')
   !< close(scratch, status='DELETE')
   !< call astring%read_file(file='read_file_test.tmp', iostat=iostat)
   !< test_passed(9) = (iostat/=0)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),    INTENT(INOUT)           :: SELF       !< The string.
   CHARACTER(LEN=*), INTENT(IN)              :: FILE       !< File name.
   LOGICAL,          INTENT(IN),    OPTIONAL :: IS_FAST    !< Flag to enable (super) fast file reading.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL :: FORM       !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL :: IOSTAT     !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: IOMSG      !< IO status message.
   LOGICAL                                   :: IS_FAST_   !< Flag to enable (super) fast file reading, local VARIABLE.
   TYPE(STRING)                              :: FORM_      !< Format of unit, local variable.
   INTEGER                                   :: IOSTAT_    !< IO status code, local variable.
   CHARACTER(LEN=:), ALLOCATABLE             :: IOMSG_     !< IO status message, local variable.
   INTEGER                                   :: UNIT       !< Logical unit.
   LOGICAL                                   :: DOES_EXIST !< Check if file exist.
   INTEGER(I4P)                              :: FILESIZE   !< Size of the file for fast reading.

   IOMSG_ = REPEAT(' ', 99) ; IF (PRESENT(IOMSG)) IOMSG_ = IOMSG
   INQUIRE(FILE=FILE, IOMSG=IOMSG_, IOSTAT=IOSTAT_, EXIST=DOES_EXIST)
   IF (DOES_EXIST) THEN
      IS_FAST_ = .FALSE. ; IF (PRESENT(IS_FAST)) IS_FAST_ = IS_FAST
      IF (IS_FAST_) THEN
         OPEN(NEWUNIT=UNIT, FILE=FILE, ACCESS='STREAM', FORM='UNFORMATTED', IOMSG=IOMSG_, IOSTAT=IOSTAT_)
         INQUIRE(FILE=FILE, SIZE=FILESIZE)
         IF (ALLOCATED(SELF%RAW)) DEALLOCATE(SELF%RAW)
         ALLOCATE(CHARACTER(LEN=FILESIZE):: SELF%RAW)
         READ(UNIT=UNIT, IOSTAT=IOSTAT_, IOMSG=IOMSG_) SELF%RAW
         CLOSE(UNIT)
      ELSE
         FORM_ = 'FORMATTED' ; IF (PRESENT(FORM)) FORM_ = FORM ; FORM_ = FORM_%UPPER()
         SELECT CASE(FORM_%CHARS())
         CASE('FORMATTED')
            OPEN(NEWUNIT=UNIT, FILE=FILE, STATUS='OLD', ACTION='READ', IOMSG=IOMSG_, IOSTAT=IOSTAT_, ERR=10)
         CASE('UNFORMATTED')
            OPEN(NEWUNIT=UNIT, FILE=FILE, STATUS='OLD', ACTION='READ', FORM='UNFORMATTED', ACCESS='STREAM', &
                 IOMSG=IOMSG_, IOSTAT=IOSTAT_, ERR=10)
         ENDSELECT
         CALL SELF%READ_LINES(UNIT=UNIT, FORM=FORM, IOMSG=IOMSG_, IOSTAT=IOSTAT_)
         10 CLOSE(UNIT)
      ENDIF
   ELSE
      IOSTAT_ = 1
      IOMSG_ = 'file not found'
   ENDIF
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
   IF (PRESENT(IOMSG)) IOMSG = IOMSG_
   ENDSUBROUTINE READ_FILE

   SUBROUTINE READ_LINE(SELF, UNIT, FORM, IOSTAT, IOMSG)
   !< Read line (record) from a connected unit.
   !<
   !< The line is read as an ascii stream read until the eor is reached.
   !<
   !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
   !<
   !<```fortran
   !< type(string)      :: astring
   !< type(string)      :: line(3)
   !< integer           :: iostat
   !< character(len=99) :: iomsg
   !< integer           :: scratch
   !< integer           :: l
   !< logical           :: test_passed(6)
   !< line(1) = ' Hello World!   '
   !< line(2) = 'How are you?  '
   !< line(3) = '   All say: "Fine thanks"'
   !< open(newunit=scratch, status='SCRATCH')
   !< write(scratch, "(A)") line(1)%chars()
   !< write(scratch, "(A)") line(2)%chars()
   !< write(scratch, "(A)") line(3)%chars()
   !< rewind(scratch)
   !< l = 0
   !< iostat = 0
   !< do
   !<   l = l + 1
   !<   call astring%read_line(unit=scratch, iostat=iostat, iomsg=iomsg)
   !<   if (iostat/=0.and..not.is_iostat_eor(iostat)) then
   !<     exit
   !<   else
   !<     test_passed(l) = (astring==line(l))
   !<   endif
   !< enddo
   !< close(scratch)
   !< open(newunit=scratch, status='SCRATCH', form='UNFORMATTED', access='STREAM')
   !< write(scratch) line(1)%chars()//new_line('a')
   !< write(scratch) line(2)%chars()//new_line('a')
   !< write(scratch) line(3)%chars()//new_line('a')
   !< rewind(scratch)
   !< l = 0
   !< iostat = 0
   !< do
   !<   l = l + 1
   !<   call astring%read_line(unit=scratch, iostat=iostat, iomsg=iomsg, form='UnfORMatteD')
   !<   if (iostat/=0.and..not.is_iostat_eor(iostat)) then
   !<     exit
   !<   else
   !<     test_passed(l+3) = (astring==line(l))
   !<   endif
   !< enddo
   !< close(scratch)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),    INTENT(INOUT)           :: SELF    !< The string.
   INTEGER,          INTENT(IN)              :: UNIT    !< Logical unit.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL :: FORM    !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL :: IOSTAT  !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: IOMSG   !< IO status message.
   TYPE(STRING)                              :: FORM_   !< Format of unit, local variable.
   INTEGER                                   :: IOSTAT_ !< IO status code, local variable.
   CHARACTER(LEN=:),          ALLOCATABLE    :: IOMSG_  !< IO status message, local variable.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE    :: LINE    !< Line storage.
   CHARACTER(KIND=CK, LEN=1)                 :: CH      !< Character storage.

   FORM_ = 'FORMATTED' ; IF (PRESENT(FORM)) FORM_ = FORM ; FORM_ = FORM_%UPPER()
   IOMSG_ = REPEAT(' ', 99) ; IF (PRESENT(IOMSG)) IOMSG_ = IOMSG
   LINE = ''
   SELECT CASE(FORM_%CHARS())
   CASE('FORMATTED')
      DO
         READ(UNIT, "(A)", ADVANCE='no', IOSTAT=IOSTAT_, IOMSG=IOMSG_, ERR=10, END=10, EOR=10) CH
         LINE = LINE//CH
      ENDDO
   CASE('UNFORMATTED')
      DO
         READ(UNIT, IOSTAT=IOSTAT_, IOMSG=IOMSG_, ERR=10, END=10) CH
         IF (CH==NEW_LINE('a')) THEN
            IOSTAT_ = IOSTAT_EOR
            EXIT
         ENDIF
         LINE = LINE//CH
      ENDDO
   ENDSELECT
   10 IF (LINE/='') SELF%RAW = LINE
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
   IF (PRESENT(IOMSG)) IOMSG = IOMSG_
   ENDSUBROUTINE READ_LINE

   SUBROUTINE READ_LINES(SELF, UNIT, FORM, IOSTAT, IOMSG)
   !< Read (all) lines (records) from a connected unit as a single ascii stream.
   !<
   !< @note All the lines are stored into the string self as a single ascii stream. Each line (record) is sepaRATED BY A `NEW_LINE`
   !< character. The line is read as an ascii stream read until the eor is reached.
   !<
   !< @note The connected unit is rewinded. At a successful exit current record is at eof, at the beginning otHERWISE.
   !<
   !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
   !<
   !<```fortran
   !< type(string)              :: astring
   !< type(string), allocatable :: strings(:)
   !< type(string)              :: line(3)
   !< integer                   :: iostat
   !< character(len=99)         :: iomsg
   !< integer                   :: scratch
   !< integer                   :: l
   !< logical                   :: test_passed(8)
   !<
   !< line(1) = ' Hello World!   '
   !< line(2) = 'How are you?  '
   !< line(3) = '   All say: "Fine thanks"'
   !< open(newunit=scratch, status='SCRATCH')
   !< write(scratch, "(A)") line(1)%chars()
   !< write(scratch, "(A)") line(2)%chars()
   !< write(scratch, "(A)") line(3)%chars()
   !< call astring%read_lines(unit=scratch, iostat=iostat, iomsg=iomsg)
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+1) = (strings(l)==line(l))
   !< enddo
   !< close(scratch)
   !< open(newunit=scratch, status='SCRATCH', form='UNFORMATTED', access='STREAM')
   !< write(scratch) line(1)%chars()//new_line('a')
   !< write(scratch) line(2)%chars()//new_line('a')
   !< write(scratch) line(3)%chars()//new_line('a')
   !< call astring%read_lines(unit=scratch, form='unformatted', iostat=iostat, iomsg=iomsg)
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+5) = (strings(l)==line(l))
   !< enddo
   !< close(scratch)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),    INTENT(INOUT)           :: SELF    !< The string.
   INTEGER,          INTENT(IN)              :: UNIT    !< Logical unit.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL :: FORM    !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL :: IOSTAT  !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: IOMSG   !< IO status message.
   INTEGER                                   :: IOSTAT_ !< IO status code, local variable.
   CHARACTER(LEN=:), ALLOCATABLE             :: IOMSG_  !< IO status message, local variable.
   TYPE(STRING)                              :: LINES   !< Lines storage.
   TYPE(STRING)                              :: LINE    !< Line storage.

   IOMSG_ = REPEAT(' ', 99) ; IF (PRESENT(IOMSG)) IOMSG_ = IOMSG
   REWIND(UNIT)
   IOSTAT_ = 0
   LINES%RAW = ''
   DO
      LINE%RAW = ''
      CALL LINE%READ_LINE(UNIT=UNIT, FORM=FORM, IOSTAT=IOSTAT_, IOMSG=IOMSG_)
      IF (IOSTAT_/=0.AND..NOT.IS_IOSTAT_EOR(IOSTAT_)) THEN
         EXIT
      ELSEIF (LINE/='') THEN
         LINES%RAW = LINES%RAW//LINE%RAW//NEW_LINE('a')
      ENDIF
   ENDDO
   IF (LINES%RAW/='') SELF%RAW = LINES%RAW
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
   IF (PRESENT(IOMSG)) IOMSG = IOMSG_
   ENDSUBROUTINE READ_LINES

   ELEMENTAL FUNCTION REPLACE(SELF, OLD, NEW, COUNT) RESULT(REPLACED)
   !< Return a string with all occurrences of substring old replaced by new.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(3)
   !< astring = 'When YOU are sad YOU should think to me :-)'
   !< test_passed(1) = (astring%replace(old='YOU', new='THEY')//''=='When THEY are sad THEY should think to me :-)')
   !< test_passed(2) = (astring%replace(old='YOU', new='THEY', count=1)//''=='When THEY are sad YOU should thiNK TO ME :-)')
   !< astring = repeat(new_line('a')//'abcd', 20)
   !< astring = astring%replace(old=new_line('a'), new='|cr|')
   !< astring = astring%replace(old='|cr|', new=new_line('a')//'    ')
   !< test_passed(3) = (astring//''==repeat(new_line('a')//'    '//'abcd', 20))
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF     !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: OLD      !< Old substring.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: NEW      !< New substring.
   INTEGER,                   INTENT(IN), OPTIONAL :: COUNT    !< Number of old occurences to be replaced.
   TYPE(STRING)                                    :: REPLACED !< The string with old replaced by new.
   INTEGER                                         :: R        !< Counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      REPLACED = SELF
      R = 0
      DO
         IF (INDEX(REPLACED%RAW, OLD)>0) THEN
            REPLACED = REPLACED%REPLACE_ONE_OCCURRENCE(OLD=OLD, NEW=NEW)
            R = R + 1
            IF (PRESENT(COUNT)) THEN
               IF (R>=COUNT) EXIT
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF
   ENDFUNCTION REPLACE

   ELEMENTAL FUNCTION REVERSE(SELF) RESULT(REVERSED)
   !< Return a reversed string.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(2)
   !< astring = 'abcdefghilmnopqrstuvz'
   !< test_passed(1) = (astring%reverse()//''=='zvutsrqponmlihgfedcba')
   !< astring = '0123456789'
   !< test_passed(2) = (astring%reverse()//''=='9876543210')
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< The string.
   TYPE(STRING)              :: REVERSED !< The reversed string.
   INTEGER                   :: LENGTH   !< Length of the string.
   INTEGER                   :: C        !< Counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      REVERSED = SELF
      LENGTH = LEN(SELF%RAW)
      DO C=1, LENGTH
         REVERSED%RAW(C:C) = SELF%RAW(LENGTH-C+1:LENGTH-C+1)
      ENDDO
   ENDIF
   ENDFUNCTION REVERSE

   FUNCTION SEARCH(SELF, TAG_START, TAG_END, IN_STRING, IN_CHARACTER, ISTART, IEND) RESULT(TAG)
   !< Search for *tagged* record into string, return the first record found (if any) matching the tags.
   !<
   !< Optionally, returns the indexes of tag start/end, thus this is not an `elemental` function.
   !<
   !< @note The tagged record is searched into self if allocated otherwise into `in_string` if passed or, evenTUALLY, INTO
   !< `in_character` is passed. If tag is not found the return string is not allocated and the start/end indexES (IF REQUESTED) ARE
   !< zero.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< type(string)                  :: anotherstring
   !< character(len=:), allocatable :: acharacter
   !< integer                       :: istart
   !< integer                       :: iend
   !< logical                       :: test_passed(5)
   !< astring = '<test> <first> hello </first> <first> not the first </first> </test>'
   !< anotherstring = astring%search(tag_start='<first>', tag_end='</first>')
   !< test_passed(1) = anotherstring//''=='<first> hello </first>'
   !< astring = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
   !< anotherstring = astring%search(tag_start='<a>', tag_end='</a>')
   !< test_passed(2) = anotherstring//''=='<a> <a> <a> the nested a </a> </a> </a>'
   !< call astring%free
   !< anotherstring = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
   !< astring = astring%search(in_string=anotherstring, tag_start='<a>', tag_end='</a>')
   !< test_passed(3) = astring//''=='<a> <a> <a> the nested a </a> </a> </a>'
   !< call astring%free
   !< acharacter = '<test> <a> <a> <a> the nested a </a> </a> </a> </test>'
   !< astring = astring%search(in_character=acharacter, tag_start='<a>', tag_end='</a>')
   !< test_passed(4) = astring//''=='<a> <a> <a> the nested a </a> </a> </a>'
   !< acharacter = '<test> <first> hello </first> <sec> <sec>not the first</sec> </sec> </test>'
   !< astring = astring%search(in_character=acharacter, tag_start='<sec>', tag_end='</sec>', istart=istart, ieND=IEND)
   !< test_passed(5) = astring//''==acharacter(31:67)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)            :: SELF         !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)            :: TAG_START    !< Start tag.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)            :: TAG_END      !< End tag.
   TYPE(STRING),              INTENT(IN),  OPTIONAL :: IN_STRING    !< Search into this string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN),  OPTIONAL :: IN_CHARACTER !< Search into this character string.
   INTEGER,                   INTENT(OUT), OPTIONAL :: ISTART       !< Starting index of tag inside the string.
   INTEGER,                   INTENT(OUT), OPTIONAL :: IEND         !< Ending index of tag inside the string.
   TYPE(STRING)                                     :: TAG          !< First tag found.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE           :: RAW          !< Raw string into which search the tag.
   INTEGER                                          :: ISTART_      !< Starting index of tag inside the string, LOCAL VARIABLE.
   INTEGER                                          :: IEND_        !< Ending index of tag inside the string, LOCAL VARIABLE.
   INTEGER                                          :: NESTED_TAGS  !< Number of nested tags inside tag.
   INTEGER                                          :: T            !< Counter.

   RAW = ''
   IF (PRESENT(IN_STRING)) THEN
      RAW = IN_STRING%RAW
   ELSEIF (PRESENT(IN_CHARACTER)) THEN
      RAW = IN_CHARACTER
   ELSE
      IF (ALLOCATED(SELF%RAW)) RAW = SELF%RAW
   ENDIF
   ISTART_ = 0
   IEND_ = 0
   IF (RAW/='') THEN
      ISTART_ = INDEX(RAW, TAG_START)
      IEND_ = INDEX(RAW, TAG_END)
      IF (ISTART_>0.AND.IEND_>0) THEN
         IEND_ = IEND_ + LEN(TAG_END) - 1
         TAG%RAW = RAW(ISTART_:IEND_)
         NESTED_TAGS = TAG%COUNT(TAG_START)
         IF (NESTED_TAGS>1) THEN
            DO T=2, NESTED_TAGS
               IEND_ = IEND_ + LEN(TAG_END) - 1 + INDEX(RAW(IEND_+1:), TAG_END)
            ENDDO
            TAG%RAW = RAW(ISTART_:IEND_)
         ENDIF
      ENDIF
   ENDIF
   IF (PRESENT(ISTART)) ISTART = ISTART_
   IF (PRESENT(IEND)) IEND = IEND_
   ENDFUNCTION SEARCH

   PURE FUNCTION SLICE(SELF, ISTART, IEND) RESULT(RAW)
   !< Return the raw characters data sliced.
   !<
   !<```fortran
   !< type(string) :: astring
   !< astring = 'the Quick Brown fox Jumps over the Lazy Dog.'
   !< print "(A)", astring%slice(11,25)
   !<```
   !=> Brown fox Jumps <<<
   CLASS(STRING), INTENT(IN)              :: SELF   !< The string.
   INTEGER,       INTENT(IN)              :: ISTART !< Slice start index.
   INTEGER,       INTENT(IN)              :: IEND   !< Slice end   index.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: RAW    !< Raw characters data.

   IF (ALLOCATED(SELF%RAW)) THEN
      RAW = SELF%RAW(ISTART:IEND)
   ELSE
      RAW = ''
   ENDIF
   ENDFUNCTION SLICE

   ELEMENTAL FUNCTION SNAKECASE(SELF, SEP)
   !< Return a string with all words lowercase separated by "_".
   !<
   !< @note Multiple subsequent separators are collapsed to one occurence.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 'the Quick Brown fox Jumps over the Lazy Dog.'
   !< test_passed(1) = astring%snakecase()//''=='the_quick_brown_fox_jumps_over_the_lazy_dog.'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF      !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   TYPE(STRING)                                    :: SNAKECASE !< Snake case string.
   TYPE(STRING), ALLOCATABLE                       :: TOKENS(:) !< String tokens.

   IF (ALLOCATED(SELF%RAW)) THEN
      CALL SELF%SPLIT(TOKENS=TOKENS, SEP=SEP)
      TOKENS = TOKENS%LOWER()
      SNAKECASE = SNAKECASE%JOIN(ARRAY=TOKENS, SEP='_')
   ENDIF
   ENDFUNCTION SNAKECASE

   PURE SUBROUTINE SPLIT(SELF, TOKENS, SEP, MAX_TOKENS)
   !< Return a list of substring in the string, using sep as the delimiter string.
   !<
   !< @note Multiple subsequent separators are collapsed to one occurrence.
   !<
   !< @note If `max_tokens` is passed the returned number of tokens is either `max_tokens` or `max_tokens + 1`.
   !<
   !<```fortran
   !< type(string)              :: astring
   !< type(string), allocatable :: strings(:)
   !< logical                   :: test_passed(11)
   !< astring = '+ab-++cre-++cre-ab+'
   !< call astring%split(tokens=strings, sep='+')
   !< test_passed(1) = (strings(1)//''=='ab-'.and.strings(2)//''=='cre-'.and.strings(3)//''=='cre-ab')
   !< astring = 'ab-++cre-++cre-ab+'
   !< call astring%split(tokens=strings, sep='+')
   !< test_passed(2) = (strings(1)//''=='ab-'.and.strings(2)//''=='cre-'.and.strings(3)//''=='cre-ab')
   !< astring = 'ab-++cre-++cre-ab'
   !< call astring%split(tokens=strings, sep='+')
   !< test_passed(3) = (strings(1)//''=='ab-'.and.strings(2)//''=='cre-'.and.strings(3)//''=='cre-ab')
   !< astring = 'Hello '//new_line('a')//'World!'
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(4) = (strings(1)//''=='Hello '.and.strings(2)//''=='World!')
   !< astring = 'Hello World!'
   !< call astring%split(tokens=strings)
   !< test_passed(5) = (strings(1)//''=='Hello'.and.strings(2)//''=='World!')
   !< astring = '+ab-'
   !< call astring%split(tokens=strings, sep='+')
   !< test_passed(6) = (strings(1)//''=='ab-')
   !< astring = '+ab-'
   !< call astring%split(tokens=strings, sep='-')
   !< test_passed(7) = (strings(1)//''=='+ab')
   !< astring = '+ab-+cd-'
   !< call astring%split(tokens=strings, sep='+')
   !< test_passed(8) = (strings(1)//''=='ab-'.and.strings(2)//''=='cd-')
   !< astring = 'ab-+cd-+'
   !< call astring%split(tokens=strings, sep='+')
   !< test_passed(9) = (strings(1)//''=='ab-'.and.strings(2)//''=='cd-')
   !< astring = '+ab-+cd-+'
   !< call astring%split(tokens=strings, sep='+')
   !< test_passed(10) = (strings(1)//''=='ab-'.and.strings(2)//''=='cd-')
   !< astring = '1-2-3-4-5-6-7-8'
   !< call astring%split(tokens=strings, sep='-', max_tokens=3)
   !< test_passed(11) = (strings(1)//''=='1'.and.strings(2)//''=='2'.and.strings(3)//''=='3'.and.strings(4)//''=='4-5-6-7-8')
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF           !< The string.
   TYPE(STRING), ALLOCATABLE, INTENT(OUT)          :: TOKENS(:)      !< Tokens substring.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP            !< Separator.
   INTEGER,                   INTENT(IN), OPTIONAL :: MAX_TOKENS     !< Fix the maximum number of returned tokENS.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_           !< Separator, default value.
   INTEGER                                         :: NO             !< Number of occurrences of sep.
   INTEGER                                         :: T              !< Character counter.
   TYPE(STRING)                                    :: TEMPORARY      !< Temporary storage.
   TYPE(STRING), ALLOCATABLE                       :: TEMP_TOKS(:,:) !< Temporary tokens substring.

   IF (ALLOCATED(SELF%RAW)) THEN
     SEP_ = SPACE ; IF (PRESENT(SEP)) SEP_ = SEP

     TEMPORARY = SELF%UNIQUE(SEP_)
     NO = TEMPORARY%COUNT(SEP_)

     IF (NO>0) THEN
       IF (PRESENT(MAX_TOKENS)) THEN
         IF (MAX_TOKENS < NO.AND.MAX_TOKENS > 0) NO = MAX_TOKENS
       ENDIF
       ALLOCATE(TEMP_TOKS(3, NO))
       TEMP_TOKS(:, 1) = TEMPORARY%PARTITION(SEP_)
       IF (NO>1) THEN
         DO T=2, NO
           TEMP_TOKS(:, T) = TEMP_TOKS(3, T-1)%PARTITION(SEP_)
         ENDDO
       ENDIF

       IF (TEMP_TOKS(1, 1)%RAW/=''.AND.TEMP_TOKS(3, NO)%RAW/='') THEN
         ALLOCATE(TOKENS(NO+1))
         DO T=1, NO
           IF (T==NO) THEN
             TOKENS(T  ) = TEMP_TOKS(1, T)
             TOKENS(T+1) = TEMP_TOKS(3, T)
           ELSE
             TOKENS(T) = TEMP_TOKS(1, T)
           ENDIF
         ENDDO
       ELSEIF (TEMP_TOKS(1, 1)%RAW/='') THEN
         ALLOCATE(TOKENS(NO))
         DO T=1, NO
           TOKENS(T) = TEMP_TOKS(1, T)
         ENDDO
       ELSEIF (TEMP_TOKS(3, NO)%RAW/='') THEN
         ALLOCATE(TOKENS(NO))
         DO T=1, NO-1
           TOKENS(T) = TEMP_TOKS(1, T+1)
         ENDDO
         TOKENS(NO) = TEMP_TOKS(3, NO)
       ELSE
         ALLOCATE(TOKENS(NO-1))
         DO T=2, NO
           TOKENS(T-1) = TEMP_TOKS(1, T)
         ENDDO
       ENDIF

     ELSE
       ALLOCATE(TOKENS(1))
       TOKENS(1) = SELF
     ENDIF
   ENDIF
   ENDSUBROUTINE SPLIT

   PURE SUBROUTINE SPLIT_CHUNKED(SELF, TOKENS, CHUNKS, SEP)
   !< Return a list of substring in the string, using sep as the delimiter string, chunked (memory-efficient) ALGORITHM.
   !<
   !< @note Multiple subsequent separators are collapsed to one occurrence.
   !<
   !< @note The split is performed in chunks of `#chunks` to avoid excessive memory consumption.
   !<
   !<```fortran
   !< type(string)              :: astring
   !< type(string), allocatable :: strings(:)
   !< logical                   :: test_passed(1)
   !< astring = '-1-2-3-4-5-6-7-8-'
   !< call astring%split_chunked(tokens=strings, sep='-', chunks=3)
   !< test_passed(1) = (strings(1)//''=='1'.and.strings(2)//''=='2'.and.strings(3)//''=='3'.and.strings(4)//''=='4'.AND. &
   !<                   strings(5)//''=='5'.and.strings(6)//''=='6'.and.strings(7)//''=='7'.and.strings(8)//''=='8')
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF      !< The string.
   TYPE(STRING), ALLOCATABLE, INTENT(OUT)          :: TOKENS(:) !< Tokens substring.
   INTEGER,                   INTENT(IN)           :: CHUNKS    !< Number of chunks.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_      !< Separator, default value.
   INTEGER                                         :: NT        !< Number of actual tokens.
   INTEGER                                         :: T         !< Counter.
   LOGICAL                                         :: ISOK

   IF (ALLOCATED(SELF%RAW)) THEN
     SEP_ = SPACE ; IF (PRESENT(SEP)) SEP_ = SEP

     NT = SELF%COUNT(SEP_)
     IF (SELF%START_WITH(PREFIX=SEP_)) NT = NT - 1
     IF (SELF%END_WITH(SUFFIX=SEP_)) NT = NT - 1
     T = 0
     CALL SELF%SPLIT(TOKENS=TOKENS, SEP=SEP_, MAX_TOKENS=CHUNKS)
     DO
       T = SIZE(TOKENS, DIM=1)
       IF (T > NT) EXIT
       CALL SPLIT_LAST_TOKEN(TOKENS=TOKENS, MAX_TOKENS=CHUNKS,ISOK=ISOK)
       IF(ISOK)THEN
       ELSE
            EXIT
       ENDIF
     ENDDO

     T = SIZE(TOKENS, DIM=1)
     IF (TOKENS(T)%COUNT(SEP_) > 0) THEN
        CALL SPLIT_LAST_TOKEN(TOKENS=TOKENS,ISOK=ISOK)
     ENDIF
   ENDIF

   CONTAINS
      PURE SUBROUTINE SPLIT_LAST_TOKEN(TOKENS, MAX_TOKENS,ISOK)
      !< Split last token.
      TYPE(STRING), ALLOCATABLE, INTENT(INOUT)        :: TOKENS(:)      !< Tokens substring.
      INTEGER,                   INTENT(IN), OPTIONAL :: MAX_TOKENS     !< Max tokens returned.
      TYPE(STRING), ALLOCATABLE                       :: TOKENS_(:)     !< Temporary tokens.
      TYPE(STRING), ALLOCATABLE                       :: TOKENS_SWAP(:) !< Swap tokens.
      INTEGER                                         :: NT_            !< Number of last created tokens.
      LOGICAL,INTENT(OUT)                             :: ISOK

      ISOK=.TRUE.
      CALL TOKENS(T)%SPLIT(TOKENS=TOKENS_, SEP=SEP_, MAX_TOKENS=MAX_TOKENS)
      IF (ALLOCATED(TOKENS_)) THEN
        NT_ = SIZE(TOKENS_, DIM=1)
        IF (NT_ >= 1) THEN
          ALLOCATE(TOKENS_SWAP(1:T-1+NT_))
          TOKENS_SWAP(1:T-1) = TOKENS(1:T-1)
          TOKENS_SWAP(T:)    = TOKENS_(:)
          CALL MOVE_ALLOC(FROM=TOKENS_SWAP, TO=TOKENS)
        ENDIF
        IF (NT_ == 1) THEN
            ISOK=.FALSE.
        END IF
        DEALLOCATE(TOKENS_)
      ENDIF
      ENDSUBROUTINE SPLIT_LAST_TOKEN
   ENDSUBROUTINE SPLIT_CHUNKED

   ELEMENTAL FUNCTION STARTCASE(SELF, SEP)
   !< Return a string with all words capitalized, e.g. title case.
   !<
   !< @note Multiple subsequent separators are collapsed to one occurence.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 'the Quick Brown fox Jumps over the Lazy Dog.'
   !< test_passed(1) = astring%startcase()//''=='The Quick Brown Fox Jumps Over The Lazy Dog.'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF      !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SEP       !< Separator.
   TYPE(STRING)                                    :: STARTCASE !< Start case string.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SEP_      !< Separator, default value.
   TYPE(STRING), ALLOCATABLE                       :: TOKENS(:) !< String tokens.

   IF (ALLOCATED(SELF%RAW)) THEN
      SEP_ = SPACE ; IF (PRESENT(SEP)) SEP_ = SEP
      CALL SELF%SPLIT(TOKENS=TOKENS, SEP=SEP_)
      TOKENS = TOKENS%CAPITALIZE()
      STARTCASE = STARTCASE%JOIN(ARRAY=TOKENS, SEP=SEP_)
   ENDIF
   ENDFUNCTION STARTCASE

   ELEMENTAL FUNCTION STRIP(SELF, REMOVE_NULLS)
   !< Return a copy of the string with the leading and trailing characters removed.
   !<
   !< @note Multiple subsequent separators are collapsed to one occurence.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = '  Hello World!   '
   !< test_passed(1) = astring%strip()//''=='Hello World!'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)           :: SELF         !< The string.
   LOGICAL,       INTENT(IN), OPTIONAL :: REMOVE_NULLS !< Remove null characters at the end.
   TYPE(STRING)                        :: STRIP        !< The stripped string.
   INTEGER                             :: C            !< Counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      STRIP = SELF%ADJUSTL()
      STRIP = STRIP%TRIM()
      IF (PRESENT(REMOVE_NULLS)) THEN
         IF (REMOVE_NULLS) THEN
            C = INDEX(SELF%RAW, CHAR(0))
            IF (C>0) STRIP%RAW = STRIP%RAW(1:C-1)
         ENDIF
      ENDIF
   ENDIF
   ENDFUNCTION STRIP

   ELEMENTAL FUNCTION SWAPCASE(SELF)
   !< Return a copy of the string with uppercase characters converted to lowercase and vice versa.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = '  Hello World!   '
   !< test_passed(1) = astring%swapcase()//''=='  hELLO wORLD!   '
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< The string.
   TYPE(STRING)              :: SWAPCASE !< Upper case string.
   INTEGER                   :: N1       !< Characters counter.
   INTEGER                   :: N2       !< Characters counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      SWAPCASE = SELF
      DO N1=1, LEN(SELF%RAW)
         N2 = INDEX(UPPER_ALPHABET, SELF%RAW(N1:N1))
         IF (N2>0) THEN
            SWAPCASE%RAW(N1:N1) = LOWER_ALPHABET(N2:N2)
         ELSE
            N2 = INDEX(LOWER_ALPHABET, SELF%RAW(N1:N1))
            IF (N2>0) SWAPCASE%RAW(N1:N1) = UPPER_ALPHABET(N2:N2)
         ENDIF
      ENDDO
   ENDIF
   ENDFUNCTION SWAPCASE

   FUNCTION TEMPNAME(SELF, IS_FILE, PREFIX, PATH)
   !< Return a safe temporary name suitable for temporary file or directories.
   !<
   !<```fortran
   !< type(string) :: astring
   !< character(len=:), allocatable :: tmpname
   !< logical                       :: test_passed(5)
   !< tmpname = astring%tempname()
   !< inquire(file=tmpname, exist=test_passed(1))
   !< test_passed(1) = .not.test_passed(1)
   !< tmpname = astring%tempname(is_file=.false.)
   !< inquire(file=tmpname, exist=test_passed(2))
   !< test_passed(2) = .not.test_passed(2)
   !< tmpname = astring%tempname(path='./')
   !< inquire(file=tmpname, exist=test_passed(3))
   !< test_passed(3) = .not.test_passed(3)
   !< astring = 'me-'
   !< tmpname = astring%tempname()
   !< inquire(file=tmpname, exist=test_passed(4))
   !< test_passed(4) = .not.test_passed(4)
   !< tmpname = astring%tempname(prefix='you-')
   !< inquire(file=tmpname, exist=test_passed(5))
   !< test_passed(5) = .not.test_passed(5)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)           :: SELF                   !< The string.
   LOGICAL,       INTENT(IN), OPTIONAL :: IS_FILE                !< True if tempname should be used for file (THE DEFAULT).
   CHARACTER(*),  INTENT(IN), OPTIONAL :: PREFIX                 !< Name prefix, otherwise self is used (if alLOCATED).
   CHARACTER(*),  INTENT(IN), OPTIONAL :: PATH                   !< Path where file/directory should be used, DEFAULT `./`.
   CHARACTER(LEN=:), ALLOCATABLE       :: TEMPNAME               !< Safe (unique) temporary name.
   LOGICAL                             :: IS_FILE_               !< True if tempname should be used for file (THE DEFAULT).
   CHARACTER(LEN=:), ALLOCATABLE       :: PREFIX_                !< Name prefix, otherwise self is used (if alLOCATED).
   CHARACTER(LEN=:), ALLOCATABLE       :: PATH_                  !< Path where file/directory should be used, DEFAULT `./`.
   LOGICAL, SAVE                       :: IS_INITIALIZED=.FALSE. !< Status of random seed initialization.
   REAL(R4P)                           :: RANDOM_REAL            !< Random number (real).
   INTEGER(I4P)                        :: RANDOM_INTEGER         !< Random number (integer).
   LOGICAL                             :: IS_HOLD                !< Flag to check if a safe tempname has been FOUND.

   IS_FILE_ = .TRUE. ; IF (PRESENT(IS_FILE)) IS_FILE_ = IS_FILE
   PATH_ = '' ; IF (PRESENT(PATH)) PATH_ = PATH
   PREFIX_ = ''
   IF (PRESENT(PREFIX)) THEN
      PREFIX_ = PREFIX
   ELSEIF (ALLOCATED(SELF%RAW)) THEN
      PREFIX_ = SELF%RAW
   ENDIF
   IF (.NOT.IS_INITIALIZED) THEN
      CALL RANDOM_SEED
      IS_INITIALIZED = .TRUE.
   ENDIF
   TEMPNAME = REPEAT(' ', LEN(PATH_) + LEN(PREFIX_) + 10) ! [path_] + [prefix_] + 6 random chars + [.tmp]
   DO
      CALL RANDOM_NUMBER(RANDOM_REAL)
      RANDOM_INTEGER = TRANSFER(RANDOM_REAL, RANDOM_INTEGER)
      RANDOM_INTEGER = IAND(RANDOM_INTEGER, 16777215_I4P)
      IF (IS_FILE_)  THEN
         WRITE(TEMPNAME, '(A,Z6.6,A)') PATH_//PREFIX_, RANDOM_INTEGER, '.tmp'
      ELSE
         WRITE(TEMPNAME, '(A,Z6.6)') PATH_//PREFIX_, RANDOM_INTEGER
         TEMPNAME = TRIM(TEMPNAME)
      ENDIF
      INQUIRE(FILE=TEMPNAME, EXIST=IS_HOLD)
      IF (.NOT.IS_HOLD) EXIT
   ENDDO
   ENDFUNCTION TEMPNAME

   ELEMENTAL FUNCTION TO_INTEGER_I1P(SELF, KIND) RESULT(TO_NUMBER)
   !< Cast string to integer (I1P).
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< integer(I1P) :: integer_
   !< logical      :: test_passed(1)
   !< astring = '127'
   !< integer_ = astring%to_number(kind=1_I1P)
   !< test_passed(1) = integer_==127_I1P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF      !< The string.
   INTEGER(I1P),  INTENT(IN) :: KIND      !< Mold parameter for kind detection.
   INTEGER(I1P)              :: TO_NUMBER !< The number into the string.

   IF (ALLOCATED(SELF%RAW)) THEN
     IF (SELF%IS_INTEGER()) READ(SELF%RAW, *) TO_NUMBER
   ENDIF
   ENDFUNCTION TO_INTEGER_I1P

#ifndef _NVF
   ELEMENTAL FUNCTION TO_INTEGER_I2P(SELF, KIND) RESULT(TO_NUMBER)
   !< Cast string to integer (I2P).
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< integer(I2P) :: integer_
   !< logical      :: test_passed(1)
   !< astring = '127'
   !< integer_ = astring%to_number(kind=1_I2P)
   !< test_passed(1) = integer_==127_I2P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF      !< The string.
   INTEGER(I2P),  INTENT(IN) :: KIND      !< Mold parameter for kind detection.
   INTEGER(I2P)              :: TO_NUMBER !< The number into the string.

   IF (ALLOCATED(SELF%RAW)) THEN
     IF (SELF%IS_INTEGER()) READ(SELF%RAW, *) TO_NUMBER
   ENDIF
   ENDFUNCTION TO_INTEGER_I2P
#endif

   ELEMENTAL FUNCTION TO_INTEGER_I4P(SELF, KIND) RESULT(TO_NUMBER)
   !< Cast string to integer (I4P).
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< integer(I4P) :: integer_
   !< logical      :: test_passed(1)
   !< astring = '127'
   !< integer_ = astring%to_number(kind=1_I4P)
   !< test_passed(1) = integer_==127_I4P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF      !< The string.
   INTEGER(I4P),  INTENT(IN) :: KIND      !< Mold parameter for kind detection.
   INTEGER(I4P)              :: TO_NUMBER !< The number into the string.

   IF (ALLOCATED(SELF%RAW)) THEN
     IF (SELF%IS_INTEGER()) READ(SELF%RAW, *) TO_NUMBER
   ENDIF
   ENDFUNCTION TO_INTEGER_I4P

   ELEMENTAL FUNCTION TO_INTEGER_I8P(SELF, KIND) RESULT(TO_NUMBER)
   !< Cast string to integer (I8P).
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< integer(I8P) :: integer_
   !< logical      :: test_passed(1)
   !< astring = '127'
   !< integer_ = astring%to_number(kind=1_I8P)
   !< test_passed(1) = integer_==127_I8P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF      !< The string.
   INTEGER(I8P),  INTENT(IN) :: KIND      !< Mold parameter for kind detection.
   INTEGER(I8P)              :: TO_NUMBER !< The number into the string.

   IF (ALLOCATED(SELF%RAW)) THEN
     IF (SELF%IS_INTEGER()) READ(SELF%RAW, *) TO_NUMBER
   ENDIF
   ENDFUNCTION TO_INTEGER_I8P

   ELEMENTAL FUNCTION TO_REAL_R4P(SELF, KIND) RESULT(TO_NUMBER)
   !< Cast string to real (R4P).
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< real(R4P)    :: real_
   !< logical      :: test_passed(1)
   !< astring = '3.4e9'
   !< real_ = astring%to_number(kind=1._R4P)
   !< test_passed(1) = real_==3.4e9_R4P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF      !< The string.
   REAL(R4P),     INTENT(IN) :: KIND      !< Mold parameter for kind detection.
   REAL(R4P)                 :: TO_NUMBER !< The number into the string.

   IF (ALLOCATED(SELF%RAW)) THEN
     IF (SELF%IS_REAL()) READ(SELF%RAW, *) TO_NUMBER
   ENDIF
   ENDFUNCTION TO_REAL_R4P

   ELEMENTAL FUNCTION TO_REAL_R8P(SELF, KIND) RESULT(TO_NUMBER)
   !< Cast string to real (R8P).
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< real(R8P)    :: real_
   !< logical      :: test_passed(1)
   !< astring = '3.4e9'
   !< real_ = astring%to_number(kind=1._R8P)
   !< test_passed(1) = real_==3.4e9_R8P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF      !< The string.
   REAL(R8P),     INTENT(IN) :: KIND      !< Mold parameter for kind detection.
   REAL(R8P)                 :: TO_NUMBER !< The number into the string.

   IF (ALLOCATED(SELF%RAW)) THEN
     IF (SELF%IS_REAL()) READ(SELF%RAW, *) TO_NUMBER
   ENDIF
   ENDFUNCTION TO_REAL_R8P

   ELEMENTAL FUNCTION TO_REAL_R16P(SELF, KIND) RESULT(TO_NUMBER)
   !< Cast string to real (R16P).
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< real(R16P)   :: real_
   !< logical      :: test_passed(1)
   !< astring = '3.4e9'
   !< real_ = astring%to_number(kind=1._R16P)
   !< test_passed(1) = real_==3.4e9_R16P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF      !< The string.
   REAL(R16P),    INTENT(IN) :: KIND      !< Mold parameter for kind detection.
   REAL(R16P)                :: TO_NUMBER !< The number into the string.

   IF (ALLOCATED(SELF%RAW)) THEN
     IF (SELF%IS_REAL()) READ(SELF%RAW, *) TO_NUMBER
   ENDIF
   ENDFUNCTION TO_REAL_R16P

   ELEMENTAL FUNCTION UNESCAPE(SELF, TO_UNESCAPE, UNESC) RESULT(UNESCAPED)
   !< Unescape double backslashes (or custom escaped character).
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(2)
   !< astring = '^\\s \\d+\\s*'
   !< test_passed(1) = (astring%unescape(to_unescape='\')//''=='^\s \d+\s*')
   !< test_passed(2) = (astring%unescape(to_unescape='s')//''=='^\s \\d+\s*')
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF        !< The string.
   CHARACTER(KIND=CK, LEN=1), INTENT(IN)           :: TO_UNESCAPE !< Character to be unescaped.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: UNESC       !< Character used to unescape.
   TYPE(STRING)                                    :: UNESCAPED   !< Escaped string.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: UNESC_      !< Character to unescape, local variable.
   INTEGER                                         :: C           !< Character counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      UNESC_ = '' ; IF (PRESENT(UNESC)) UNESC_ = UNESC
      UNESCAPED%RAW = ''
      C = 1
      DO
         IF (C>LEN(SELF%RAW)) EXIT
         IF (C==LEN(SELF%RAW)) THEN
            UNESCAPED%RAW = UNESCAPED%RAW//SELF%RAW(C:C)
            EXIT
         ELSE
            IF (SELF%RAW(C:C+1)==BACKSLASH//TO_UNESCAPE) THEN
               UNESCAPED%RAW = UNESCAPED%RAW//TO_UNESCAPE
               C = C + 2
            ELSE
               UNESCAPED%RAW = UNESCAPED%RAW//SELF%RAW(C:C)
               C = C + 1
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   ENDFUNCTION UNESCAPE

   ELEMENTAL FUNCTION UNIQUE(SELF, SUBSTRING) RESULT(UNIQ)
   !< Reduce to one (unique) multiple (sequential) occurrences of a substring into a string.
   !<
   !< For example the string ' ab-cre-cre-ab' is reduce to 'ab-cre-ab' if the substring is '-cre'.
   !< @note Eventual multiple trailing white space are not reduced to one occurrence.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = '+++ab-++cre-++cre-ab+++++'
   !< test_passed(1) = astring%unique(substring='+')//''=='+ab-+cre-+cre-ab+'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF       !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN), OPTIONAL :: SUBSTRING  !< Substring which multiple occurences must bE REDUCED TO ONE.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE          :: SUBSTRING_ !< Substring, default value.
   TYPE(STRING)                                    :: UNIQ       !< String parsed.
#ifdef _NVF
   CHARACTER(9999)                                 :: NVF_BUG  !< Work around for NVFortran bug.
#endif

   IF (ALLOCATED(SELF%RAW)) THEN
     SUBSTRING_ = SPACE ; IF (PRESENT(SUBSTRING)) SUBSTRING_ = SUBSTRING

     UNIQ = SELF
     DO
#ifdef _NVF
       NVF_BUG = SUBSTRING_
       IF (.NOT.UNIQ%INDEX(REPEAT(TRIM(NVF_BUG), 2))>0) EXIT
       UNIQ = UNIQ%REPLACE(OLD=REPEAT(TRIM(NVF_BUG), 2), NEW=SUBSTRING_)
#else
       IF (.NOT.UNIQ%INDEX(REPEAT(SUBSTRING_, 2))>0) EXIT
       UNIQ = UNIQ%REPLACE(OLD=REPEAT(SUBSTRING_, 2), NEW=SUBSTRING_)
#endif
     ENDDO
   ENDIF
   ENDFUNCTION UNIQUE

   ELEMENTAL FUNCTION UPPER(SELF)
   !< Return a string with all uppercase characters.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 'Hello WorLD!'
   !< test_passed(1) = astring%upper()//''=='HELLO WORLD!'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF  !< The string.
   TYPE(STRING)              :: UPPER !< Upper case string.
   INTEGER                   :: N1    !< Characters counter.
   INTEGER                   :: N2    !< Characters counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      UPPER = SELF
      DO N1=1, LEN(SELF%RAW)
         N2 = INDEX(LOWER_ALPHABET, SELF%RAW(N1:N1))
         IF (N2>0) UPPER%RAW(N1:N1) = UPPER_ALPHABET(N2:N2)
      ENDDO
   ENDIF
   ENDFUNCTION UPPER

   SUBROUTINE WRITE_FILE(SELF, FILE, FORM, IOSTAT, IOMSG)
   !< Write a single string stream into file.
   !<
   !< @note For unformatted read only `access='stream'` is supported with new_line as line terminator.
   !<
   !<```fortran
   !< type(string)              :: astring
   !< type(string)              :: anotherstring
   !< type(string), allocatable :: strings(:)
   !< type(string)              :: line(3)
   !< integer                   :: iostat
   !< character(len=99)         :: iomsg
   !< integer                   :: scratch
   !< integer                   :: l
   !< logical                   :: test_passed(8)
   !< line(1) = ' Hello World!   '
   !< line(2) = 'How are you?  '
   !< line(3) = '   All say: "Fine thanks"'
   !< anotherstring = anotherstring%join(array=line, sep=new_line('a'))
   !< call anotherstring%write_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
   !< call astring%read_file(file='write_file_test.tmp', iostat=iostat, iomsg=iomsg)
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(1) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+1) = (strings(l)==line(l))
   !< enddo
   !< call anotherstring%write_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
   !< call astring%read_file(file='write_file_test.tmp', form='unformatted', iostat=iostat, iomsg=iomsg)
   !< call astring%split(tokens=strings, sep=new_line('a'))
   !< test_passed(5) = (size(strings, dim=1)==size(line, dim=1))
   !< do l=1, size(strings, dim=1)
   !<   test_passed(l+5) = (strings(l)==line(l))
   !< enddo
   !< open(newunit=scratch, file='write_file_test.tmp')
   !< close(unit=scratch, status='delete')
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),    INTENT(IN)              :: SELF    !< The string.
   CHARACTER(LEN=*), INTENT(IN)              :: FILE    !< File name.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL :: FORM    !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL :: IOSTAT  !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: IOMSG   !< IO status message.
   TYPE(STRING)                              :: FORM_   !< Format of unit, local variable.
   INTEGER                                   :: IOSTAT_ !< IO status code, local variable.
   CHARACTER(LEN=:), ALLOCATABLE             :: IOMSG_  !< IO status message, local variable.
   INTEGER                                   :: UNIT    !< Logical unit.

   IOMSG_ = REPEAT(' ', 99) ; IF (PRESENT(IOMSG)) IOMSG_ = IOMSG
   FORM_ = 'FORMATTED' ; IF (PRESENT(FORM)) FORM_ = FORM ; FORM_ = FORM_%UPPER()
   SELECT CASE(FORM_%CHARS())
   CASE('FORMATTED')
      OPEN(NEWUNIT=UNIT, FILE=FILE, ACTION='WRITE', IOMSG=IOMSG_, IOSTAT=IOSTAT_, ERR=10)
   CASE('UNFORMATTED')
      OPEN(NEWUNIT=UNIT, FILE=FILE, ACTION='WRITE', FORM='UNFORMATTED', ACCESS='STREAM', IOMSG=IOMSG_, IOSTAT=IOSTAT_, ERR=10)
   ENDSELECT
   CALL SELF%WRITE_LINES(UNIT=UNIT, FORM=FORM, IOMSG=IOMSG_, IOSTAT=IOSTAT_)
   10 CLOSE(UNIT)
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
   IF (PRESENT(IOMSG)) IOMSG = IOMSG_
   ENDSUBROUTINE WRITE_FILE

   SUBROUTINE WRITE_LINE(SELF, UNIT, FORM, IOSTAT, IOMSG)
   !< Write line (record) to a connected unit.
   !<
   !< @note If the connected unit is unformatted a `new_line()` character is added at the end (if necessary) tO MARK THE END OF LINE.
   !<
   !< @note There is no doctests, this being tested by means of [[string:write_file]] doctests.
   CLASS(STRING),    INTENT(IN)              :: SELF    !< The string.
   INTEGER,          INTENT(IN)              :: UNIT    !< Logical unit.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL :: FORM    !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL :: IOSTAT  !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: IOMSG   !< IO status message.
   TYPE(STRING)                              :: FORM_   !< Format of unit, local variable.
   INTEGER                                   :: IOSTAT_ !< IO status code, local variable.
   CHARACTER(LEN=:), ALLOCATABLE             :: IOMSG_  !< IO status message, local variable.

   IOSTAT_ = 0
   IOMSG_ = REPEAT(' ', 99) ; IF (PRESENT(IOMSG)) IOMSG_ = IOMSG
   IF (ALLOCATED(SELF%RAW)) THEN
      FORM_ = 'FORMATTED' ; IF (PRESENT(FORM)) FORM_ = FORM ; FORM_ = FORM_%UPPER()
      SELECT CASE(FORM_%CHARS())
      CASE('FORMATTED')
         WRITE(UNIT, "(A)", IOSTAT=IOSTAT_, IOMSG=IOMSG_) SELF%RAW
      CASE('UNFORMATTED')
         IF (SELF%END_WITH(NEW_LINE('a'))) THEN
            WRITE(UNIT, IOSTAT=IOSTAT_, IOMSG=IOMSG_) SELF%RAW
         ELSE
            WRITE(UNIT, IOSTAT=IOSTAT_, IOMSG=IOMSG_) SELF%RAW//NEW_LINE('a')
         ENDIF
      ENDSELECT
   ENDIF
   IF (PRESENT(IOSTAT)) IOSTAT = IOSTAT_
   IF (PRESENT(IOMSG)) IOMSG = IOMSG_
   ENDSUBROUTINE WRITE_LINE

   SUBROUTINE WRITE_LINES(SELF, UNIT, FORM, IOSTAT, IOMSG)
   !< Write lines (records) to a connected unit.
   !<
   !< This method checks if self contains more than one line (records) and writes them as lines (records).
   !<
   !< @note If the connected unit is unformatted a `new_line()` character is added at the end (if necessary) tO MARK THE END OF LINE.
   !<
   !< @note There is no doctests, this being tested by means of [[string:write_file]] doctests.
   CLASS(STRING),    INTENT(IN)              :: SELF     !< The string.
   INTEGER,          INTENT(IN)              :: UNIT     !< Logical unit.
   CHARACTER(LEN=*), INTENT(IN),    OPTIONAL :: FORM     !< Format of unit.
   INTEGER,          INTENT(OUT),   OPTIONAL :: IOSTAT   !< IO status code.
   CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: IOMSG    !< IO status message.
   TYPE(STRING), ALLOCATABLE                 :: LINES(:) !< Lines.
   INTEGER                                   :: L        !< Counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      CALL SELF%SPLIT(TOKENS=LINES, SEP=NEW_LINE('a'))
      DO L=1, SIZE(LINES, DIM=1)
         CALL LINES(L)%WRITE_LINE(UNIT=UNIT, FORM=FORM, IOSTAT=IOSTAT, IOMSG=IOMSG)
      ENDDO
   ENDIF
   ENDSUBROUTINE WRITE_LINES

   ! inquire
   ELEMENTAL FUNCTION END_WITH(SELF, SUFFIX, START, END, IGNORE_NULL_EOF)
   !< Return true if a string ends with a specified suffix.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(5)
   !< astring = 'Hello WorLD!'
   !< test_passed(1) = astring%end_with(suffix='LD!').eqv..true.
   !< test_passed(2) = astring%end_with(suffix='lD!').eqv..false.
   !< test_passed(3) = astring%end_with(suffix='orLD!', start=5).eqv..true.
   !< test_passed(4) = astring%end_with(suffix='orLD!', start=8, end=12).eqv..true.
   !< test_passed(5) = astring%end_with(suffix='!').eqv..true.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF             !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: SUFFIX           !< Searched suffix.
   INTEGER,                   INTENT(IN), OPTIONAL :: START            !< Start position into the string.
   INTEGER,                   INTENT(IN), OPTIONAL :: END              !< End position into the string.
   LOGICAL,                   INTENT(IN), OPTIONAL :: IGNORE_NULL_EOF  !< Ignore null character at the end of FILE.
   LOGICAL                                         :: END_WITH         !< Result of the test.
   INTEGER                                         :: START_           !< Start position into the string, locaL VARIABLE.
   INTEGER                                         :: END_             !< End position into the string, local VARIABLE.
   LOGICAL                                         :: IGNORE_NULL_EOF_ !< Ignore null character at the end of FILE, LOCAL VARIABLE.

   END_WITH = .FALSE.
   IF (ALLOCATED(SELF%RAW)) THEN
      START_           = 1             ; IF (PRESENT(START))           START_           = START
      END_             = LEN(SELF%RAW) ; IF (PRESENT(END))             END_             = END
      IGNORE_NULL_EOF_ = .FALSE.       ; IF (PRESENT(IGNORE_NULL_EOF)) IGNORE_NULL_EOF_ = IGNORE_NULL_EOF
      IF (IGNORE_NULL_EOF_.AND.(SELF%RAW(END_:END_) == CHAR(0))) END_ = END_ - 1
      IF (LEN(SUFFIX) <= LEN(SELF%RAW(START_:END_))) THEN
         END_WITH = SELF%RAW(END_-LEN(SUFFIX)+1:END_) == SUFFIX
      ENDIF
   ENDIF
   ENDFUNCTION END_WITH

   ELEMENTAL FUNCTION IS_ALLOCATED(SELF)
   !< Return true if the string is allocated.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(2)
   !< test_passed(1) = astring%is_allocated().eqv..false.
   !< astring = 'hello'
   !< test_passed(2) = astring%is_allocated().eqv..true.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< The string.
   LOGICAL                   :: IS_ALLOCATED !< Result of the test.

   IS_ALLOCATED = ALLOCATED(SELF%RAW)
   ENDFUNCTION IS_ALLOCATED

   ELEMENTAL FUNCTION IS_DIGIT(SELF)
   !< Return true if all characters in the string are digits.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(2)
   !< astring = '   -1212112.3 '
   !< test_passed(1) = astring%is_digit().eqv..false.
   !< astring = '12121123'
   !< test_passed(2) = astring%is_digit().eqv..true.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< The string.
   LOGICAL                   :: IS_DIGIT !< Result of the test.
   INTEGER                   :: C        !< Character counter.

   IS_DIGIT = .FALSE.
   IF (ALLOCATED(SELF%RAW)) THEN
      DO C=1, LEN(SELF%RAW)
         SELECT CASE (SELF%RAW(C:C))
         CASE ('0':'9')
            IS_DIGIT = .TRUE.
         CASE DEFAULT
            IS_DIGIT = .FALSE.
            EXIT
         END SELECT
      ENDDO
   ENDIF
   ENDFUNCTION IS_DIGIT

   ELEMENTAL FUNCTION IS_INTEGER(SELF, ALLOW_SPACES)
   !< Return true if the string contains an integer.
   !<
   !< The regular expression is `\s*[\+\-]?\d+([eE]\+?\d+)?\s*`. The parse algorithm is done in stages:
   !<
   !< | S0  | S1      | S2  | S3   | S4  | S5  | S6  |
   !< |-----|---------|-----|------|-----|-----|-----|
   !< |`\s*`|`[\+\-]?`|`\d+`|`[eE]`|`\+?`|`\d+`|`\s*`|
   !<
   !< Exit on stages-parsing results in:
   !<
   !< | S0 | S1 | S2 | S3 | S4 | S5 | S6 |
   !< |----|----|----|----|----|----|----|
   !< |  F |  F |  T |  F |  F |  T |  T |
   !<
   !< @note This implementation is courtesy of
   !< [tomedunn](https://github.com/tomedunn/fortran-string-utility-module/blob/master/src/string_utility_moduLE.F90#L294)
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(6)
   !< astring = '   -1212112 '
   !< test_passed(1) = astring%is_integer().eqv..true.
   !< astring = '   -1212112'
   !< test_passed(2) = astring%is_integer(allow_spaces=.false.).eqv..false.
   !< astring = '-1212112   '
   !< test_passed(3) = astring%is_integer(allow_spaces=.false.).eqv..false.
   !< astring = '+2e20'
   !< test_passed(4) = astring%is_integer().eqv..true.
   !< astring = ' -2E13 '
   !< test_passed(5) = astring%is_integer().eqv..true.
   !< astring = ' -2 E13 '
   !< test_passed(6) = astring%is_integer().eqv..false.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)           :: SELF          !< The string.
   LOGICAL,       INTENT(IN), OPTIONAL :: ALLOW_SPACES  !< Allow leading-trailing spaces.
   LOGICAL                             :: IS_INTEGER    !< Result of the test.
   LOGICAL                             :: ALLOW_SPACES_ !< Allow leading-trailing spaces, local variable.
   INTEGER                             :: STAGE         !< Stages counter.
   INTEGER                             :: C             !< Character counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      ALLOW_SPACES_ = .TRUE. ; IF (PRESENT(ALLOW_SPACES)) ALLOW_SPACES_ = ALLOW_SPACES
      STAGE = 0
      IS_INTEGER = .TRUE.
      DO C=1, LEN(SELF%RAW)
         SELECT CASE(SELF%RAW(C:C))
         CASE(SPACE, TAB)
            SELECT CASE(STAGE)
            CASE(0, 6)
               IS_INTEGER = ALLOW_SPACES_
            CASE(2, 5)
               IS_INTEGER = ALLOW_SPACES_
               STAGE = 6
            CASE DEFAULT
               IS_INTEGER = .FALSE.
            ENDSELECT
         CASE('-')
            SELECT CASE(STAGE)
            CASE(0)
               STAGE = 1
            CASE DEFAULT
               IS_INTEGER = .FALSE.
            END SELECT
         CASE('+')
            SELECT CASE(STAGE)
            CASE(0)
               STAGE = 1
            CASE(3)
               STAGE = 4
            CASE DEFAULT
               IS_INTEGER = .FALSE.
            ENDSELECT
         CASE('0':'9')
            SELECT CASE(STAGE)
            CASE(0:1)
               STAGE = 2
            CASE(3:4)
               STAGE = 5
            CASE DEFAULT
               CONTINUE
            ENDSELECT
         CASE ('e','E')
            SELECT CASE(STAGE)
            CASE(2)
               STAGE = 3
            CASE DEFAULT
               IS_INTEGER = .FALSE.
            ENDSELECT
         CASE DEFAULT
            IS_INTEGER = .FALSE.
         ENDSELECT
         IF (.NOT.IS_INTEGER) EXIT
      ENDDO
   ENDIF
   IF (IS_INTEGER) THEN
      SELECT CASE(STAGE)
      CASE(2, 5, 6)
         IS_INTEGER = .TRUE.
      CASE DEFAULT
         IS_INTEGER = .FALSE.
      END SELECT
   ENDIF
   ENDFUNCTION IS_INTEGER

   ELEMENTAL FUNCTION IS_LOWER(SELF)
   !< Return true if all characters in the string are lowercase.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(3)
   !< astring = ' Hello World'
   !< test_passed(1) = astring%is_lower().eqv..false.
   !< astring = ' HELLO WORLD'
   !< test_passed(2) = astring%is_lower().eqv..false.
   !< astring = ' hello world'
   !< test_passed(3) = astring%is_lower().eqv..true.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< The string.
   LOGICAL                   :: IS_LOWER !< Result of the test.
   INTEGER                   :: C        !< Character counter.

   IS_LOWER = .FALSE.
   IF (ALLOCATED(SELF%RAW)) THEN
      IS_LOWER = .TRUE.
      DO C=1, LEN(SELF%RAW)
         IF (INDEX(UPPER_ALPHABET, SELF%RAW(C:C))>0) THEN
            IS_LOWER = .FALSE.
            EXIT
         ENDIF
      ENDDO
   ENDIF
   ENDFUNCTION IS_LOWER

   ELEMENTAL FUNCTION IS_NUMBER(SELF, ALLOW_SPACES)
   !< Return true if the string contains a number (real or integer).
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(7)
   !< astring = '   -1212112 '
   !< test_passed(1) = astring%is_number().eqv..true.
   !< astring = '   -121.2112 '
   !< test_passed(2) = astring%is_number().eqv..true.
   !< astring = '   -1212112'
   !< test_passed(3) = astring%is_number(allow_spaces=.false.).eqv..false.
   !< astring = '-12121.12   '
   !< test_passed(4) = astring%is_number(allow_spaces=.false.).eqv..false.
   !< astring = '+2e20'
   !< test_passed(5) = astring%is_number().eqv..true.
   !< astring = ' -2.4E13 '
   !< test_passed(6) = astring%is_number().eqv..true.
   !< astring = ' -2 E13 '
   !< test_passed(7) = astring%is_number().eqv..false.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)           :: SELF         !< The string.
   LOGICAL,       INTENT(IN), OPTIONAL :: ALLOW_SPACES !< Allow leading-trailing spaces.
   LOGICAL                             :: IS_NUMBER    !< Result of the test.

   IS_NUMBER = (SELF%IS_INTEGER(ALLOW_SPACES=ALLOW_SPACES).OR.SELF%IS_REAL(ALLOW_SPACES=ALLOW_SPACES))
   ENDFUNCTION IS_NUMBER

   ELEMENTAL FUNCTION IS_REAL(SELF, ALLOW_SPACES)
   !< Return true if the string contains a real.
   !<
   !< The regular expression is `\s*[\+\-]?\d*(|\.?\d*([deDE][\+\-]?\d+)?)\s*`. The parse algorithm is done in STAGES:
   !<
   !< | S0  | S1      | S2  | S3  | S4  | S5     | S6      | S7  | S8  |
   !< |-----|---------|-----|-----|-----|--------|---------|-----|-----|
   !< |`\s*`|`[\+\-]?`|`\d*`|`\.?`|`\d*`|`[deDE]`|`[\+\-]?`|`\d*`|`\s*`|
   !<
   !< Exit on stages-parsing results in:
   !<
   !< | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 |
   !< |----|----|----|----|----|----|----|----|----|
   !  |  F |  F |  T |  T |  T |  F |  F |  T |  T |
   !<
   !< @note This implementation is courtesy of
   !< [tomedunn](https://github.com/tomedunn/fortran-string-utility-module/blob/master/src/string_utility_moduLE.F90#L614)
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(6)
   !< astring = '   -1212112.d0 '
   !< test_passed(1) = astring%is_real().eqv..true.
   !< astring = '   -1212112.d0'
   !< test_passed(2) = astring%is_real(allow_spaces=.false.).eqv..false.
   !< astring = '-1212112.d0   '
   !< test_passed(3) = astring%is_real(allow_spaces=.false.).eqv..false.
   !< astring = '+2.e20'
   !< test_passed(4) = astring%is_real().eqv..true.
   !< astring = ' -2.01E13 '
   !< test_passed(5) = astring%is_real().eqv..true.
   !< astring = ' -2.01 E13 '
   !< test_passed(6) = astring%is_real().eqv..false.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)           :: SELF              !< The string.
   LOGICAL,       INTENT(IN), OPTIONAL :: ALLOW_SPACES      !< Allow leading-trailing spaces.
   LOGICAL                             :: IS_REAL           !< Result of the test.
   LOGICAL                             :: ALLOW_SPACES_     !< Allow leading-trailing spaces, local variable.
   LOGICAL                             :: HAS_LEADING_DIGIT !< Check the presence of leading digits.
   INTEGER                             :: STAGE             !< Stages counter.
   INTEGER                             :: C                 !< Character counter.

   IF (ALLOCATED(SELF%RAW)) THEN
      ALLOW_SPACES_ = .TRUE. ; IF (PRESENT(ALLOW_SPACES)) ALLOW_SPACES_ = ALLOW_SPACES
      STAGE = 0
      IS_REAL = .TRUE.
      HAS_LEADING_DIGIT = .FALSE.
      DO C=1, LEN(SELF%RAW)
         SELECT CASE(SELF%RAW(C:C))
         CASE(SPACE, TAB)
            SELECT CASE(STAGE)
            CASE(0, 8)
               IS_REAL = ALLOW_SPACES_
               CONTINUE
            CASE(2:4, 7)
               IS_REAL = ALLOW_SPACES_
               STAGE = 8
            CASE DEFAULT
               IS_REAL = .FALSE.
            ENDSELECT
         CASE('+', '-')
            SELECT CASE(STAGE)
            CASE(0)
               STAGE = 1
            CASE(5)
               STAGE = 6
            CASE DEFAULT
               IS_REAL = .FALSE.
            ENDSELECT
         CASE('0':'9')
            SELECT CASE(STAGE)
            CASE(0:1)
               STAGE = 2
               HAS_LEADING_DIGIT = .TRUE.
            CASE(3)
               STAGE = 4
            CASE(5:6)
               STAGE = 7
            CASE DEFAULT
               CONTINUE
            ENDSELECT
         CASE('.')
            SELECT CASE(STAGE)
            CASE(0:2)
               STAGE = 3
            CASE DEFAULT
               IS_REAL = .FALSE.
            ENDSELECT
         CASE('e','E','d','D')
            SELECT CASE(STAGE)
            CASE(2:4)
               STAGE = 5
            CASE DEFAULT
               IS_REAL = .FALSE.
            ENDSELECT
         CASE DEFAULT
            IS_REAL = .FALSE.
         ENDSELECT
         IF (.NOT.IS_REAL) EXIT
      ENDDO
   ENDIF
   IF (IS_REAL) THEN
      SELECT CASE(STAGE)
      CASE(2, 4, 7, 8)
         IS_REAL = .TRUE.
      CASE(3)
         IS_REAL = HAS_LEADING_DIGIT
      CASE DEFAULT
         IS_REAL = .FALSE.
      ENDSELECT
   ENDIF
   ENDFUNCTION IS_REAL

   ELEMENTAL FUNCTION IS_UPPER(SELF)
   !< Return true if all characters in the string are uppercase.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(3)
   !< astring = ' Hello World'
   !< test_passed(1) = astring%is_upper().eqv..false.
   !< astring = ' HELLO WORLD'
   !< test_passed(2) = astring%is_upper().eqv..true.
   !< astring = ' hello world'
   !< test_passed(3) = astring%is_upper().eqv..false.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: SELF     !< The string.
   LOGICAL                   :: IS_UPPER !< Result of the test.
   INTEGER                   :: C        !< Character counter.

   IS_UPPER = .FALSE.
   IF (ALLOCATED(SELF%RAW)) THEN
      IS_UPPER = .TRUE.
      DO C=1, LEN(SELF%RAW)
         IF (INDEX(LOWER_ALPHABET, SELF%RAW(C:C))>0) THEN
            IS_UPPER = .FALSE.
            EXIT
         ENDIF
      ENDDO
   ENDIF
   ENDFUNCTION IS_UPPER

   ELEMENTAL FUNCTION START_WITH(SELF, PREFIX, START, END)
   !< Return true if a string starts with a specified prefix.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(4)
   !< astring = 'Hello WorLD!'
   !< test_passed(1) = astring%start_with(prefix='Hello').eqv..true.
   !< test_passed(2) = astring%start_with(prefix='hell').eqv..false.
   !< test_passed(3) = astring%start_with(prefix='llo Wor', start=3).eqv..true.
   !< test_passed(4) = astring%start_with(prefix='lo W', start=4, end=7).eqv..true.
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)           :: SELF       !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)           :: PREFIX     !< Searched prefix.
   INTEGER,                   INTENT(IN), OPTIONAL :: START      !< Start position into the string.
   INTEGER,                   INTENT(IN), OPTIONAL :: END        !< End position into the string.
   LOGICAL                                         :: START_WITH !< Result of the test.
   INTEGER                                         :: START_     !< Start position into the string, local variABLE.
   INTEGER                                         :: END_       !< End position into the string, local variabLE.

   START_WITH = .FALSE.
   IF (ALLOCATED(SELF%RAW)) THEN
      START_ = 1             ; IF (PRESENT(START)) START_ = START
      END_   = LEN(SELF%RAW) ; IF (PRESENT(END))   END_   = END
      IF (LEN(PREFIX)<=LEN(SELF%RAW(START_:END_))) THEN
         START_WITH = INDEX(SELF%RAW(START_:END_), PREFIX)==1
      ENDIF
   ENDIF
   ENDFUNCTION START_WITH

   ! private methods

   ! assignments
   PURE SUBROUTINE STRING_ASSIGN_STRING(LHS, RHS)
   !< Assignment operator from string input.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(1)
   !< astring = 'hello'
   !< anotherstring = astring
   !< test_passed(1) = astring%chars()==anotherstring%chars()
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: LHS !< Left hand side.
   TYPE(STRING),  INTENT(IN)    :: RHS !< Right hand side.

   IF (ALLOCATED(RHS%RAW)) LHS%RAW = RHS%RAW
   ENDSUBROUTINE STRING_ASSIGN_STRING

   PURE SUBROUTINE STRING_ASSIGN_CHARACTER(LHS, RHS)
   !< Assignment operator from character input.
   !<
   !<```fortran
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 'hello'
   !< test_passed(1) = astring%chars()=='hello'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(INOUT) :: LHS !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)    :: RHS !< Right hand side.

   LHS%RAW = RHS
   ENDSUBROUTINE STRING_ASSIGN_CHARACTER

   PURE SUBROUTINE STRING_ASSIGN_INTEGER_I1P(LHS, RHS)
   !< Assignment operator from integer input.
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 127_I1P
   !< test_passed(1) = astring%to_number(kind=1_I1P)==127_I1P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: LHS !< Left hand side.
   INTEGER(I1P),  INTENT(IN)    :: RHS !< Right hand side.

   LHS%RAW = TRIM(STR(RHS))
   ENDSUBROUTINE STRING_ASSIGN_INTEGER_I1P

   PURE SUBROUTINE STRING_ASSIGN_INTEGER_I2P(LHS, RHS)
   !< Assignment operator from integer input.
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 127_I2P
   !< test_passed(1) = astring%to_number(kind=1_I2P)==127_I2P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: LHS !< Left hand side.
   INTEGER(I2P),  INTENT(IN)    :: RHS !< Right hand side.

   LHS%RAW = TRIM(STR(RHS))
   ENDSUBROUTINE STRING_ASSIGN_INTEGER_I2P

   PURE SUBROUTINE STRING_ASSIGN_INTEGER_I4P(LHS, RHS)
   !< Assignment operator from integer input.
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 127_I4P
   !< test_passed(1) = astring%to_number(kind=1_I4P)==127_I4P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: LHS !< Left hand side.
   INTEGER(I4P),  INTENT(IN)    :: RHS !< Right hand side.

   LHS%RAW = TRIM(STR(RHS))
   ENDSUBROUTINE STRING_ASSIGN_INTEGER_I4P

   PURE SUBROUTINE STRING_ASSIGN_INTEGER_I8P(LHS, RHS)
   !< Assignment operator from integer input.
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 127_I8P
   !< test_passed(1) = astring%to_number(kind=1_I8P)==127_I8P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: LHS !< Left hand side.
   INTEGER(I8P),  INTENT(IN)    :: RHS !< Right hand side.

   LHS%RAW = TRIM(STR(RHS))
   ENDSUBROUTINE STRING_ASSIGN_INTEGER_I8P

   PURE SUBROUTINE STRING_ASSIGN_REAL_R4P(LHS, RHS)
   !< Assignment operator from real input.
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 3.021e6_R4P
   !< test_passed(1) = astring%to_number(kind=1._R4P)==3.021e6_R4P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: LHS !< Left hand side.
   REAL(R4P),     INTENT(IN)    :: RHS !< Right hand side.

   LHS%RAW = TRIM(STR(RHS))
   ENDSUBROUTINE STRING_ASSIGN_REAL_R4P

   PURE SUBROUTINE STRING_ASSIGN_REAL_R8P(LHS, RHS)
   !< Assignment operator from real input.
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 3.021e6_R8P
   !< test_passed(1) = astring%to_number(kind=1._R8P)==3.021e6_R8P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: LHS !< Left hand side.
   REAL(R8P),     INTENT(IN)    :: RHS !< Right hand side.

   LHS%RAW = TRIM(STR(RHS))
   ENDSUBROUTINE STRING_ASSIGN_REAL_R8P

   PURE SUBROUTINE STRING_ASSIGN_REAL_R16P(LHS, RHS)
   !< Assignment operator from real input.
   !<
   !<```fortran
   !< use penf
   !< type(string) :: astring
   !< logical      :: test_passed(1)
   !< astring = 3.021e6_R8P
   !< test_passed(1) = astring%to_number(kind=1._R8P)==3.021e6_R8P
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(INOUT) :: LHS !< Left hand side.
   REAL(R16P),    INTENT(IN)    :: RHS !< Right hand side.

   LHS%RAW = TRIM(STR(RHS))
   ENDSUBROUTINE STRING_ASSIGN_REAL_R16P

   ! contatenation operators
   PURE FUNCTION STRING_CONCAT_STRING(LHS, RHS) RESULT(CONCAT)
   !< Concatenation with string.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(1)
   !< astring = 'Hello '
   !< anotherstring = 'Bye bye'
   !< test_passed(1) = astring//anotherstring=='Hello Bye bye'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)              :: LHS    !< Left hand side.
   TYPE(STRING),  INTENT(IN)              :: RHS    !< Right hand side.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: CONCAT !< Concatenated string.

   CONCAT = ''
   IF (ALLOCATED(LHS%RAW)) CONCAT = LHS%RAW
   IF (ALLOCATED(RHS%RAW)) CONCAT = CONCAT//RHS%RAW
   ENDFUNCTION STRING_CONCAT_STRING

   PURE FUNCTION STRING_CONCAT_CHARACTER(LHS, RHS) RESULT(CONCAT)
   !< Concatenation with character.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(1)
   !< astring = 'Hello '
   !< acharacter = 'World!'
   !< test_passed(1) = astring//acharacter=='Hello World!'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)  :: LHS    !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)  :: RHS    !< Right hand side.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: CONCAT !< Concatenated string.

   IF (ALLOCATED(LHS%RAW)) THEN
      CONCAT = LHS%RAW//RHS
   ELSE
      CONCAT = RHS
   ENDIF
   ENDFUNCTION STRING_CONCAT_CHARACTER

   PURE FUNCTION CHARACTER_CONCAT_STRING(LHS, RHS) RESULT(CONCAT)
   !< Concatenation with character (inverted).
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(1)
   !< astring = 'Hello '
   !< acharacter = 'World!'
   !< test_passed(1) = acharacter//astring=='World!Hello '
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)  :: LHS    !< Left hand side.
   CLASS(STRING),             INTENT(IN)  :: RHS    !< Right hand side.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: CONCAT !< Concatenated string.

   IF (ALLOCATED(RHS%RAW)) THEN
      CONCAT = LHS//RHS%RAW
   ELSE
      CONCAT = LHS
   ENDIF
   ENDFUNCTION CHARACTER_CONCAT_STRING

   ELEMENTAL FUNCTION STRING_CONCAT_STRING_STRING(LHS, RHS) RESULT(CONCAT)
   !< Concatenation with string.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< type(string) :: yetanotherstring
   !< logical      :: test_passed(1)
   !< astring = 'Hello '
   !< anotherstring = 'Bye bye'
   !< yetanotherstring = astring.cat.anotherstring
   !< test_passed(1) = yetanotherstring%chars()=='Hello Bye bye'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN)              :: LHS       !< Left hand side.
   TYPE(STRING),  INTENT(IN)              :: RHS       !< Right hand side.
   TYPE(STRING)                           :: CONCAT    !< Concatenated string.
   CHARACTER(KIND=CK, LEN=:), ALLOCATABLE :: TEMPORARY !< Temporary concatenated string.

   TEMPORARY = ''
   IF (ALLOCATED(LHS%RAW)) TEMPORARY = LHS%RAW
   IF (ALLOCATED(RHS%RAW)) TEMPORARY = TEMPORARY//RHS%RAW
   IF (TEMPORARY/='') CONCAT%RAW = TEMPORARY
   ENDFUNCTION STRING_CONCAT_STRING_STRING

   ELEMENTAL FUNCTION STRING_CONCAT_CHARACTER_STRING(LHS, RHS) RESULT(CONCAT)
   !< Concatenation with character.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< type(string)                  :: yetanotherstring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(1)
   !< astring = 'Hello '
   !< acharacter = 'World!'
   !< yetanotherstring = astring.cat.acharacter
   !< test_passed(1) = yetanotherstring%chars()=='Hello World!'
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN)  :: LHS    !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)  :: RHS    !< Right hand side.
   TYPE(STRING)                           :: CONCAT !< Concatenated string.

   IF (ALLOCATED(LHS%RAW)) THEN
      CONCAT%RAW = LHS%RAW//RHS
   ELSE
      CONCAT%RAW = RHS
   ENDIF
   ENDFUNCTION STRING_CONCAT_CHARACTER_STRING

   ELEMENTAL FUNCTION CHARACTER_CONCAT_STRING_STRING(LHS, RHS) RESULT(CONCAT)
   !< Concatenation with character (inverted).
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< type(string)                  :: yetanotherstring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(1)
   !< astring = 'Hello '
   !< acharacter = 'World!'
   !< yetanotherstring = acharacter.cat.astring
   !< test_passed(1) = yetanotherstring%chars()=='World!Hello '
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)  :: LHS    !< Left hand side.
   CLASS(STRING),             INTENT(IN)  :: RHS    !< Right hand side.
   TYPE(STRING)                           :: CONCAT !< Concatenated string.

   IF (ALLOCATED(RHS%RAW)) THEN
     CONCAT%RAW = LHS//RHS%RAW
   ELSE
     CONCAT%RAW = LHS
   ENDIF
   ENDFUNCTION CHARACTER_CONCAT_STRING_STRING

   ! logical operators
   ELEMENTAL FUNCTION STRING_EQ_STRING(LHS, RHS) RESULT(IS_IT)
   !< Equal to string logical operator.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(2)
   !< astring = '  one '
   !< anotherstring = 'two'
   !< test_passed(1) = ((astring==anotherstring).eqv..false.)
   !< astring = 'the same '
   !< anotherstring = 'the same '
   !< test_passed(2) = ((astring==anotherstring).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: LHS   !< Left hand side.
   TYPE(STRING),  INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                   :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW == RHS%RAW
   ENDFUNCTION STRING_EQ_STRING

   ELEMENTAL FUNCTION STRING_EQ_CHARACTER(LHS, RHS) RESULT(IS_IT)
   !< Equal to character logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(2)
   !< astring = '  one '
   !< acharacter = 'three'
   !< test_passed(1) = ((astring==acharacter).eqv..false.)
   !< astring = 'the same '
   !< acharacter = 'the same '
   !< test_passed(2) = ((astring==acharacter).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN) :: LHS   !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW == RHS
   ENDFUNCTION STRING_EQ_CHARACTER

   ELEMENTAL FUNCTION CHARACTER_EQ_STRING(LHS, RHS) RESULT(IS_IT)
   !< Equal to character (inverted) logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(2)
   !< astring = '  one '
   !< acharacter = 'three'
   !< test_passed(1) = ((acharacter==astring).eqv..false.)
   !< astring = 'the same '
   !< acharacter = 'the same '
   !< test_passed(2) = ((acharacter==astring).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: LHS   !< Left hand side.
   CLASS(STRING),             INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = RHS%RAW == LHS
   ENDFUNCTION CHARACTER_EQ_STRING

   ELEMENTAL FUNCTION STRING_NE_STRING(LHS, RHS) RESULT(IS_IT)
   !< Not equal to string logical operator.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(2)
   !< astring = '  one '
   !< anotherstring = 'two'
   !< test_passed(1) = ((astring/=anotherstring).eqv..true.)
   !< astring = 'the same '
   !< anotherstring = 'the same '
   !< test_passed(2) = ((astring/=anotherstring).eqv..false.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: LHS   !< Left hand side.
   TYPE(STRING),  INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                   :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW /= RHS%RAW
   ENDFUNCTION STRING_NE_STRING

   ELEMENTAL FUNCTION STRING_NE_CHARACTER(LHS, RHS) RESULT(IS_IT)
   !< Not equal to character logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(2)
   !< astring = '  one '
   !< acharacter = 'three'
   !< test_passed(1) = ((astring/=acharacter).eqv..true.)
   !< astring = 'the same '
   !< acharacter = 'the same '
   !< test_passed(2) = ((astring/=acharacter).eqv..false.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN) :: LHS   !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW /= RHS
   ENDFUNCTION STRING_NE_CHARACTER

   ELEMENTAL FUNCTION CHARACTER_NE_STRING(LHS, RHS) RESULT(IS_IT)
   !< Not equal to character (inverted) logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(2)
   !< astring = '  one '
   !< acharacter = 'three'
   !< test_passed(1) = ((acharacter/=astring).eqv..true.)
   !< astring = 'the same '
   !< acharacter = 'the same '
   !< test_passed(2) = ((acharacter/=astring).eqv..false.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: LHS   !< Left hand side.
   CLASS(STRING),             INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = RHS%RAW /= LHS
   ENDFUNCTION CHARACTER_NE_STRING

   ELEMENTAL FUNCTION STRING_LT_STRING(LHS, RHS) RESULT(IS_IT)
   !< Lower than to string logical operator.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(2)
   !< astring = 'one'
   !< anotherstring = 'ONE'
   !< test_passed(1) = ((astring<anotherstring).eqv..false.)
   !< astring = 'ONE'
   !< anotherstring = 'one'
   !< test_passed(2) = ((astring<anotherstring).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: LHS   !< Left hand side.
   TYPE(STRING),  INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                   :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW < RHS%RAW
   ENDFUNCTION STRING_LT_STRING

   ELEMENTAL FUNCTION STRING_LT_CHARACTER(LHS, RHS) RESULT(IS_IT)
   !< Lower than to character logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(2)
   !< astring = 'one'
   !< acharacter = 'ONE'
   !< test_passed(1) = ((astring<acharacter).eqv..false.)
   !< astring = 'ONE'
   !< acharacter = 'one'
   !< test_passed(2) = ((astring<acharacter).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN) :: LHS   !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW < RHS
   ENDFUNCTION STRING_LT_CHARACTER

   ELEMENTAL FUNCTION CHARACTER_LT_STRING(LHS, RHS) RESULT(IS_IT)
   !< Lower than to character (inverted) logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(2)
   !< astring = 'one'
   !< acharacter = 'ONE'
   !< test_passed(1) = ((acharacter<astring).eqv..true.)
   !< astring = 'ONE'
   !< acharacter = 'one'
   !< test_passed(2) = ((acharacter<astring).eqv..false.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: LHS   !< Left hand side.
   CLASS(STRING),             INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS < RHS%RAW
   ENDFUNCTION CHARACTER_LT_STRING

   ELEMENTAL FUNCTION STRING_LE_STRING(LHS, RHS) RESULT(IS_IT)
   !< Lower equal than to string logical operator.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(3)
   !< astring = 'one'
   !< anotherstring = 'ONE'
   !< test_passed(1) = ((astring<=anotherstring).eqv..false.)
   !< astring = 'ONE'
   !< anotherstring = 'one'
   !< test_passed(2) = ((astring<=anotherstring).eqv..true.)
   !< astring = 'ONE'
   !< anotherstring = 'ONE'
   !< test_passed(3) = ((astring<=anotherstring).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: LHS   !< Left hand side.
   TYPE(STRING),  INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                   :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW <= RHS%RAW
   ENDFUNCTION STRING_LE_STRING

   ELEMENTAL FUNCTION STRING_LE_CHARACTER(LHS, RHS) RESULT(IS_IT)
   !< Lower equal than to character logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(3)
   !< astring = 'one'
   !< acharacter = 'ONE'
   !< test_passed(1) = ((astring<=acharacter).eqv..false.)
   !< astring = 'ONE'
   !< acharacter = 'one'
   !< test_passed(2) = ((astring<=acharacter).eqv..true.)
   !< astring = 'ONE'
   !< acharacter = 'ONE'
   !< test_passed(3) = ((astring<=acharacter).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN) :: LHS   !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW <= RHS
   ENDFUNCTION STRING_LE_CHARACTER

   ELEMENTAL FUNCTION CHARACTER_LE_STRING(LHS, RHS) RESULT(IS_IT)
   !< Lower equal than to character (inverted) logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(3)
   !< astring = 'one'
   !< acharacter = 'ONE'
   !< test_passed(1) = ((acharacter<=astring).eqv..true.)
   !< astring = 'ONE'
   !< acharacter = 'one'
   !< test_passed(2) = ((acharacter<=astring).eqv..false.)
   !< astring = 'ONE'
   !< acharacter = 'ONE'
   !< test_passed(3) = ((acharacter<=astring).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: LHS   !< Left hand side.
   CLASS(STRING),             INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS <= RHS%RAW
   ENDFUNCTION CHARACTER_LE_STRING

   ELEMENTAL FUNCTION STRING_GE_STRING(LHS, RHS) RESULT(IS_IT)
   !< Greater equal than to string logical operator.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(3)
   !< astring = 'one'
   !< anotherstring = 'ONE'
   !< test_passed(1) = ((astring>=anotherstring).eqv..true.)
   !< astring = 'ONE'
   !< anotherstring = 'one'
   !< test_passed(2) = ((astring>=anotherstring).eqv..false.)
   !< astring = 'ONE'
   !< anotherstring = 'ONE'
   !< test_passed(3) = ((astring>=anotherstring).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: LHS   !< Left hand side.
   TYPE(STRING),  INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                   :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW >= RHS%RAW
   ENDFUNCTION STRING_GE_STRING

   ELEMENTAL FUNCTION STRING_GE_CHARACTER(LHS, RHS) RESULT(IS_IT)
   !< Greater equal than to character logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(3)
   !< astring = 'one'
   !< acharacter = 'ONE'
   !< test_passed(1) = ((astring>=acharacter).eqv..true.)
   !< astring = 'ONE'
   !< acharacter = 'one'
   !< test_passed(2) = ((astring>=acharacter).eqv..false.)
   !< astring = 'ONE'
   !< acharacter = 'ONE'
   !< test_passed(3) = ((astring>=acharacter).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN) :: LHS   !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW >= RHS
   ENDFUNCTION STRING_GE_CHARACTER

   ELEMENTAL FUNCTION CHARACTER_GE_STRING(LHS, RHS) RESULT(IS_IT)
   !< Greater equal than to character (inverted) logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(3)
   !< astring = 'one'
   !< acharacter = 'ONE'
   !< test_passed(1) = ((acharacter>=astring).eqv..false.)
   !< astring = 'ONE'
   !< acharacter = 'one'
   !< test_passed(2) = ((acharacter>=astring).eqv..true.)
   !< astring = 'ONE'
   !< acharacter = 'ONE'
   !< test_passed(3) = ((acharacter>=astring).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: LHS   !< Left hand side.
   CLASS(STRING),             INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS >= RHS%RAW
   ENDFUNCTION CHARACTER_GE_STRING

   ELEMENTAL FUNCTION STRING_GT_STRING(LHS, RHS) RESULT(IS_IT)
   !< Greater than to string logical operator.
   !<
   !<```fortran
   !< type(string) :: astring
   !< type(string) :: anotherstring
   !< logical      :: test_passed(2)
   !< astring = 'one'
   !< anotherstring = 'ONE'
   !< test_passed(1) = ((astring>anotherstring).eqv..true.)
   !< astring = 'ONE'
   !< anotherstring = 'one'
   !< test_passed(2) = ((astring>anotherstring).eqv..false.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING), INTENT(IN) :: LHS   !< Left hand side.
   TYPE(STRING),  INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                   :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW > RHS%RAW
   ENDFUNCTION STRING_GT_STRING

   ELEMENTAL FUNCTION STRING_GT_CHARACTER(LHS, RHS) RESULT(IS_IT)
   !< Greater than to character logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(2)
   !< astring = 'one'
   !< acharacter = 'ONE'
   !< test_passed(1) = ((astring>acharacter).eqv..true.)
   !< astring = 'ONE'
   !< acharacter = 'one'
   !< test_passed(2) = ((astring>acharacter).eqv..false.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CLASS(STRING),             INTENT(IN) :: LHS   !< Left hand side.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS%RAW > RHS
   ENDFUNCTION STRING_GT_CHARACTER

   ELEMENTAL FUNCTION CHARACTER_GT_STRING(LHS, RHS) RESULT(IS_IT)
   !< Greater than to character (inverted) logical operator.
   !<
   !<```fortran
   !< type(string)                  :: astring
   !< character(len=:), allocatable :: acharacter
   !< logical                       :: test_passed(2)
   !< astring = 'one'
   !< acharacter = 'ONE'
   !< test_passed(1) = ((acharacter>astring).eqv..false.)
   !< astring = 'ONE'
   !< acharacter = 'one'
   !< test_passed(2) = ((acharacter>astring).eqv..true.)
   !< print '(L1)', all(test_passed)
   !<```
   !=> T <<<
   CHARACTER(KIND=CK, LEN=*), INTENT(IN) :: LHS   !< Left hand side.
   CLASS(STRING),             INTENT(IN) :: RHS   !< Right hand side.
   LOGICAL                               :: IS_IT !< Opreator test result.

   IS_IT = LHS > RHS%RAW
   ENDFUNCTION CHARACTER_GT_STRING

   ! IO
   SUBROUTINE READ_FORMATTED(DTV, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
   !< Formatted input.
   !<
   !< @bug Change temporary acks: find a more precise length of the input string and avoid the trimming!
   !<
   !< @bug Read listdirected with and without delimiters does not work.
   CLASS(STRING),             INTENT(INOUT) :: DTV         !< The string.
   INTEGER,                   INTENT(IN)    :: UNIT        !< Logical unit.
   CHARACTER(LEN=*),          INTENT(IN)    :: IOTYPE      !< Edit descriptor.
   INTEGER,                   INTENT(IN)    :: V_LIST(:)   !< Edit descriptor list.
   INTEGER,                   INTENT(OUT)   :: IOSTAT      !< IO status code.
   CHARACTER(LEN=*),          INTENT(INOUT) :: IOMSG       !< IO status message.
   CHARACTER(LEN=LEN(IOMSG))                :: LOCAL_IOMSG !< Local variant of iomsg, so it doesn't get inapprOPRIATELY REDEFINED.
   CHARACTER(KIND=CK, LEN=1)                :: DELIM       !< String delimiter, if any.
   CHARACTER(KIND=CK, LEN=100)              :: TEMPORARY   !< Temporary storage string.

   IF (IOTYPE == 'LISTDIRECTED') THEN
      CALL GET_NEXT_NON_BLANK_CHARACTER_ANY_RECORD(UNIT=UNIT, CH=DELIM, IOSTAT=IOSTAT, IOMSG=IOMSG)
      IF (IOSTAT/=0) RETURN
      IF (DELIM=='"'.OR.DELIM=="'") THEN
         CALL DTV%READ_DELIMITED(UNIT=UNIT, DELIM=DELIM, IOSTAT=IOSTAT, IOMSG=LOCAL_IOMSG)
      ELSE
         ! step back before the non-blank
         READ(UNIT, "(TL1)", IOSTAT=IOSTAT, IOMSG=IOMSG)
         IF (IOSTAT /= 0) RETURN
         CALL DTV%READ_UNDELIMITED_LISTDIRECTED(UNIT=UNIT, IOSTAT=IOSTAT, IOMSG=LOCAL_IOMSG)
      ENDIF
      IF (IS_IOSTAT_EOR(IOSTAT)) THEN
         ! suppress IOSTAT_EOR
         IOSTAT = 0
      ELSEIF (IOSTAT /= 0) THEN
         IOMSG = LOCAL_IOMSG
      ENDIF
      RETURN
   ELSE
      READ(UNIT, "(A)", IOSTAT=IOSTAT, IOMSG=IOMSG)TEMPORARY
      DTV%RAW = TRIM(TEMPORARY)
   ENDIF
   ENDSUBROUTINE READ_FORMATTED

   SUBROUTINE READ_DELIMITED(DTV, UNIT, DELIM, IOSTAT, IOMSG)
   !< Read a delimited string from a unit connected for formatted input.
   !<
   !< If the closing delimiter is followed by end of record, then we return end of record.
   !<
   !< @note This does not need a doctest, it being tested by [[string::read_formatted]].
   CLASS(STRING),             INTENT(OUT)   :: DTV       !< The string.
   INTEGER,                   INTENT(IN)    :: UNIT      !< Logical unit.
   CHARACTER(KIND=CK, LEN=1), INTENT(IN)    :: DELIM     !< String delimiter.
   INTEGER,                   INTENT(OUT)   :: IOSTAT    !< IO status code.
   CHARACTER(KIND=CK, LEN=*), INTENT(INOUT) :: IOMSG     !< IO status message.
   CHARACTER(KIND=CK, LEN=1)                :: CH        !< A character read.
   LOGICAL                                  :: WAS_DELIM !< Indicates that the last character read was a delimITER.

   WAS_DELIM = .FALSE.
   DTV%RAW = ''
   DO
      READ(UNIT, "(A)", IOSTAT=IOSTAT, IOMSG=IOMSG) CH
      IF (IS_IOSTAT_EOR(IOSTAT)) THEN
         IF (WAS_DELIM) THEN
           ! end of delimited string followed by end of record is end of the string. Pass back the
           ! end of record condition to the caller
           RETURN
         ELSE
           ! end of record without terminating delimiter - move along
           CYCLE
         ENDIF
      ELSEIF (IOSTAT /= 0) THEN
        RETURN
      ENDIF
      IF (CH == DELIM) THEN
         IF (WAS_DELIM) THEN
            ! doubled delimiter is one delimiter in the value
            DTV%RAW = DTV%RAW // CH
            WAS_DELIM = .FALSE.
         ELSE
            ! need to test next character to see what is happening
            WAS_DELIM = .TRUE.
         ENDIF
      ELSEIF (WAS_DELIM) THEN
         ! the previous character was actually the delimiter for the end of the string. Put back this character
         READ(UNIT, "(TL1)", IOSTAT=IOSTAT, IOMSG=IOMSG)
         RETURN
      ELSE
         DTV%RAW = DTV%RAW // CH
      ENDIF
   ENDDO
   ENDSUBROUTINE READ_DELIMITED

  SUBROUTINE READ_UNDELIMITED_LISTDIRECTED(DTV, UNIT, IOSTAT, IOMSG)
  !< Read an undelimited (no leading apostrophe or double quote) character value according to the rules for liST DIRECTED INPUT.
  !<
  !< A blank, comma/semicolon (depending on the decimal mode), slash or end of record terminates the string.
  !<
  !< If input is terminated by end of record, then this procedure returns an end-of-record condition.
  CLASS(STRING),    INTENT(INOUT) :: DTV           !< The string.
  INTEGER,          INTENT(IN)    :: UNIT          !< Logical unit.
  INTEGER,          INTENT(OUT)   :: IOSTAT        !< IO status code.
  CHARACTER(LEN=*), INTENT(INOUT) :: IOMSG         !< IO status message.
  LOGICAL                         :: DECIMAL_POINT !<True if DECIMAL=POINT in effect.

  CALL GET_DECIMAL_MODE(UNIT=UNIT, DECIMAL_POINT=DECIMAL_POINT, IOSTAT=IOSTAT, IOMSG=IOMSG)
  IF (IOSTAT /= 0) RETURN
  CALL DTV%READ_UNDELIMITED(UNIT=UNIT, TERMINATORS=' '//'/'//MERGE(CK_',', CK_';', DECIMAL_POINT), IOSTAT=IOSTAT, IOMSG=IOMSG)
  ENDSUBROUTINE READ_UNDELIMITED_LISTDIRECTED

  SUBROUTINE READ_UNDELIMITED(DTV, UNIT, TERMINATORS, IOSTAT, IOMSG)
  !< Read an undelimited string up until end of record or a character from a set of terminators is encountered.
  !<
  !< If a terminator is encountered, the file position will be at that terminating character. If end of record IS ENCOUNTERED, THE
  !< file remains at end of record.
  CLASS(STRING),             INTENT(INOUT) :: DTV         !< The string.
  INTEGER,                   INTENT(IN)    :: UNIT        !< Logical unit.
  CHARACTER(KIND=CK, LEN=*), INTENT(IN)    :: TERMINATORS !< Characters that are considered to terminate the sTRING.
                                                          !< Blanks in this string are meaningful.
  INTEGER,                   INTENT(OUT)   :: IOSTAT      !< IO status code.
  CHARACTER(LEN=*),          INTENT(INOUT) :: IOMSG       !< IO status message.
  CHARACTER(KIND=CK, LEN=1)                :: CH          !< A character read.

  DTV%RAW = ''
  DO
    READ(UNIT, "(A)", IOSTAT=IOSTAT, IOMSG=IOMSG) CH
    IF (IS_IOSTAT_EOR(IOSTAT)) THEN
      ! end of record just means end of string. We pass on the condition
      RETURN
    ELSEIF (IOSTAT /= 0) THEN
      ! something odd happened
      RETURN
    ENDIF
    IF (SCAN(CH, TERMINATORS) /= 0) THEN
      ! change the file position so that the next read sees the terminator
      READ(UNIT, "(TL1)", IOSTAT=IOSTAT, IOMSG=IOMSG)
      IF (IOSTAT /= 0) RETURN
      IOSTAT = 0
      RETURN
    ENDIF
    ! we got a character - append it
    DTV%RAW = DTV%RAW // CH
  ENDDO
  ENDSUBROUTINE READ_UNDELIMITED

   SUBROUTINE WRITE_FORMATTED(DTV, UNIT, IOTYPE, V_LIST, IOSTAT, IOMSG)
   !< Formatted output.
   CLASS(STRING),             INTENT(IN)    :: DTV       !< The string.
   INTEGER,                   INTENT(IN)    :: UNIT      !< Logical unit.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)    :: IOTYPE    !< Edit descriptor.
   INTEGER,                   INTENT(IN)    :: V_LIST(:) !< Edit descriptor list.
   INTEGER,                   INTENT(OUT)   :: IOSTAT    !< IO status code.
   CHARACTER(KIND=CK, LEN=*), INTENT(INOUT) :: IOMSG     !< IO status message.

   IF (ALLOCATED(DTV%RAW)) THEN
     WRITE(UNIT, "(A)", IOSTAT=IOSTAT, IOMSG=IOMSG)DTV%RAW
   ELSE
     WRITE(UNIT, "(A)", IOSTAT=IOSTAT, IOMSG=IOMSG)''
   ENDIF
   ENDSUBROUTINE WRITE_FORMATTED

   SUBROUTINE READ_UNFORMATTED(DTV, UNIT, IOSTAT, IOMSG)
   !< Unformatted input.
   !<
   !< @bug Change temporary acks: find a more precise length of the input string and avoid the trimming!
   CLASS(STRING),             INTENT(INOUT) :: DTV       !< The string.
   INTEGER,                   INTENT(IN)    :: UNIT      !< Logical unit.
   INTEGER,                   INTENT(OUT)   :: IOSTAT    !< IO status code.
   CHARACTER(KIND=CK, LEN=*), INTENT(INOUT) :: IOMSG     !< IO status message.
   CHARACTER(KIND=CK, LEN=100)              :: TEMPORARY !< Temporary storage string.

   READ(UNIT, IOSTAT=IOSTAT, IOMSG=IOMSG)TEMPORARY
   DTV%RAW = TRIM(TEMPORARY)
   ENDSUBROUTINE READ_UNFORMATTED

   SUBROUTINE WRITE_UNFORMATTED(DTV, UNIT, IOSTAT, IOMSG)
   !< Unformatted output.
   CLASS(STRING),             INTENT(IN)    :: DTV    !< The string.
   INTEGER,                   INTENT(IN)    :: UNIT   !< Logical unit.
   INTEGER,                   INTENT(OUT)   :: IOSTAT !< IO status code.
   CHARACTER(KIND=CK, LEN=*), INTENT(INOUT) :: IOMSG  !< IO status message.

   IF (ALLOCATED(DTV%RAW)) THEN
     WRITE(UNIT, IOSTAT=IOSTAT, IOMSG=IOMSG)DTV%RAW
   ELSE
     WRITE(UNIT, IOSTAT=IOSTAT, IOMSG=IOMSG)''
   ENDIF
   ENDSUBROUTINE WRITE_UNFORMATTED

   ! miscellanea
   ELEMENTAL FUNCTION REPLACE_ONE_OCCURRENCE(SELF, OLD, NEW) RESULT(REPLACED)
   !< Return a string with the first occurrence of substring old replaced by new.
   !<
   !< @note The doctest is not necessary, this being tested by [[string:replace]].
   CLASS(STRING),             INTENT(IN)  :: SELF      !< The string.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)  :: OLD       !< Old substring.
   CHARACTER(KIND=CK, LEN=*), INTENT(IN)  :: NEW       !< New substring.
   TYPE(STRING)                           :: REPLACED  !< The string with old replaced by new.
   INTEGER                                :: POS       !< Position from which replace old.

   IF (ALLOCATED(SELF%RAW)) THEN
      REPLACED = SELF
      POS = INDEX(STRING=SELF%RAW, SUBSTRING=OLD)
      IF (POS>0) THEN
         IF (POS==1) THEN
            REPLACED%RAW = NEW//SELF%RAW(LEN(OLD)+1:)
         ELSE
            REPLACED%RAW = SELF%RAW(1:POS-1)//NEW//SELF%RAW(POS+LEN(OLD):)
         ENDIF
      ENDIF
   ENDIF
   ENDFUNCTION REPLACE_ONE_OCCURRENCE

   ! non type-bound-procedures
   SUBROUTINE GET_DELIMITER_MODE(UNIT, DELIM, IOSTAT, IOMSG)
   !< Get the DELIM changeable connection mode for the given unit.
   !<
   !< If the unit is connected to an internal file, then the default value of NONE is always returned.
   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : IOSTAT_INQUIRE_INTERNAL_UNIT
   INTEGER,                   INTENT(IN)    :: UNIT         !< The unit for the connection.
   CHARACTER(LEN=1, KIND=CK), INTENT(OUT)   :: DELIM        !< Represents the value of the DELIM mode.
   INTEGER,                   INTENT(OUT)   :: IOSTAT       !< IOSTAT error code, non-zero on error.
   CHARACTER(*),              INTENT(INOUT) :: IOMSG        !< IOMSG explanatory message - only defined if iosTAT IS NON-ZERO.
   CHARACTER(10)                            :: DELIM_BUFFER !< Buffer for INQUIRE about DELIM, sized for APOSTROHPE.
   CHARACTER(LEN(IOMSG))                    :: LOCAL_IOMSG  !< Local variant of iomsg, so it doesn't get inappROPRIATELY REDEFINED.

   ! get the string representation of the changeable mode
   INQUIRE(UNIT, DELIM=DELIM_BUFFER, IOSTAT=IOSTAT, IOMSG=LOCAL_IOMSG)
   IF (IOSTAT == IOSTAT_INQUIRE_INTERNAL_UNIT) THEN
      ! no way of determining the DELIM mode for an internal file
      IOSTAT = 0
      DELIM = ''
      RETURN
   ELSEIF (IOSTAT /= 0) THEN
      IOMSG = LOCAL_IOMSG
      RETURN
   ENDIF
   ! interpret the DELIM string
   IF (DELIM_BUFFER == 'QUOTE') THEN
      DELIM = '"'
   ELSEIF (DELIM_BUFFER == 'APOSTROPHE') THEN
      DELIM = ''''
   ELSE
      DELIM = '"'
   ENDIF
   ENDSUBROUTINE GET_DELIMITER_MODE

   SUBROUTINE GET_NEXT_NON_BLANK_CHARACTER_THIS_RECORD(UNIT, CH, IOSTAT, IOMSG)
   !< Get the next non-blank character in the current record.
   INTEGER,                   INTENT(IN)    :: UNIT   !< Logical unit.
   CHARACTER(KIND=CK, LEN=1), INTENT(OUT)   :: CH     !< The non-blank character read. Not valid if IOSTAT is NON-ZERO.
   INTEGER,                   INTENT(OUT)   :: IOSTAT !< IO status code.
   CHARACTER(KIND=CK, LEN=*), INTENT(INOUT) :: IOMSG  !< IO status message.

   DO
      ! we spcify non-advancing, just in case we want this callable outside the context of a child input stateMENT
      ! the PAD specifier simply saves the need for the READ statement to define ch if EOR is hit
      ! read(unit, "(A)", iostat=iostat, iomsg=iomsg, advance='NO') ch
      ! ...but that causes ifort to blow up at runtime
      READ(UNIT, "(A)", IOSTAT=IOSTAT, IOMSG=IOMSG, PAD='NO') CH
      IF (IOSTAT /= 0) RETURN
      IF (CH /= '') EXIT
   ENDDO
   ENDSUBROUTINE GET_NEXT_NON_BLANK_CHARACTER_THIS_RECORD

   SUBROUTINE GET_NEXT_NON_BLANK_CHARACTER_ANY_RECORD(UNIT, CH, IOSTAT, IOMSG)
   !< Get the next non-blank character, advancing records if necessary.
   INTEGER,                   INTENT(IN)    :: UNIT        !< Logical unit.
   CHARACTER(KIND=CK, LEN=1), INTENT(OUT)   :: CH          !< The non-blank character read. Not valid if IOSTAT IS NON-ZERO.
   INTEGER,                   INTENT(OUT)   :: IOSTAT      !< IO status code.
   CHARACTER(KIND=CK, LEN=*), INTENT(INOUT) :: IOMSG       !< IO status message.
   CHARACTER(LEN(IOMSG))                    :: LOCAL_IOMSG !< Local variant of iomsg, so it doesn't get inapprOPRIATELY REDEFINED.

   DO
      CALL GET_NEXT_NON_BLANK_CHARACTER_THIS_RECORD(UNIT=UNIT, CH=CH, IOSTAT=IOSTAT, IOMSG=LOCAL_IOMSG)
      IF (IS_IOSTAT_EOR(IOSTAT)) THEN
         ! try again on the next record
         READ (UNIT, "(/)", IOSTAT=IOSTAT, IOMSG=IOMSG)
         IF (IOSTAT /= 0) RETURN
      ELSEIF (IOSTAT /= 0) THEN
         ! some sort of problem
         IOMSG = LOCAL_IOMSG
         RETURN
      ELSE
         ! got it
         EXIT
      ENDIF
   ENDDO
   ENDSUBROUTINE GET_NEXT_NON_BLANK_CHARACTER_ANY_RECORD

   SUBROUTINE GET_DECIMAL_MODE(UNIT, DECIMAL_POINT, IOSTAT, IOMSG)
   !< Get the DECIMAL changeable connection mode for the given unit.
   !<
   !< If the unit is connected to an internal file, then the default value of DECIMAL is always returned. This MAY NOT BE THE
   !< actual value in force at the time of the call to this procedure.
   USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : IOSTAT_INQUIRE_INTERNAL_UNIT
   INTEGER,                   INTENT(IN)    :: UNIT           !< Logical unit.
   LOGICAL,                   INTENT(OUT)   :: DECIMAL_POINT  !< True if the decimal mode is POINT, false otheRWISE.
   INTEGER,                   INTENT(OUT)   :: IOSTAT         !< IO status code.
   CHARACTER(KIND=CK, LEN=*), INTENT(INOUT) :: IOMSG          !< IO status message.
   CHARACTER(5)                             :: DECIMAL_BUFFER !< Buffer for INQUIRE about DECIMAL, sized for POINT OR COMMA.
   CHARACTER(LEN(IOMSG))                    :: LOCAL_IOMSG    !< Local iomsg, so it doesn't get inappropriatelY REDEFINED.

   INQUIRE(UNIT, DECIMAL=DECIMAL_BUFFER, IOSTAT=IOSTAT, IOMSG=LOCAL_IOMSG)
   IF (IOSTAT == IOSTAT_INQUIRE_INTERNAL_UNIT) THEN
      ! no way of determining the decimal mode for an internal file
      IOSTAT = 0
      DECIMAL_POINT = .TRUE.
      RETURN
   ELSE IF (IOSTAT /= 0) THEN
      IOMSG = LOCAL_IOMSG
      RETURN
   ENDIF
   DECIMAL_POINT = DECIMAL_BUFFER == 'POINT'
   ENDSUBROUTINE GET_DECIMAL_MODE
ENDMODULE ModLib_StringiforString
